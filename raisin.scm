(define-module (jyc raisin)
  #:replace (peek bind async)
  #:export (new-ivar ivar-fill! ivar-read
            upon bind return 
            scheduler-start! scheduler-stop!
            any all

            never after

            >>=
            async
            >>$ seq

            ?..
            !!))

(use-modules
  (srfi srfi-69)
  (srfi srfi-9)
  (srfi srfi-9 gnu)
  (srfi srfi-26)
  (ice-9 format)
  (ice-9 match)
  (ice-9 control)
  (ice-9 threads))

#;(begin
  (include "debug.scm")
  (define-syntax debug
    (syntax-rules ()
      ((_ fmt arg ...)
       (print fmt arg ...))))) 

(define-syntax debug
  (syntax-rules ()
    ((_ fmt arg ...)
     #t)))

;;; `(define-record name field ...)` is shorthand for:
;;; ```
;;; (define-record-type <name>
;;;   (make-<name> field ...)
;;;   name?
;;;   (field name-field name-field-set!)
;;;   ...)
;;; ```
;;; The shorthand is from CHICKEN.
(define-syntax define-record
  (lambda (x)
    (syntax-case x ()
      ((_ name field ...)
       (match-let* (((_ name* . fields*) (syntax->datum x))
                    (% (lambda (fmt . args) (datum->syntax x (string->symbol (apply format #f fmt args)))))
                    (tname (% "<~a>" name*))
                    (constructor (% "make-~a" name*))
                    (predicate (% "~a?" name*))
                    (getter (lambda (field*) (% "~a-~a" name* field*)))
                    (setter (lambda (field*) (% "~a-~a-set!" name* field*)))
                    (field-entries (map (lambda (field*) `(,(datum->syntax x field*) ,(getter field*) ,(setter field*))) fields*)))
         #`(define-record-type #,tname
             (#,constructor field ...)
             #,predicate
             #,@field-entries))))))

(define mutex (make-recursive-mutex))
(define ready '())
(define unsuspend-condition (make-condition-variable))
(define suspended #f)
(define current #f)

(define* (funnel!)
  (lock-mutex mutex))

(define* (unfunnel! #:key condition)
  (if condition
      (begin
        (debug "unfunnel!: waiting for condition...\n")
        (unlock-mutex mutex condition)
        (debug "unfunnel!: waited\n"))
      (unlock-mutex mutex)))

(define (unsuspend!)
  (set! suspended #f)
  (debug "unsuspend!: unsuspending... ~a\n" unsuspend-condition)
  (broadcast-condition-variable unsuspend-condition)
  (debug "unsuspend!: broadcasted...\n")
  )

(define-record ivar
  name      ; uninterned symbol
  x	    ; the value the ivar becomes determined to, initially #f
  filled    ; boolean
  bound     ; procedure list
  )

(define-record deferred
  name      ; uninterned symbol
  ivar
  )

(set-record-type-printer! <ivar>
  (match-lambda*
    ((out ($ <ivar> name x filled bound))
     (format out "#<~A>" (ivar-name x)))))

(set-record-type-printer! <deferred>
  (match-lambda
    ((out ($<deferred> name ivar))
     (format out "#<~A of ~A>" (deferred-name x) (ivar-name (deferred-ivar x)))))) 

(define (new-ivar)
  (make-ivar (gensym "ivar") #f #f '()))

(define (ivar-fill! i x)
  (funnel!)
  (if (ivar-filled i)
      (throw 'raisin:ivar-stuffed "Ivar [i] was already filled."))
  (ivar-x-set! i x)
  (ivar-filled-set! i #t)
  (set! ready (cons i ready))
  (if suspended
      (unsuspend!))
  (unfunnel!))

(define (ivar-read i)
  (make-deferred (gensym "deferred") i))

(define (upon d f)
  (let ((i (deferred-ivar d)))
    (funnel!)
    (ivar-bound-set! i (cons f (ivar-bound i)))
    (if (ivar-filled i)
        (set! ready (cons i ready)))
    (unfunnel!)))

(define (bind d f)
  (if (not (procedure? f))
      (throw 'raisin:expected-proc "Expected procedure."))
  (if (not (deferred? d))
      (throw 'raisin:expected-deferred "Expected deferred to bind to."))
  (let ((i (new-ivar)))
    (upon d
          (lambda (x)
            (let ((d* (f x)))
              (if (not (deferred? d*))
                  (throw 'raisin:expected-deferred "Expected deferred result."))
              (upon d* (cut ivar-fill! i <>)))))
    (ivar-read i)))

(define (return x)
  (let ((i (new-ivar)))
    (ivar-fill! i x)
    (ivar-read i)))

(define (peek d)
  (let ((i (deferred-ivar d)))
    (if (ivar-filled i)
        (cons 'filled (ivar-x i))
        'empty)))

(define* (scheduler-start! #:key on-stop)
  (let ((this (gensym)))
    (funnel!)
    (if current
        (throw 'raisin:already-scheduling "A scheduler is already running.")
        (set! current this))
    (let loop ()
      (debug "scheduler-start!: looping\n")
      (if (not (and current (eq? current this)))
          ;; We're no longer the current scheduler (we've been stopped /or someone else is running).
          ;; Shut down.
          (begin
            (debug "scheduer-start!: stopped\n")
            (unfunnel!)
            (if on-stop (on-stop)))
          ;; Still running!
          (begin
            ;; Run procedures bound to things in `ready`.
            ;; Copy to ready* beacuse bound procedures might themselves make
            ;; things ready.
            (let/ec exit
              (let ((ready* ready))
                (set! ready '())
                (for-each
                  (lambda (i)
                    (for-each
                      (lambda (f)
                        (f (ivar-x i))
                        (unless (and current (eq? current this))
                          (exit #t)))
                      (ivar-bound i))
                    (ivar-bound-set! i '()))
                  ready*)))
            ;; Suspend ourselves if necessary (nothing is ready, but other
            ;; things running on other threads might become ready).
            ;; It's possible things we just ran might have made more things
            ;; ready, in which case we don't need to suspend at all.
            (if (not (and current (eq? current this)))
                (unfunnel!)
                (begin
                  (when (null? ready)
                    (set! suspended #t)
                    (let wait ()
                      (when suspended
                        (debug "scheduler-start!: suspending ~a\n" unsuspend-condition)
                        (unfunnel! #:condition unsuspend-condition)
                        (debug "scheduler-start!: unsuspended\n")
                        (funnel!)
                        (wait))))
                  (loop))))))))

(define (scheduler-stop!)
  (funnel!)
  (unless current
    (throw 'raisin:not-scheduling "No scheduler is currently running, so no scheduler can be stopped.")
    (unfunnel!))
  (set! current #f)
  (set! ready '())
  (debug "scheduler-stop!: unsuspending!\n")
  (unsuspend!)
  (debug "scheduler-stop!: unsuspended.\n")
  (unfunnel!))

(define (any ds)
  (let ((i (new-ivar))
        (done #f))
    (for-each
      (lambda (d)
        (bind d
              (lambda (x)
                (when (not done)
                  (set! done #t)
                  (ivar-fill! i x))
                (return '()))))
      ds)
    (ivar-read i)))

(define (all ds)
  (if (null? ds)
      (return '())
      (let ((d (car ds))
            (ds (cdr ds)))
        (bind d
              (lambda (x)
                (bind (all ds)
                      (lambda (y)
                        (return (cons x y)))))))))
(define-syntax >>=
  (syntax-rules ()
    ((_ a b c ...)
     (>>= (bind a b)
          c ...))
    ((_ a)
     a)))

(define-syntax async
  (syntax-rules ()
    ((_ body ...)
     (let* ((i (new-ivar))
            (d (ivar-read i))
            (f (lambda ()
                 (catch #t
                   (lambda ()
                     (let ((x (begin body ...)))
                       (debug "async: filling ivar\n")
                       (ivar-fill! i x)
                       (debug "async: filled\n")))
                   (lambda (e . params)
                     ;; The default thread error handler will not do anything as of Guile 2.0.2.0.
                     (backtrace)
                     (format #t "ERROR: Throw to key `~a' with args `~a'~%~" e params))))))
       (debug "async: starting thread\n")
       (begin-thread (f))
       (debug "async: done\n")
       d))))

(define-syntax >>$
  (syntax-rules ()
    ((_ (d f ...) ...)
     (let ((i (new-ivar))
           (done #f))
       (bind d
             (lambda (x)
               (when (not done)
                 (set! done #t)
                 (bind (>>= (return x)
                            f ...)
                       (lambda (x)
                         (ivar-fill! i x)
                         (return '()))))
               (return '())))
       ...
       (ivar-read i)))))

(define-syntax seq
  (syntax-rules (<- <* _ **)
    ((_ x <- d)
     d)
    ((_ x <- d rest ...)
     (bind d (lambda (x) (seq rest ...))))
    ((_ _ <- d rest ...)
     d)
    ((_ _ <- d rest ...)
     (bind d (lambda (ignored) (seq rest ...))))
    ((_ x <* (d ...))
     (return (begin d ...)))
    ((_ x <* (d ...))
     (return (begin d ...)))
    ((_ x <* (d ...) rest ...)
     (bind (return (begin d ...)) (lambda (x) (seq rest ...))))
    ((_ _ <* (d ...))
     (return (begin d ...)))
    ((_ ** (d ...) rest ...)
     (bind (return (begin d ...)) (lambda (_) (seq rest ...))))
    ((_ d)
     d)
    ((_ d rest ...)
     (bind d (lambda (_) (seq rest ...))))
    ((_)
     (return '()))))

(define (never)
  (ivar-read (new-ivar)))

(define (after s)
  (async
    (debug "here\n")
    (usleep (inexact->exact (truncate (* s 1000000))))
    '()))

(define async-prompt-tag (make-prompt-tag))

(define in-async-prompt (make-fluid #f))

(define (!! d)
  (if (not (fluid-ref in-async-prompt))
      (throw 'raisin:!!-called-but-not-in-async-prompt "!! was called outside of a ?.. block."))
  (abort-to-prompt async-prompt-tag d))

(define (async-prompt-handler k d)
  (fluid-set! in-async-prompt #f)
  (>>= d
       (lambda (x)
         (call-with-prompt async-prompt-tag
           (lambda ()
             (fluid-set! in-async-prompt #t)
             (k x))
           async-prompt-handler))))

(define-syntax ?..
  (syntax-rules ()
    ((_ e ...)
     (let ((i (new-ivar)))
       (>>= (return #t)
            (lambda (_)
              (call-with-prompt async-prompt-tag
                (lambda ()
                  (fluid-set! in-async-prompt #t)
                  (let ((x (begin e ...)))
                    (fluid-set! in-async-prompt #f)
                    (return x)))
                async-prompt-handler)))))))
