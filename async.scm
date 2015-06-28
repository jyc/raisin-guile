(module async (new-ivar ivar-fill! ivar-read
               bind return
               peek 
               scheduler-start! scheduler-stop!

               after

               (syntax: >>= bind return)
               (syntax: seq bind)
               (syntax: async bind)
               )
  (import scheme chicken)
  (use extras)
  (use srfi-18 srfi-69)

  (define-syntax define-deferred-exn
    (ir-macro-transformer
      (lambda (x i c)
        (if (not (symbol? (cadr x)))
          (abort "Expected (define-deferred-exn name)."))
        `(define (,(i (string->symbol (format "make-~A-exn" (i (cadr x))))) message . fargs)
           (make-composite-condition
             (make-property-condition 'exn 'message (apply format message fargs))
             (make-property-condition 'deferred)
             (make-property-condition (quote ,(i (cadr x)))))))))

  (define-deferred-exn ivar-stuffed)
  (define-deferred-exn expected-deferred)
  (define-deferred-exn expected-proc)
  (define-deferred-exn unexpected-lock)
  (define-deferred-exn unexpected-nesting)
  (define-deferred-exn already-scheduling)
  (define-deferred-exn not-scheduling)

  (define global-mutex (make-mutex))
  (define global-mutex-nesting 0)
  (define ready '())
  (define unsuspend-condition (make-condition-variable))
  (define suspended #f)
  (define current #f)

  (define (funnel! #!key (assert-only #f))
    (if (not (eq? (mutex-state global-mutex) (current-thread)))
      (mutex-lock! global-mutex)
      (if assert-only
        (abort (make-unexpected-lock-exn "Mutex was unexpectedly locked."))))
    (set! global-mutex-nesting (+ global-mutex-nesting 1)))

  (define (unfunnel! #!key (condition #f))
    (set! global-mutex-nesting (- global-mutex-nesting 1))
    (if (= global-mutex-nesting 0)
      (if condition
        (mutex-unlock! global-mutex condition)
        (mutex-unlock! global-mutex))
      (if condition
        (abort (make-unexpected-nesting-exn "Mutex lock was unexpectedly nested.")))))

  (define (unsuspend!)
    (set! suspended #f)
    (condition-variable-broadcast! unsuspend-condition))

  (define-record ivar
    x
    filled
    bound
    )

  (define-record deferred
    ivar
    )

  (define (new-ivar)
    (make-ivar #f #f '())
    )

  (define (ivar-fill! i x)
    (funnel!)
    (if (ivar-filled i)
      (abort (make-ivar-stuffed-exn "Ivar [i] was already filled.")))
    (ivar-x-set! i x)
    (ivar-filled-set! i #t)
    (set! ready (cons i ready))
    (if suspended
      (unsuspend!))
    (unfunnel!))

  (define (ivar-read i)
    (make-deferred i))

  (define (bind* i f)
    (ivar-bound-set! i (cons f (ivar-bound i)))
    (if (ivar-filled i)
      (set! ready (cons i ready))))

  (define (bind d f)
    (if (not (procedure? f))
      (abort (make-expected-proc-exn "Expected procedure.")))
    (let* ((i (deferred-ivar d))
           (i* (new-ivar))
           (f* (lambda (x)
                 (let* ((d* (f x))
                        (i** (if (not (deferred? d*))
                               (abort (make-expected-deferred-exn "Expected deferred result."))
                               (deferred-ivar d*)))
                        (f** (lambda (x)
                               (ivar-fill! i* x)))) 
                   (bind* i** f**)))))
      (bind* i f*)
      (ivar-read i*)))

  (define (peek d)
    (let ((i (deferred-ivar d)))
      (if (ivar-filled i)
        (cons 'filled (ivar-x i))
        'empty)))

  (define (return x)
    (let ((i (new-ivar)))
      (ivar-fill! i x)
      (ivar-read i)))

  (define-syntax >>=
    (syntax-rules ()
      ((_ a b c ...)
       (>>= (bind a b)
            c ...))
      ((_ a)
       a)))

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

  (define-syntax async
    (syntax-rules ()
      ((_ body ...)
       (let* ((i (new-ivar))
              (d (ivar-read i))
              (f (lambda ()
                   (let ((x (begin body ...)))
                     (ivar-fill! i x)))))
         (thread-start! (make-thread f))
         d))))

  (define (time-after s)
    (seconds->time (+ s (time->seconds (current-time)))))

  (define (after s)
    (async
      (thread-sleep! (time-after s))
      '()))

  (define (scheduler-start! #!key (on-stop (lambda () #t)))
    (let ((this (gensym)))
      (funnel!)
      (if current
        (abort (make-already-scheduling-exn "A scheduler is already running."))
        (set! current this))
      (unfunnel!)
      (let loop ()
        (funnel! assert-only: #t)
        (if (not (and current (eq? current this)))
          (begin
            (unfunnel!)
            (on-stop))
          (begin
            (call/cc 
              (lambda (exit)
                (let ((ready* ready))
                  (set! ready '())
                  (for-each (lambda (i)
                              (for-each
                                (lambda (f)
                                  (unfunnel!)

                                  (f (ivar-x i))

                                  (funnel! assert-only: #t)
                                  (if (not (and current (eq? current this)))
                                    (exit #t)))
                                (ivar-bound i))
                              (ivar-bound-set! i '()))
                            ready*))))
            (if (not (and current (eq? current this))) 
              (unfunnel!)
              (begin
                (if (null? ready)
                  (begin
                    (set! suspended #t)
                    (let wait ()
                      (if suspended
                        (begin
                          (unfunnel! condition: unsuspend-condition)
                          (funnel! assert-only: #t)
                          (wait))))))
                (unfunnel!)
                (loop))))))))

  (define (scheduler-stop!)
    (funnel!)
    (if (not current)
      (begin
        (abort (make-not-scheduling-exn "No scheduler is currently running, so no scheduler can be stopped."))
        (unfunnel!)))
    (set! current #f)
    (set! ready '())
    (unsuspend!)
    (unfunnel!))

  )
