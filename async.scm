(module async (new-ivar ivar-fill! ivar-read
               bind return
               peek 
               scheduler-start! scheduler-stop!
               (syntax: >>= bind return)
               (syntax: >>+ bind return)
               )
  (import scheme chicken)
  (use extras)
  (use srfi-69)
  (use srfi-18)

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
        (mutex-unlock! global-mutex))))

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
      (abort (make-expected-deferred-exn "Expected procedure.")))
    (let* ((i (deferred-ivar d))
           (i* (new-ivar))
           (f* (lambda (x)
                 (let* ((d* (f x))
                        (i** (deferred-ivar d*))
                        (f** (lambda (x)
                               (ivar-fill! i* x))))
                   (if (not (deferred? d*))
                     (abort (make-expected-deferred-exn "Expected deferred result.")))
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
                  (for-each
                    (lambda (i)
                      (for-each
                        (lambda (f)
                          (if (not current)
                            (exit))
                          (f (ivar-x i)))
                        (ivar-bound i))
                      (ivar-bound-set! i '()))
                    ready*))
                (if (null? ready)
                  (begin
                    (set! suspended #t)
                    (let loop ()
                      (if suspended
                        (begin
                          (unfunnel! condition: unsuspend-condition)
                          (funnel!)
                          (loop))))))))
            (unfunnel!)
            (loop))))))

  (define (scheduler-stop!)
    (funnel!)
    (if (not current)
      (begin
        (abort (make-not-scheduling-exn "No scheduler is currently running, so no scheduler can be stopped."))
        (unfunnel!)))
    (set! current #f)
    (unsuspend!)
    (unfunnel!))

  (define-syntax >>=
    (syntax-rules ()
      ((_ a b c ...)
       (>>= (bind a b)
            c ...))
      ((_ a)
       a)
      ((_) (return '()))))

  (define-syntax >>+
    (syntax-rules ()
      ((_ a ((x) b ...) c ...)
       (>>+ (bind a
                  (lambda (x)
                    b ...))
            c ...))
      ((_ a (() b ...) c ...)
       (>>+ a ((_) b ...) c ...))
      ((_ a b c ...)
       (>>+ a (() b) c ...))
      ((_ x) x)))
  )
