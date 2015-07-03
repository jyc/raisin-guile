(module async-extras (>>$
                      any
                      all
                      after
                      (syntax: seq bind)
                      )
  (import scheme chicken)
  (use srfi-18)
  (use async)

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
         ...))))

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


  (define (time-after s)
    (seconds->time (+ s (time->seconds (current-time)))))

  (define (after s)
    (async
      (thread-sleep! (time-after s))
      '()))

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
  )
