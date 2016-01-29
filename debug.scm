;; Apparently Guile 2.2 will have better synchronization for ports.
;; For now if you try to print to the same port from multiple threads without
;; synchronization, you will get bad errors.
(define print-mutex (make-mutex))

(define (print . args)
  (lock-mutex print-mutex)
  (display (apply format #f args))
  (unlock-mutex print-mutex))
