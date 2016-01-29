(define-module (jyc raisin example))
(load "../raisin.scm")
(include "../debug.scm")
(use-modules
  (jyc raisin)
  (srfi srfi-18)
  (srfi srfi-69)
  (ice-9 rdelim))

(define on-line-ht (make-hash-table))

(define (on-line s)
  (unless (hash-table-exists? on-line-ht s)
    (hash-table-set! on-line-ht s (new-ivar)))
  (ivar-read (hash-table-ref on-line-ht s)))

(thread-start! (make-thread scheduler-start!))

(>>= (on-line "a")
     (lambda (_) (on-line "b"))
     (lambda (_) (on-line "c"))
     (lambda (_)
       (print "a, b, and c entered.~%")
       (return '())))

(>>= (any (list (on-line "1") (on-line "2")))
     (lambda (x)
       (print "~a entered. Entering the other number will not cause anything to happen now.~%" x)
       (return '())))

(let loop ()
  (let ((s (read-line)))
    (when (hash-table-exists? on-line-ht s)
      (ivar-fill! (hash-table-ref on-line-ht s) s)
      (hash-table-delete! on-line-ht s)
      ; Yield so the scheduler can run. This is a limitation of CHICKEN's
      ; lightweight threads.
      ))
  (loop))
