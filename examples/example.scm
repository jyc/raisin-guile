(define-module (jyc raisin example))
(load "../raisin.scm")
(include "../debug.scm")
(use-modules
  (jyc raisin)
  (srfi srfi-18))

(define i (new-ivar))
(define d (ivar-read i))

(write (peek d))
(newline)

(ivar-fill! i "hi")

(write (peek d))
(newline)

(define (print . args)
  (display (apply format #f args)))

(print "Starting...~%")
(>>= (after 1.5)
     (lambda (_) (print "A (around 1.5 seconds after start)~%") (return "A"))
     (lambda (x) (print "B (this should be A: ~A~%" x) (return '()))
     (lambda (_) (after 3.5))
     (lambda (_) (print "C (around 5 seconds after start!)~%") (return '())))

;(seq (after 1.5)
;     x <* ((print "A (around 1.5 seconds after start)~%") "A")
;     ** ((print "B (this should be A: ~A~%" x))
;     (after 3.5)
;     ** ((print "C (around 5 seconds after start!)~%")))

; Need to run this in a separate thread or Guile will run us in the same thread
; as the Raisin scheduler. The Raisin scheduler might be blocked on a
; condition, which it doesn't seem we can signal when we are on the same
; thread.
(call-with-new-thread
  (lambda ()
    (sigaction SIGINT
      (lambda (sig)
        (scheduler-stop!)))

    (let loop ()
      (sleep 1)
      (loop))))

(scheduler-start! #:on-stop (lambda () (print "Stopped.~%")))    
