(include "async.scm")
(import async)

(use srfi-18)
(use posix)

(define i (new-ivar))
(define d (ivar-read i))

(write (peek d))
(newline)

(ivar-fill! i "hi")

(write (peek d))
(newline)

(printf "Starting...~%")
(>>= (after 1.5)
     (lambda (_) (printf "A (around 1.5 seconds after start)~%") (return "A"))
     (lambda (x) (printf "B (this should be A: ~A~%" x) (return '()))
     (lambda (_) (after 3.5))
     (lambda (_) (printf "C (around 5 seconds after start!)~%") (return '())))

;(seq (after 1.5)
;     x <* ((printf "A (around 1.5 seconds after start)~%") "A")
;     ** ((printf "B (this should be A: ~A~%" x))
;     (after 3.5)
;     ** ((printf "C (around 5 seconds after start!)~%")))

(define (time-after s)
  (seconds->time (+ s (time->seconds (current-time)))))

; sleep forever to prevent the scheduler from thinking we're deadlocked when everything is done...
(thread-start!
  (make-thread
    (lambda ()
      (let loop ()
        (thread-sleep! (time-after 100))
        (loop)))))

(set-signal-handler! signal/int
                     (lambda (sig)
                       (scheduler-stop!)))

(scheduler-start! on-stop: (lambda () (printf "Stopped.~%")))
