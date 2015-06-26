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

(define (time-after s)
  (seconds->time (+ s (time->seconds (current-time)))))

(define (after s)
  (let* ((i (new-ivar))
        (deadline (time-after s))
        (thunk (lambda ()
                 (thread-sleep! deadline)
                 (ivar-fill! i '()))))
    (thread-start! (make-thread thunk))
    (ivar-read i)))

(printf "Starting...~%")
(>>= (after 1.5)
     (lambda (_) (printf "A (around 1.5 seconds after start)~%") (return "A"))
     (lambda (x) (printf "B (this should be A: ~A~%" x) (return '()))
     (lambda (_) (after 3.5))
     (lambda (_) (printf "C (around 5 seconds after start!)~%") (return '())))

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
