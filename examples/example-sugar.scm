(define-module (jyc raisin example))
(load "../raisin.scm")
(include "../debug.scm")
(use-modules
  (jyc raisin)
  (srfi srfi-18))

(>>= (?..
       (display "waiting one second.\n")
       (!! (after 1))
       (display "waiting another second...\n")
       (!! (after 1))
       (display "waited! we can even do short-circuit evaluation?!\n")
       (or (!! (return #t)) (!! (>>= (after 5) (lambda (_) (display "I SHOULDN'T BE SEEN\n") (return #f)))))
       (display "very cool! the last expression should be a non-deferred.\n")
       5)
     (lambda (_)
       (display "all done.\n")
       (scheduler-stop!)
       (return '())))

(scheduler-start!)
