(include "raisin.scm")
(import raisin)

; Branching >>=.
(>>$ ((return 0)
      (lambda (_)
        (print "This should be printed!")
        (return '())))
     ((after 1)
      (lambda (_)
        (print "This should never be printed.")
        (return '()))))

(bind (any (list 
             (return 'success)
             (after 3)
             (after 7)))
      (lambda (x)
        (printf "This should be success: ~A~%" x)
        (return '())))

(bind (all (list
             (return 4)
             (return 8)
             (return 15)))
      (lambda (x)
        (printf "This should be (4 8 15): ~A~%" x)
        (return '())))

(>>= (after 2)
     (lambda (_)
       (scheduler-stop!)
       (return '())))

(scheduler-start!)
