(use-modules (jyc raisin))

(>>= (after 5)
     (lambda (_)
       (display "hi\n")
       (throw 'blah )
       (return '()))) 

(scheduler-start!)
