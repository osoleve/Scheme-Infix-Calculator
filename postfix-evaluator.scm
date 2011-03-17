


(set! %load-path (cons "/home/andy/Dropbox/Programming/Scheme" %load-path))

(for-each (lambda (file)
            (load-from-path file))
          '("shunting-yard.scm"))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (char->operator char)
  (cond ((eq? char #\+) '+)
        ((eq? char #\-) '-)
        ((eq? char #\*) '*)
        ((eq? char #\/) '/)
        ((eq? char #\%) 'remainder)
        ((eq? char #\!) 'factorial)
        ((eq? char #\^) 'expt)
        ((eq? char #\=) '=)
        ((eq? char #\<) '<)
        ((eq? char #\>) '>)))

(define (%postfix-eval eqn stack)
  (cond ((null? eqn) stack)
        ((number? (car eqn))
         (%postfix-eval (cdr eqn) (cons (car eqn) stack)))
        ((operator? (car eqn))
         ;; Factorial takes one argument, while all
         ;; other operators take two arguments.
         (if (not (eq? (car eqn) #\!))
             (%postfix-eval (cdr eqn) (cons (eval
                                             `(,(char->operator (car eqn))
                                               ,(cadr stack)
                                               ,(car stack))
                                             (interaction-environment))
                                            (cddr stack)))
             (%postfix-eval (cdr eqn) (cons (eval
                                             `(,(char->operator (car eqn))
                                               ,(car stack))
                                             (interaction-environment))
                                            (cdr stack)))))
        (else (%postfix-eval (cdr eqn) stack))))
  
(define (postfix-eval eqn)
  (%postfix-eval eqn '()))
