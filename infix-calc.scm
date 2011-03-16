

(set! %load-path (cons "/home/andy/Dropbox/Programming/Scheme" %load-path))

(for-each (lambda (file)
            (load-from-path file))
          '("shunting-yard.scm"
            "rd-parser.scm"
            "postfix-evaluator.scm"))

(define (infix-calc eqn)
  (postfix-eval (shunting-yard eqn)))

(define (calc eqn)
  (if (parse (string->list eqn))
      (infix-calc (string->list eqn))
      (display "Error: Malformed equation. Unable to compute.\n")))
