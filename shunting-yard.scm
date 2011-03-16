;;; For Dijkstra's Shunting-yard Algorithm

;; Determines if a given token from a string is a mathematical operator
(define (operator? symbol)
  (member symbol '(#\+ #\- #\* #\/ #\% #\^ #\! #\= #\< #\>)))

;; Determines the associativity of a given mathematical operator
(define (associativity-of operator)
  (if (member operator '(#\+ #\- #\* #\/ #\%))
      'left
      'right))

;; Determines the precedence of a given mathematical operator
(define (precedence-of operator)
  (case operator
    ((#\= #\< #\>) 1)
    ((#\+ #\-)     2)
    ((#\* #\/ #\%) 3)
    ((#\^ #\!)     4)
    (else          5)))

;; Returns the last element of a list
(define (last lst)
  (if (null? (cdr lst))
      lst
      (last (cdr lst))))

;; Is the character provided a numerical digit?
(define (digit-char? char)
  (> 10
     (- (char->integer char) (char->integer #\0))
     (- 1)))

;; Actions to take if the token in the stmt is an operator
(define (operator-actions stmt stack)
  (let* ((token-precedence (precedence-of (car stmt)))
         (token-assoc (associativity-of (car stmt)))
         (stack-oper (if (not (null? stack))
                         (car stack)
                         '()))
         (stack-precedence (if (not (null? stack-oper))
                               (precedence-of stack-oper)
                               0)))
    (cond ((or (and (eq? token-assoc 'left)
                    (<= token-precedence stack-precedence))
               (and (eq? token-assoc 'right)
                    (< token-precedence stack-precedence)))
           (cons stack-oper (%shunting-yard stmt (cdr stack))))
          (else (%shunting-yard (cdr stmt) (cons (car stmt) stack))))))

;; Actions to take if (null? stmt)
(define (stack-operations stack)
  ;; If a left-parenthesis is found on the stack,
  ;; it means there was no right-parenthesis to match it
  ;; and thus the statement has unbalanced parentheses.
  (cond ((and (not (null? stack))
              (eq? (car stack) #\())
         (display "Unbalanced parenthesis"))
        ((null? stack) '())
        (else (cons (car stack) (%shunting-yard '() (cdr stack))))))

;; Implementation of Dijkstra's Shunting-yard Algorithm
(define (%shunting-yard stmt stack)
  "Converts infix-notation mathematical equations into
postfix-notation mathematical equations, using an
implementation of Dijkstra's Shunting-yard Algorithm."
  (define (parse-stack lst)
    (cond ((null? lst)
           (display "Unbalanced parenthesis")
           '())
          ((not (eq? (car lst) #\())
           (cons (car lst) (parse-stack (cdr lst))))
          ((eq? (car lst) #\()
           '())
          (else '())))
  (define (strip-stack)
    (if (member (last (parse-stack stack)) stack)
        (member (last (parse-stack stack)) stack)
        '()))
  (cond ((null? stmt)
         (stack-operations stack))
        ((digit-char? (car stmt))
         (cons (car stmt) (%shunting-yard (cdr stmt) stack)))
        ((operator? (car stmt))
         (operator-actions stmt stack))
        ((eq? (car stmt) #\()
         (%shunting-yard (cdr stmt) (cons (car stmt) stack)))
        ((eq? (car stmt) #\))     
         (cons (parse-stack stack)
               (%shunting-yard (cdr stmt) (strip-stack))))))

(define (shunting-yard stmt)
  (%shunting-yard stmt '()))
