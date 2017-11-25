(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (if (null? items)
      nil 
      (cons (proc (car items)) (map proc (cdr items)))
  )
)

(define (cons-all first rests)
  (map (lambda (x) (cons first x)) rests)
)

(define (zip pairs)
  (if (null? pairs) 
      nil 
      (list (cons (car (car pairs)) (car (zip (cdr pairs)))) (cons (car (cdr (car (pairs)))) (car (cdr (zip (cdr pairs))))))
  )
)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (enumerate-helper s u)
    (if (null? s)
        nil
        (cons (list u (car s)) (enumerate-helper (cdr s) (+ u 1)))
    )
  )
  (enumerate-helper s 0)
)
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
    ((or (= total 0) (null? denoms)) nil)
    ((= total (car denoms)) (cons (list (car denoms)) (list-change total (cdr denoms))))
    ((> total (car denoms)) (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
                                    (list-change total (cdr denoms))))
    (else (list-change total (cdr denoms)))
  )
)
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons form (cons params (map let-to-lambda body)))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons (cons 'lambda (cons (let-to-lambda (map (lambda (x) (car x)) values))
             (let-to-lambda body))) (let-to-lambda (map (lambda (x) (car (cdr x))) values)))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (if (pair? expr)
             (map let-to-lambda expr)
             expr
         )
         ; END PROBLEM 19
         )))
