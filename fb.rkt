#lang racket

(require eopl/eopl)

(require "ast.rkt")

(require racket/set)

(provide
  free-vars
  free?)

;;; free-vars : ast? -> (set-of id?)
(define free-vars
  (lambda (a)
    (cases ast a
      [id-ref (id) (set id)]
      [function (i b) (let ([s (free-vars b)]) (if (set-member? s i) (set-remove s i) s))] 
      [app (a b) (set-union (free-vars a) (free-vars b))]
      )))  ; FIX THIS DEFINITION! 


;;; free? checks if id x is free in  ast a. 
;;; free?: [id?  ast?] -> boolean?
(define free?
  (lambda (id a)
    (if (set-member? (free-vars a) id) #t #f)
   )) ; FIX THIS DEFINITION!

(require rackunit)

(define set-empty (set))

;;; THESE WILL PASS ONLY AFTER YOU SUPPLY THE CORRECT
;;; DEFINITIONS ABOVE?

(check-equal? (free-vars (id-ref 'x)) (set 'x))
(check-equal? (free-vars (function 'x (id-ref 'x))) set-empty)
(check-equal?  (free? 'x (id-ref 'x)) #t)
(check-equal?  (free? 'x (function 'x (id-ref 'x))) #f)
(check-equal?  (free? 'y (function 'x (id-ref 'x))) #f)
(check-equal?  (free? 'y (function 'x (id-ref 'y))) #t)
(check-equal?  (free? 'y (function 'x (app (id-ref 'y) (id-ref 'x)))) #t)
(check-equal?  (free? 'x (function 'x (app (id-ref 'y) (id-ref 'x)))) #f)







  