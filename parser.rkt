#lang racket

(require eopl/eopl)
(require "ast.rkt")
(require "ct.rkt")

(provide 
 parse
 unparse)

;;; parse takes a cexp
;;; and returns an ast.
;;; parse : cexp? -> ast?
       
(define parse
  (lambda (d)
    (match d
      ((? id? d) (id-ref d))
      ((? list? d) (if (eq? (length d ) 1)
                       (first d)
                       (if (eq? (first d) 'lambda) 
                           (function (parse (second d)) (parse (third d))) 
                           (app (parse (first d)) (parse (second d))))))
      ))) ; FIX THIS DEFINITION!

;;; unit tests

(require rackunit)


;;; THESE WILL PASS ONLY AFTER YOU HAVE SUPPLIED THE CORRECT DEFINITIONS!
(check-equal? (parse 'x) (id-ref 'x) "parse 01")

(check-equal?
  (parse '(x y))
  (app (id-ref 'x) (id-ref 'y))
  "parse 02")

(check-equal?
  (parse '((x y) z))
  (app 
    (app (id-ref 'x) (id-ref 'y))
    (id-ref 'z))
  "parse 03")

(check-equal?
  (parse '(x (y z)))
  (app
    (id-ref 'x)
    (app
      (id-ref 'y)
      (id-ref 'z)))
  "parse 04")

(check-equal?
  (parse '(lambda (x) x))
  (function
    'x
    (id-ref 'x))
  "parse 05")

(check-equal?
  (parse '(lambda (x) (x y)))
  (function
    'x
    (app (id-ref 'x) (id-ref 'y)))
  "parse 06")

(check-equal?
  (parse '((lambda (x) (x y)) z))
  (app
    (function
      'x
      (app (id-ref 'x) (id-ref 'y)))
    (id-ref 'z))
  "parse 07")


;;; unparse takes an ast and returns an cexp
;;; unparse : ast? -> cexp?
(define unparse
  (lambda (a)
    (cases ast a
      [id-ref (id) id]
      [function (a b) (list 'lambda (list a) (unparse b) )]
      [app (a b) (list ( unparse a) (unparse b))]
      )))
     ;   FIX THIS DEFINITION!



;;; THESE WILL PASS ONLY AFTER YOU HAVE SUPPLIED THE CORRECT DEFINITIONS!
(check-equal?
 (unparse (id-ref 'x))
 'x
 "unparse 01")

(check-equal?
 (unparse (app (id-ref 'x) (id-ref 'y)))
 '(x y)
 "unparse 02")

(check-equal?
 (unparse (function 'x (id-ref 'x)))
 '(lambda (x) x)
 "unparse 03")

(check-equal?
 (unparse (function 'x (app (id-ref 'x) (id-ref 'y))))
 '(lambda (x) (x y))
 "unparse 04")

(check-equal?
 (unparse (app (function 'x (app (id-ref 'x) (id-ref 'y))) (id-ref 'z)))
 '((lambda (x) (x y)) z)
 "unparse 05")



