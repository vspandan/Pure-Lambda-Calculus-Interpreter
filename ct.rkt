#lang racket
;;; =======================================
;;; Concrete Syntax for the LAMBDA CALCULUS
;;; =======================================

;;; canonical single-argument syntax
;;;
;;; <cexp> ::= 
;;;           <id> |
;;;           (lambda (<id>) <cexp>) |
;;;           (<cexp> <cexp>)


;;; extended multi-argument syntax
;;;
;;; <exp> ::= 
;;;           <id> |
;;;           (lambda (<id> <id> ...) <exp>) |
;;;           (<exp> <exp> ...)

(provide
  ct
  )
  
(require eopl/eopl)
(require "ast.rkt")

;;; ct: transforms an expression in the multi-argument
;;; lambda calculus into the canonical (i.e., curried)
;;; lambda calculus.  note that exp? and cexp? are types
;;; that (informally) denote the class of extended
;;; multi-argument lambda calculus expressions and the
;;; canonical lambda calculus expressions, respectively.

;;; ct: exp? -> cexp?
(define ct
  (lambda (d)
    (match d
      [(? id? x) x]
      
      [(list 'lambda (list (? id? x)) body)
       `(lambda (,x) ,(ct body))]

      ;; (lambda (x y ...) b) => (lambda (x) (lambda (y ...) b))
      [(list 'lambda (list (? id? x) (? id? more) ...) body)
       (ct `(lambda (,x) (lambda ,more ,body)))]

      [(list rator rand)
       `(,(ct rator) ,(ct rand))]

      ;;; (x y z ...) => ((x y) z ...)
      [(list rator rand more ...)
       (ct `((,rator ,rand) ,@more))]
      
      [_ (error 'ct "incorrect concrete syntax ~a" d)])))


(require rackunit)

(check-equal? (ct 'x) 'x "ct 01")
(check-equal? (ct '(x y)) '(x y) "ct 02")
(check-equal? (ct '(x y z)) '((x y) z) "ct 03")
(check-equal? (ct '(x (y z))) '(x (y z)) "ct 04")
(check-equal? (ct '(lambda (x) x)) '(lambda (x) x) "ct 05")
(check-equal? (ct '(lambda (x) (x y))) '(lambda (x) (x y)) "ct 06")
(check-equal? (ct '(lambda (x y) x)) '(lambda (x) (lambda (y) x)) "ct 07")
(check-equal? (ct '(lambda (x y) (x y z))) '(lambda (x) (lambda (y) ((x y) z))) "ct 08")
(check-exn exn:fail? (lambda () (ct '(x))) "ct 09")
(check-exn exn:fail? (lambda () (ct '())) "ct 10")
(check-exn exn:fail? (lambda () (ct '(lambda x (x y)))) "ct 11")
(check-exn exn:fail? (lambda () (ct '(lambda () (x y)))) "ct 12")
(check-exn exn:fail? (lambda () (ct '(lambda (x y) x (x y)))) "ct 13")
