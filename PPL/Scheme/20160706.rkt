#lang racket

(define f1 (lambda (x) (+ 1 x)))
(define f2 (lambda (x) (* 2 x)))
(define f3 (lambda (x) (- x 10)))

(define (ftree fs xs)
  (cond
    [(null? fs) xs]
    [(list? fs) (cons (ftree (car fs) (car xs))
                      (ftree (cdr fs) (cdr xs)))]
    [else (fs xs)]))
                      
                             