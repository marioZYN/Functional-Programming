#lang racket

(struct promise
  (proc
   value?
   ) #:mutable)

(define-syntax delay
  (syntax-rules ()
    ((_ (expr ...))
     (promise (lambda ()
                (expr ...))
              #f))))

(define (force prom)
  (cond
    [(not (promise? prom)) prom]
    [(promise-value? prom) (promise-proc prom)]
    [else
     (set-promise-proc! prom ((promise-proc prom)))
     (set-promise-value?! prom #t)
     (promise-proc prom)]))

(define (iterate f v)
(delay (cons v (iterate f (f v)))))

(define (take n prom)
  (if (= 0 n)
      '()
      (let ((v (force prom)))
        (cons (car v) (take (- n 1) (cdr v))))))

