#lang racket

(define (maki lst)
  (call/cc (lambda (exit)
             (for-each
              (lambda (x)
                (call/cc (lambda (yield)
                           (exit (cons x yield))
))) lst))))

(define (doit)
  (let ((x (maki '(a b c d))))
    (when (cons? x)
      (displayln (car x))
      ((cdr x)))))

#|
maki creates a sequence of operation using foreach. However in the first iteration, the procedure is stopped, and the continuation is stored in a pair ('a, continuation) where the continuation starts from the second interation of foreach.

|#

(define-syntax curried
  (syntax-rules ()
    ((_ (x) e1 ...)
     (lambda (x) e1 ...))
    ((_ (x1 x2 ...) e1 ...)
     (lambda (x1)
       (curried (x2 ...) e1 ...)))))

(define f (curried (x y) (+ x y)))