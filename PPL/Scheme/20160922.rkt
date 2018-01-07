#lang racket

(define (<*> funcs elems)
  (define (rec fs es r)
    (if (null? fs)
        r
        (begin
          (let ((f (car fs)))
            (rec (cdr fs) es (append r (map f es)))))))
  (rec funcs elems '()))
              
     
(<*> (list (lambda (x) (+ 1 x)) (lambda (x) (* 2 x))) '(1 2 3))

(define (concatmap f lst)
  (foldr append '() (map f lst)))

(define (<*>2 fs es)
  (concatmap (lambda (f) (map f es)) fs))

(<*>2 (list (lambda (x) (+ 1 x)) (lambda (x) (* 2 x))) '(1 2 3))