#lang racket

(define (my-map f lst)
  (foldr (lambda (a b)
           (cons (f a) b))
         '()
         lst))

(define (my-filter p lst)
  (foldr (lambda (a b)
           (if (p a)
               (cons a b)
               b))
         '()
         lst))

(define (cos-min i j)
(if (= i j) j
      (let ((k (cos-min (+ i 1) j)))
        (if (< (cos i) (cos k))
i
k))))

(define (cos-min-tail i j)
  (define (rec i j res)
    (if (= i j)
        res
        (if (< (cos i) (cos res))
            (rec (+ i 1) j i)
            (rec (+ i 1) j res))))
  (rec i j i))
           

