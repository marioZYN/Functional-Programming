#lang racket

(define (sublists lst start end)
  (define (rec l i j k res)
    (if (> k j)
        (reverse res)
        (if (>= k i)
            (rec (cdr l) i j (+ 1 k) (cons (car l) res))
            (rec (cdr l) i j (+ 1 k) res))))
  (rec lst start end 0 '()))

(define (co-sublist lst start end)
  (define sublist (sublists lst start end))
  (define (rec l res)
    (if (null? l)
        (reverse res)
        (if (member (car l) sublist)
            (rec (cdr l) res)
            (rec (cdr l) (cons (car l) res)))))
  (rec lst '()))

(define <- '<-)
(define -> '->)

(define (subl . args)
  (let loop ((state #f)
             (res '())
             (ls args))
    (cond
      [(null? ls) res]
      [(eq? (car ls) ->) (loop #t res (cdr ls))]
      [(eq? (car ls) <-) (loop #f res (cdr ls))]
      [state (loop state (append res (list (car ls))) (cdr ls))]
      [(not state) (loop state res (cdr ls))])))
                  