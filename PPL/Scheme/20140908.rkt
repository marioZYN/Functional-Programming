#lang racket

#|
1 Scheme

1.1 Multiple Apply (3 pts)
Define a procedure called multiple-apply which takes another procedure f, a natural number n and an item x, and applies f n times to x, i.e. it should return fn(x).

1.2 Position of Max (4 pts)
Define a procedure called position-of-max, that takes a list l and returns the position of l which contains the maximum value present in l.
E.g. (position-of-max ’(2 3 1 -2)) is 1.

1.3 Max of the Longest (6 pts)
Consider a definition of norm, where the norm of a number is the number itself, while the norm of a string is its length. Write a procedure called max-of-the-longest, that takes a list of lists, containing either strings or numbers, and returns the maximum norm of the elements in the longest of the lists.
E.g. (max-of-the-longest ’((99 0) (2 3 "hi, there!") (3 "hi there" 1 -1 -1))) is 8.

|#

(define (multiple-apply f n x)
  (define (rec f n res)
    (if (= n 0)
        res
        (rec f (- n 1) (f res))))
  (rec f n x))

(define (position-of-max l)
  (define max-val (apply max l))
  (define (rec l c)
    (if (= (car l) max-val)
        c
        (rec (cdr l) (+ c 1))))
  (rec l 0))

(define (max-of-the-longest lst)
  (let* ((l1 (map length lst))
         (pos (position-of-max l1))
         (l2 (list-ref lst pos)))
    (apply max
           (map (lambda (x)
                  (if (string? x)
                      (string-length x)
                      x))
                l2))))
         