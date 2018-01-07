#lang racket

#|
1 Scheme
1.1 Duplicates (7 pts)

Define a procedure called rep which takes a list L of elements and returns a list of the elements of L that are repeated at least twice. The procedure must have linear time complexity, and it can be imperative and use imperative data structures.
E.g. (rep ’(3 2 "hi" 2 "hello" hello "hi")) is (2 "hi").

1.2 Duplicates, functional version (6 pts)

Define a purely functional version of rep without any limits of time complexity

|#

(define (rep L)
  (let ((h (make-hash))
        (out '()))
    (for-each (lambda (x)
                (hash-set! h x (+ 1 (hash-ref h x 0))))
              L)
    (hash-for-each h
                   (lambda (el n)
                     (when (> n 1)
                       (set! out (cons el out)))))
    out))

(define (rep2 l)
  (define (rec l1 l2 res)
    (if (null? l1)
        res
        (let* ((e (car l1))
               (lst (filter (lambda (x) (eq? x e)) l2)))
          (if (and (>= (length lst) 2) (not (member e res)))
              (rec (cdr l1) l2 (cons e res))
              (rec (cdr l1) l2 res)))))
  (rec l l '()))
                       