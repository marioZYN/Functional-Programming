#lang racket

#|
1.1 Split (4 points)

Define a tail-recursive function split, which, given a list L and a number n, returns a vector of two elements: the prefix sublist of L, with elements up to the n-th, and the remaining suffix.
E.g.
> (split '(0 1 2 3 4) 2)
’#((0 1) (2 3 4))

|#

(define (split lst n)
  (define (rec l1 l2 n)
    (if (= n 0)
        (vector l1 l2)
        (rec (append l1 (list (car l2))) (cdr l2) (- n 1))))
  (rec '() lst n))

#|
1.2 Factors (6 points)

Use split to define the function 3-factors, which, given a list L, returns all the possible contiguous sublists A, B, C, such that (equal? L (append A B C)). A,B,C, cannot be empty.

E.g.
> (3-factors '(0 1 2 3 4))
’(((0 1 2) (3) (4))
  ((0 1) (2 3) (4))
  ((0 1) (2) (3 4))
  ((0) (1 2 3) (4))
  ((0) (1 2) (3 4))
  ((0) (1) (2 3 4)))
|#

(define (3-factors lst)
  (define res '())
  (let loop1 ((i 1))
    (if (> i (- (length lst) 2))
        res
        (let ((first (vector-ref (split lst i) 0))
              (second (vector-ref(split lst i) 1)))
          
          (let loop2 ((j 1))
            (if (> j (- (length second) 1))
                (loop1 (+ 1 i))
                 (let* ((s (split second j))
                       (f1 (vector-ref s 0))
                       (f2 (vector-ref s 1))
                       (v1 (cons (list f1) (list f2)))
                       (v (cons first v1)))
                   (set! res (cons v res))
                   (loop2 (+ 1 j)))))))))
