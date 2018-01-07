#lang racket
#|
1 Scheme
A program collects data from different nodes of the network and put them in a list containing elements of different types - we call this list “unsorted”. E.g. (3 "bob" #(6 6 1) 4 #(1 2) -2 end 9).
We want to take from the unsorted list all the elements that are numbers or vectors: the numbers are summed, while the vectors are collected in another list (it is not necessary to maintain the order of the original list). To memorize the data, we introduce a structure called demuxed, that has two fields named num and vec. E.g. for the previous case: 5 and (#(6 6 1) #(1 2)), respectively.

1.1 Imperative (5 pts)
Define the demuxed data strucure, and a procedure (called demux-imperative) that has two parameters d and l. d is a demuxed data strucure, while l is an unsorted list. This procedure must update d with data from l, stopping when the element end is found, if present.

1.2 Tail recursive (6 pts)
Assume that demuxed is immutable. Define a functional, tail-recursive procedure demux-tail-rec that takes an unsorted list and returns a demuxed data structure containing the data, processed as in before. You may use as many additional parameters as you need, but you must specify their initial value.
|#

(struct demuxed
  (num
   vec) #:mutable)

(define origin (demuxed 0 '()))
(define lst '(3 "bob" #(6 6 1) 4 #(1 2) -2 end 9))

(define (demux-imperative d l)
  (call/cc (lambda (exit)
             (for-each (lambda (x)
                         (cond
                           [(eq? x 'end) (exit d)]
                           [(number? x) (set-demuxed-num! d (+ x (demuxed-num d)))]
                           [(vector? x) (set-demuxed-vec! d (cons x (demuxed-vec d)))]))
                       l))))

(define (demux-tail-rec l)
  (define (rec lst num vec)
    (if (null? lst)
        (demuxed num vec)
        (let ((cur (car lst)))
          (if (eq? cur 'end)
              (demuxed num vec)
              (cond
                [(number? cur) (rec (cdr lst) (+ num cur) vec)]
                [(vector? cur) (rec (cdr lst) num (cons cur vec))]
                [else (rec (cdr lst) num vec)])))))
  (rec l 0 '()))
           