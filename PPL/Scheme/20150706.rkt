#lang racket

#|
1 Scheme
1.1 Closures (6 points)

Consider this code:
(define (producer ag1 ag2)
  (let loop ((i 1))
   (if (< i 10)
     (begin
       ((if (odd? i) ag1 ag2) i)
       (loop (+ i 1)))
     (cons
       (ag2 ’end)
       (ag1 ’end)))))
Define two closures clos1 and clos2 for passingthem to producer such that it returns the list ’(20 9 7 5 3 1) (note that 20 = 2 + 4 + 6 + 8).
|#

(define (producer ag1 ag2)
  (let loop ((i 1))
   (if (< i 10)
     (begin
       ((if (odd? i) ag1 ag2) i)
       (loop (+ i 1)))
     (cons
       (ag2 'end)
       (ag1 'end)))))

(define ag1
  (let ((vals '()))
    (lambda (x)
      (if (eq? 'end x)
          vals
          (set! vals (cons x vals))))))

(define ag2
  (let ((val 0))
    (lambda (x)
      (if (eq? 'end x)
          val
          (set! val (+ val x))))))

#|

1.2 Macro (6 points)

Define a macro multiple-apply, (multiple-apply (fun-1 fun-2 ...) to list-1 list-2 ...) where fun-i are functions and to is a keyword, which returns a list containing the result of applying fun-i to list-i.

E.g.
> (multiple-apply (+ - *) to '(1 2 3) '(3 4) '(9 -2))
(6 -1 -18)

|#
    
(define-syntax multiple-apply
  (syntax-rules (to)
    ((_ (f ...) to vs ...)
     (list (apply f vs) ...))))
