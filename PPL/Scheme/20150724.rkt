#lang racket
#|
Define a tail recursive procedure, called urmax, that takes a list of lists ((a1a12a13...)(a21a2a23...)(a31a32a3...)...) and returns (max a1 a2 a3 ...).
E.g. (urmax ’((-1)(1 2)(1 2 3)(10 2 3 -4))) is 3.
|#

(define (urmax lol)
  (define (rec ll res)
    (if (= (length res) (length lol))
        (apply max res)
        (rec (cdr ll) (cons (list-ref (car ll) (length res)) res))))
  (rec lol '()))



#|
Define a variant of urmax based on higher order functions like map (you cannot use iterative loops or recursion in it).
|#

(define counter
  (let ((start -1))
    (lambda ()
      (set! start (+ 1 start))
      start)))

(define (hurmax lol)
  (apply max (map (lambda (x)
                    (list-ref x (counter)))
                  lol)))
