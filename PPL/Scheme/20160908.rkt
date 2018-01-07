#lang racket

(define (create-row k n)
  (let ((res '()))
    (let loop((count 1))
         (if (> count n)
             (reverse res)
             (begin
               (if (= count k)
                   (set! res (cons '1 res))
                   (set! res (cons '0 res)))
               (loop (+ 1 count)))))))

(define (genFig n)
  (define (rec num res)
    (if (= (length res) n)
        (reverse res)
        (let ((row (create-row (+ 1 (length res)) n)))
          (rec num (cons row res)))))
  (rec n '()))
       