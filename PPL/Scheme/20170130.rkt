#lang racket

(define (sublists lst k)
  (let ((curr lst)
        (size (length lst)))
    (lambda ()
      (if (< size k)
          'end
          (begin
            (let ((v (take curr k)))
              (set! size (- size 1))
              (set! curr (cdr curr))
              v))))))

(define (sublists2 lst k)
  (define curr lst)
  (define size (length lst))
  (lambda ()
      (if (< size k)
          'end
          (begin
            (let ((v (take curr k)))
              (set! size (- size 1))
              (set! curr (cdr curr))
              v)))))

(define i (sublists2 '(1 2 3) 2))

(define (checklist lst factors)
  (let* ((k (length (car factors)))
         (iter (sublists lst k)))
    (foldl
     (lambda (x r)
       (let ((curr (iter)))
         (if (member curr (cons 'end factors))
             r
             (cons curr r))))
     '()
     lst)))
     
        

