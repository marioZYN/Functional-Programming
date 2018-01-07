#lang racket

(struct leaf ((val #:mutable)))

(struct branch ((left #:mutable)
                (right #:mutable)))

(define (tmap! f t)
  (if (leaf? t)
      (set-leaf-val! t (f (leaf-val t)))
      (begin
        (tmap! f (branch-left t))
        (tmap! f (branch-right t)))))

(define (show t)
  (if (leaf? t)
      (begin 
        (display "leaf ")
        (display (leaf-val t))
        (display " "))
      (begin
        (display "branch ")
        (show (branch-left t))
        (show (branch-right t)))))

(define (get-values t)
  (if (leaf? t)
      (list (leaf-val t))
      (begin
        (append
         (get-values (branch-left t))
         (get-values (branch-right t))))))

(define (reverse! t)
  (let ((values (reverse (get-values t))))
    (tmap! (lambda (x)
             (let ((v (car values)))
               (set! values (cdr values))
               v))
           t)))
    


(define t1 (branch (branch (leaf 1)(leaf 2))(leaf 3)))
              