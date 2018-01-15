#lang racket


;; helper function
(define (subsets e)
  (let loop((l e)
            (out '(())))
    (if (null? l)
        out
        (loop (cdr l)
              (append out
                      (map (lambda (x) (cons (car l) x)) out))))))


(define (make-object lst)
  (let ((values lst))

    ;; methods
    (define (member? v)
      (list? (member v lst)))

    (define (subsetsum v)
      (let* ((sub (subsets lst))
             (sumsubs (map (lambda(l)
                             (foldl + 0 l)) sub)))
        (list? (member v sumsubs))))

    (lambda (message . args)
      (apply (case message
               [(member?) member?]
               [(subsetsum) subsetsum]
               [else (error "Unkown message")]
               )
             args))))

(define ob (make-object '(3 2 7)))
    