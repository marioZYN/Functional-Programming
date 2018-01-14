#lang racket

(define my-tree (vector
                 '(? . R)
                 '(0 . A)
                 '(0 . B)
                 '(1 . c)
                 '(1 . D)
                 '(1 . E)
                 '(2 . F)
                 '(? . W)
                 '(7 . X)
                 '(y. Y)))
                     

(define (find-root tree node-index)
  (let* ((cur (vector-ref tree node-index))
         (pointer (car cur)))
    (if (eq? '? pointer)
        node-index
        (find-root tree pointer))))

(define (union? tree node-index1 node-index2)
  (let ((r1 (find-root node-index1))
        (r2 (find-root node-index2)))
    (when (not (eqv? r1 r2))
      (vector-set! tree r2 (cons r1 (cdr (vector-ref r2)))))))
           
        
         