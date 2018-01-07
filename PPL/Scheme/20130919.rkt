#lang racket
#|
Define an object, using the “closures as objects” technique seen in class, that works as a simple immutable container of integer numbers. It must offer two methods:
member?, that checks if a number is contained in the object;
and subsetsum, that checks if a given number is the sum of elements contained in the object (at most each element must be taken once).
For instance,if you define (define ob (make-object ’(3 2 7))),then (ob ’member? 9) is false, while (ob ’subsetsum 9) is true.
Hint: you can call this procedure in your code:

|#

(define (subsets e)
  (let loop ((l e)
         (out '(())))
    (if (null? l)
        out
        (loop (cdr l)
              (append out
                     (map (lambda (x) (cons (car l) x)) out))))))

(define (make-object lst)
  (let ((my-list lst))
    (define (member? x)
      (member x my-list))

    (define (subsetsum x)
      (let* ((subs (subsets my-list))
             (subs2 (map (lambda (x)
                           (if (null? x)
                               #f
                               (foldl + 0 x))) subs)))
        (if (member x subs2)
            #t
            #f)))

    (lambda (msg . args)
      (apply (case msg
               ((member?) member?)
               ((subsetsum) subsetsum)
               (else (error "Unkown methods")))
             args))))

(define ob (make-object '(3 2 7)))
(ob 'member? 9)
(ob 'subsetsum 9)
        
        