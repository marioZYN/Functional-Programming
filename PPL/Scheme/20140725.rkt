#lang racket
#|
Exercise 1.1 (4 points)
Scheme

Define a procedure (called vecstrings) that accepts two parameters: a vector V and a list L of strings. vecstrings is used to put every string s in L in V, depending on its length: s is placed at position V[|s|], while strings too long are discarded. If more than one strings have the same length, they are collected in a list.

Example:
(define ex '("hi" "there" "have" "an" "interesting" "day"))
(define v1 (make-vector 7 #f))
(vecstrings v1 ex) is the vector #(#f #f (“an” “hi”) “day” “have” “there” #f)

|#

(define ex '("hi" "there" "have" "an" "interesting" "day"))
(define v1 (make-vector 7 #f))


(define (vecstrings vec lst)
  (for-each (lambda (x)
              (let ((len (string-length x)))
                (cond
                  [(> len (vector-length vec)) x]
                  [(eq? (vector-ref vec len) #f) (vector-set! vec len x)]
                  [(string? (vector-ref vec len)) (vector-set! vec len (append (list x) (list (vector-ref vec len))))]
                  [(list? (vector-ref vec len)) (vector-set! vec len (append (list x) (vector-ref vec len)))])))
            lst))

(vecstrings v1 ex)


#|
Exercise 1.2 (6 points)

Define the procedure make-vecstring, which is a variant of vecstrings returning a closure over V. Such closure has one parameter that must be a string s and works like vecstrings, by putting s in V. When the closure is called with the parameter 'return, it must return the current value of V.

Example:
(define my-v (make-vecstring v1)) ; the definition of v1 is in Ex. 1.1
(my-v "another")
(my-v "member")
(my-v "no")
(my-v 'return) is the vector #(#f #f (“no” “an” “hi”) “day” “have” “there” “member”)

|#
(define (make-vecstring vec)
  (lambda (x)
    (if (eq? x 'return)
        vec
        (let ((len (string-length x) ))
          (when (< len (vector-length vec))
            (vector-set! vec len
                         (let ((old (vector-ref vec len)))
                           (cond
                             [(string? old) (list x old)]
                             [(list? old) (cons x old)]
                             [else x]))))))))
(define my-v (make-vecstring v1))
(my-v "another")
(my-v "member")
(my-v "no")
(my-v 'return)