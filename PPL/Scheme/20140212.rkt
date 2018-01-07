#lang racket
#|
1 Scheme
Consider a procedure string-from-strings that receives as input a list of objects, keeps all the objects that are strings, discarding all the others, and returns the ordered concatenation of all such strings.
E.g. (string-from-strings '(1 "hello" ", " 2 "world")) is "hello, world"

1.1 Recursive (3 pts)
Define a functional (non tail) recursive version of string-from-strings (without using map, filter, fold).

1.2 Tail recursive (4 pts)
Define a tail recursive version of string-from-strings (without using map, filter, fold).

1.3 Functional higher-order (3 pts)
Give an implementation of string-from-strings using the classical functional higher order functions, i.e. map, filter, fold...

|#

(define (string-from-strings lst)
  (if (null? lst)
      ""
      (let ((cur (car lst)))
        (if (string? cur)
            (string-append cur (string-from-strings (cdr lst)))
            (string-from-strings (cdr lst))))))

(define (string-from-strings-tail lst)
  (define (rec lst res)
    (if (null? lst)
        res
        (let ((cur (car lst)))
          (if (string? cur)
              (rec (cdr lst) (string-append res cur))
              (rec (cdr lst) res)))))
  (rec lst ""))

(define (string-from-strings-hi lst)
  (foldl (lambda (x y)
           (if (string? x)
               (string-append y x)
               y))
         ""
         lst))
       