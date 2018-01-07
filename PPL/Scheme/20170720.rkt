#lang racket

#| 

Consider the following code:

|#

(define (print+sub x y)
  (display x)
  (display " ")
  (display " -> ")
  (- x y))

(define (puzzle)
  (call/cc (lambda (exit)
             (define (local e)
               (call/cc (lambda (local-exit)
                          (exit (print+sub e (call/cc (lambda (new-exit)
                                                       (set! exit new-exit)
                                                       (local-exit #f))))))))
             (local 6)
             (exit 2))))


#|

1) Describe how it works.

2) What is the output of the evaluation of (define x (puzzle))? What is the value of x?

3) Can you see problems with this code?

|#


;; Solve

#|
1) When you call (puzzle), (local 6) is first called and result to : (exit print+sub 6 (new-exit 2)).
   Then (new-exit 2) is called, which picks up the continuation of "(exit (print+sub 6 _))" context. Thus (exit (print+sub 6 2)) is called.
2) The output is 6 -> 4
3) The problem of this approach is that the coder assumes that the outside exit call is evaulated before its parameters. Otherwise the function
   Will be in the form :
   (new-exit (print+sub 6 (new-exit 2)))
   This is an infinite loop:
   6 -> 4
   6 -> 2
   6 -> 4
   ...

   To fix this problem, we can modify the code as:
|#

(define (puzzle_mod)
  (call/cc (lambda (exit)
           (define old-exit exit)
           (define (local e)
             (call/cc (lambda (local-exit)
                      (old-exit (print+sub e (call/cc (lambda (new-exit)
                                                      (set! exit new-exit)
                                                      (local-exit #f))))))))
           (local 6)
           (exit 2))))

