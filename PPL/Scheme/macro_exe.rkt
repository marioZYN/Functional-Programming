#lang racket

;; Exercises on Scheme macros
;; MPradella, MMXII

;; 1) Define a macro "for", which works like the standard procedure for-each but
;; with a more conventional syntax. 
;; E.g. (for x in '(1 2 3) (display x)(newline))

(define-syntax for
  (syntax-rules(in)
    ((_ var in lst expr ...)
     (for-each (lambda(var)
                 expr ...)
               lst))))

;; 2) Define a procedure for computing the "Cartesian product" of two lists: 
;; e.g.
;; (cartesian-product '(a b) '(1 2 3)) 
;; returns 
;; ((a 1) (a 2) (a 3) (b 1) (b 2) (b 3))


(define (cartesian-product l1 l2)
  (define res '())
  (for-each (lambda(x)
              (for-each (lambda(y)
                          (set! res (append res (list (append (list x) (list y))))))
                        l2))
            l1)
  res)

;; 3) Define a macro for "list comprehesions", with syntax 
;; (list/co <outexpr> <condexpr> for <list1> ...)
;; where <outexpr> and <condexpr> are *optional* procedures with one argument.
;; These procedures expect a list containing one argument taken from each list
;; in <list1> ...
;; <outexpr> computes the output value to be put in the comprehension, while
;; <condexpr> is a predicate used to filter out unwanted values.
;;
;; (Hint: use cartesian-product)
;;
;; Examples:
;; (list/co (lambda (x) 
;;            (cons (car x) (cadr x))) 
;;          for '(1 2 3) '(a b c))
;; returns 
;; ((1 . a) (1 . b) (1 . c) (2 . a) (2 . b) (2 . c) (3 . a)
;;   (3 . b) (3 . c)) 
;; 
;; (list/co (lambda (x) 
;;            (* (car x) (cadr x))) 
;;          (lambda (x) 
;;            (> (car x) (cadr x))) 
;;          for '(1 2 3) '(-2 4))
;; returns
;; (-2 -4 -6)

(define-syntax list/co 
  (syntax-rules (for)
	((_ expr for l1)
	 (map expr l1))
	((_ expr test for l1) 
	 (map expr (filter test l1)))
	((_ expr for l1 l2 ...)
	 (map expr 
              (foldl (lambda (x y) (cartesian-product y x)) l1 (list l2 ...))))
	((_ expr test for l1 l2 ...)
	 (map expr 
              (filter test 
                      (foldl (lambda (x y) (cartesian-product y x)) l1 (list l2 ...)))))
	))
       

(list/co (lambda (x)
           (cons (car x) (cadr x))) 
          for '(1 2 3) '(a b c))

(list/co (lambda (x) 
            (* (car x) (cadr x))) 
          (lambda (x) 
           (> (car x) (cadr x))) 
          for '(1 2 3) '(-2 4))


;; -- OPTIONAL/ADVANCED --
;; 4) Define a better list comprehension macro, by exploiting for the
;; translation the List Monad seen in Haskell.

;; for example:
;; (monadic/co (cons x y) from x '(1 2 3 4) y '(-1 2 3)) returns
;; ((1 . -1) (1 . 2) (1 . 3) (2 . -1) (2 . 2) (2 . 3) (3 . -1)
;;   (3 . 2) (3 . 3) (4 . -1) (4 . 2) (4 . 3))
;; (monadic/co (cons x y) when (> x y) from x '(1 2 3 4) y '(-1 2 3)) returns
;; ((1 . -1) (2 . -1) (3 . -1) (3 . 2) (4 . -1) (4 . 2)
;;   (4 . 3))



(define (concat-map f lst)
  (apply append (map f lst)))

(define-syntax monadic/co
  (syntax-rules (when from)
    
    ((_ expr from var lst)
     (concat-map (lambda(var)
          (list expr))
      lst))
    
    ((_ expr when pred from var lst)
     (concat-map (lambda(var)
                   (if pred
                       (list expr)
                       '()))
                 lst))
    
    ((_ expr from v1 l1 v2 l2 ... )
     (concat-map (lambda(v1)
                    (monadic/co expr from v2 l2 ...))
                  l1))

    ((_ expr when pred from v1 l1 v2 l2 ...)
     (concat-map (lambda(v1)
                   (monadic/co expr when pred from v2 l2 ...))
                 l1))))
                     
        
                
                         
       
     
      