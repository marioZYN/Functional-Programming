#lang racket

(struct DList
  (prev
   val
   next)#:mutable)

(define Nil (DList #f #f #f))
(define (Nil? node) (eq? Nil node))

(define (Dcar node)
  (if (Nil? node)
      (error "Nil can not car")
      (DList-val node)))

(define (Dcdr node)
  (if (Nil? node)
      (error "Nil can not cdr")
      (DList Nil (DList-val (DList-next node)) (DList-next node))))

(define (Dcons x node)
  (if (Nil? node)
      (DList Nil x Nil)
      (let ((new (DList Nil x node)))
        (set-DList-prev! node new)
        new)))

(define (DList=? node1 node2)
  (cond
    [(and (Nil? node1) (Nil? node2)) #t]
    [(or (Nil? node1) (Nil? node2)) #f]
    [else
     (and (eqv? (DList-val node1) (DList-val node2))
          (DList=? (DList-prev node1) (DList-prev node2))
          (DList=? (DList-next node1) (DList-next node2)))]))