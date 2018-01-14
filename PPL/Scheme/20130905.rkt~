#lang racket
#|
Consider the parent-pointer implementation of general trees (PPT), where every node has local data (e.g. a name), and only one pointer, pointing to its parent. Of course, roots nodes do not point to other nodes.
PPTs are generally implemented as arrays containing pairs (index of parent, node name), and one of such data structures can contain one or more different trees, e.g. using a Scheme-like syntax:
 #((? . R) (0 . A) (0 . B) (1 . C) (1 . D) (1 . E) (2 . F) (? . W) (7 . X) (7 . Y))
where root nodes have first component “?”. Nodes are usually referenced through their index.
PPTs are efficient for checking if two nodes belong to the same tree (we have just to check if the root is
the same for both), so are often used to represent partitions.
You are requested to implement a mutable version of PPTs in Scheme. In particular, you must:

1.Define the operation find-root, to obtain the root of the tree containing the given node.

2.Define the operation union!, that takes two nodes and, if they belong to different trees, merges the two
trees by making the root of the first node’s tree the parent of the root of the second node’s tree.

|#

 (define vec #((? . R) (0 . A) (0 . B) (1 . C) (1 . D) (1 . E) (2 . F) (? . W) (7 . X) (7 . Y)))

(define (find-root node ppt)
  (if (eq? (car node) '?)
      node
      (find-root (vector-ref ppt (car node)))))

(define (get-index node ppt)
  (let loop ((i 0))
    (if (= i (vector-length ppt))
        #f
        (if (eq? node (vector-ref ppt i))
            i
            (loop (+ i 1))))))

(define (union! node1 node2 ppt)
  (let ((r1 (find-root node1 ppt))
        (r2 (find-root node2 ppt)))
    (if (eq? r1 r2)
        #t
        (let ((i (get-index r1 ppt))
              (j (get-index r2 ppt)))
          (vector-set! ppt i (cons j (cdr r1)))))))
        