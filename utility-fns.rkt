#lang racket

(provide tree-depth
         sub-at-pos
         obj-at-pos
         sub-list-at-pos)

; utility fns

(define (tree-depth source)
  (if (and (list? source) (not (empty? source)))
      (add1 (apply max (map tree-depth source)))
      1))

(define/match (sub-at-pos obj-tree pos)
  [(tree `()) (first tree)]
  [(_ `(,a . ,as)) (sub-at-pos (list-ref obj-tree (add1 (modulo (sub1 a) (sub1 (length obj-tree))))) as)])

(define/match (obj-at-pos obj-tree pos)
  [(_ `()) (first obj-tree)]
  [(_ `(,a . ,as)) (obj-at-pos (list-ref (rest obj-tree) a) as)])

(define/match (sub-list-at-pos obj-tree pos)
  [(tree `()) tree]
  [(_ `(,a . ,as)) (sub-list-at-pos (list-ref obj-tree (add1 (modulo (sub1 a) (sub1 (length obj-tree))))) as)])