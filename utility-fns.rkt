#lang racket

(provide tree-depth)

; utility fns

(define (tree-depth source)
  (if (and (list? source) (not (empty? source)))
      (add1 (apply max (map tree-depth source)))
      1))