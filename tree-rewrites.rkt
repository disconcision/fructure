#lang racket


; (struct S (ls)) ; denotes a selection
; (struct D (ls)) ; denotes a de-selection


(define/match (push-sibling-r))
(define/match (push-sibling-l))


; segment selection

(define/match (wrap expr))
(define/match (merge expr))
(define/match (pop/splice expr))
(define/match (slurp-r expr))
(define/match (slurp-l expr))
(define/match (barf-r expr))
(define/match (barf-l expr))


; multi-selection

(define/match (swap expr)) ; swap 2 selections
(define/match (cycle/permute expr perm)) ; generalized swap


; hetero-selection

(define/match (copy/clone)) ; copy one selection to another


; segment selection movement

(define/match (all-children expr))


; utility fns

(define (pos-to-sel tree pos)
  'tree-with-selection)

(define (sel-to-tree sel-tree)
  'tree-without-selection)

(define (sel-to-pos sel-tree)
  'position-of-selection)


; refactoring

(define/match (extract-let expr id))
(define/match (extract-let-lambda expr id))

(define/match (pullin-lets expr))
(define/match (pushout-lets expr))

(define/match (merge-variables expr)) ; merge vars in same let with identical inits

