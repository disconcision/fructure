#lang racket


; (struct S (ls)) ; denotes a (singular) selection
; (struct Ss (ls)) ; denotes a segment selection
; (struct D (ls)) ; denotes a de-selection


; single selection

(define/match (delete expr)
  [(`(,a ... (S(,d ...)) ,b ...)) `(S (,@a  ,@b ))])

(define/match (insert-child-r expr child)
  [(`(S (,a ...)) _) `(,@a (S ,child))])

(define/match (insert-child-l expr child)
  [(`(S (,a ...)) _) `((S ,child) ,@a)])

(define/match (new-sibling-r expr child)
  [(`(,a ... (S (,b ...)) ,c ...) _) `(,@a ,@b (S (,child)) ,@c)])

(define/match (new-sibling-l expr child)
  [(`(,a ... (S (,b ...)) ,c ...) _) `(,@a (S (,child)) ,@b ,@c)])

(define/match (wrap-single expr)
  [(`(S (,a ...))) `(S ((,@a)))])


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


(delete '(0 1 (S(1)) 2))
(insert-child-r '(S ("a" "b")) "new")
(insert-child-l '(S ("a" "b")) "new")
(new-sibling-r '("a" (S ("b")) "d") "c")
(new-sibling-l '("a" (S ("b")) "d") "c")
(wrap-single '(S (1 2)))


; multi-selection

(define/match (swap expr)) ; swap 2 selections
(define/match (cycle/permute expr perm)) ; generalized swap


; hetero-selection

(define/match (copy/clone)) ; copy one selection to another


; selection movement as rewrite

(define/match (parent expr)
  [(`(,a ... (S (,b ...)) ,c ...)) `(S (,@a ,@b ,@c))])

(define/match (first-child expr)
  [(`(S (,a ,b ...))) `((S (,a)) ,@b)])

(define/match (last-child expr)
  [(`(S (,a ... ,b))) `(,@a (S ,b))])

(define/match (next-sibling expr)
  [(`(,a ... (S ,b) ,c ,d ...)) `(,@a ,b (S ,c) ,@d)])

(define/match (prev-sibling expr)
  [(`(,a ... ,b (S ,c) ,d ...)) `(,@a (S ,b) ,c ,@d)])


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

