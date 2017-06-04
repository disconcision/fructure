#lang racket


; Structural Selection Model

#; ($ some-node)
; some-node is selected

#; `(($ a) b c ($ d) ($ e) f)
; multi-selection


; multi-selection

(define/match (swap expr)) ; swap 2 selections
(define/match (cycle/permute expr perm)) ; generalized swap


; hetero-selection

(define/match (copy/clone)) ; copy one selection to another


; segment selection movement

(define/match (all-children expr))



; PATTERN MATCHING CONSTRUCT TO IMPLEMENT MAP??



; refactoring

(define/match (extract-let expr id))
#; (fn1 (fn2))
#; (let [new-var fn2]
     (fn1 new-var))

(define/match (extract-let-lambda expr id))
#; (fn1 (fn2))
#; (let [new-fn (λ (y) (fn1 y))]
     (new-fn (f2)))

(define/match (pullin-lets expr))
(define/match (pushout-lets expr))

(define/match (merge-variables expr)) ; merge vars in same let with identical inits



#; ($ (let ,inits ,c ... $,d ...))
#; ($ (,c ... $(let ,inits ,d ...)))
; nested selection (let barf/slurp)


#; ($ (fn1 (fn2 $(fna1 0
                       (fna2 1
                             0))
                $ 0
                (fn3 $(fnc1 (fnc2 1))))))
#;`(let ([a (fna1 0
                  (fna2 1
                        0))]
         [b 0]
         [c (fnc1 (fnc2 1))])
     (fn1 (fn1 a b (fn3 c))))
; multi let extract