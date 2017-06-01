#lang racket



#|


Structural Selection Model



|#

; PATTERN MATCHING CONSTRUCT TO IMPLEMENT MAP??


#; ($ some-node)
; some-node is selected

#; `(($ a) b c ($ d) ($ e) f)
; multi-selection

#; ($ (let ,inits ,c ... $,d ...))
#; ($ (,c ... $(let ,inits ,d ...)))
; nested selection (let barf/slurp)


#; (fn1 (fn2))
#; (let [new-var fn2]
     (fn1 new-var))
; extract-let


#; (fn1 (fn2))
#; (let [new-fn (λ (y) (fn1 y))]
     (new-fn (f2)))
; extract-let-lambda



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
