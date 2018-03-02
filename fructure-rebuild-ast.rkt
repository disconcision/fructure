#lang racket


`(datum d)
`(var v)
`(λ (id) expr)
`(app expr expr)
`(if expr expr expr)

; fns: cons


`(grammar:
  (expr (alt `hole
             `(datum ,num)
             `(var ,id)
             `(λ (,id) ,expr)
             `(app ,expr ,expr)
             `(if ,expr ,expr ,expr)))
  (id (alt (/id alpha) ; or empty id?
           (/id (seq id alphanum)))) ; note this order is left-rec
  (num (alt digit
            (seq digit num)))
  (alphanum (alt alpha digit))
  (alpha (? alpha?))
  (digit (? digit?)))


;transformations:
; constructors
#;([,expr -> `(datum ,num)]
   [,expr -> `(var ,id)]
   [,expr -> `(λ (,id) ,expr)]
   [,expr -> `(app ,expr ,expr)]
   [,expr -> `(if ,expr ,expr ,expr)]
   [,id -> `(/id ,alpha . ,+id)]
   [`(/id ,alphas ...) -> `(/id ,alphas ... ,alpha)])
; the semantics of unquote above is basically hole

; destructors
#;([`(datum ,num) -> ,expr]
   [`(var ,id) -> ,expr]
   [`(λ (,id) ,expr) -> ,expr]
   [`(app ,expr ,expr) -> ,expr]
   [`(if ,expr ,expr ,expr) -> ,expr]
   [`(/id ,alphas ...) -> ,id])

; snippets
#;([,expr ->`((λ (,id) ,expr:body) ,expr:init)] ; let as pattern
   )
; side note: named holes as above? as annotation?

; transforms
#;([`((λ (,x) ,body) ,init) -> `(let ([,x ,init] ,body))]
   )





; this is more graph than tree though...
`(meso-model-grammar:
  (closure code env)
  (env parent-env bindings)
  (bindings `((,id ,val) ...)))
; this is going to take some thought...