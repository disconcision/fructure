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





; interface:



; mode patterns:
; modes : select search transform
; better modes?: searchlection
; do mode as a prop:
; current game loop:
#;[_ (case mode
       ...
       ['transform (match key-code
                     ...
                     ['down (!do ([(c▹▹ ,a) ⋱↦ ((c▹▹ ,a))]))]
                     ...)]
       ...)]

; current transform:
#; [(c▹▹ ,a) ⋱↦ ((c▹▹ ,a))]
; mode version:
#; [([mode : transform] ⋱ (c▹▹ ,a)) -> ((c▹▹ ,a))]
; or more symmetric form?:
#; [([mode : transform] ⋱ (c▹▹ ,a)) -> ([] ⋱ ((c▹▹ ,a)))]
; to avoid too much duplication in interface we project these as such:

#; (gameloop
    ...
    ([mode : transform]
     ...
     'down [⋱(c▹▹ ,a) -> ((c▹▹ ,a))]
     (or 'escape 'return) (compose [⋱(s▹ ,buffer ,selection) -> (▹ ,selection)]
                                   ; bare ⋱ looses the context; your transform will be local
                                   ; putting ⋱ in sexp  and capturing props allows for changes on top level
                                   [⋱(▹▹ ,subselection) -> ,subselection]
                                   ; top level change: mode switch
                                   ; (empty [] captures existing props
                                   [([] ,a ...) -> ([mode : select] ,a ...)]                                   
                                   )   
     ...))




; interface elements
; top level panels
;   world
;   search pattern
;   paint (selection) pattern
;   transforms list
;     filter pattern
;     (sort)


; meta-interface elements
;   editable grammar (or integrate as grammar pebel)
;   editable stylesheet (or integrate this direct into regular editing via a style panel?)
;   (both of above can be selected-node specific; accessing on top yields general case)
;   editable catelog of metamodes/gameloops (each is a mode/transform machine triggered by keypresses)

; idea: make the grammar an editable element which is seperate for each world tree node
; nodes (by default?) inherit their parents grammar, but you can stack transformations on top of that
; these transformations, e.g. additions to the grammar, are recorded as props (scope/bindings)
; later: add rules to generate/instantiate these props from/as syntactic (binding) forms in the object grammar



; detailed selection mode:
; press a key to get an 'expanded view' of current node, i.e.
;   (a) expand important properies, including comments and annotations; english-expanded-form
;   (b) (?) ellipses-hole-patterns made visible
;   (c) (?) help search hits on subtree



; we need a standard (limited) form of searchlection/editing which is used
; to (a) select between current metamode



; an inventory 'stone' which magically creates e.g. the constrcutors/destructors of a grammar
; i.e. physicalize classes of transforms for which the instances can manifest when required
; so if you have this stone you can do the kind of free-editing it exposes
















