#lang racket

; spicing attempt 1:
#;(match '(a 8 (a 1 (4 a)) a 2)
    [(⋱+ c (? number? n) )
     (⋱ c 9 2 5 3)])
; problem: how often in advance to we actually known
; the number of args?
; vs (⋱ '(9 2 5 3))
; (conseq: (⋱ n) would be id here)
; and
#;(match '(a 8 (a 1 (4 a)) a 2)
    [(⋱⋯ c (? number? n) )
     (⋱⋯ c (add1 n))])
#;(match '(a 8 (a 1 (4 a)) a 2)
    [(⋱+ c (? number? n) )
     (⋱+ c (map (λ (x) (add1 x)) n))]) ; need apply dep on above
; but this rewriting needs to be scope-aware
; whats the closet ew could get without scope
#;(match '(a 8 (a 1 (4 a)) a 2)
    [(⋱+ c (pattern capturing x, y, z ...) )
     (⋱+ c (map (λ (X Y Z) expression-XYZ) x y z))])
; or really just:
#; (⋱+ c (map (λ (x y z) expression-xyz) x y z))
; just the info:
#; (⋱+ c [x y z] expression)
; but will hygene allow this?
; really what we need is
; let B = list of all variables bound by pattern
#; (⋱+ c expression) ; =>
#; (⋱+ c (map (λ (,@B) expression) B))
; note scope should take care of itself in the template,
; no renamings needed
(define-syntax-rule (test [id ...] expr)
  (map (λ (id ...) expr) id ...))
; yah it works!
#; (define a '(1 2 3))
#; (test [a] (add1 a))
#; '(2 3 4)
#; (define b '(4 5 6))
#; (test [a b] (+ b a))

; super low-hanging repl idea:
; replace arrow with #;
; so you can copy repl contents direct
; to defs in commented-out-form
