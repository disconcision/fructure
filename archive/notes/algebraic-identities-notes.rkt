#lang racket

#; A = A
#; A = ⊤
#; A = ⊥

#; (△ A) = ⊥ ; △ constant ⊥, A is a zero of △
#; (△ A) = A ; △ = id, A fixed under △
#; (△ (△ A)) = A ; e.g. NOT. compare △ ∘ △ = id


#; (× ⊥ A) = ⊥ ; nullity e.g. 0x1=0
#; (× A ⊤) = ⊥ ; e.g. (log A 1) = 0
#; (× ⊤ A) = A ; ⊤ ×-tive identity, invariance


#; (× A A) = ⊥ ; A is an involution, self-inverse

#; (× A A) = A ; A is idempotent


#; (× A B) = (× B A) ; comm
#; (× (× A B) C) = (× A (× B C)) ; assoc


#; (△ (× A B)) = (× (△ A) (△ B)) ; △ preserves ×
#; (△ (× A B)) = (× A (△ B))

#; (× A (△ A)) = ⊥ ; △ is ×-tive inverse 
#; (× A B) = (△ (× B A)) ; e.g. anti-commutivity


#; (× A (+ A B)) = A ; absorbtivity e.g. conj/disj
#; (× (× A B) C) = (× A (+ B C)) ; e.g. exponentiation over multiplication (x^a)^b = x^(ab)
#; (× A (+ B C)) (+ (× A B) (× A C)) ; distributivity

#; (△ (× B A)) = (+ (△ B) (△ A)) =  ; e.g. de-morgans

#; (△ (× A B)) = (+ A (△ B)) ; △ homogenous or △ left-preserves × into +  



; --------------------

#; ((× ⊤ _) A) = A ; (1x)A = A
#; (× ⊤ _) = id
#; ((× ⊥ _) A) = ⊥ ; (0x)A = 0 

#; ((× _ (△ _)) A A) = ⊥ ; version of △ is ×-tive inverse
#; ((× A _) (△ A)) = ⊥ ; version of △ is ×-tive inverse

#; ((× A _) (+ B C)) = (+ ((× A _) B) ((× A _) C)) ; distributivity is preservtivity under partial composition


; to add:
; (x^a)(x^b) = x^(a+b) (× (^ x a) (^ x b)) = (^ x (+ a b)

; (x^a)(y^a) = (xy)^a

; (log b (x a c)) = (+ (log b a) (log b c))

; currying type transform?
