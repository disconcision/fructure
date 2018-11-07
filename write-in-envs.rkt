#lang racket

(provide write-in-envs)

(require "../projects/fructerm/f-match.rkt")

(define p1
  '(◇
     (p/
      #hash((sort . expr))
      (λ (p/ #hash() ((p/ #hash((sort . pat)) ⊙))) (p/ #hash((sort . expr) (▹ . ▹)) ⊙)))))


(define p2
  '(◇
     (p/ #hash((sort . expr))
         (λ (p/ #hash() ((p/ #hash((sort . pat) (▹ . ▹)) (var (p/ #hash((sort . char)) name)))))
           (p/ #hash((sort . expr))
               (λ (p/ #hash() ((p/ #hash((sort . pat) (▹ . ▹)) (var (p/ #hash((sort . char)) name2)))))
                 (p/ #hash((sort . expr))
                     (app (p/ #hash((sort . expr) (▹ . ▹)) ⊙)
                          (p/ #hash((sort . expr)) ⊙)))))))))


(module+ test
  (require rackunit)
  (check-equal?
   (write-in-envs p2 )
   '(◇ (p/
        #hash((in-scope . ()) (sort . expr))
        (λ (p/ #hash() ((p/ #hash((sort . pat) (▹ . ▹)) (var (p/ #hash((sort . char)) name)))))
          (p/ #hash((in-scope . (name)) (sort . expr))
              (λ (p/ #hash() ((p/ #hash((sort . pat) (▹ . ▹)) (var (p/ #hash((sort . char)) name2)))))
                (p/ #hash((in-scope . (name2 name)) (sort . expr))
                    (app (p/ #hash((in-scope . (name2 name)) (sort . expr) (▹ . ▹)) ⊙)
                         (p/ #hash((in-scope . (name2 name)) (sort . expr)) ⊙))))))))))


#;'(p/ #hash((in-scope . ()) (sort . expr))
     (λ (p/ #hash() ((p/ #hash((sort . pat) (▹ . ▹)) ⊙)))
       (p/ #hash((sort . expr)) ⊙)))


(define (write-in-envs stx)
  (define W (curry write-in-envs))
  (f/match stx
    
    [`(◇ ,(anns ... / prog))
     `(◇ ,(W (('in-scope '()) anns ... / prog)))]

    [(anns ... / '⊙)
     (anns ... / '⊙)]

    [(anns ... / 0)
     (anns ... / 0)]
    
    [(('in-scope env) top-rest ... /
                      `(λ ,(c ... / `(,(a ... / `(var ,(b ... / id)))))
                         ,(body-anns ... / body)))
     (('in-scope env) top-rest ... /
                      `(λ ,(c ... / `(,(a ... / `(var ,(b ... / id)))))
                         ,(W (('in-scope `(,id ,@env)) body-anns ... / body))))]

    [(('in-scope env) top-rest ... /
                      `(λ ,(c ... / `(,(a ... / '⊙)))
                         ,(body-anns ... / body)))
     (('in-scope env) top-rest ... /
                      `(λ ,(c ... / `(,(a ... / '⊙)))
                         ,(W (('in-scope env) body-anns ... / body))))]
    
    [(('in-scope env) top-rest ... /
                      `(λ ,(c ... / `(,(a ... / b)))
                         ,(body-anns ... / body)))
     (('in-scope env) top-rest ... /
                      `(λ ,(c ... / `(,(a ... / b)))
                         ,(W (body-anns ... / body))))]

    [(('in-scope env) top-rest ... /
                      `(var ,(a ... / a-expr)))
     (('in-scope env) top-rest ... /
                      `(var ,(W (('in-scope env) a ... / a-expr))))]

    [(('in-scope env) top-rest ... /
                      `(app ,(a ... / f-expr)
                            ,(b ... / a-expr)))
     (('in-scope env) top-rest ... /
                      `(app ,(W (('in-scope env) a ... / f-expr))
                            ,(W (('in-scope env) b ... / a-expr))))]

    [_ (println `(write-in-envs ,stx)) (error (~a `("error in write-in-envs", stx)))]
    ))


