#lang racket

#;(require "../fructerm/f-match.rkt")
(require "new-syntax.rkt")

(provide augment) ; syntax -> attributed-syntax


(define (augment stx)
  (augment-internal stx))


; needs updating
(module+ test
  (require rackunit)
  (check-equal? (augment-internal
                 `(◇ ,(/ 0)))
                `(◇ ,(/ (in-scope '()) 0)))
  (check-equal? (augment-internal
                 `(◇ ,(/ `(app ,(/ 0) ,(/ 0)))))
                0)
  #;`(λ ,(/ xs/ `(,(/ ys/ (and my-stx
                               `(id ,(/ _/ chars) ...)))))
       ,(/ bs/ body))
  #;(check-equal? (augment-internal
                 `(◇ ,(/ `(λ ,(/ `(,(/ `(id ,(/ 'a) ,(/ 'b) ,(/ '⊙)))))
                       ,(/ 0)))))
                0)
  (define p0
    '(p/ #hash((in-scope . ()) (sort . expr))
         (λ (p/ #hash() ((p/ #hash((sort . pat) (▹ . ▹)) ⊙)))
           (p/ #hash((sort . expr)) ⊙))))
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
  #;(check-equal?
     (augment-internal p2)
     '(◇ (p/
          #hash((in-scope . ()) (sort . expr))
          (λ (p/ #hash() ((p/ #hash((sort . pat) (▹ . ▹)) (var (p/ #hash((sort . char)) name)))))
            (p/ #hash((in-scope . (name)) (sort . expr))
                (λ (p/ #hash() ((p/ #hash((sort . pat) (▹ . ▹)) (var (p/ #hash((sort . char)) name2)))))
                  (p/ #hash((in-scope . (name2 name)) (sort . expr))
                      (app (p/ #hash((in-scope . (name2 name)) (sort . expr) (▹ . ▹)) ⊙)
                           (p/ #hash((in-scope . (name2 name)) (sort . expr)) ⊙))))))))))





(define (augment-internal stx)
  (println `(augment-internal ,stx))
  (define W (curry augment-internal))
  (match stx
    
    [`(◇ ,(/ a/ prog))
     (println "diamonsd")
     `(◇ ,(W (/ (in-scope '()) a/ prog)))]

    [(/ anns/ '⊙)
     (/ anns/ '⊙)]

    [(/ anns/ 0)
     (/ anns/ 0)]

    ; don't need to descend into references
    [(/ in-scope _/ top-rest/
        `(ref ,whatever))
     (/ in-scope _/ top-rest/
        `(ref ,whatever))]
    
    ; passthrough apps
    [(/ in-scope top-rest/
        `(app ,(/ f/ f)
              ,(/ a/ a)))
     (/ in-scope top-rest/
        `(app ,(W (/ in-scope f/ f))
              ,(W (/ in-scope a/ a))))]

    [(/ in-scope top-rest/
        `(λ ,(/ xs/ `(,(/ ys/ (and my-stx
                                   `(id ,(/ _/ chars) ...)))))
           ,(/ bs/ body)))
     (println "lmabndapatry")
     (define new-var
       (string->symbol
        (apply string-append
               (map symbol->string
                    (drop-right chars 1)))))
     (define new-in-scope
       (if (equal? '|| new-var)
           in-scope
           `(,new-var ,@in-scope)))
     (/ in-scope top-rest/
        `(λ ,(/ xs/ `(,(/ ys/ my-stx)))
           ,(W (/ ('in-scope new-in-scope) bs/ body))))]
    
    #;[(('in-scope env)
        top-rest ... /
        `(λ ,(xs ... / `(,(ys ... / (and my-stx
                                         `(id ,(_ ... / chars) ...)))))
           ,(bs ... / body)))
       (define new-var
         (string->symbol (apply string-append (map symbol->string (drop-right chars 1)))))
       (define new-in-scope
         (if (equal? '|| new-var)
             env
             `(,new-var ,@env)))
       (('in-scope env)
        top-rest ... /
        `(λ ,(xs ... / `(,(ys ... / my-stx)))
           ,(W (('in-scope new-in-scope) bs ... / body))))]

    #;[(('in-scope env) top-rest ... /
                        `(λ ,(c ... / `(,(a ... / '⊙)))
                           ,(body-anns ... / body)))
       (('in-scope env) top-rest ... /
                        `(λ ,(c ... / `(,(a ... / '⊙)))
                           ,(W (('in-scope env) body-anns ... / body))))]
    
    #;[(('in-scope env) top-rest ... /
                        `(λ ,(c ... / `(,(a ... / b)))
                           ,(body-anns ... / body)))
       (('in-scope env) top-rest ... /
                        `(λ ,(c ... / `(,(a ... / b)))
                           ,(W (body-anns ... / body))))]

    #;[(('in-scope env) top-rest ... /
                        `(λ ,(c ... / `(,(a ... / `(var ,(b ... / id)))))
                           ,(body-anns ... / body)))
       (('in-scope env) top-rest ... /
                        `(λ ,(c ... / `(,(a ... / `(var ,(b ... / id)))))
                           ,(W (('in-scope `(,id ,@env)) body-anns ... / body))))]

    #;[(('in-scope env) top-rest ... /
                        `(λ ,(c ... / `(,(a ... / '⊙)))
                           ,(body-anns ... / body)))
       (('in-scope env) top-rest ... /
                        `(λ ,(c ... / `(,(a ... / '⊙)))
                           ,(W (('in-scope env) body-anns ... / body))))]
    
    #;[(('in-scope env) top-rest ... /
                        `(λ ,(c ... / `(,(a ... / b)))
                           ,(body-anns ... / body)))
       (('in-scope env) top-rest ... /
                        `(λ ,(c ... / `(,(a ... / b)))
                           ,(W (body-anns ... / body))))]

    ; ------------------------------------



    [_ (error (~a `("attribute generation error on stx: ", stx))) 0]
    ))

