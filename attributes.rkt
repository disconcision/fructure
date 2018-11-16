#lang racket

#;(require "../fructerm/f-match.rkt")
(require "new-syntax.rkt"
         "../containment-patterns/containment-patterns.rkt")

(provide augment
         augment-transform) ; syntax -> attributed-syntax


; augment: stx -> stx
(define (augment stx)
  (augment-internal stx))


; augment-transform: stx -> stx
; writes attributes into the the transform product
(define (augment-transform stx)
  (match stx
    [(⋱ c⋱
        (/ (transform (/ ts/ t))
           (in-scope i)
           as/ a))
     (⋱ c⋱
        (/ (transform (augment (/ (in-scope i) ts/ t)))
           (in-scope i)
           as/ a))]
    [x x]))


(module+ test
  (require rackunit)
  (check-equal? (augment-internal
                 `(◇ ,(/ 0)))
                `(◇ ,(/ (in-scope '()) 0)))
  (check-equal? (augment-internal
                 `(◇ ,(/ `(app ,(/ 0) ,(/ 0)))))
                `(◇ ,(/ (in-scope '())
                        `(app ,(/ (in-scope '()) 0)
                              ,(/ (in-scope '()) 0)))))
  (check-equal? (augment-internal
                 `(◇ ,(/ `(λ ,(/ `(,(/ `(id ,(/ 'a) ,(/ 'b) ,(/ '⊙)))))
                            ,(/ 0)))))
                `(◇ ,(/ (in-scope '())
                        `(λ ,(/ `(,(/ `(id ,(/ 'a) ,(/ 'b) ,(/ '⊙)))))
                           ,(/ (in-scope '(ab))
                               0)))))
  )





(define (augment-internal stx)
  #;(println `(augment-internal ,stx))
  (define W (curry augment-internal))
  (match stx
    
    [`(◇ ,(/ a/ prog))
     `(◇ ,(W (/ (in-scope '()) a/ prog)))]

    [(/ anns/ '⊙)
     (/ anns/ '⊙)]

    [(/ anns/ 0)
     (/ anns/ 0)]

    [(/ in-scope _/
        `(ref ,whatever))
     (/ in-scope _/
        `(ref ,whatever))]
    
    [(/ in-scope _/
        `(app ,(/ f/ f)
              ,(/ a/ a)))
     (/ in-scope _/
        `(app ,(W (/ in-scope f/ f))
              ,(W (/ in-scope a/ a))))]

    [(/ in-scope λ/
        `(λ ,(/ params/ `(,(/ id/ (and my-stx
                                       `(id ,(/ chars/ chars) ...)))))
           ,(/ body/ body)))
     (define new-var
       (string->symbol
        (apply string-append
               (map symbol->string
                    (drop-right chars 1)))))
     (define new-in-scope
       (if (equal? '|| new-var)
           in-scope
           `(,new-var ,@in-scope)))
     ; for now we leave duplicate things in scope
     ; later we'll deal with this with unique identifier ids
     (/ in-scope λ/
        `(λ ,(/ params/ `(,(/ id/ my-stx)))
           ,(W (/ (in-scope new-in-scope) body/ body))))]
    
    [_ (error (~a `("attribute generation error on stx: ", stx))) 0]
    ))

