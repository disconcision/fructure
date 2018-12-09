#lang racket

(provide fruct-augment
         augment
         augment-transform
         paint-handle)

(require "new-syntax.rkt"
         "../containment-patterns/containment-patterns.rkt")



(define (paint-handle fr)
  ; paint-handle: stx -> stx
  ; add selectability handles
  (match fr
    ; slight hack? with proper order of application
    ; i prob shouldn't have to recurse on tempalte here
    ; note below fails to paint selectable on target
    #;[(/ [transform template] a/ a)
       (/ [transform (new-aug template)] a/ (new-aug a))]
    ; don't recurse into metavariables
    ; whoops, but what about old handles. need to erase them first...
    [(/ [metavar m]
        [sort (and my-sort (or 'expr 'char))] a/ a)
     (/ [metavar m] [sort my-sort] [handle #t] a/ a)]
    [(/ [metavar m] a/ a)
     (/ [metavar m] a/ a)]
    [(/ [sort (and my-sort (or 'expr 'char))] a/ a)
     (/ [sort my-sort] [handle #t] a/ (paint-handle a))]
    [(/ a/ a)
     (/ a/ (paint-handle a))]
    [(? list?) (map paint-handle fr)]
    [_ fr]))

(define (erase-handles fr)
  (match fr
    [(/ handle a/ a)
     (/ a/ (erase-handles a))]
    [(? list?) (map erase-handles fr)]
    [_ fr]))

(define (augment stx)
  ; augment: stx -> stx
  (augment-internal stx))


(define (augment-transform stx)
  ; augment-transform: stx -> stx
  ; writes attributes into the the transform product
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

  ; id refactor
  (check-equal? (augment-internal
                 `(◇ ,(/ `(λ ,(/ `(,(/ `(id ,(/ 'a) ,(/ 'b) ,(/ '⊙)))))
                            ,(/ 0)))))
                `(◇ ,(/ (in-scope '())
                        `(λ ,(/ `(,(/ `(id ,(/ 'a) ,(/ 'b) ,(/ '⊙)))))
                           ,(/ (in-scope `((id ,(/ 'a) ,(/ 'b) ,(/ '⊙))))
                               0))))))




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
       ; trying to refactor to make identifiers handled generically
       (match (/ id/ my-stx)
         [(/ id/ `(id ,chars ... ,(/ hole/ '⊙)))
          (if (empty? chars)
              '||
              (/ id/ `(id ,@chars )))])
       #; (string->symbol
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

    ; SILENT FALLTHROUGH WHILE IMPLEMENTING NEW FORMS
    [_ stx]
    
    [_ (error (~a `("attribute generation error on stx: ", stx))) 0]
    ))


(define fruct-augment
  ; augments syntax with attributes
  (compose augment-transform
           augment
           paint-handle
           erase-handles))