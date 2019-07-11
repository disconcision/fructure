#lang racket

(require "../../shared/containment-patterns/containment-patterns/main.rkt"
         "../../shared/slash-patterns/slash-patterns.rkt"
         "../common.rkt")

(provide fruct-augment
         augment
         augment-transform
         paint-handle)


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
        [sort (and my-sort (or 'expr 'CP #;'pat #;'char #;'digit))] a/ a)
     (/ [metavar m] [sort my-sort] [handle #t] a/ a)]
    [(/ [metavar m] a/ a)
     (/ [metavar m] a/ a)]
    [(/ [sort (and my-sort (or 'expr 'CP #;'pat #;'char #;'digit))] a/ a)
     (/ [sort my-sort] [handle #t] a/ (paint-handle a))]
    [(/ a/ a)
     (/ a/ (paint-handle a))]
    [(? list?) (map paint-handle fr)]
    [_ fr]))

(define (add-depth fr (running-depth 0))
  (match fr
    [(/ a/ a)
     (/ [depth running-depth] a/ (add-depth a (add1 running-depth)))]
    [(? list?) (map (curryr add-depth running-depth) fr)]
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
  (check-equal? (augment-internal
                 `(◇ ,(/ `(λ ,(/ `(,(/ `(id ,(/ 'a) ,(/ 'b) ,(/ '⊙)))))
                            ,(/ 0)))))
                `(◇ ,(/ (in-scope '())
                        `(λ ,(/ `(,(/ `(id ,(/ 'a) ,(/ 'b) ,(/ '⊙)))))
                           ,(/ (in-scope `((id ,(/ 'a) ,(/ 'b) ,(/ '⊙))))
                               0))))))

(define (augment-internal stx)
  (define W (curry augment-internal))
  (match stx
    ; at the top, nothing is in scope
    ; todo: properly add starter library to scope here
    [`(◇ ,(/ a/ prog))
     `(◇ ,(W (/ (in-scope '()) a/ prog)))]

    ; atomic forms which don't pass down scope
    [(/ in-scope a/ a)
     #:when (match? a
              'else ; hack
              '⊙
              '⊙+
              0
              `(ref ,_)
              `(num ,_))
     (/ in-scope a/ a)]
    
    ; simple forms which map scope to children
    [(/ in-scope _/
        `(,head ,(/ as/ as) ...))
     #:when (set-member? (set 'app
                              #;'begin
                              'if
                              'list ; temp hack
                              'cond
                              'cp)
                         head)
     (/ in-scope _/
        `(,head ,@(map (λ (a/ a) (W (/ in-scope a/ a))) as/ as)))]   

    ; the real mcdeal, scopewise speaking
    [(/ in-scope λ/
        `(λ ,params ,body))
     (/ in-scope λ/
        `(λ ,params ,(W (add-to-scope in-scope params body))))]
    
    #;[(/ in-scope define/
          `(define ,params ,body))
       ; HACK: introduce function binding to top-level scope
       ; see begin case below
       (add-to-scope
        in-scope
        #;(match params [(/ p/ `(,p ,ps ...)) (/ p/ `(,p))])
        (introduce-to-scope-define params (match params (/ p/ ,ps) ps))
        (/ define/
           `(define ,params ,(W (add-to-scope in-scope params body)))))]

    [(/ in-scope define/
        `(define ,params ,body))
     ; HACK: introduce function binding to top-level scope
     ; see begin case below
     (match-define (/ p/ inner-params) params)
     (add-to-scope
      (append in-scope (introduce-to-scope-define inner-params))
      (match params [(/ p/ `(,p ,ps ...)) (/ p/ `(,p))])
      (/ define/
         `(define ,params ,(W (add-to-scope-define in-scope params body)))))]

    #; ((⋱ (▹ (sort expr) xs ... / ⊙)
           (▹ (sort expr) xs ... /
              (app ((sort expr) / (ref ((sort pat) / (id ((sort char) / 'f)
                                                         ((sort char) / 'i)
                                                         ((sort char) / 'r)
                                                         ((sort char) / 's)
                                                         ((sort char) / 't)))))
                   ((sort expr) / ⊙)))))

    ; defines splice scope into begins
    [(/ [in-scope top-scope] begin/ `(begin ,as ...))
     (define-values (stuff _)
       (for/fold ([running-stx '()]
                  [running-scope top-scope])
                 ([a as])
         (match a
           [(/ b/ b)
            (match-define (/ [in-scope new-scope] x/ x)
              (W (/ [in-scope running-scope] b/ b)))
            (values `(,@running-stx ,(/ [in-scope new-scope] x/ x))
                    new-scope)])))
     (/ [in-scope top-scope] begin/ `(begin ,@stuff))]

    ; NOTE: PERMISSIVE FALLTHROUGH WHILE IMPLEMENTING NEW FORMS
    [_ (println `(WARNING: ATTRIBUTES: NOT-IMPLEMENTED: ,stx)) stx]
    [_ (error (~a `(ERROR: ATTRIBUTES: NOT-IMPLEMENTED: ,stx))) void]))

(define (introduce-to-scope my-stx)
  (match my-stx
    [(/ id/ (or '⊙ '⊙+))
     '()]
    [(/ id/ `(id ,chars ... ,(/ _/ (or '⊙ '⊙+))))
     (if (empty? chars) '() `(([⋱
                                 (▹ [sort expr] xs ... / ⊙)
                                 (▹ [sort expr] xs ... / (ref ',(/ id/ `(id ,@chars))))])))
     #;(if (empty? chars) '() `(,(/ id/ `(id ,@chars))))]
    ; POSSIBLY BULLSHIT CASE (without terminal hole)
    [(/ id/ `(id ,chars ...))
     (if (empty? chars) '() `(([⋱
                                 (▹ [sort expr] xs ... / ⊙)
                                 (▹ [sort expr] xs ... / (ref ',(/ id/ `(id ,@chars))))])))
     #;(if (empty? chars) '() `(,(/ id/ `(id ,@chars))))]))

(define (add-to-scope in-scope params-fr fr)
  (match-define (/ body/ body) fr)
  (match-define (/ _/ params) params-fr)
  (/ [in-scope (remove-duplicates
                (append in-scope (append-map introduce-to-scope params)))]
     body/ body))

(define (introduce-to-scope-define my-stx)
    (match my-stx
      [`(,(/ id-1/ `(id ,chars-a ... ,(/ _ '⊙)))
         ,(/ id-2/ `(id ,chars-as ... ,(/ _ '⊙))) ...)
       `(([⋱
            (▹ [sort expr] xs ... / ⊙)
            (▹ [sort expr] xs ... / (app ([sort expr] / (ref ',(/ id-1/ `(id ,@chars-a))))
                                         ,@(map (λ (_) `([sort expr] / ⊙)) chars-as)))]))]
      [_ '()]))

(define (add-to-scope-define in-scope params-fr fr)
  (match-define (/ body/ body) fr)
  (match-define (/ _/ params) params-fr)
  #;(println `(params-fr ,params-fr))
  #;(println `(params , params))
  (/ [in-scope (remove-duplicates
                (append in-scope
                        (introduce-to-scope-define params)
                        (append-map introduce-to-scope params)))]
     body/ body))

(define fruct-augment
  ; augments syntax with attributes
  (compose augment-transform
           add-depth
           augment
           paint-handle
           erase-handles))