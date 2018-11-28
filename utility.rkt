#lang racket

(require "../fructerm/fructerm.rkt"
         #;"../fructerm/f-match.rkt"
         "new-syntax.rkt")

(provide define-from
         transform-in
         apply-in!
         project
         atomic?
         selected?
         select-▹
         desugar-fruct
         select-first-⊙-under-▹
         stx->fruct)

(provide literals
         if-like-id?
         lambda-like-id?
         form-id?
         affo-id?)

; -------------------------------------------------

; LANGUAGE DATA


; primary

(define unary-ids '(ref id))
(define if-like-ids '(and app))
(define lambda-like-ids '(λ lambda))

(define affordances '(▹ ⊙ ◇ →))
(define sort-names '(expr char pat params))



; derived

(define form-ids (append unary-ids if-like-ids lambda-like-ids))

(define if-like-id? (curryr member if-like-ids))
(define lambda-like-id? (curryr member lambda-like-ids))
(define form-id? (curryr member form-ids))
(define affo-id? (curryr member affordances))

(define literals
  (for/fold ([hs (hash)])
            ([lit (append form-ids
                          affordances
                          sort-names
                          )])
    (hash-set hs lit '())))



; -------------------------------------------------


; 👻👻 SPOOKY GHOST OO STUFF 👻👻


; attribute accessors ooooooo
#;(define-syntax-rule (transform-in state attr f)
    (match state
      [(hash-table ('attr attr))
       (hash-set state 'attr (f attr))]))

(define-syntax-rule (transform-in state (attr f) ...)
  ((compose
    (match-lambda
      [(hash-table ('attr attr))
       (hash-set state 'attr (f attr))]) ...)
   state))

(define-syntax-rule (apply-in! object 'attr f)
  (set! object (transform-in object (attr f))))

; bind a bunch of attributes oooooooo
(define-syntax-rule (define-from state attrs ...)
  (match-define (hash-table ('attrs attrs) ...) state))



; -------------------------------------------------

; SEX PROJECTION LIBRARY

#| project: stx -> s-expr
   projects cursor and sort info for holes|#
(define (project stx)
  (define @ project)
  (match stx
    ; strip top
    [`(◇ ,x)
     (@ x)]
    ; transform mode
    #;[(/ (transform template) _/ (▹ stx))
     `(▹ [,stx -> ,(project template)])]
    ; label sorts of holes
    [(/ (sort sort) _/ (▹ '⊙))
     `(▹ (⊙ ,sort))]
    ; flatten symbols
    [(/ _ `(id ,(/ [sort 'char] _ (and c (not '⊙))) ... ,(/ [sort 'char] _ '⊙) ...))
     (string->symbol (apply string-append (map symbol->string c)))]
    [(/ (sort sort) _/ '⊙)
     `(⊙ ,sort)]
    ; embed cursor
    #;[(/ _/ (▹ stx))
     `(▹ ,(@ stx))]
    ; or nor
    [(/ _/ (▹ stx))
     (@ stx)]
    [(/ _/ stx)
     (@ stx)]
    [(? list?) (map @ stx)] [x x]))



(define (stx->fruct stx)
  (define s2f stx->fruct)
  (match stx
    [(? (disjoin symbol? number?))
     (/ stx)]
    [`(▹ ,(? (disjoin symbol? number?) s))
     (/ (▹ s))]
    [`(▹ (,(? form-id? a) ,as ...))
     (/ (▹ `(,a ,@(map s2f as))))]
    [`(▹ ,a)
     (/ (▹ (map s2f a)))]
    [`(,(? form-id? a) ,as ...)
     (/ `(,a ,@(map s2f as)))]
    [(? list?)
     (/ (map s2f stx))]))



; -------------------------------------------------

; FRUCT WRANGLING

(define (desugar-fruct literals)
  (compose (curry restructure literals #hash()) desugar))

(define (atomic? fr)
  (match fr
    [(/ c/ (? (negate list?))) #t]
    [_ #f]))

;  check if the current node is selected
(define (selected? fr)
  (match fr
    [(/ c/ (▹ a)) #t] [_ #f]))

(define (select-▹ fr)
  (match fr
    [(/ a/ a) (/ (▹ '▹) a/ a)]))

(define (select-first-⊙-under-▹ literals)
  (curry runtime-match literals
         '([(c ⋱ (▹ ys ... / (d ⋱ (xs ... / ⊙))))
            (c ⋱ (ys ... / (d ⋱ (▹ xs ... / ⊙))))]
           [A A])))

#;(define (select-first-⊙-in-unselected literals)
    (curry runtime-match literals
           '([(c ⋱ (xs ... / ⊙))
              (c ⋱ (▹ xs ... / ⊙))]
             [A A])))




; -------------------------------------------------

; TESTS

(module+ test
  (require rackunit)
  (check-equal? (stx->fruct
                 'false)
                '(p/ #hash() false))
  (check-equal? (stx->fruct
                 '(lambda (x)
                    x
                    (and x (and true false))))
                '(p/
                  #hash()
                  (lambda
                      (p/ #hash() ((p/ #hash() x)))
                    (p/ #hash() x)
                    (p/
                     #hash()
                     (and
                      (p/ #hash() x)
                      (p/
                       #hash()
                       (and
                        (p/ #hash() true)
                        (p/ #hash() false)))))))))
