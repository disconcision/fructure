#lang racket

(require "common.rkt"
         "new-syntax.rkt")

(provide base-transforms
         initial-stx)

(provide stx->fruct
         project)

(provide literals
         if-like-id?
         lambda-like-id?
         form-id?
         affo-id?)




; -------------------------------------------------
; non-packaged constructors for transform mode

(define base-constructors
  ; constructors for app and λ
  ; constructors for variable references are introduced dynamically
  (list '([⋱
            (▹ [sort expr] xs ... / ⊙)
            (▹ [sort expr] xs ... / (app ([sort expr] / ⊙)
                                         ([sort expr] / ⊙)))])
        '([⋱
            (▹ [sort expr] xs ... / ⊙)
            (▹ [sort expr] xs ... / (λ ([sort params]
                                        / (([sort pat]
                                            / (id ([sort char] / ⊙)))))
                                      ([sort expr] / ⊙)))])))


(define base-destructors
  ; destructors for all syntactic forms
  (list
   '([⋱
       (▹ xs ... / (ref a))
       (▹ xs ... / ⊙)]
     [⋱
       (▹ xs ... / (app a b))
       (▹ xs ... / ⊙)]
     [⋱
       (▹ xs ... / (λ a b))
       (▹ xs ... / ⊙)]
     )))


(define alphabet
  ; character set for identifiers
  '(a b c d e f g h i j k l m n o p q r s t u v w x y z))


(define alpha-constructors
  ; char constructors for each letter in the alphabet
  (cons
   ; identity
   `([⋱
       (xs ... / (id as ... (▹ [sort char] ys ... / ⊙) bs ...))
       (xs ... / (id as ... (▹ [sort char] ys ... / ⊙) bs ...))])
   (for/list ([x alphabet])
     `([⋱
         (xs ... / (id as ... (▹ [sort char] ys ... / ⊙) bs ...))
         (xs ... / (id as ... (▹ [sort char] ys ... / ',x) ([sort char] / ⊙) bs ...))]))))


(define base-transforms
  (append base-constructors
          alpha-constructors
          base-destructors))



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


; INITIAL SYNTAX

(define initial-stx
  ; initial syntax for this language
  ((desugar-fruct literals) '(◇ (▹ (sort expr) / ⊙))))


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

