#lang racket

(require "../../shared/slash-patterns/slash-patterns.rkt"
         "../common.rkt")

(provide base-transforms
         base-library-transforms
         initial-stx)

(provide stx->fruct
         project)

(provide literals
         if-like-id?
         lambda-like-id?
         cond-like-id?
         form-id?
         affo-id?)

; -------------------------------------------------

(define base-constructors
  ; CONSTRUCTORS FOR BASE SYNTACTIC FORMS
  (append
   ; LAMBDA CALC
   (list '(#;[⋱
               (▹ [sort expr] xs ... / ⊙)
               (▹ [sort expr] xs ... / (app ([sort expr] / ⊙)
                                            ([sort expr] / ⊙)))])
         '([⋱
             (▹ [sort expr] xs ... / ⊙)
             (▹ [sort expr] xs ... / (app ([sort expr] [variadic #true] / ⊙+)))])
         '([⋱
             (xs ... / (app as ... (▹ ys ... / ⊙+) ))
             (xs ... / (app as ... (▹ [sort expr] [variadic #true] / ⊙) (ys ... / ⊙+)))])
         '([⋱
             (▹ [sort expr] xs ... / ⊙)
             (▹ [sort expr] xs ... / (λ ([sort params]
                                         / (([sort pat] / (id ([sort char] / ⊙)))))
                                       ([sort expr] / ⊙)))])
         #;'([⋱
               (▹ [sort expr] xs ... / ⊙)
               (▹ [sort expr] xs ... / (λ ([sort params]
                                           / (([sort pat] [variadic #true] / (id ([sort char] / ⊙)))
                                              ([sort pat] [variadic #true] / ⊙+)))
                                         ([sort expr] / ⊙)))])

         #;'([⋱
               (▹ [sort pat] / ⊙)
               (▹ [sort pat] / (id ([sort char] / ⊙)))])

         '([⋱
             ([sort params]
              / (as ... (▹ xs ... / ⊙+) bs ...))
             ([sort params]
              / (as ... (▹ xs ... / (id ([sort char] / ⊙))) (xs ... / ⊙+) bs ...))])


         )
   ; EXTENDED FORMS
   (list
    '([⋱
        (▹ [sort expr] xs ... / ⊙)
        (▹ [sort expr] xs ... /
           (define ([sort params]
                    / (([sort pat] / (id ([sort char] / ⊙)))
                       ([sort pat] / (id ([sort char] / ⊙)))))
             ([sort expr] / ⊙)))])
    ; so-far failed variadic define attempt; fails in semantics on 171
    #;'([⋱
          (▹ [sort expr] xs ... / ⊙)
          (▹ [sort expr] xs ... /
             (define ([sort params]
                      / (([sort pat] / (id ([sort char] [variadic #true] / ⊙)))
                         ([sort pat] / (id ([sort char] [variadic #true] / ⊙+)))))
               ([sort expr] / ⊙)))])
    #; '([⋱
           (▹ [sort expr] xs ... / ⊙)
           (▹ [sort expr] xs ... / (begin
                                     ([sort expr] [variadic #true] / ⊙)
                                     ([sort expr] [variadic #true] / ⊙+)))])
    #;'([⋱
          ( xs ... /
               (define ([sort params]
                        / (as ...
                           (▹ bs ... / (id (cs ... / ⊙+)))))
                 body))
          ( xs ... /
               (define ([sort params]
                        / (as ...
                           (▹ [sort pat] / (id ([sort char] [variadic #true] / ⊙)))
                           (bs ... / (id (cs ... / ⊙+)))
                           ))
                 body))])
    #;'([⋱
          (xs ... / (begin as ...
                           (▹ bs ... / ⊙+) ))
          (xs ... / (begin  as ...
                            (▹ [sort expr] [variadic #true] / ⊙)
                            (bs ... / ⊙+)))])
    '([⋱
        (▹ [sort expr] xs ... / ⊙)
        (▹ [sort expr] xs ... / (num ([sort digit] / ⊙)))])
    #;'([⋱
          (▹ [sort expr] xs ... / ⊙)
          (▹ [sort expr] xs ... / (num ([sort digit] / 1) ([sort digit] / ⊙)))])
         
         
    '([⋱
        (▹ [sort expr] xs ... / ⊙)
        (▹ [sort expr] xs ... / (if ([sort expr] / ⊙)
                                    ([sort expr] / ⊙)
                                    ([sort expr] / ⊙)))])
         
    '([⋱
        (▹ [sort expr] xs ... / ⊙)
        (▹ [sort expr] xs ... / (begin
                                  ([sort expr] [variadic #true] / ⊙)
                                  ([sort expr] [variadic #true] / ⊙+)))])
         
    ; problem we're trying to solve: autoadvances to next hole after transformation,
    ; but in this case, we're inserting a new hole and want to stay on it
    ; HACK: mark as 'variadic' and special-case it in select-next-hole in transform
    ; basically for these variadic holes, we don't autoadvance the cursor if its on one
    ; disadvantage: can't leave placeholder holes in variadic forms
    '([⋱
        (xs ... / (begin as ... (▹ bs ... / ⊙+) ))
        (xs ... / (begin  as ... (▹ [sort expr] [variadic #true] / ⊙)(bs ... / ⊙+)))])

           
    '([⋱
        (▹ [sort expr] xs ... / ⊙)
        (▹ [sort expr] xs ... / (list
                                 ; DON'T REMOVE BELOW HOLE WILLY-NILLY
                                 ; A BUG (drop-right) in draw-fruct
                                 ; appears when the length-conditional-cutoff
                                 ; drops below a certain value
                                 ([sort expr] [variadic #true] / ⊙)
                                 ([sort expr] [variadic #true] / ⊙+)))])
    '([⋱
        (xs ... / (list as ... (▹ bs ... / ⊙+) ))
        (xs ... / (list as ... (▹ [sort expr] [variadic #true] / ⊙)(bs ... / ⊙+)))])

    '([⋱
        (▹ [sort expr] xs ... / ⊙)
        (▹ [sort expr] xs ... / (cond
                                  ([sort CP] [variadic #true] / (cp ([sort expr] / ⊙)
                                                                    ([sort expr] / ⊙)))
                                  ([sort CP] [variadic #true] / ⊙+)))])
    '([⋱
        ([sort expr] xs ... / (cond
                                as ...
                                (▹ bs ... / ⊙+)))
        ([sort expr] xs ... / (cond
                                as ...
                                (▹ [sort CP] #;[variadic #true] / (cp ([sort expr] / ⊙)
                                                                      ([sort expr] / ⊙)))
                                (bs ... / ⊙+)))])
    '([⋱
        ([sort expr] xs ... / (cond
                                as ...
                                (▹ bs ... / ⊙+)))
        ([sort expr] xs ... / (cond
                                as ...
                                (▹ [sort CP] #;[variadic #true] / (cp ([sort else] / else)
                                                                      ([sort expr] / ⊙)))
                                (bs ... / ⊙+)))])
    #;'([⋱
          (▹ [sort expr] xs ... / ⊙)
          (▹ [sort expr] xs ... / (match ([sort expr] / ⊙)
                                    ([sort MP] [variadic #true] / (mp ([sort expr] / ⊙)
                                                                      ([sort expr] / ⊙)))
                                    ([sort MP] [variadic #true] / ⊙+)))])
    #;'([⋱
          ([sort expr] xs ... / (match ([sort expr] / ⊙)
                                  a ...
                                  (▹ [sort MP] [variadic #true] bs ... / ⊙+)))
          ([sort expr] xs ... / (match ([sort expr] / ⊙)
                                  a ...
                                  (▹ [sort MP] [variadic #true] / (mp ([sort pat] / ⊙)
                                                                      ([sort expr] / ⊙)))
                                  ([sort MP] [variadic #true] bs ... / ⊙+)))])
    #;'([⋱
          (▹ [sort expr] xs ... / ⊙)
          ; sort params below is a hack to use lambda layout routines; TODO fix
          (▹ [sort expr] xs ... / (let ([sort LPS] / (lps ([sort LP] / (lp
                                                                        ([sort params]
                                                                         / (([sort pat]
                                                                             / (id ([sort char] / ⊙)))))
                                                                        #;([sort pat]
                                                                           / (id ([sort char] / ⊙)))
                                                                        ([sort expr] / ⊙)))
                                                          ([sort LP] / ⊙+)))
                                    ([sort expr] / ⊙)))])
    #;'([⋱
          ([sort expr] xs ... / (let ([sort LPS] / (lps a ...
                                                        (▹ [sort LP] bs ... / ⊙+)))
                                  ([sort expr] / ⊙)))
          ([sort expr] xs ... / (let ([sort LPS] / (lps a ...
                                                        (▹ [sort LP] / (lp
                                                                        ; HACK, see above
                                                                        ([sort params]
                                                                         / (([sort pat]
                                                                             / (id ([sort char] / ⊙)))))                                                                     
                                                                        ([sort expr] / ⊙)))
                                                        ([sort LP] bs ... / ⊙+)))
                                  ([sort expr] / ⊙)))])
         
    ; identity transform
    ; redundant to generalmost destructor
    #;'([⋱
          (▹ [sort expr] xs ... / ⊙)
          (▹ [sort expr] xs ... / ⊙)]))))


(define base-destructors
  ; DESTRUCTORS FOR BASE SYNTACTIC FORMS
  (list
   (append
    '([⋱
        (▹ xs ... / (ref a))
        (▹ xs ... / ⊙)]
      [⋱
        (▹ xs ... / (app as ...))
        (▹ xs ... / ⊙)]
      [⋱
        (▹ xs ... / (λ a b))
        (▹ xs ... / ⊙)])
    '([⋱
        (▹ xs ... / (num a ...))
        (▹ xs ... / ⊙)]
      [⋱
        (▹ xs ... / (if a b c))
        (▹ xs ... / ⊙)]
      [⋱
        (▹ xs ... / (define a ...))
        (▹ xs ... / ⊙)]
      [⋱
        (▹ xs ... / (begin a ...))
        (▹ xs ... / ⊙)]
      [⋱
        (▹ xs ... / (list a ...))
        (▹ xs ... / ⊙)]
      [⋱
        (▹ xs ... / (cond a ...))
        (▹ xs ... / ⊙)]
      #;[⋱
          (▹ xs ... / (match a ...))
          (▹ xs ... / ⊙)]
      #;[⋱
          (▹ xs ... / (let a ...))
          (▹ xs ... / ⊙)]
      ; general fallthough for now
      ; don't need identity constructor with this
      ; but needs this hacky guard
      ; actually that doesn't work, thre's still a superfluous transform
      [⋱
        (▹ xs ... / ⊙)
        (▹ xs ... / ⊙)]
      #;[⋱
          (▹ xs ... / ⊙+)
          (▹ xs ... / ⊙+)]
      #;[⋱
          (▹ xs ... / a)
          (▹ xs ... / ⊙)]))))


(define alphabet
  ; character set for identifiers
  (append #;'(😗 🤟 😮 🤛 😉 ✌ 😏 👌 😎 👈 👉 😣 🤙 😁)
          '(a b c d e f g h i j k l m n o p q r s t u v w x y z)
          '(? ! - \| 😗 🤟)))

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


(define non-zero-digits
  '(1 2 3 4 5 6 7 8 9))

(define digits
  (cons 0 non-zero-digits))

(define digit-constructors
  ; char constructors for each letter in the alphabet
  (append
   (list
    '([⋱
        (▹ [sort expr] xs ... / ⊙)
        (▹ [sort expr] xs ... / (num ([sort digit] / 0)))])
    '([⋱
        (▹ [sort expr] xs ... / ⊙)
        (▹ [sort expr] xs ... / (num ([sort digit] / 1)))]))
   (for/list ([x digits])
     `([⋱
         (xs ... / (num as ... (▹ [sort digit] ys ... / ⊙) bs ...))
         (xs ... / (num as ... (▹ [sort digit] ys ... / ',x) ([sort digit] / ⊙) bs ...))]))))

(define base-library-signatures
  ; BASE LIBRARY FUNCTIONS & SIGNATURES
  ; later: populate base sigs from contracts? procedure-props?
  (append
   '(true false (not ⊙))
   '((=? ⊙ ⊙))
   '((zero? ⊙) (add1 ⊙) (sub1 ⊙) (< ⊙ ⊙))
   '(null (empty? ⊙) (cons ⊙ ⊙) (first ⊙) (rest ⊙))))

(define (symbol->proper-ref sym)
  ((compose (λ (stuff) `(ref ([sort pat] / (id ,@stuff))))
            (curry map (λ (s) `([sort char] / ',(string->symbol (string s)))))
            string->list symbol->string)
   sym))

(define base-library-transforms
  (apply
   append
   (for/list ([x base-library-signatures])
     (match x
       [(? symbol?)
        `(([⋱
             (▹ [sort expr] xs ... / ⊙)
             (▹ [sort expr] xs ... / ,(symbol->proper-ref x))]))]
       [`(,a ,as ...)
        ; order of these two is very intentional
        `(([⋱
             (▹ [sort expr] xs ... / ⊙)
             (▹ [sort expr] xs ... / (app ([sort expr] / ,(symbol->proper-ref a))
                                          ,@(map (λ (_) `([sort expr] / ⊙)) as)))])
          ([⋱
             (▹ [sort expr] xs ... / ⊙)
             (▹ [sort expr] xs ... / ,(symbol->proper-ref a))]))]))))

#;(define base-library
    (append
     '(true false not)
     '(zero? add1 sub1)
     '(null empty? cons first rest)))

#;(define base-library-transforms
    (for/list ([x base-library])
      `([⋱
          (▹ [sort expr] xs ... / ⊙)
          (▹ [sort expr] xs ... / ,(symbol->proper-ref x))])))

; BASIC REFACTORING TRANSFORMATIONS
; eventually, populate some of these from macros

#; '([⋱
       (▹ [sort expr] xs ... / ⊙)
       (▹ [sort expr] xs ... / (cond
                                 ([sort CP] [variadic #true] / (cp ([sort expr] / ⊙)
                                                                   ([sort expr] / ⊙)))
                                 ([sort CP] [variadic #true] / ⊙+)))])
(define base-refactors
  (list
   '([⋱
       (▹ [sort expr] xs ... / (cond
                                 ([sort CP] / (cp a b))
                                 ([sort CP] / (cp ([sort else] / else) c))
                                 ([sort CP] / ⊙+)))
       (▹ [sort expr] xs ... / (if a b c))])
   #;'([⋱
         (▹ [sort expr] xs ... / (if a b c))
         (▹ [sort expr] xs ... / (cond
                                   ([sort CP] [variadic #true] / (cp a b))
                                   ([sort CP] [variadic #true] / (cp ([sort else] / else) c))
                                   ([sort CP] [variadic #true] / ⊙+)))])))

; BASE TRANSFORMATION MARSHALLING
(define base-transforms
  (append base-destructors 
          base-constructors
          base-refactors
          alpha-constructors
          digit-constructors))


; -------------------------------------------------
; LANGUAGE DATA
; metadata which will be eventually derived automatically

; RESERVED primary symbols
(define unary-ids (append '(ref id) '(quote qq uq p-not num)))
(define if-like-ids (append '(app mapp and) '(if iff mp lp #;cp begin list p-and p-or p-list)))
(define lambda-like-ids (append '(λ lambda) '(match let define local)))
(define cond-like-ids '(cond match-λ λm lps)) ; lps is let pairs

; RESERVED meta symbols
(define affordances '(▹ ⊙ ⊙+ ◇ →))
(define sort-names (append '(expr char digit pat params) '(MP LP LPS CP def else)))
; else above is hack


; derived symbol functions
(define form-ids (append unary-ids if-like-ids lambda-like-ids cond-like-ids))
(define if-like-id? (curryr member if-like-ids))
(define lambda-like-id? (curryr member lambda-like-ids))
(define cond-like-id? (curryr member cond-like-ids))
(define form-id? (curryr member form-ids))
(define affo-id? (curryr member affordances))

(define literals
  (for/fold ([hs (hash)])
            ([lit (append form-ids
                          affordances
                          sort-names)])
    (hash-set hs lit '())))


; -------------------------------------------------
; INITIAL SYNTAX

(define initial-stx
  ; initial syntax for this language
  ((desugar-fruct literals) '(◇ (▹ (sort expr) / ⊙))))


; -------------------------------------------------
; SEX PROJECTION LIBRARY

(define (project stx)
  ; project: fruct -> sexpr
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
  ; stx->fruct : sexpr -> fruct
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

