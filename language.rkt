#lang racket

; andrew blinn 2018

(require "common.rkt"
         "new-syntax.rkt")

; you may think that we're insane
; but ai will reward us when it reigns

(provide base-transforms
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
; BASE TRANSFORMS

(define base-constructors
  ; constructors for app and λ
  ; constructors for variable references are introduced dynamically
  (append
   (list '([⋱
             (▹ [sort expr] xs ... / ⊙)
             (▹ [sort expr] xs ... / (app ([sort expr] / ⊙)
                                          ([sort expr] / ⊙)))])
         '([⋱
             (▹ [sort expr] xs ... / ⊙)
             (▹ [sort expr] xs ... / (λ ([sort params]
                                         / (([sort pat]
                                             / (id ([sort char] / ⊙)))))
                                       ([sort expr] / ⊙)))]))
   (list '([⋱
             (▹ [sort expr] xs ... / ⊙)
             (▹ [sort expr] xs ... / (num ([sort digit] / ⊙)))]
           [⋱
             (▹ [sort expr] xs ... / ⊙)
             (▹ [sort expr] xs ... / (λm ([sort params]
                                          / (([sort pat]
                                              / ⊙+)))
                                         ([sort expr] / ⊙)))])
         '([⋱
             ([sort params]
              / (as ... (▹ xs ... / ⊙+) bs ...))
             ([sort params]
              / (as ... (▹ xs ... / (id ([sort char] / ⊙))) (xs ... / ⊙+) bs ...))])
         '([⋱
             (▹ [sort expr] xs ... / ⊙)
             (▹ [sort expr] xs ... / (if ([sort expr] / ⊙)
                                         ([sort expr] / ⊙)
                                         ([sort expr] / ⊙)))])
         '([⋱
             (▹ [sort expr] xs ... / ⊙)
             (▹ [sort expr] xs ... / (iff ([sort expr] / ⊙)
                                          ([sort expr] / ⊙)
                                          ([sort expr] / ⊙)))])
         '([⋱
             (▹ [sort expr] xs ... / ⊙)
             (▹ [sort expr] xs ... / (begin
                                       ([sort expr] / ⊙)
                                       ([sort expr] / ⊙+)))])
         '([⋱
             ([sort expr] xs ... / (begin
                                     a ...
                                     (bs ... / b)
                                     (▹ [sort expr] / ⊙+)))
             ([sort expr] xs ... / (begin
                                     a ...
                                     ; does this work? or does transform traversal
                                     ; just skip past it...
                                     ; might need to start with cursor before...
                                     ; ie match as ... a above, put cursor last in a here
                                     ; hacky af...
                                     ; doesn;t even work; need to select last element of that
                                     ; waay too hacky
                                     #;(▹ bs ... / b)
                                     (▹ [sort expr] / ⊙+)
                                     ([sort expr] / ⊙)
                                     ([sort expr] / ⊙+)))])

         '([⋱
             (▹ [sort expr] xs ... / ⊙)
             (▹ [sort expr] xs ... / (cond
                                       ([sort CP] / (cp ([sort expr] / ⊙)
                                                        ([sort expr] / ⊙)))
                                       ([sort CP] / ⊙+)))])
         '([⋱
             ([sort expr] xs ... / (cond
                                     a ...
                                     (▹ [sort CP] / ⊙+)))
             ([sort expr] xs ... / (cond
                                     a ...
                                     (▹ [sort CP] / (cp ([sort expr] / ⊙)
                                                        ([sort expr] / ⊙)))
                                     ([sort CP] / ⊙+)))])
         '([⋱
             (▹ [sort expr] xs ... / ⊙)
             (▹ [sort expr] xs ... / (match ([sort expr] / ⊙)
                                       ([sort MP] / (mp ([sort expr] / ⊙)
                                                        ([sort expr] / ⊙)))
                                       ([sort MP] / ⊙+)))])
         '([⋱
             ([sort expr] xs ... / (match ([sort expr] / ⊙)
                                     a ...
                                     (▹ [sort MP] / ⊙+)))
             ([sort expr] xs ... / (match ([sort expr] / ⊙)
                                     a ...
                                     (▹ [sort MP] / (mp ([sort pat] / ⊙)
                                                        ([sort expr] / ⊙)))
                                     ([sort MP] / ⊙+)))])
         '([⋱
             (▹ [sort expr] xs ... / ⊙)
             (▹ [sort expr] xs ... / (let ([sort pairs] / (([sort LP] / (lp ([sort pat]
                                                                             / (id ([sort char] / ⊙)))
                                                                            ([sort expr] / ⊙)))
                                                           ([sort LP] / ⊙+)))
                                       ([sort expr] / ⊙)))])
         '([⋱
             ([sort expr] xs ... / (let ([sort pairs] / (a ...
                                                         (▹ [sort LP] / ⊙+)))
                                     ([sort expr] / ⊙)))
             ([sort expr] xs ... / (let ([sort pairs] / (a ...
                                                         (▹ [sort LP] / (lp ([sort pat]
                                                                             / (id ([sort char] / ⊙)))
                                                                            ([sort expr] / ⊙)))
                                                         ([sort LP] / ⊙+)))
                                     ([sort expr] / ⊙)))])


         ; identity transform
         ; redundant to generalmost destructor
         #;'([⋱
               (▹ [sort expr] xs ... / ⊙)
               (▹ [sort expr] xs ... / ⊙)])))
  )


(define base-destructors
  ; destructors for all syntactic forms
  (list
   (append
    '([⋱
        (▹ xs ... / (ref a))
        (▹ xs ... / ⊙)]
      [⋱
        (▹ xs ... / (app a b))
        (▹ xs ... / ⊙)]
      [⋱
        (▹ xs ... / (λ a b))
        (▹ xs ... / ⊙)])
    '(
      #;#;#;#;#;#;
      [⋱
        (▹ xs ... / (if a b c))
        (▹ xs ... / ⊙)]
      [⋱
        (▹ xs ... / (cond a ...))
        (▹ xs ... / ⊙)]
      [⋱
        (▹ xs ... / (λm a ...))
        (▹ xs ... / ⊙)]
      [⋱
        (▹ xs ... / (begin a ...))
        (▹ xs ... / ⊙)]
      [⋱
        (▹ xs ... / (match a ...))
        (▹ xs ... / ⊙)]
      [⋱
        (▹ xs ... / (let a ...))
        (▹ xs ... / ⊙)]
     
      ; general fallthough for now
      ; don't need identity constructor with this
      ; but needs this hacky guard
      ; actually that doesn't work, thre's still a superfluous transform
      [⋱
        (▹ xs ... / ⊙+)
        (▹ xs ... / ⊙+)]
      [⋱
        (▹ xs ... / a)
        (▹ xs ... / ⊙)]
      ))))


(define alphabet
  ; character set for identifiers
  '(😗 🤟 😮 🤛 😉 ✌ 😏 👌 😎 👈 👉 😣 🤙 😁
      a b c d e f g h i j k l m n o p q r s t u v w x y z))

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
  (cons
   ; identity
   '()
   #;`([⋱
         (xs ... / (num as ... (▹ [sort digit] ys ... / ⊙) bs ...))
         (xs ... / (num as ... (▹ [sort digit] ys ... / ⊙) bs ...))])
   (for/list ([x digits])
     `([⋱
         (xs ... / (num as ... (▹ [sort digit] ys ... / ⊙) bs ...))
         (xs ... / (num as ... (▹ [sort digit] ys ... / ',x) ([sort digit] / ⊙) bs ...))]))))


(define base-transforms
  (append base-destructors
          base-constructors
          alpha-constructors
          digit-constructors))


; -------------------------------------------------
; LANGUAGE DATA


; primary symbols

(define unary-ids (append '(ref id) '(quote qq uq p-not num)))
(define if-like-ids (append '(app and) '(if iff mp lp cp begin list p-and p-or p-list)))
(define lambda-like-ids (append '(λ lambda) '(match let define local)))
(define cond-like-ids '(cond match-λ λm))

(define affordances '(▹ ⊙ ⊙+ ◇ →))
(define sort-names (append '(expr char digit pat params) '(MP LP CP def)))


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
                          sort-names
                          )])
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

