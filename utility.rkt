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
         select-first-⊙-under-▹)


; -------------------------------------------------

; 👻👻 SPOOKY GHOST OO STUFF 👻👻


; attribute accessors ooooooo
#;(define-syntax-rule (transform-in state 'attr f)
    (match state
      [(hash-table ('attr attr))
       (hash-set state 'attr (f attr))]))

(define-syntax-rule (transform-in state ('attr f) ...)
  ((compose
    (match-lambda
      [(hash-table ('attr attr))
       (hash-set state 'attr (f attr))]) ...)
   state))

(define-syntax-rule (apply-in! object 'attr f)
  (set! object (transform-in object ('attr f))))

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
    ; transform mode
    [(/ (template template) _/ (▹ stx))
     `(▹ [,stx -> ,(project template)])]
    ; label sorts of holes
    [(/ (sort sort) _/ (▹ '⊙))
     `(▹ (⊙ ,sort))]
    [(/ (sort sort) _/ '⊙)
     `(⊙ ,sort)]
    ; embed cursor
    [(/ _/ (▹ stx))
     `(▹ ,(@ stx))]
    [(/ _/ stx)
     (@ stx)]
    [(? list?) (map @ stx)] [x x]))



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
