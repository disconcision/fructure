#lang racket

(require containment-patterns
         "../fructerm/fructerm.rkt"
         "new-syntax.rkt")

(provide define-from
         update-map
         apply-in!

         define-map
         match?
         match-lambda?
         
         atomic?
         selected?
         select-▹
         desugar-fruct
         select-first-⊙-under-▹
         hole-under-cursor?

         has-captures?
         erase-captures
         capture-at-cursor)


; -------------------------------------------------
; 👻👻 SPOOKY GHOST OO STUFF 👻👻


; attribute accessors ooooooo
#;(define-syntax-rule (transform-in state attr f)
    (match state
      [(hash-table ('attr attr))
       (hash-set state 'attr (f attr))]))

(define-syntax-rule (update-map state (attr f) ...)
  ((compose
    (match-lambda
      [(hash-table ('attr attr))
       (hash-set state 'attr (f attr))]) ...)
   state))

(define-syntax-rule (apply-in! object 'attr f)
  (set! object (update-map object (attr f))))

; bind a bunch of attributes oooooooo
(define-syntax-rule (define-from state attrs ...)
  (match-define (hash-table ('attrs attrs) ...) state))


; -------------------------------------------------
; SYNTAX CONVINIENCE

(define-syntax-rule (define-map <hs> <inits> ...)
  (define <hs> (hash <inits> ...)))

(define-syntax-rule (match-lambda? <pat> ...)
  (match-lambda [<pat> #t] ... [_ #f]))

(define-syntax-rule (match? <expr> <pat> ...)
  (match <expr> [<pat> #t] ... [_ #f]))


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

#; (define (no-⊙? stx)
     (f/match stx
       [(c ⋱ '⊙) #f]
       [_ #t]))

(define (erase-captures fr)
  (match fr
    [(/ metavar a/ a)
     (/ a/ (erase-captures a))]
    [(? list? a)
     (map erase-captures a)]
    [_ fr]))

(define (has-captures? fr)
  (match fr
    [(/ metavar _/ _) #t]
    [(? list? xs) (ormap has-captures? xs)]
    [_ #f]))

(define (capture-at-cursor fr)
  (match fr
    [(⋱+ c⋱ #;(capture-when (or (/ _ (▹ _)) (/ [metavar _] _ _)))
         (and ls (or (/ _ (▹ _)) (/ [metavar _] _ _))))
     (define new-ls
       (match ls
         ['() '()]
         [`(,a ... ,(/ s/ (▹ s)) ,b ...)
          `(,@a ,(erase-captures (/ s/ (▹ s))) ,@b)]))
     (⋱+ c⋱
         (map (λ (t m) (match t [(/ x/ x)
                                 (/ [metavar m] x/ x)]))
              new-ls (range 0 (length ls))))]))

(define hole-under-cursor?
  (match-lambda? (⋱ c⋱ (/ _/ (▹ (or '⊙ '⊙+))))))