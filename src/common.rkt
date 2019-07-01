#lang racket

(require "../shared/containment-patterns/containment-patterns/main.rkt"
         "../shared/fructerm/fructerm.rkt"
         "../shared/slash-patterns/slash-patterns.rkt")

(provide match?
         match-lambda?

         div-integer
         
         define-from
         update-map
         apply-in!
         define-map

         updater

         desugar-fruct
         atomic?

         ref->symbol
         
         selected?
         select-▹
         select-first-⊙-under-▹
         hole-under-cursor?
         
         has-captures?
         erase-captures
         capture-at-cursor

         fruct/command)


(struct fruct/command (fruct command-buffer command-pointer) #:transparent)

(define (div-integer x y)
  (inexact->exact (round (div x y))))

; -------------------------------------------------
; 👻👻 SPOOKY GHOST OO STUFF 👻👻


; attribute accessors ooooooo
#;(define-syntax-rule (transform-in state attr f)
    (match state
      [(hash-table ('attr attr))
       (hash-set state 'attr (f attr))]))

(define-syntax-rule (update-map state [kw f] ...)
  ((compose
    (λ (s) (hash-update s kw f)) ...)
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

(define ((updater state key) . stuff)
  (define-from state
    stx history keypresses)
  (define base-state
    (hash-set* state
               'history (cons stx history)
               'keypresses (cons key keypresses)))
  (apply hash-set* base-state stuff))


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

(define (ref->symbol ref)
  (match ref
    [(/ r/ `(ref ,(/ [sort pat] p/ `(id ,(/ [sort char] c/ chars) ...))))
     (string->symbol (apply string-append (map symbol->string chars)))]
    [_ '||]))