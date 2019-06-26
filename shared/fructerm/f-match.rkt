#lang racket


(provide f/match
         f/match-lambda
         f/match-lambda?
         ⋱ ⋱+ /# /#+ //)

(provide ⋱t) ;temp

#|

   FRUCTERM/MATCH

   this library implements several match-expanders which
   can be required seperately and used with racket/match

   it optionally extends racket/match with special sugar
   to ergonomically support matching with these matchers


   1. CONTAINMENT PATTERNS

   ⋱ and ⋱+ are match-expanders which implement
   containment patterns. these descend into an s-expression
   to captures matches and their multi-holed context.

   f-match extends racket match with infix syntax for ⋱


   2. HASH + ANNOTATION PATTERNS

   /# and /#+ implement an alternate syntax for capturing
   hash patterns, intended to be used with attributed
   s-expressions

   f-match extends racket match with infix syntax /
   which denotes an annotated s-expressions,
   with hashed attributes before the / and
   the regular list of datums after the /


   and now, the tests:

|#


(module+ test
  (require rackunit)

  #|
    tests for ⋱ aka 'contains'

    (⋱ <pattern>)

    this macher preorder-traverses an s-expression,
    matching on the first instance of <pattern>

    ACTUALLY: currently, the instance must be unique
    still deciding on the semantics here

  |#


  ; ⋱ finds left-to-write
  (check-true (f/match `(0 1 2 3)
                [(⋱ 1) #t]))

  ; ⋱ fails
  (check-true (f/match `(0 0 2 3)
                [(not (⋱ 1)) #t]))
  
  ; ⋱ decends
  (check-true (f/match `(0 (0 1) 2 3)
                [(⋱ 1) #t]))
  
  ; ⋱ does nothing
  (check-equal? (f/match `(0 1 2 3)
                  [(⋱ a) a])
                `(0 1 2 3))
  
  ; ⋱ finds arbitrary patterns
  (check-equal? (f/match `(0 (1 zap) 2 3)
                  [(⋱ `(1 ,a)) a])
                `zap)

  ; ⋱ works top-down
  (check-equal? (f/match `(zap (0 (zap 3)))
                  [(⋱ `(zap ,a)) a])
                `(0 (zap 3)))

  
  #|

    pattern : (<context-id> ⋱1 <pattern>)

    finds unique occurence of <pattern>

  |#

  
  ; ⋱1 insists on a UNIQUE match
  (check-true (f/match `(0 1 1)
                [(not (⋱1 1)) #t]))
  

  
  #|

    pattern : (<context-id> ⋱ <pattern>)

    this generalization captures a one-holed context
    such that application as a function (<context-id> arg)
    replaces the first <pattern> with the value of arg

  |#

  ; contexts have holes
  (check-equal? (f/match `(0 1 2 3)
                  [(c ⋱ 2)
                   (c ⋱ 'zap)])
                `(0 1 zap 3))

  ; context is identity
  (check-equal? (f/match `(0 1 2 3)
                  [(c ⋱ a)
                   (c ⋱ 'whatever)])
                'whatever)

  (check-equal? (f/match `(0 1 2 3)
                  [(c ⋱ a)
                   a])
                `(0 1 2 3))
 
  ; contexts descend 
  (check-equal? (f/match '(0 0 ((▹ 7) 0 0))
                  [(c ⋱ `(▹ ,a))
                   (c `(▹▹ ,a))])
                '(0 0 ((▹▹ 7) 0 0)))

  #|

    template: (<context-id> ⋱ arg)

    note that (<context-id> ⋱ arg) can be
    optionally used in template for symmetry

  |#

  (check-equal? (f/match '(0 0 ((▹ 7) 0 0))
                  [(c ⋱ `(▹ ,a))
                   (c ⋱ `(▹▹ ,a))])
                '(0 0 ((▹▹ 7) 0 0)))

  #|

    (<context-id> ⋱ <pattern> ...)

    (note ... is literal)

    this generalization captures a multi-holed context
    such that application as a function (<context-id> args ...)
    replaces each <pattern> matches with its respective arg

  |#

  (check-equal? (f/match '(0 1 (0 1 (1 0)) 0 1)
                  [(c ⋱ 1 ...) (c ⋱ '(3 4 5 6) ...)])
                '(0 3 (0 4 (5 0)) 0 6))

  #|

    we can use this with a nested match to rewrite
    the captured sub-patterns as a list:

  |#

  (check-equal? (f/match '(0 1 (0 1 (1 (▹ 0))) 0 1)
                  [(c ⋱ (and a (or `(▹ ,_) (? number?))) ...)
                   (c ⋱ (match a [`(,x ... (▹ ,y) ,z ,w ...)
                                  `(,@x ,y (▹ ,z) ,@w)]) ...)])
                '(0 1 (0 1 (1 0)) (▹ 0) 1))

  (check-equal? (f/match '(0 (s 1) (2 (s (▹ 3)) (4 5)) (s 6) 7)
                  [(c ⋱ (and a `(s ,_)) ...)
                   (c ⋱ (match a [`(,x ... (s (▹ ,y)) (s ,z) ,w ...)
                                  `(,@x (s ,y) (s (▹ ,z)) ,@w)]) ...)])
                '(0 (s 1) (2 (s 3) (4 5)) (s (▹ 6)) 7))

  #|

    (<context-id> ⋱ (capture-when <when-pattern>) <list-pattern>)

    this generalization captures all matches on when-pattern
    and the matches the list of results against <list-pattern>

  |#

  ; moving a cursor to the next atom
  (check-equal? (f/match '(0 1 (0 1 (1 (▹ 0))) 0 1)
                  [(c ⋱ (capture-when (or `(▹ ,_) (? number?)))
                      `(,x ... (▹ ,y) ,z ,w ...))
                   (c ⋱
                      `(,@x ,y (▹ ,z) ,@w) ...)])
                '(0 1 (0 1 (1 0)) (▹ 0) 1))

  ; #goals?
  #;(check-equal? (f/match '(0 1 (0 1 (1 (▹ 0))) 0 1)
                    [(c ⋱ #:capture-when (or `(▹ ,_) (? number?))
                        x ... `(▹ ,y) z w ...)
                     (c ⋱
                        x ... ,y `(▹ ,z) w ...)])
                  '(0 1 (0 1 (1 0)) (▹ 0) 1))

  ; toy scope-aware subtitution
  (check-equal? (f/match `(let ([w 1])
                            z
                            (let ([z 2])
                              z)
                            (let ([y 3])
                              z))
                  [(c ⋱ (until `(let ([z ,_]) ,_ ...))
                      'z)
                   (c ⋱ (make-list 2 'new-name) ...)])
                `(let ([w 1])
                   new-name
                   (let ([z 2])
                     z)
                   (let ([y 3])
                     new-name)))
  
  #|

    notes on extended forms:

    TODO: use optionals to combine when-capture and until
 
    INVESTIGATE:  other syntax options for when-capture
    (<context-id> ⋱ <pattern> ... -> <pattern> ooo)

  |#

  #;(define (temp x)
      (f/match x
        [(c ⋱ (capture-when (or `(Pair ,_ ,_) (? number?)))
            `(,x ,xs ...))
         (c ⋱
            `((▹ ,x) ,@xs))]
        [w w]))
  
  #;(check-equal? (f/match `(▹ (Pair (Pair 1 2) (Pair 3 4)))
                    [(c ⋱ `(▹ ,(and a (app temp b))))
                     #:when (not (equal? a b))
                     (println `(trig ,b))
                     (c ⋱ b)]
                    [(c ⋱ (capture-when (app temp (or `(▹ ,_) (? number?))))
                        `(,x ... (▹ ,y) ,z ,w ...))
                     (c ⋱
                        `(,@x ,y (▹ ,z) ,@w) ...)]
                    [0 0])
                  `(Pair (▹ (Pair 1 2)) (Pair 3 4)))
  )



; THE IMPLEMENTATION

(require (for-syntax racket/match
                     racket/list
                     racket/function)
         racket/hash
         "fructerm-common.rkt")

#|

  Implementation Notes

  1. actual matching logic for containment patterns
     is in fructerm-common.rkt as it is shared with fructerm

|#



; F-MATCH PROPER

(define-syntax (f/match stx)
  ; rewrites f/match into match
  (syntax-case stx (/)
    [(f/match source pairs ...)
     (let ([new-pairs (map rewrite-pairs (syntax->datum #'(pairs ...)))])
       (with-syntax ([(newest-pairs ...) (datum->syntax stx new-pairs)])
         #'(match source newest-pairs ...
             [_ (displayln `(f/match-error source newest-pairs ...))
                (error "f-match error")])))]))


(define-syntax-rule (f/match-lambda <pairs> ...)
  (λ (x) (f/match x <pairs> ...)))

(define-syntax-rule (f/match-lambda? <pat>)
  (f/match-lambda [<pat> #t] [_ #f]))


(define-for-syntax (rewrite-pairs stx)
  ; rewrite the patterns and templates of match 'pairs'
  ; while skipping interstitial content
  (match stx
    [`(,pat ,xs ... ,tem)
     `(,(rewriter pat) ,@xs ,(rewriter tem))]
    [_ stx]))


(define-for-syntax (rewriter stx)
  ; this current rewrites both <patterns> and <templates>
  ; symmetry is nice but there's signs of breakage
  ; TODO: seperate pattern/template rewriting?
  (define R rewriter)
  (match stx
    ; containment pattern infix syntax
    [`(⋱ ,pat)
     `(⋱ ,(gensym) ,(R pat))]
    [`(,context ⋱ ,pat)
     `(⋱ ,context ,(R pat))]
    [`(⋱1 ,pat)
     `(⋱1 ,(gensym) ,(R pat))]
    [`(,context ⋱1 ,pat)
     `(⋱1 ,context ,(R pat))]
    [`(⋱+ ,pat)
     `(⋱+ ,(gensym) ,(R pat))]
    [(list context '⋱ pat '...)
     `(⋱+ ,context ,(R pat))]
    [(list context '⋱... pat) ; alternate syntax
     `(⋱+ ,context ,(R pat))]
    
    [`(,context ⋱ (capture-when ,x) ,pat)
     `(⋱+ ,context (capture-when ,x) ,(R pat))]
    [`(,context ⋱ (until ,x) ,pat)
     `(⋱+ ,context (until ,x) ,(R pat))]
    #;[`(,context ⋱+ ,pat ...)
       `(⋱+ ,context ,@(map rewrite-p/ pat))]

    ; hash & annotation pattern syntax
    [(list anns ... and-pat '... / a)
     (list 'quasiquote (list 'p/ (list 'unquote `(/#+ ,@anns ,and-pat)) (list 'unquote (R a))))]
    [`(,anns ... / ,a)
     (list 'quasiquote (list 'p/ (list 'unquote `(/# ,@anns)) (list 'unquote (R a))))]    

    ; budget recursion scheme
    [(? list?) (map R stx)]
    [_ stx]))




; CONTAINMENT PATTERNS


; temporary form while fixing things
(define-match-expander ⋱t
  ; containment pattern (returns first match)
  (λ (stx)
    (syntax-case stx ()
      [(⋱t context-id <internal-pat>)
       #'(app
          (curry first-containment (match-lambda? <internal-pat>))
          `(,context-id (,<internal-pat>)))]))
  (λ (stx)
    (syntax-case stx ()
      [(⋱t context-id internal-template)
       #'(context-id internal-template)])))


(define-match-expander ⋱
  ; containment pattern (returns first match)
  (λ (stx)
    (syntax-case stx ()
      [(⋱ context-id <internal-pat>)
       #'(app
          (curry first-containment (f/match-lambda? <internal-pat>))
          `(,context-id (,<internal-pat>)))]))
  (λ (stx)
    (syntax-case stx ()
      [(⋱ context-id internal-template)
       #'(context-id internal-template)])))

(define-match-expander ⋱1
  ; containment pattern (returns first match)
  (λ (stx)
    (syntax-case stx ()
      [(⋱ context-id <internal-pat>)
       #'(app
          (curry multi-containment (f/match-lambda? <internal-pat>))
          `(,context-id (,<internal-pat>)))]))
  (λ (stx)
    (syntax-case stx ()
      [(⋱ context-id internal-template)
       #'(context-id internal-template)])))


(define-match-expander ⋱+
  ; containment pattern matcher
  ; returns multiple matches; template is list to splice in
  (λ (stx)
    (syntax-case stx (capture-when)
      [(⋱+ context-id (capture-when <cond-pat>) <internal-pat>)
       #'(app
          (curry multi-containment (f/match-lambda? <cond-pat>))
          `(,context-id ,<internal-pat>))]
      [(⋱+ context-id (until <cond-pat>) <internal-pat>)
       #'(app
          (λ (x) (multi-containment (f/match-lambda? <internal-pat>) x
                                    (f/match-lambda? <cond-pat>)))
          `(,context-id (,<internal-pat> (... ...))))]
      [(⋱+ context-id <internal-pat>)
       #'(app
          (curry multi-containment (f/match-lambda? <internal-pat>))
          `(,context-id (,<internal-pat> (... ...))))]))
  (λ (stx)
    (syntax-case stx ()
      [(⋱+ context-id internal-template)
       #'(apply context-id internal-template)])))






; ATTRIBUTE HASHES


(define-match-expander //
  (λ (stx)
    (syntax-case stx (// ...)
      [(// <anns> ... <rest-ann> (... ...) // <thing>)
       (let ([new-anns
              (for/list ([ann (syntax->datum #'(<anns> ...))])
                (if (symbol? ann)
                    `(,(list 'quote ann) ,ann)
                    ann))])
         (with-syntax ([(newest-anns ...) (datum->syntax stx new-anns)])
           #'`(p/ ,(hash-table newest-anns ... <rest-ann> (... ...)) ,<thing>)))]
      [(// <anns> ... // <thing>)
       (let ([new-anns
              (for/list ([ann (syntax->datum #'(<anns> ...))])
                (if (symbol? ann)
                    `(,(list 'quote ann) ,ann)
                    ann))])
         ; above is kind of a hack (also see below) for selection-list
         (with-syntax ([(newest-anns ...) (datum->syntax stx new-anns)])
           #'`(p/ ,(hash-table newest-anns ...) ,<thing>)))
       ]))
  (λ (stx)
    (syntax-case stx (// ...)
      [(// <anns> ... <rest-ann> (... ...) // <thing>)
       (let ([new-anns
              (apply append
                     (for/list ([ann (syntax->datum #'(<anns> ...))])
                       (if (symbol? ann)
                           `(,(list 'quote ann) ,ann)
                           ann)))])
         (with-syntax ([(newest-anns ...) (datum->syntax stx new-anns)])
           #'`(p/ ,(hash-union (for/fold ([acc #hash()])
                                   ([r <rest-ann>])
                           (match r
                             [`(,k ,v) (hash-set acc k v)]))
                         (hash newest-anns ...)
                         #:combine/key (λ (k v1 v2) (if (equal? v1 v2) v1 v2)))
                  ,<thing>)))]
      [(// <anns> ... // <thing>)
       (let ([new-anns
              (for/list ([ann (syntax->datum #'(<anns> ...))])
                (if (symbol? ann)
                    `(,(list 'quote ann) ,ann)
                    `(,(first ann) . ,(rest ann))))])
         ; above is kind of a hack (also see below) for selection-list
         (with-syntax ([(newest-anns ...) (datum->syntax stx new-anns)])
           #'`(p/ ,(hash newest-anns ...) ,<thing>)))
       ])))




(define-match-expander /#
  ; expands /# into regular hash patterns
  ; hackily (!!!) descends rewriting into
  ; hash sub-patterns
  ; todo: CLARIFY SEMANTICS
  (λ (stx)
    (syntax-case stx ()
      [(phash anns ...)
       (let ([new-anns
              (for/list ([ann (syntax->datum #'(anns ...))])
                (if (symbol? ann)
                    `(,(list 'quote ann) ,ann)
                    `(,(first ann) . ,(rewriter (rest ann)))#;ann))])
         ; above is kind of a hack (also see below) for selection-list
         (with-syntax ([(newest-anns ...) (datum->syntax stx new-anns)])
           #'(hash-table newest-anns ...)))]))
  (λ (stx)
    (syntax-case stx ()
      [(phash anns ...)
       (let ([new-anns
              (apply append
                     (for/list ([ann (syntax->datum #'(anns ...))])
                       (if (symbol? ann)
                           `(,(list 'quote ann) ,ann)
                           `(,(first ann) . ,(rewriter (rest ann))))))])
         ; above is hack see above
         (with-syntax ([(newest-anns ...) (datum->syntax stx new-anns)])
           #'(hash newest-anns ...)))])))


(define-match-expander /#+
  ; see above
  (λ (stx)
    (syntax-case stx ()
      [(phash-plus anns ... rest-pat)
       (let ([new-anns
              (for/list ([ann (syntax->datum #'(anns ...))])
                (if (symbol? ann)
                    `(,(list 'quote ann) ,ann)
                    `(,(first ann) . ,(rewriter (rest ann)))))])
         (with-syntax ([(newest-anns ...) (datum->syntax stx new-anns)])
           #'(hash-table newest-anns ... rest-pat (... ...))))]))
  (λ (stx)
    (syntax-case stx ()
      [(phash-plus anns ... rest-pat)
       (let ([new-anns
              (apply append
                     (for/list ([ann (syntax->datum #'(anns ...))])
                       (if (symbol? ann)
                           `(,(list 'quote ann) ,ann)
                           `(,(first ann) . ,(rewriter (rest ann))))))])
         (with-syntax ([(newest-anns ...) (datum->syntax stx new-anns)])
           #'(hash-union (for/fold ([acc #hash()])
                                   ([r rest-pat])
                           (match r
                             [`(,k ,v) (hash-set acc k v)]))
                         (hash newest-anns ...)
                         #:combine/key (λ (k v1 v2) (if (equal? v1 v2) v1 v2 #;(error (string-append "hash error" (~v v1) " " (~v v2))))))))])))

(module+ test
  (require rackunit)

  ; tests for hash patterns
  
  (check-equal? (match #hash((a . 70))
                  [(/# a) a])
                70)
  (check-equal? (match #hash((a . 70) (b . 80))
                  [(/#+ b rd) rd])
                '((a 70)))

  
  ; tests for annotation patterns
  
  (check-equal? (f/match '(p/ #hash( (> . >) (sort . expr)) (p/ #hash((b . 8)) 2))
                  [(> a ... / (b / 2))
                   a])
                '((sort expr)))

  ; alternate prefix syntax, no rewrite required
  (check-equal? (match '(p/ #hash( (> . >) (sort . expr)) (p/ #hash((b . 8)) 2))
                  [(// > a ... // (// b // 2))
                   a])
                '((sort expr)))

  (check-equal? (f/match '(p/ #hash( (> . >) (sort . expr)) (p/ #hash((b . 8)) 666))
                  [(> a ... / (b / 666))
                   (a ... / (b / 667))])
                '(p/
                  #hash((sort . expr))
                  (p/ #hash((b . 8)) 667)))

  (check-equal? (f/match '(p/ #hash( (> . 2) (sort . expr)) (p/ #hash((b . 8)) 666))
                  [((> w) a ... / (b / 666))
                   ((> w) a ... / (b / 667))])
                '(p/
                  #hash((> . 2) (sort . expr))
                  (p/ #hash((b . 8)) 667)))

  (check-equal? (f/match '(p/ #hash( (> . >) (sort . expr)) (p/ #hash((b . 8)) 2))
                  [(> (sort 'expr) / (b / 2))
                   b])
                8)

  (check-equal? (f/match '(p/ #hash( (> . >) (sort . expr)) (p/ #hash((b . 8)) 2))
                  [(> (sort 'expr) / (b / 2))
                   (> (sort 'pat) / (b / 2))])
                '(p/
                  #hash((sort . pat) (> . >))
                  (p/ #hash((b . 8)) 2)))
  )




(module+ test

  ; regression testing for fructure data

  (check-equal? (f/match '(p/ #hash((in-scope . ()) (sort . expr) (▹ . ▹)) 0)
                  [(c ⋱ (▹ As ... / 0))
                   (c ⋱ (▹ As ... / '⊙))])
                '(p/ #hash((in-scope . ()) (sort . expr) (▹ . ▹)) ⊙))
  
  ; TODO: verify this test case is actually right
  (check-equal? (f/match '(p/
                           #hash((in-scope . ())
                                 (selection-list
                                  .
                                  ((p/ #hash((sort . expr) (♦ . ♦)) 0)
                                   (p/
                                    #hash((sort . expr))
                                    (app (p/ #hash((sort . expr)) ⊙) (p/ #hash((sort . expr)) ⊙)))
                                   (p/
                                    #hash((sort . expr))
                                    (λ ((p/ #hash((sort . pat)) ⊙)) (p/ #hash((sort . expr)) ⊙)))
                                   (p/ #hash((sort . expr)) (var (p/ #hash((sort . char)) ⊙)))))
                                 (sort . expr)
                                 (▹ . ▹))
                           ⊙)
                  [(▹ ('selection-list `(,xs ... ,(♦ anns ... / whatever) ,( anns2 ... / w2) ,ws ...))
                      / s)
                   (▹ ('selection-list `(,@xs ,(anns ... / whatever) ,(♦ anns2 ... / w2) ,@ws))
                      / s)]
                  #;[(▹ ('selection-list `(,xs ... (p/ ,(hash-table (♦  ♦) y-rest ...) ,y) (p/ ,(hash-table z-rest ...) ,z) ,ws ...))
                        / s)
                     0])
                '(p/
                  #hash((selection-list
                         .
                         ((p/ #hash((sort . expr)) 0)
                          (p/
                           #hash((sort . expr) (♦ . ♦))
                           (app (p/ #hash((sort . expr)) ⊙) (p/ #hash((sort . expr)) ⊙)))
                          (p/
                           #hash((sort . expr))
                           (λ ((p/ #hash((sort . pat)) ⊙)) (p/ #hash((sort . expr)) ⊙)))
                          (p/ #hash((sort . expr)) (var (p/ #hash((sort . char)) ⊙)))))
                        (▹ . ▹))
                  ⊙)))