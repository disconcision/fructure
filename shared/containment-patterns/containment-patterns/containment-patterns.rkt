#lang racket

; andrew blinn 2019

(provide ⋱ ⋱1 ⋱+)


#|


   CONTAINMENT PATTERNS

   this library implements several match-expanders which
   can be required seperately and used with racket/match

   i made ⋱ as a sugar to concisely perform updates on
   highly nested structures for syntax rewriting purposes
   in structured editing and algebraic stepper prototypes

   ⋱ , ⋱+ , ⋱1  are match-expanders which implement
   containment patterns. these descend into an s-expressions
   to capture both matches and their multi-holed context

   an n-holed context is captured as a normal n-ary procedure

   the pattern (⋱ <context-name> <pattern>) binds a procedure
   to <context-name> and a list of <matches> to <pattern>
   satisfying (equal? (apply <context> <matches>) target)


|#


(module+ test
  (require rackunit)
  
  #| (⋱ <pattern>)

    this macher preorder-traverses an s-expression,
    matching on the first instance of <pattern> |#

  
  ; ⋱ finds left-to-write
  (check-true (match `(0 1 2 3)
                [(⋱ 1) #t]))

  ; ⋱ fails
  (check-true (match `(0 0 2 3)
                [(not (⋱ 1)) #t]))
  
  ; ⋱ decends
  (check-true (match `(0 (0 1) 2 3)
                [(⋱ 1) #t]))
  
  ; ⋱ does nothing
  (check-equal? (match `(0 1 2 3)
                  [(⋱ a) a])
                `(0 1 2 3))
  
  ; ⋱ finds arbitrary patterns
  (check-equal? (match `(0 (1 zap) 2 3)
                  [(⋱ `(1 ,a)) a])
                `zap)

  ; ⋱ works top-down
  (check-equal? (match `(zap (0 (zap 3)))
                  [(⋱ `(zap ,a)) a])
                `(0 (zap 3)))

    
  #| pattern : (⋱ <context-id> <pattern>)

    this generalization captures a one-holed context
    such that application as a function (<context-id> arg)
    replaces the first <pattern> with the value of arg |#
  

  ; contexts have holes
  (check-equal? (match `(0 1 2 3)
                  [(⋱ c 2)
                   (⋱ c 'zap)])
                `(0 1 zap 3))

  ; context is identity
  (check-equal? (match `(0 1 2 3)
                  [(⋱ c a)
                   (⋱ c 'whatever)])
                'whatever)

  (check-equal? (match `(0 1 2 3)
                  [(⋱ c a)
                   a])
                `(0 1 2 3))
 
  ; contexts descend 
  (check-equal? (match '(0 0 ((▹ 7) 0 0))
                  [(⋱ c `(▹ ,a))
                   (c `(▹▹ ,a))])
                '(0 0 ((▹▹ 7) 0 0)))

  
  
  #| template: (⋱ <context-id> arg)

     note that (⋱ <context-id> arg) can be
     optionally used in template for symmetry |#

  (check-equal? (match '(0 0 ((▹ 7) 0 0))
                  [(⋱ c `(▹ ,a))
                   (⋱ c `(▹▹ ,a))])
                '(0 0 ((▹▹ 7) 0 0))))


(define-match-expander ⋱
  ; containment pattern (returns first match)
  (λ (stx)
    (syntax-case stx ()
      [(⋱ <internal-pat>)
       #'(⋱ _ <internal-pat>)]
      [(⋱ context-id <internal-pat>)
       #'(app (curry first-containment
                     (match-lambda? <internal-pat>))
              `(,context-id (,<internal-pat>)))]))
  (λ (stx)
    (syntax-case stx ()
      [(⋱ context-id internal-template)
       #'(context-id internal-template)])))



(module+ test #|

    pattern : (⋱1  <context-id> <pattern>)

    matches a unique occurence of <pattern> |#
  
  ; ⋱1 insists on a UNIQUE match
  (check-true (match `(0 1 1)
                [(not (⋱1 1)) #t])))



(define-match-expander ⋱1
  ; containment pattern (returns unique match)
  (λ (stx)
    (syntax-case stx ()
      [(⋱1 <internal-pat>)
       #'(⋱1 _ <internal-pat>)]
      [(⋱1 context-id <internal-pat>)
       #'(app
          (curry multi-containment (match-lambda? <internal-pat>))
          `(,context-id (,<internal-pat>)))]))
  (λ (stx)
    (syntax-case stx ()
      [(⋱1 context-id internal-template)
       #'(context-id internal-template)])))



(module+ test #|

    (⋱+ <context-id> <pattern>)

    this generalization captures a multi-holed context
    such that application as a function (<context-id> args ...)
    replaces each <pattern> matches with its respective arg |#

  (check-equal? (match '(0 1 (0 1 (1 0)) 0 1)
                  [(⋱+ c 1)
                   (⋱+ c '(3 4 5 6))])
                '(0 3 (0 4 (5 0)) 0 6))

  #|

    we can use this with a nested match to rewrite
    the captured sub-patterns as a list:

  |#

  (check-equal? (match '(0 1 (0 1 (1 (▹ 0))) 0 1)
                  [(⋱+ c (and a (or `(▹ ,_) (? number?))))
                   (⋱+ c (match a [`(,x ... (▹ ,y) ,z ,w ...)
                                   `(,@x ,y (▹ ,z) ,@w)]))])
                '(0 1 (0 1 (1 0)) (▹ 0) 1))

  (check-equal? (match '(0 (s 1) (2 (s (▹ 3)) (4 5)) (s 6) 7)
                  [(⋱+ c (and a `(s ,_)))
                   (⋱+ c (match a [`(,x ... (s (▹ ,y)) (s ,z) ,w ...)
                                   `(,@x (s ,y) (s (▹ ,z)) ,@w)]))])
                '(0 (s 1) (2 (s 3) (4 5)) (s (▹ 6)) 7))

  #|

    (⋱+ <context-id> (capture-when <when-pattern>) <list-pattern>)

    this generalization captures all matches on when-pattern
    and the matches the list of results against <list-pattern>

  |#

  ; moving a cursor to the next atom
  (check-equal? (match '(0 1 (0 1 (1 (▹ 0))) 0 1)
                  [(⋱+ c (capture-when (or `(▹ ,_) (? number?)))
                       `(,x ... (▹ ,y) ,z ,w ...))
                   (⋱+ c 
                       `(,@x ,y (▹ ,z) ,@w))])
                '(0 1 (0 1 (1 0)) (▹ 0) 1))


  ; toy scope-aware subtitution (until syntax)
  (check-equal? (match `(let ([w 1])
                          z
                          (let ([z 2])
                            z)
                          (let ([y 3])
                            z))
                  [(⋱+ c (until `(let ([z ,_]) ,_ ...))
                       'z)
                   (⋱+ c (make-list 2 'new-name))])
                `(let ([w 1])
                   new-name
                   (let ([z 2])
                     z)
                   (let ([y 3])
                     new-name)))

  
  ; toy scope-aware subtitution (generalized traversal syntax)
  (check-equal?
   (parameterize ([⋱traversal
                   (sexp-traversal
                    (match-lambda? `(let ([z ,_]) ,_ ...)))])
     (match `(let ([w 1])
               z
               (let ([z 2])
                 z)
               (let ([y 3])
                 z))
       [(⋱+ c 
            'z)
        (⋱+ c (make-list 2 'new-name))]))
   `(let ([w 1])
      new-name
      (let ([z 2])
        z)
      (let ([y 3])
        new-name)))

  )


(define-match-expander ⋱+
  ; containment pattern matcher
  ; returns multiple matches; template is list to splice in
  (λ (stx)
    (syntax-case stx (capture-when)
      [(⋱+ context-id (capture-when <cond-pat>) <internal-pat>)
       #'(app
          (curry multi-containment
                 (match-lambda? <cond-pat>))
          `(,context-id ,<internal-pat>))]
      [(⋱+ context-id (until <cond-pat>) <internal-pat>)
       #'(app
          (curry multi-containment-until
                 (match-lambda? <internal-pat>)
                 (match-lambda? <cond-pat>))
          `(,context-id (,<internal-pat> (... ...))))]
      [(⋱+ context-id <internal-pat>)
       #'(app
          (curry multi-containment
                 (match-lambda? <internal-pat>))
          `(,context-id (,<internal-pat> (... ...))))]))
  (λ (stx)
    (syntax-case stx ()
      [(⋱+ context-id internal-template)
       #'(apply context-id internal-template)])))





(module+ test #|

   CORE IMPLEMENTATION

   (multi-containment match? target) returns a pair
   of a procedure called <context> and a list of <matches>
   such that (apply <context> <matches>) reconstitutes target

   (first-containment match? target) works similarly, except
   that it returns at most one match

   CONTEXTS VIA COMPOSABLE CONTINUATIONS

   the general approach here is that we turn the internal pattern
   from above into a predicate called match?, and search for match?
   hits in target in a left-to-right preorder traversal. the
   traversal bottoms out both at atoms.

   we return the 'context' around these matches within the target
   by returning a delimited continutation which encompasses the
   traversal up to the point of hitting the first match. this
   continuation, when invoked, can be used to both continue the
   traversal and to fill the 'holes' left by the matches 


|#

  
  (check-equal? ((first (multi-containment
                         (curry equal? 1)
                         '(0 0 1 0 1)))
                 3 4)
                '(0 0 3 0 4))
  
  (check-equal? ((first (multi-containment
                         (curry equal? 1)
                         '(0 1 (0 1 (1 0)) 0 1) ))
                 3 4 5 6)
                '(0 3 (0 4 (5 0)) 0 6))

  (check-equal? ((first (first-containment
                         (curry equal? 1)
                         '(0 0 1 0 1)))
                 3)
                '(0 0 3 0 1))

  (check-equal? ((first (first-containment
                         (curry equal? 1)
                         '(0 0 0 0 0))))
                '(0 0 0 0 0))

  )



(require racket/control)
(define-struct zipper (a-continuation a-match))

#| CORE IMPLEMENTATION

   Thanks to oleg-at-pobox.com for the method of abstracting
   the traversal from the zipper (1), to Gary Baumgartner for
   help with the earlier call/comp implementation, and
   Sam Tobin-Hochstadt with the original procedure version.

   (1) http://okmij.org/ftp/Scheme/zipper-in-scheme.txt

|#


(define ((sexp-traversal until?) handler? x)
  ; depth-first left-to-right s-expression traversal
  (cond
    [(until? x) x]
    [(handler? x) => identity]
    [(list? x) (map (curry (sexp-traversal until?) handler?) x)]
    [else x]))


#;(define (hash-traversal handler? x)
    (cond
      [(handler? x) => identity]
      [(hash? x)
       (for/hash ([(k v) (in-hash x)])
         (values k (hash-traversal handler? v)))]
      [else x]))
#;(parameterize ([⋱traversal hash-traversal])
    (match (hash 1 (hash 11 'b
                         12 'b)
                 2 (hash 21 'b
                         22 (hash 221 'smol-a
                                  222 'b))
                 3 'b)
      [(⋱ ctx 'smol-a) (⋱ ctx 'swole-A)]))


(define ⋱traversal (make-parameter (sexp-traversal (const #f))))


(define (first-containment match? xs)
  ; this returns a list of two elements
  ; the first element is a one-holed context as a fn
  ; the second is a one-element list of the content of that hole
  (define context (unzip (⋱traversal) match? xs))
  (define matches (extract-matches context))
  (if (empty? matches)
      `(,(thunk xs) ())
      `(,(λ (x) (fill-holes context (list* x (rest matches))))
        (,(first matches)))))


(define (multi-containment match? xs)
  ; this returns a list of two elements
  ; the first element is the multi-holed context as a fn
  ; the second is a list of the contents of those holes
  (define context (unzip (⋱traversal) match? xs))
  (define matches (extract-matches context))
  (if (empty? matches)
      `(,(thunk xs) ())
      `(,(procedure-reduce-arity
          (λ x (fill-holes context x))
          (length matches))
        ,matches)))


(define (multi-containment-until match? until? xs)
  ; legacy until syntax
  (parameterize ([⋱traversal (sexp-traversal until?)])
    (multi-containment match? xs)))


(define (unzip traversal match? xs)
  ; returns a zipper which generically traverses through xs
  ; which which is unzipped up to the first match
  
  ; delimits the continuation and descend into xs looking for matches.
  ; when a match is found, reset, returning a zipper - a pair of
  ; the match found and the continuation up to the original delimiter.
  ; when the returned continuation is invoked the traversal continues.
  (reset (traversal
          (λ (x) (and (match? x)
                      (shift context (zipper context x))))
          xs)))


(define (fill-holes zip inserts)
  ; zip the zipper filling holes with inserts
  (let loop ([z zip]
             [i inserts])
    (match z
      [(zipper context _)
       (loop (reset (context (first i)))
             (rest i))]
      [_ z])))


(define (extract-matches zip)
  ; zip the zipper returning the contents of all holes
  (reverse
   (let loop ([z zip]
              [acc '()])
     (match z
       [(zipper context content)
        (loop (reset (context 666))
              (list* content acc))]
       [_ acc]))))


(define-syntax-rule (match-lambda? <pat>)
  ; converts a pattern into a predicate
  (match-lambda [<pat> #t] [_ #f]))



#| OLD IMPLEMENTATION (before abstracting traversal, zipper-style) |#

#; (define (containment-comp match? xs (until? (λ (x) #f)))
     (reset
      (let rec ([x xs])
        (cond
          [(until? x) x]
          [(match? x) ; equivalently: (shift hole (cm-pair hole x))
           (call/comp (λ (hole) (abort (cm-pair hole x))))]
          [(list? x) (map rec x)]
          [else x]))))



#| OLDER IMPLEMENTATION (uses procedures instead of continuations)

   This is retained for reference.

   (multi-containment match? target) returns a pair
   of a procedure called <context> and a list of <matches>
   such that (apply <context> <matches>) gives you back target

   the general approach here is that we turn the internal pattern
   from above into a predicate called match?, and search for match?
   hits in target in a left-to-right preorder traversal

   if the target is a hit, we return the identity function and
   the target. if it's an atomic non-hit, we return a thunk with
   value target and nothing. if it's a non-atomic non-hit, we
   recursively map over it and 'horizontally compose' the child
   procedures into a new procedure with as many holes/paramaters
   as the sum of the children's

|#

#; (require memoize)

#;(define/memo (multi-containment-old match? xs (until? (λ (x) #f)))
    ; this returns a list of two elements
    ; the first element is the multi-holed context as a fn
    ; the second is a list of the contents of those holes
    (cond
      [(match? xs)
       (list (λ (x) x) `(,xs))]
      [(or (not (list? xs)) (until? xs))
       (list (λ () xs) `())]
      [else
       (match-define `((,subcontexts ,submatches) ...)
         (for/list ([x xs])
           (multi-containment match? x until?)))
       (define subcontext-arities
         (map procedure-arity subcontexts))
       (define (context-candidate . xs)
         (for/list
             ([subctx subcontexts]
              [arg-list (multi-split xs subcontext-arities)])
           (apply subctx arg-list)))
       (define new-context
         (procedure-reduce-arity
          context-candidate
          (apply + subcontext-arities)))
       (define new-matches
         (apply append submatches))
       (list new-context
             new-matches)]))


#;(define/memo (first-containment-old match? xs (until? (λ (x) #f)))
    ; this returns a list of two elements
    ; the first element is a one-holed context as a fn
    ; the second is a one-element list of the content of that hole
    ; this currently is just a gloss for mult-containment
    ; it could be implemented more efficiently separately
    (match-define `(,context ,matches)
      (multi-containment match? xs until?))
    (match matches
      [`() `(,context ,matches)]
      [`(,a ,as ...) `(,(λ (x) (apply context x as)) (,a))]))


#;(define (multi-split ls lengths)
    ; splits list ls into segments of lengths lengths
    (unless (equal? (length ls)
                    (apply + lengths))
      (error "length of list doesn't partition"))
    (define-values (actual extra)
      (for/fold ([acc '()]
                 [ls ls])
                ([l lengths])
        (define-values (this those)
          (split-at ls l))
        (values (cons this acc) those)))
    (reverse actual))



; the end