#lang racket

(require racket/hash
         "fructerm-common.rkt"
         memoize)

(module+ test
  (require rackunit))

#|

fructerm

a syntax-rewriting-oriented pattern-matching system
in the style of racket/match, but where patterns can
be written at run-time.



currently implemented:

- pattern variables/data

- fixed-length list pattern

- ellipses-patterns

- quote (needs more tests)

- quasiquote/unquote (NON-NESTED)

- simple containment patterns


to-come:

- quasiquote/unquote NESTED
- unquote/splicing?
  not necessary, but for the hell of it?

- non-binding wildcard

- complex containment patterns

- sets & hashes

|#


; rewriting internals
(provide runtime-match ; literals → pattern-templates → stx → stx
         desugar
         destructure   ; stx → env
         restructure)  ; env → stx → stx


; temporary forms for annotation patterns
(provide anno-runtime-match 
         desugar-annotation)


; ratch is a match-like syntactic form
(provide ratch)


#| desugar-pattern : stx → stx

 desugar-pattern makes pattern combinators
 more explicit; in particular, it rewrites
 bare sexprs into pconses, and, if they contain
 ellipses literals, into pconses & p...es.

 i'll also take the opportunity to introduce
 some of the combinators.|#

(define (desugar stx)
  (define D desugar)
  (match stx

    ; don't parse quoted content
    [`(quote ,_) stx]


    ; annotation patterns
    #;[`(▹ / ,stx)
       `(p/ ▹ ,(D stx))]
    #;[`(_ / ,stx)
       `(p/ _ ,(D stx))]
    [`(,anns ... / ,stx)
     `(p/ ,(D `(phash ,@anns)) ,(D stx))]
    [`(,ann / ,stx)
     `(p/ ,ann ,(D stx))]
    [(list 'phash anns ... and-pat '...)
     `(phash ,@(for/list ([ann anns])
                 (if (symbol? ann)
                     `(,ann ,ann)
                     ann))
             ,and-pat '...)]
    [`(phash ,anns ...)
     `(phash ,@(for/list ([ann anns])
                 (if (symbol? ann)
                     `(,ann ,ann)
                     ann)))]
    
    ; containment patterns
    [`(⋱ ,pat)
     (D `(p⋱ _ ,pat))]
    [`(⋱ (until ,?-pat) ,pat)
     (D `(p⋱until _ ,?-pat ,pat))]
    [`(,id ⋱ ,pat)
     (D `(p⋱ ,id ,pat))]
    [`(,id ⋱+ ,pat)
     (D `(p⋱+ ,id ,pat))]
    [`(,id ⋱ (until ,?-pat) ,pat)
     (D `(p⋱until ,id ,?-pat ,pat))]

    
    #| lists and ellipses patterns:

   these forms can be most readily understood
   by examining their reconstructions; into cons
   and append respectively. explictly, (p... a b)
   greedily matches pattern a to the initial
   segments of a list|#
    
    [`(,(and (not 'phash)
             (not 'quote)
             (not 'p/)
             (not 'p⋱)
             (not 'p⋱+)
             (not 'p⋱until)
             (not 'p...)
             (not 'pcons)) . ,_)
     (D (foldr
         (λ (x acc)
           (match acc
             [`(,_ ,(== '...) ,y ,ys ...)
              `(p... ,x ,y)]
             [_ `(pcons ,x ,acc)]))
         '() stx))]

    ; budget recursion scheme
    [(? list?) (map D stx)]
    [_ stx]))


; quasiquotation desugaring (this is WIP)
(define (desugar-qq stx [quote-level 0])
  (define D (curryr desugar-qq quote-level))
  (match stx

    [`(quote ,_) stx]
    [`(quasiquote ,(and x (not (? list?))))
     `(quote ,x)]
    [`(quasiquote ,(? list? xs))
     #:when (equal? 0 quote-level)
     (D (map (curryr desugar-qq (add1 quote-level)) xs))]
    [(and (? symbol?)
          #;(not 'quote)
          #;(not 'quasiquote)
          #;(note 'unquote)
          (not 'p/)
          (not 'p⋱)
          (not 'p⋱+)
          (not 'p⋱until)
          (not 'p...)
          (not 'pcons)
          (not 'phash))
     (match quote-level
       [0 stx]
       [_ `(quote ,stx)])]
    [(list 'unquote x)
     ; #:when (quote-level . = . 1)
     (when (zero? quote-level) (error "bad unquote"))
     (desugar-qq x (sub1 quote-level))]
    
    ; budget recursion scheme
    [(? list?) (map D stx)]
    [_ stx]))


(define (desugar-annotation stx)
  (define D desugar-annotation)
  (match stx
    ; annotation patterns
    
    [`(,ann / ,stx)
     `(p/ ,ann ,(D stx))]
    
    [`(▹ ,stx)
     `(p/ ▹ ,(D stx))]

    [(or (? symbol?) (? empty?) (? number?))
     stx]
    
    [`(,(and spec
             (or 'quote
                 'p/
                 'p⋱
                 'p⋱+
                 'p⋱until
                 'p...
                 'pcons
                 'phash))
       ,stx ...)
     `(,spec ,@(map D stx))]

    [(? list?)
     #; (println stx)
     `(p/ () ,(map D stx))]
    ))


; wip
(define (apply-expr f stx)
  (match stx
  
    #;[`(quasiquote ,x) 0]
    #;[(list 'unquote x) 0]

    [`(quote ,p)
     `(quote ,(f p))]
    [`(pcons ,p ,ps)
     `(pcons ,(f p) ,(f ps))]
    [`(p... ,p ,ps)
     `(p... ,(f p) ,(f ps))]
    [`(p⋱ ,(? symbol? id) ,arg)
     `(p⋱ ,id ,(f arg))]
    
    [(? symbol?) stx]
    [(or (? number?) (? empty?)) stx]
    [(? list?) (map f stx)])
  )



; wip
(define ((quote-literals literals) stx)
  (define literal?
    (curry hash-has-key? literals))
  (match stx
    [(? literal?) `(quote ,stx)]
    [`(quote ,p) stx]
    [_ (apply-expr (quote-literals literals) stx)]))


; wip
#;(define ((process-qq quote-level) stx)
    (println "process-qq")
    (println quote-level)
    (println stx)
    (when (quote-level . < . 0) (error "bad unquote"))
    (match stx
      [(? number?) stx]
      [`(quote ,x) `(quote ,x)]
      [`(quasiquote ,x)
       ((process-qq (add1 quote-level)) x)]
      [(and (? symbol?))
       #:when (not (zero? quote-level))
       `(quote ,stx)]
      [(list 'unquote x)
       #:when (equal? 1 quote-level)
       x]
      [(list 'unquote x)
       #:when (quote-level . > . 1)
       (match ((process-qq (sub1 quote-level)) x)
         [`(quote ,w) (list 'unquote ((process-qq (sub1 quote-level)) w))]
         [_ ((process-qq (sub1 quote-level)) x)])
       ]
      [(? list?)
       #:when (not (zero? quote-level))
       (println "lsit case")
       (println stx)
       (println (second stx))
     
       (map (process-qq quote-level) stx)]))

#;(define (explicit⋱ stx)
    (match stx
      [`(,id ⋱ ,pat)
       `(p⋱ ,id ,pat)]))

(module+ test

  (check-equal? ((quote-literals #hash((a . _))) 'a)
                '(quote a))
  (check-equal? ((quote-literals #hash((a . _))) '(1 a))
                '(1 (quote a)))
  (check-equal? ((quote-literals #hash((b . _))) '(1 a))
                '(1 a))
  (check-equal? ((quote-literals #hash((b . _))) '(1 (pcons a b)))
                '(1 (pcons a (quote b)))))


; desugar examples

(module+ test
  (check-equal? (desugar `(a ⋱ 1))
                '(p⋱ a 1))

  (check-equal? (desugar `(a ⋱+ 1))
                '(p⋱+ a 1))

  (check-equal? (desugar `(a ⋱ (until 2) 1))
                '(p⋱until a 2 1))

  (check-equal? (desugar `(a ⋱ (1 b ...)))
                '(p⋱ a (pcons 1 (p... b ()))))

  (check-equal? (desugar '(1 2 3 d ... 5 f ... 7 8))
                '(pcons 1 (pcons 2 (pcons 3 (p... d (pcons 5 (p... f (pcons 7 (pcons 8 ())))))))))


  )



(module+ test
  (check-equal? (desugar '(ann1 (ann2 val2) / 0))
                `(p/ (phash (ann1 ann1) (ann2 val2)) 0))

  (check-equal? (destructure #hash() #hash()
                             '(p/ #hash((ann1 . 1) (ann2 . 2)) 0)
                             '(p/ (phash (ann1 ann1) (ann2 val2)) 0))
                #hash((ann1 . 1) (val2 . 2)))

  (check-equal? (destructure #hash() #hash()
                             '(p/ #hash((ann1 . 1) (ann2 . 2)) 0)
                             '(p/ (phash (ann1 ann1)) 0))
                #hash((ann1 . 1)))

  (check-equal? (destructure #hash() #hash()
                             '(p/ #hash((ann2 . 2)) 0)
                             '(p/ (phash (ann1 ann1) (ann2 val2)) 0))
                'no-match)

  (check-equal? (runtime-match #hash()
                               `(((p/ (phash (ann1 ann1) (ann2 val2)) 0)
                                  (p/ (phash (ann2 val2)) 0)))
                               '(p/ #hash((ann1 . 1) (ann2 . 2)) 0))
                `(p/ #hash((ann2 . 2)) 0))

  (check-equal? (runtime-match #hash()
                               `((((ann1 ann1) (ann2 val2) / 0)
                                  ((ann2 val2) ann1 / 0)))
                               '(p/ #hash((ann1 . 1) (ann2 . 2)) 0))
                `(p/ #hash((ann1 . 1) (ann2 . 2)) 0))

  ; note below handling of the non-variable ann2
  #| need a better way of handling this generally
     basically if a pair is specified, the first thing
     should always be interpreted as a literal, and
     the second thing should be a pattern/template.
     if a single thing is specified, we want it to
     expand into (literal pat-var) in patterns,
     but (literal whatever) in templates. since
     i dont care about the values for now, i'm
     going to come back to this later. |#
  (check-equal? (runtime-match #hash((ann2 . _))
                               `((((ann1 ann1) (ann2 val2) / 0)
                                  (ann2 ann1 / 0)))
                               '(p/ #hash((ann1 . 1) (ann2 . 2)) 0))
                `(p/ #hash((ann1 . 1) (ann2 . ann2)) 0))

  ; capture rest of hash subpattern:
  
  (check-equal? (destructure #hash() #hash()
                             #hash((ann1 . 2)(ann2 . 2))
                             '(phash others ...))
                #hash((others . #hash((ann1 . 2) (ann2 . 2)))))

  (check-equal? (destructure #hash() #hash()
                             #hash((ann1 . 2)(ann2 . 2))
                             '(phash (ann1 a) others ...))
                #hash((a . 2)(others . #hash((ann2 . 2)))))

  (check-equal? (destructure #hash() #hash()
                             #hash((ann1 . 1) (ann2 . 2))
                             '(phash (ann2 val) others ...))
                #hash((val . 2)(others . #hash((ann1 . 1)))))

  (check-equal? (runtime-match #hash()
                               '(((phash others ...)
                                  (phash others ...)))
                               #hash((ann1 . 1) (ann2 . 2)))
                #hash((ann1 . 1) (ann2 . 2)))

  (check-equal? (runtime-match #hash()
                               '(((phash (ann1 val) others ...)
                                  (phash others ...)))
                               #hash((ann1 . 1) (ann2 . 2)))
                #hash((ann2 . 2)))
  
  (check-equal? (runtime-match #hash()
                               '(((phash (ann2 val) others ...)
                                  (phash (ann1 val) others ...)))
                               #hash((ann1 . 1) (ann2 . 2)))
                #hash((ann1 . 2)))

  (check-equal? (runtime-match #hash()
                               '(((phash (ann2 val) others ...)
                                  (phash (ann4 val) others ...)))
                               #hash((ann1 . 1) (ann2 . 2) (ann3 . 3)))
                #hash((ann1 . 1) (ann3 . 3) (ann4 . 2)))
  
  )



; helpers for destructuring

#| bind : maybe-env → (env → maybe-env) → maybe-env
   passthrough match failures |#
(define (bind x f)
  (if (equal? 'no-match x)
      'no-match
      (f x)))

#| append-hashes : hash → hash → hash
   for incrementally binding lists along folds|#
(define (append-hashes h1 h2)
  (hash-union
   h1
   ; must be a better way to do this
   ; which doesnt involve rebuilding the hash
   (make-hash (hash-map h2 (λ (k v) (cons k (list v)))))
   #:combine (λ (a b) (append a b))))


#| destructure : stx → env
   info forthcoming. see tests |#
(define/memo* (destructure literals c-env arg pat)
  #;(println `(destructure ,arg ,pat ,c-env))
  (define D (curry destructure literals c-env))
  (define (accumulate-matches pat x acc)
    (bind acc
          (λ (_) (bind (destructure literals #hash() x pat)
                       (curry append-hashes acc)))))
  (define constructor-id?
    (curry hash-has-key? literals))
  (define literal?
    (disjoin number? constructor-id? empty?))
  ; note the empty set case
  (define pattern-variable?
    (conjoin symbol? (negate literal?)))
  
  (match* (pat arg)
    [((? literal?) (== pat))
     c-env]
    [((? pattern-variable?) _)
     (hash-set c-env pat arg)]
    [(`(quote ,p) _)
     #:when (equal? p arg)
     c-env]

    ; annotation patterns
    [(`(p/ (phash ,pairs ...) ,stx-pat)
      `(p/ ,(? hash? ann-arg) ,stx-arg))
     (bind (D ann-arg `(phash ,@pairs))
           (λ (env1)
             (destructure literals env1 stx-arg stx-pat)))]
    [((list 'phash pairs ... and-val ''...)
      (? hash? ann-arg))
     (bind (D ann-arg `(phash ,@pairs))
           (λ (new-env)
             #;(println new-env)
             (define remainder
               (for/fold ([env ann-arg])
                         ([pair pairs])
                 #;(println env)
                 (match pair
                   [`(,key ,value)
                    #;(println `(removing ,key))
                    (hash-remove env key)])))
             #;(println remainder)
             (hash-set new-env and-val remainder)))]
    [((list 'phash pairs ... and-val '...)
      (? hash? ann-arg))
     (bind (D ann-arg `(phash ,@pairs))
           (λ (new-env)
             #;(println new-env)
             (define remainder
               (for/fold ([env ann-arg])
                         ([pair pairs])
                 #;(println env)
                 (match pair
                   [`(,key ,value)
                    #;(println `(removing ,key))
                    (hash-remove env key)])))
             #;(println remainder)
             (hash-set new-env and-val remainder)))]
    [(`(phash ,pairs ...)
      (? hash? ann-arg))
     (for/fold ([env c-env])
               ([pair pairs])
       (match env
         ['no-match 'no-match]
         [_
          (match pair
            [`(,key ,pat-value)
             (bind (hash-ref ann-arg key 'no-match)
                   (λ (arg-value)
                     (destructure literals env arg-value pat-value)))])]))]
    [(`(p/ ,ann-pat ,stx-pat) `(p/ ,ann-arg ,stx-arg))
     ; obviously not full generality
     #:when (and (or (symbol? ann-pat)
                     (number? ann-pat)
                     (equal? '() ann-pat))
                 (equal? ann-pat ann-arg))
     (D stx-arg stx-pat)]

    ; containment patterns
    [(`(p⋱ ,context-name ,(app (curry D arg) (? hash? new-env)))
      _) ; should this be (? list?) ?
     (hash-union new-env (hash-set c-env context-name identity)
                 #:combine/key (λ (k v v1) v))]
    ; do i actually want to overwrite keys above?
    ; was getting an error in pre-fructure
    ; the below is exponential without memoization
    [(`(p⋱ ,context-name ,find-pat)
      `(,xs ...))
     ; split arg list at first match
     (define-values (initial-segment terminal-segment)
       (splitf-at xs
                  (λ (x)
                    (not (hash? (D x `(p⋱ ,context-name ,find-pat)))))))
     ; draw the rest of the owl
     (match* (initial-segment terminal-segment)
       [((== xs) _) 'no-match]
       [(`(,is ...) `(,hit ,ts ...))
        (define new-env (D hit `(p⋱ ,context-name ,find-pat)))

        (hash-set new-env context-name
                  (compose (λ (x) `(,@is ,x ,@ts))
                           (hash-ref new-env context-name)))])]
    [(`(p⋱+ ,context-name ,find-pat)
      `(,xs ...))
     (define (matcher stx)
       (match (D stx find-pat)
         ['no-match #f]
         [_ #t]))
     (match-define (list ctx contents)
       (multi-containment matcher xs))
     #;(println `(find-pat ,find-pat ctx ,ctx contents ,contents))
     (define new-env (hash-set c-env
                               context-name ctx))
     #;(define list-of-hashes
         (map (λ (x) (D x find-pat)) contents))
     
     (define newer-env
       (for/fold ([env new-env])
                 ([c contents]
                  #;[x xs])
         (append-hashes env (D c find-pat))))
     
     newer-env]
    
    ; list and ellipses patterns
    [(`(pcons ,first-pat ,rest-pat)
      `(,first-arg ,rest-arg ...))
     (bind (D (first arg) first-pat)
           (λ (h) (bind (D rest-arg rest-pat)
                        ; not sure how duplicates end up in here
                        ; but they do, somehow, due to annotation patterns
                        (λ (x)
                          (hash-union
                           h x
                           #:combine/key
                           (λ (k v1 v2) (if (equal? v1 v2)
                                            v1
                                            (error "error")))))
                        #;(curry hash-union h))))]    
    [(`(p... ,p ,ps)
      (? list?))
     (define/match (greedy arg-init arg-tail)
       [('() _)
        (bind (D arg-tail ps)
              (bind (D `() p)
                    ; see above ...
                    (λ (x)
                      (λ (y)
                        (hash-union
                         x y
                         #:combine/key
                         (λ (k v1 v2) (if (equal? v1 v2)
                                          v1
                                          (error "error"))))))
                    #;(curry hash-union)))]
       [(`(,as ... ,b) `(,cs ...))
        (match (D cs ps)
          ['no-match (greedy as `(,b ,@cs))]
          [new-env
           (match (foldl (curry accumulate-matches p)
                         #hash() `(,@as ,b))
             ['no-match (greedy as `(,b ,@cs))]
             [old-env (hash-union new-env old-env)])])])
     (greedy arg '())]
    
    [(_ _) 'no-match]))


#| destructure : literals → env → stx
   info forthcoming. see tests |#
(define (restructure types env stx)
  #;(println `(restructure ,types ,env ,stx))
  (define R (curry restructure types env))
  (define (constructor-id? id)
    (hash-has-key? types id))
  (define literal?
    (disjoin number? boolean? string? constructor-id? empty?))
  (define variable?
    (conjoin symbol? (negate literal?)))
  (match stx
    [(? literal? d) d]
    [(? variable? id)
     (if (hash-has-key? env id)
         (hash-ref env id)
         id)
     ; HACK for fructure: treat unbound variables as literals
     ; introduced this when implementing pattern painting
     ; because when i create new transforms from painted structure
     ; the identifier chars in those structures are undeclared literals
     #;(hash-ref env id)] ; todo: add error message
    [`(quote ,d) d]

    ; list and ellipses templates
    [`(pcons ,p ,ps)
     (cons (R p) (R ps))]
    [`(p... ,p ,ps)
     (append (R p) (R ps))]

    ; containment templates
    [`(p⋱ ,(? symbol? id) ,arg)
     ((hash-ref env id) (R arg))]

    ; wip
    [`(p⋱+ ,(? symbol? id) ,arg)
     (apply (hash-ref env id) (map R arg))]

    ; annotation template
    [`(p/ (phash ,pairs ...) ,stx-tem)
     `(p/ ,(R `(phash ,@pairs)) ,(R stx-tem))]
    [(list 'phash pairs ... and-val ''...)
     ; NOTE HACKY DOUBLE-QUOTE ABOVE!!!!!!!
     (define pair-hash
       (for/hash ([pair pairs])
         (match pair
           [`(,key ,value)
            (values key (R value))])))
     #;(println `(blarg ,(hash-ref env and-val) ,pair-hash))
     (hash-union (hash-ref env and-val) pair-hash
                 #:combine/key (λ (k v1 v2) v2))]
    [`(phash ,pairs ...)
     #;(println env)
     (for/hash ([pair pairs])
       (match pair
         [`(,key ,value)
          (values key (R value))]))]
    
    [`(p/ ,ann-tem ,stx-tem)
     ; obviously not full generality
     #:when (or (symbol? ann-tem)
                (number? ann-tem)
                (equal? ann-tem '()))
     `(p/ ,ann-tem ,(R stx-tem))]))


#| runtime-match : literals → pattern/templates → stx → stx |#
(define/memo* (runtime-match literals pat-tems source)
  #;(println `(runtime-match ,pat-tems ,source))
  (define new-pat-tems (map runtime-match-rewriter pat-tems))
  (match new-pat-tems
    [`() 'no-match]
    [`((,(app (compose desugar desugar-qq) pattern)
        ,(app (compose desugar desugar-qq) template))
       ,other-clauses ...)
     #;(println `(pat-tem-src ,pattern ,template ,source))
     #;(println `(des ,(destructure types #hash() source pattern)))
     (define env (destructure literals #hash() source pattern))
     (if (equal? 'no-match env)
         (runtime-match literals other-clauses source)
         (restructure literals env template))]))


(define/memo* (anno-runtime-match types pat-tems source)
  (define new-pat-tems (map runtime-match-rewriter pat-tems))
  (match new-pat-tems
    [`() 'no-match]
    [`((,(app (compose desugar
                       desugar-qq
                       desugar-annotation) pattern)
        ,(app (compose desugar
                       desugar-annotation) template))
       ,other-clauses ...)
     (define env (destructure types #hash() source pattern))
     (if (equal? 'no-match env)
         (anno-runtime-match types other-clauses source)
         (restructure types env template))]))

(module+ test
  
  (check-equal? (desugar-annotation '(▹ / a))
                '(p/ ▹ a))

  (check-equal? (desugar-annotation '(p/ ▹ 2))
                '(p/ ▹ 2))

  (check-equal? (desugar-annotation '(p/ ▹ (p/ () 2)))
                '(p/ ▹ (p/ () 2)))

  (check-equal? (destructure #hash() #hash() '(p/ ▹ 1) '(p/ ▹ 1))
                #hash())

  (check-equal? (destructure #hash() #hash() '(p/ ▹ 1) '(p/ ▹ 1))
                #hash())

  (check-equal? (anno-runtime-match #hash() '(((▹ / 1) (▹ / 1)))
                                    `(p/ ▹ 1))
                `(p/ ▹ 1))
  
  (check-equal? (anno-runtime-match #hash() '(((▹ / a) (▹ / 2)))
                                    `(p/ ▹ 1))
                `(p/ ▹ 2))
  

  (define literals
    #hash((app . ())
          (λ . ())
          (let . ())
          (pair . ())
          (◇ . ())
          (▹ . ())
          (⊙ . ())
          (expr . ())))

  (define initial-state
    '(p/ ()
         (◇ (p/ ▹
                (p/ ()
                    (⊙ expr))))))
  
  (check-equal? (anno-runtime-match
                 literals
                 '([⋱
                     (▹ (⊙ expr))
                     (app (▹ (⊙ expr)) (⊙ expr))])
                 initial-state)
                '(p/ ()
                     (◇
                       (p/ ()
                           (app
                            (p/ ▹ (p/ () (⊙ expr)))
                            (p/ () (⊙ expr)))))))

  (check-equal? (anno-runtime-match
                 literals
                 '([⋱
                     (▹ (⊙ expr))
                     (▹ 0)])
                 '(p/ ()
                      (◇
                        (p/ ()
                            (app (p/ ▹ (p/ () (⊙ expr)))
                                 (p/ () (⊙ expr)))))))
                '(p/ () (◇ (p/ () (app (p/ ▹ 0) (p/ () (⊙ expr)))))))


  )


#| runtime-match-rewriter : pattern/templates → pattern/templates
   an ad-hoc rewriting phase; possible seed for macro system |#
(define (runtime-match-rewriter pat-tem)
  (match pat-tem
    [(or `(,a ⋱ ,b) `(⋱ ,a ,b))
     (let ([ctx (gensym)])
       `((p⋱ ,ctx  ,a) (p⋱ ,ctx ,b)))]
    [_ pat-tem]))


#| ratch : looks like match but tastes like fructerm |#
(define-syntax-rule (ratch source clauses ...)
  (runtime-match #hash() '(clauses ...) source))


; ratch example usage
(module+ test
  (check-equal?
   (ratch '(let ([a 1] [b 2]) 0)
     [(form ([id init] ...) body)
      (id ... init ...)])
   (match '(let ([a 1] [b 2]) 0)
     [`(,form ([,id ,init] ...) ,body)
      `(,@id ,@init)])))



#| EXAMPLES & TESTS |#

(module+ test

  
  ; destructure examples and tests

  (define (test-destr source pattern)
    (destructure #hash() #hash()
                 source (desugar pattern)))

  
  ; destructure literal/variable tests

  (check-equal? (test-destr 1 1)
                #hash())

  (check-equal? (test-destr 1 'a)
                #hash((a . 1)))

  (check-equal? (test-destr 'a 'a)
                #hash((a . a)))

  
  ; destructure quote tests

  (check-equal? (test-destr 'a '(quote a))
                #hash())

  (check-equal? (test-destr '(quote a) 'a)
                #hash((a . (quote a))))

  (check-equal? (test-destr 'a '(quote b))
                'no-match)

  (check-equal? (test-destr '(a 1) '((quote a) 1))
                #hash())

  (check-equal? (test-destr '(a 1) '((quote a) b))
                #hash((b . 1)))

  (check-equal? (test-destr '(a 1) '(quote (a 1)))
                #hash())


  ; destructure quasiquote/unquote tests

  (check-equal?
   (ratch '(if 1 2 3)
     [`(if ,a ,b ,c)
      `(cond [,a ,b]
             [else ,c])])
   (match '(if 1 2 3)
     [`(if ,a ,b ,c)
      `(cond [,a ,b]
             [else ,c])]))

  ;
  ; FIX BUG
  #;
  (check-equal?
   (ratch  '(if 1 2 3)
     [`(if ,a ,b ,c)
      `(cond [`,a ,b]
             [else ,c])])
   (match  '(if 1 2 3)
     [`(if ,a ,b ,c)
      `(cond [`,a ,b]
             [else ,c])]))
  

  
  ; destructure tests for pcons pattern

  (check-equal? (test-destr '() '(pcons () anything))
                'no-match)

  (check-equal? (test-destr '(1) '(pcons 1 ()))
                #hash())

  (check-equal? (test-destr  '(1) '(pcons a ()))
                #hash((a . 1)))

  (check-equal? (test-destr '(1 2) '(pcons 1 (2)))
                #hash())

  (check-equal? (test-destr '(1 2) '(pcons 1 (pcons 2 ())))
                #hash())

  (check-equal? (test-destr '(1 2 3) '(pcons a (2 3)))
                #hash((a . 1)))

  (check-equal? (test-destr '(1 2 3) '(pcons 1 (pcons 2 (pcons 3 ()))))
                #hash())


  ; destructure  tests for p... pattern

  (check-equal? (test-destr '() '(p... a ()))
                #hash((a . ())))

  (check-equal? (test-destr '(1) '(p... a ()))
                #hash((a . (1))))

  (check-equal? (test-destr '(1 2) '(p... a (pcons 2 ())))
                #hash((a . (1))))

  ; destructure tests for p... pattern with complex subpattern

  (check-equal? (test-destr
                 '((1 1) (1 2) (4 5) 3 4)
                 '(p... (a b) (3 4)))
                #hash((a . (1 1 4)) (b . (1 2 5))))


  ; destructure tests for ... multi-pattern

  ; FIX THIS BUG!!!!!!
  #; (check-equal? (test-destr
                    '()
                    '(1 ...))
                   #hash())

  (check-equal? (test-destr
                 '(1)
                 '(2 ...))
                'no-match)

  (check-equal? (test-destr
                 '(a)
                 '(1 ...))
                'no-match)

  (check-equal? (test-destr
                 '(1)
                 '(1 ...))
                #hash())

  (check-equal? (test-destr
                 '()
                 '(f ...))
                #hash((f . ())))

  (check-equal? (test-destr
                 '(1)
                 '(f ...))
                #hash((f . (1))))

  (check-equal? (test-destr
                 '(1)
                 '(1 f ...))
                #hash((f . ())))

  (check-equal? (test-destr
                 '(1 2 3 4 5)
                 '(1 2 d ...))
                '#hash((d . (3 4 5))))

  (check-equal? (test-destr
                 '(1 2 3 4 5)
                 '(1 2 3 d ... 5))
                '#hash((d . (4))))

  (check-equal? (test-destr
                 '(1 2 3 4 5)
                 '(a ... 2 b ... 5))
                #hash((a . (1)) (b . (3 4))))

  (check-equal? (test-destr
                 '(1 2 3 4 5)
                 '(1 a ... 2 b ... 5))
                #hash((a . ()) (b . (3 4))))

  (check-equal? (test-destr
                 '(1 2 3 4 5)
                 '(1 2 3 d ... 5 f ...))
                #hash((f . ()) (d . (4))))

  (check-equal? (test-destr
                 '(1 2 3 4 5 6)
                 '(1 2 3 d ... 5 f ... 6))
                #hash((f . ()) (d . (4))))

  (check-equal? (test-destr
                 '(1 2 3 4 5 6 7)
                 '(1 2 3 d ... 5 f ... 7))
                #hash((f . (6)) (d . (4))))

  (check-equal? (test-destr
                 '(1 2 3 4 4 5 6 6 6 7 8)
                 '(1 2 3 d ... 5 f ... 7 8))
                #hash((f . (6 6 6)) (d . (4 4))))


  ; destructure tests for ... multi-pattern with complex subpattern

  (check-equal? (test-destr
                 '(1 (4 1 (6 2)) (4 3 (6 4)))
                 '(1 (4 a (6 b)) ...))
                #hash((a . (1 3)) (b . (2 4))))


  ; destructure tests for nested ... multi-pattern

  (check-equal? (test-destr
                 '(1 (1 2 3) (4 5 6))
                 '(1 (a ...) ...))
                #hash((a . ((1 2 3) (4 5 6)))))

  (check-equal? (test-destr
                 '(1 (1 2 3) (1 5 6))
                 '(1 (1 a ...) ...))
                #hash((a . ((2 3) (5 6)))))

  (check-equal? (test-destr
                 '(1 (1 0 0 0 1 0 0) (1 0 1 0 0 0))
                 '(1 (1 a ... 1 b ...) ...))
                #hash((a . ((0 0 0) (0)))
                      (b . ((0 0) (0 0 0)))))


  ; destructure tests for simple containment pattern

  (define contain-test
    (λ (source pattern)
      (destructure #hash() #hash()
                   source (desugar pattern))))

  (check-equal? ((hash-ref (contain-test
                            '(1)
                            '(a ⋱ 1))
                           'a)
                 1)
                '(1))

  (check-equal? ((hash-ref (contain-test
                            '(1 0)
                            '(a ⋱ 1))
                           'a)
                 1)
                '(1 0))

  (check-equal? ((hash-ref (contain-test
                            '(0 1)
                            '(a ⋱ 1))
                           'a)
                 1)
                '(0 1))

  (check-equal? ((hash-ref (contain-test
                            '(0 1 0)
                            '(a ⋱ 1))
                           'a)
                 1)
                '(0 1 0))

  (check-equal? ((hash-ref (contain-test
                            '(0 (0 1) 0)
                            '(a ⋱ 1))
                           'a)
                 1)
                '(0 (0 1) 0))

  ; TODO: should this be valid? or only match lists???
  (check-equal? ((hash-ref (contain-test
                            1
                            '(a ⋱ 1))
                           'a)
                 2)
                2)

  (check-equal? (hash-ref (contain-test
                           1
                           '(a ⋱ b))
                          'b)
                1)

  (check-equal? ((hash-ref (contain-test
                            '(0 2)
                            '(a ⋱ (0 b)))
                           'a)
                 '(0 3))
                '(0 3))

  (check-equal? ((hash-ref (contain-test
                            '(0 (0 2) 0)
                            '(a ⋱ (0 b)))
                           'a)
                 '(0 3))
                '(0 (0 3) 0))



  ; integration tests: basic restructuring

  (check-equal? (runtime-match #hash() '((a 2)) '1)
                2)

  (check-equal? (runtime-match #hash() '(((a b) b)) '(1 2))
                2)

  (check-equal? (runtime-match #hash() '(((a ...) (a ...))) '(1 2))
                '(1 2))

  ; BUG!!!! returns '(0 (1 2)). Need to change restructuring approach
  ; possibly destructuring approach, for both ... and ⋱+
  #;
  (check-equal? (runtime-match #hash() '(((a ...) ((0 a) ...))) '(1 2))
                '((0 1) (0 2)))


  ; notes on trying to fix this:
  #; (match '(0 0 1 0 0 0)
       [`(,a ... 1 ,b ...)
        `((x ,a ,(y b) ...) ...)])
  #; (match '(0 0 1 0 0 0)
       [(p... a (pcons 1 (p... b '())))
        (p... (pcons 'x (pcons a (p... (pcons y (pcons b '()))'())))'())])
  #; (match '(0 0 1 0 0 0)
       [`(,a ... 1 ,b ...)
        `(,@(map (λ (a) `(x ,a ,@(map (λ (b) `(y ,b)) b))) a))])
  #; (match '(0 0 1 0 0 0)
       [`(,a ... 1 ,b ...)
        `((x ,a ,(y b) ...) ...)])


  ; integration tests: ellipses restructuring
  
  (check-equal? (runtime-match #hash() '(((a b ...) (b ... (a)))) '(1 2 3))
                '(2 3 (1)))

  (check-equal? (runtime-match #hash() '(((a b ...) (b ... (a)))) '(1))
                '((1)))

  
  ; integration tests: basic containment de/restructuring
  
  (check-equal? (runtime-match #hash()
                               '(((a ⋱ 1)
                                  (a ⋱ 2)))
                               '(1))
                '(2))

  (check-equal? (runtime-match #hash()
                               '(((a ⋱ 1)
                                  (a ⋱ 2)))
                               '(0 1))
                '(0 2))

  (check-equal? (runtime-match #hash()
                               '(((a ⋱ 1)
                                  (a ⋱ 2)))
                               '(0 (0 0 1) 0))
                '(0 (0 0 2) 0))

  (check-equal? (runtime-match #hash()
                               '(((a ⋱ (0 b))
                                  (a ⋱ (1 b))))
                               '(0 (0 2) 0))
                '(0 (1 2) 0))

  ; todo: more examples!

  ; multi-containment pattern tests

  (check-equal? ((hash-ref (contain-test
                            '(0 1)
                            '(a ⋱+ 1))
                           'a)
                 2)
                '(0 2))

  (check-equal? ((hash-ref (contain-test
                            '(0 1 1)
                            '(a ⋱+ 1))
                           'a)
                 2 3)
                '(0 2 3))

  #;
  (check-equal? (runtime-match #hash()
                               '(((a ⋱+ (0 b))
                                  (a ⋱+ ((1 b)))))
                               '(0 (0 2) 0))
                '(0 (1 2) 0))
  #;
  (check-equal? (runtime-match #hash()
                               '(((a ⋱+ (0 b))
                                  (a ⋱+ (1 b))))
                               '(0 (0 2) (0 3)))
                '(0 (1 2) (1 3)))
  
  )