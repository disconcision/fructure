#lang racket

(require racket/hash)

; fructure uses some additional match syntax for rewriting
(require "../fructerm/fructerm.rkt"
         "../fructerm/f-match.rkt")

(require "attributes.rkt" ; syntax->attributed-syntax
         "layout.rkt" ; syntax->pixels
         )

(require "new-syntax.rkt"
         ; temporary renames so as not to intefere with f-match
         (only-in "../containment-patterns/containment-patterns.rkt"
                  (⋱ ⋱x)
                  (⋱1 ⋱1x)
                  (⋱+ ⋱+x)))

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


(define literals
  #hash((var . ())
        (ref . ())
        (id . ())
        (app . ())
        (and . ())
        (or . ())
        (not . ())
        (lambda . ())
        (λ . ())
        (let . ())
        (|| . ()) ; hmmm....
        (♦ . ())
        (◇ . ())
        (▹ . ())
        (▹▹ . ())
        (⊙ . ())
        (+ . ())
        (expr . ())
        (pat . ())
        (char .())))

(define my-desugar
  (compose (curry restructure literals #hash()) desugar))

(define initial-state
  (hash 'stx (my-desugar '(◇ (▹ (sort expr) / ⊙)))
        'mode 'nav
        'transforms '()
        'messages '("hello world")))


(struct -> (class props payload) #:transparent)

(define (wrap⋱select mstx)
  (for/list ([s mstx])
    (match s
      [`[,a ,b]
       `[⋱ ,(select a) ,(select b)]])))

(define (wrap⋱ mstx)
  (match mstx
    [`[,a ,b]
     `[⋱ ,a ,b]]))

; this is a select fn for sugared syntax
; it won't work on raw p/
(define (select mstx)
  (match mstx
    [`(,y ... / ,d ...)
     `(▹ ,@y / ,@d)]))


(define (make-constructor raw-rule)
  `(compose->
    ,(-> 'runtime
         (set 'meta 'move-▹)
         '([(c ⋱ (▹ ys ... / (d ⋱ (xs ... / ⊙))))
            (c ⋱ (ys ... / (d ⋱ (▹ xs ... / ⊙))))]
           [A A]))
    ,(-> 'runtime
         (set 'object 'constructor)
         (wrap⋱select raw-rule))))

(define make-destructor
  make-constructor)

(define identity->
  (-> 'runtime
      (set 'object)
      '([A A])))

(define (make-movement raw-rule)
  (-> 'runtime
      (set 'meta 'move-▹)
      raw-rule))

(define select-first-⊙
  (curry runtime-match literals
         '([(c ⋱ (▹ ys ... / (d ⋱ (xs ... / ⊙))))
            (c ⋱ (ys ... / (d ⋱ (▹ xs ... / ⊙))))]
           [A A])))



(define alphabet
  '(a b c d e f g h i j k l m n o p q r s t u v w x y z))


; make constructors for each character
(define alpha-constructors
  (for/fold ([alpha (hash)])
            ([x alphabet])
    (hash-set alpha
              (symbol->string x)
              (-> 'runtime (set)
                  `([⋱
                      (xs ... / (id as ... (▹ ys ... / b) bs ...))
                      (xs ... / (id as ... ([sort char] / ',x) (▹ ys ... / b) bs ...))])))))


(define raw-ish-base-constructor-list
  (list '([⋱
            (▹ [sort expr] xs ... / ⊙)
            (▹ [sort expr] xs ... / 0)])
        '([⋱
            (▹ [sort expr] xs ... / ⊙)
            (▹ [sort expr] xs ... / (app ([sort expr] / ⊙)
                                         ([sort expr] / ⊙)))])
        '([⋱
            (▹ [sort expr] xs ... / ⊙)
            (▹ [sort expr] xs ... / (λ ( / (( / (id ([sort char] / ⊙)))))
                                      ([sort expr] / ⊙)))])))


(define raw-ish-base-destructor-list
  (list
   '([⋱
       (▹ xs ... / 0)
       (▹ xs ... / ⊙)]
     [⋱
       (▹ xs ... / (ref a))
       (▹ xs ... / ⊙)]
     [⋱
       (▹ xs ... / (app a b))
       (▹ xs ... / ⊙)]
     [⋱
       (▹ xs ... / (λ a b))
       (▹ xs ... / ⊙)]
     )))


(define raw-ish-alpha-constructors
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
  (append raw-ish-base-constructor-list
          raw-ish-base-destructor-list
          raw-ish-alpha-constructors))


(define (transforms->menu raw-constructor-list stx)
  (for/fold ([menu '()])
            ([constructor raw-constructor-list])
    (if (test-apply-single-> constructor stx)
        `(,@menu (,constructor
                  ,(f/match (runtime-match literals constructor stx)
                     [(c ⋱ (▹ as ... / a))
                      (as ... / a)])))
        menu)))


(define (make-menu in-scope current-selection)
  (transforms->menu
   (append base-transforms
           (for/list ([id in-scope])
             `([⋱
                 (▹ [sort expr] xs ... / ⊙)
                 (▹ [sort expr] xs ... /
                    (ref ',id))])))
   current-selection))




#;(define base-constructor-list
    (map make-constructor
         raw-base-constructor-list))




(module+ test
  (require rackunit)
  (define raw-base-constructor-list
    #;(list '([(/ [sort: expr] a/ ⊙)
               (/ a/ 0)])
            '([(/ [sort: expr] a/ ⊙)
               (/ a/ (app (/ [sort: expr] ⊙)
                          (/ [sort: expr] ⊙)))])
            '([(/ [sort: expr] a/ ⊙)
               (/ a/ (λ (/ [sort: params]
                           `(,(/ [sort: pat]
                                 `(id ,(/ [sort: char] ⊙)))))
                       (/ (sort: expr) ⊙)))])
            #| need to codify selectability pattern
             to start: only selectables are sort: exprs|# 
            )
    (list '([([sort expr] xs ... / ⊙)
             ([sort expr] xs ... / 0)])
          '([([sort expr] xs ... / ⊙)
             ([sort expr] xs ... / (app ([sort expr] / ⊙)
                                        ([sort expr] / ⊙)))])
          #;'([([sort expr] xs ... / ⊙)
               ([sort expr] xs ... / (λ ([cont params] / (([sort pat] / (id ([sort char] / ⊙)))))
                                       ([sort expr] / ⊙)))])
          ; leaving sort off pat, [container params] for now for smooth sort-based movement
          '([([sort expr] xs ... / ⊙)
             ([sort expr] xs ... / (λ ( / (( / (id ([sort char] / ⊙)))))
                                     ([sort expr] / ⊙)))])))
  (check-equal?
   (transforms->menu
    raw-base-constructor-list
    '(p/  #hash((sort . expr) (▹ . ▹)) ⊙))
   '((((((sort expr) xs ... / ⊙) ((sort expr) xs ... / 0)))
      (p/ #hash((sort . expr)) 0))
     (((((sort expr) xs ... / ⊙)
        ((sort expr) xs ... / (app ((sort expr) / ⊙)
                                   ((sort expr) / ⊙)))))
      (p/ #hash((sort . expr))
          (app (p/ #hash((sort . expr)) ⊙)
               (p/ #hash((sort . expr)) ⊙))))
     (((((sort expr) xs ... / ⊙)
        ((sort expr) xs ... / (λ (/ ((/ (id ((sort char) / ⊙)))))
                                ((sort expr) / ⊙)))))
      (p/ #hash((sort . expr))
          (λ (p/ #hash() ((p/ #hash() (id (p/ #hash((sort . char)) ⊙)))))
            (p/ #hash((sort . expr)) ⊙)))))))

(define (id->ref-constructor id)
  (make-constructor
   `([([sort expr] xs ... / ⊙)
      ([sort expr] xs ... /
                   (ref ',id))])))


(define keymap
  ; map from keys to functions
  (hash

   ; constructors
   
   "1" (make-constructor
        '([([sort expr] xs ... / ⊙)
           ([sort expr] xs ... / 0)]))
   
   "2" (make-constructor
        '([([sort expr] xs ... / ⊙)
           ([sort expr] xs ... / (app ([sort expr] / ⊙)
                                      ([sort expr] / ⊙)))]))
   "3" (make-constructor
        '([([sort expr] xs ... / ⊙)
           ([sort expr] xs ... / (λ ( / ((#;[sort pat] / (id ([sort char] / ⊙)))))
                                   ([sort expr] / ⊙)))]))

   ; destructors
   
   "\b" (-> 'runtime (set)
            '([⋱
                (xs ... / (id as ... a (▹ ys ... / b) bs ...))
                (xs ... / (id as ... (▹ ys ... / b) bs ...))]))

   "\u007F" `(fallthrough->
              ,(-> 'runtime (set)
                   '([⋱
                       (xs ... / (id as ... (▹ ys ... / a) (zs ... / b) bs ...))
                       (xs ... / (id as ... (▹ zs ... / b) bs ...))]))
              ,(make-destructor
                '([(xs ... / 0)
                   (xs ... / ⊙)]
                  [(xs ... / (ref a))
                   (xs ... / ⊙)]
                  [(xs ... / (id a))
                   (xs ... / ⊙)]
                  [(xs ... / (app a b))
                   (xs ... / ⊙)]
                  [(xs ... / (λ a b))
                   (xs ... / ⊙)]
                  )))

   ; movements
   
   "up" (make-movement
         '(#;[(◇ a ... (▹ As ... / b) c ...)
              (◇ a ... (▹ As ... / b) c ...)]
           [⋱
             (a ... / (λ (b ... / ((c ... / (id x ... (▹ ys ... / y) z ...)))) e))
             (▹ a ... / (λ (b ... / ((c ... / (id x ... (ys ... / y) z ...)))) e))]
           [⋱
             (As ... / (a ... (▹ Bs ... / b) c ...))
             (▹ As ... / (a ... (Bs ... / b) c ...))]
           ))

   "down" (make-movement
           '(#;[⋱
                 (▹ As ... / ⊙)
                 (▹ As ... / ⊙)]
             #;[⋱
                 (▹ As ... / 0)
                 (▹ As ... / 0)]
             [⋱
               (▹ a ... / (λ (b ... / ((c ... / (id (ys ... / y) z ...)))) e))
               (a ... / (λ (b ... / ((c ... / (id (▹ ys ... / y) z ...)))) e))]
             [⋱
               (▹ As ... / (ctx ⋱ (sort Bs ... / b)))
               (As ... / (ctx ⋱ (▹ sort Bs ... / b)))]
             ))

   #;#;"left" (make-movement
               '([⋱
                   (◇ (▹ As ... / c))
                   (◇ (▹ As ... / c))]
                 [⋱
                   (var (▹ As ... / c))
                   (var (▹ As ... / c))]
                 [⋱
                   (app (▹ As ... / c) d ...)
                   (app (▹ As ... / c) d ...)]
                 [⋱
                   (λ (Cs ... / ((▹ Bs ... / a))) b)
                   (λ (Cs ... / ((▹ Bs ... / a))) b)]
                 [⋱
                   (λ (Cs ... / ((As ... / a))) (▹ Bs ... / b))
                   (λ (Cs ... / ((▹ As ... / a))) (Bs ... / b))]
                 [⋱
                   ((▹ As ... / c) d ...)
                   ((▹ As ... / c) d ...)]
                 [⋱
                   (a ... (As ... / b) (▹ Bs ... / c) d ...)
                   (a ... (▹ As ... / b) (Bs ... / c) d ...)]))

   #;#;"right" (make-movement
                '([⋱
                    (λ (Cs ... / ((▹ As ... / a))) (Bs ... / b))
                    (λ (Cs ... / ((As ... / a))) (▹ Bs ... / b))]
                  [⋱
                    (a ... (▹ As ... / b) (Bs ... / c) d ...)
                    (a ... (As ... / b) (▹ Bs ... / c) d ...)]))  
   ))



; perform a sequence of actions
(define (do-seq stx actions)
  (for/fold ([s stx])
            ([a actions])
    (runtime-match literals a s)))



(define (apply-> transform state)
  (define update (curry hash-set* state))
  (define-from state
    stx mode transforms messages)
  (match transform
    [`(fallthrough-> ,t0 ,t1)
     (let ([new-state (apply-> t0 state)])
       (if (equal? new-state state)
           (apply-> t1 state)
           new-state))]
    [`(compose-> ,x)
     (apply-> x state)]
    [`(compose-> ,xs  ..1 ,x)
     (apply-> `(compose-> ,@xs)
              (apply-> x state))]
    [(-> 'runtime _ t)
     (match (runtime-match literals t stx)
       ['no-match state]
       [new-stx
        (update
         'stx new-stx
         'transforms `(,t ,@transforms)
         'messages `("performed action" ,@messages)
         )])]))


(define (test-apply-single-> transform stx)
  (match (runtime-match literals transform stx)
    ['no-match #f]
    [_ #t]))


(define (extract-selection-and-scope stx)
  (f/match stx
    [(c ⋱ (▹ in-scope As ... / a))
     (values (in-scope As ... / a)
             in-scope)]    
    ; fallthrough case - current λ params list has no in-scope
    ; do/should i actually need this case?
    [(c ⋱ (▹ As ... / a))
     (values (As ... / a)
             '())]))


(define (my-select stx)
  (runtime-match literals
                 '([(y ... / a)
                    (▹ y ... / a)]) stx))



(define (mode:navigate key state)
  ; navigation major mode
  (define-from state
    stx mode transforms messages)
  (define update (curry hash-set* state))
  
  (match key
    ["right" ; moves cursor right in preorder traversal
     (define new-stx
       (f/match stx
         [(c ⋱ (▹ ys ... / (d ⋱ (sort xs ... / a))))
          (c ⋱ (ys ... / (d ⋱ (▹ sort xs ... / a))))]
         [(c ⋱ (capture-when (or (('▹ _) _ ... / _)
                                 (('sort _) _ ... / (not (⋱ (('▹ _) _ ... / _))))))
             `(,as ... ,(▹ ws ... / a) ,(zs ... / b) ,bs ...))
          (c ⋱... 
             `(,@as ,(ws ... / a) ,(▹ zs ... / b) ,@bs))]
         [x x]))
     (update 'stx new-stx)]
    
    ["left" ; moves cursor left in preorder traversal
     (define new-stx
       (f/match stx
         ; if there is a left-sibling (⋱c2) to the cursor which contains-or-is a sort
         [(⋱c1 ⋱ `(,as ...
                   ; find its rightmost sort not containing another sort (c) 
                   ,(⋱c2 ⋱ (capture-when (sort _ ... / (not (_ ⋱ (sort _ ... / _)))))
                         `(,bs ... ,(cs ... / c)))
                   ,ds ... ,(▹ es ... / e) ,fs ...))
          ; and move the cursor there
          (⋱c1 ⋱ `(,@as ,(⋱c2 ⋱... `(,@bs ,(▹ cs ... / c)))
                        ,@ds ,(es ... / e) ,@fs))]
         ; otherwise, find the most immediate containing sort
         [(⋱c1 ⋱ (and (sort as ... / (⋱c2 ⋱ (▹ bs ... / b)))
                      ; that is, a containing sort not containing a sort containing ▹ 
                      (not (sort _ ... / (_ ⋱ (sort _ ... / (_ ⋱ (▹ _ ... / _))))))))
          (⋱c1 ⋱ (▹ sort as ... / (⋱c2 ⋱ (bs ... / b))))]
         
         [x x]))
     (update 'stx new-stx)]

    ["\r" ; ENTER: switch to transform mode
     (update
      'mode 'menu
      'stx (f/match stx
             [(c ⋱ (▹ as ... / a))
              (c ⋱ (('transform (insert-menu-at-cursor (▹ as ... / a)))
                    as ... / a))]))]

    ["/"  (update 'messages (cons transforms messages))]
    
    ; undo (currently broken)
    [","  (match transforms
            ['() (update 'messages
                         `("no undo states" ,messages))]
            [_ (update 'messages `("reverting to previous state" ,@messages)
                       'stx (do-seq (hash-ref initial-state 'stx)
                                    (reverse (rest transforms)))
                       'transforms (rest transforms))])]
    ; transform keys
    [_
     (define transform
       (hash-ref (hash-union #;(make-ref-hash my-in-scope) alpha-constructors keymap #:combine/key (λ (k v v1) v)) key identity->))
     (apply-> transform state)]))



(define (select-first-⊙-in-unselected stx)
  (runtime-match
   literals
   '([(c ⋱ (xs ... / ⊙))
      (c ⋱ (▹ xs ... / ⊙))]
     [A A])
   stx))

(define (no-⊙? stx)
  (f/match stx
    [(c ⋱ '⊙) #f]
    [_ #t]))

(define (advance-cursor-to-next-hole stx)
  (f/match stx
    [(c ⋱ (▹ ys ... / (d ⋱ (xs ... / '⊙))))
     (c ⋱ (ys ... / (d ⋱ (▹ xs ... / '⊙))))]
    [(c ⋱ (capture-when (or (('▹ _) _ ... / _)
                            (_ ... / '⊙)))
        `(,as ... ,(▹ ws ... / a) ,(zs ... / b) ,bs ...))
     (c ⋱... 
        `(,@as ,(ws ... / a) ,(▹ zs ... / b) ,@bs))]
    [x (println "bullshit no hitter") x]))


(define (insert-menu-at-cursor stx)
  (define-values (current-selection in-scope)
    (extract-selection-and-scope stx))
  (define menu-stx (make-menu in-scope stx))
  (f/match stx
    [(c ⋱ (▹ xs ... / x))
     #:when (not (empty? menu-stx))
     (define menu-with-selection
       (match menu-stx
         [`((,t ,r) ,xs ...)
          `((,t ,(my-select r)) ,@xs)]))
     (c ⋱ (▹ ('menu menu-with-selection) xs ... / x))]
    [x (println "warning: no menu inserted")
       (when (empty? menu-stx)
         (println "warning: menu was empty; not handled"))
       x]))


(define (move-menu template)
  (define (local-augment stx)
    (f/match stx
      [(ctx ⋱ (in-scope ts ... / t))
       (ctx ⋱ (augment (in-scope ts ... / t)))]
      [x (println "warning: local-augment no-match") x]))
  ((compose (λ (stx)
              (if (no-⊙? template) stx (insert-menu-at-cursor stx)))
            ; HACK: prevents hitting right on an atom from bringing up the delete menu
            ; sort of... there might be a bug left here
            #;insert-menu-at-cursor
            local-augment
            advance-cursor-to-next-hole)
   template))


(define (mode:transform key state)
  ; transformation major mode
  
  (define-from state stx)
  (define update (curry hash-set* state))
  (match-define (⋱x ctx (/ [transform template] xs/ pattern)) stx)
  
  (match key
    
    ["escape" 
     (update 'mode 'nav
             'stx (⋱x ctx (/ [▹ '▹] xs/ pattern)))]
    
    [" "
     (define new-template
       (f/match template
         [(c ⋱ (capture-when (or (('▹ _) ('menu _) _ ... / _)
                                 (_ ... / '⊙)))
             `(,as ... ,(▹ menu ws ... / a) ,(zs ... / '⊙) ,bs ...))
          (c ⋱... 
             `(,@as ,(ws ... / a) ,(move-menu (▹ zs ... / '⊙)) ,@bs))]
         [x x]))
     (update 'stx (⋱x ctx (/ [transform new-template] xs/ pattern)))]
    
    ["right"
     (define new-template
       ; we get the transform corresponding to the selected menu item
       ; then apply that transform to the WHOLE template
       ; we then move the cursor and menu the next hole after* the cursor
       (f/match template
         [(ctx2 ⋱ (('menu `(,a ... (,transform ,(▹ Bs ... / c)) ,d ...)) wws ... / wwx))
          (define post-transform-template
            (runtime-match literals transform template))
          (f/match post-transform-template
            [(ctx2 ⋱ (▹ ('menu whatever) ws ... / x))
             (move-menu (ctx2 ⋱ (▹ ws ... / x)))]
            [x x])]
         [x x]))
     (update 'stx (⋱x ctx (/ (transform new-template) xs/ pattern)))]
    
    ["up"
     (define new-template
       (f/match template
         [(ctx2 ⋱ (('menu `(,a ... (,t1 ,( As ... / b)) (,t2 ,(▹ Bs ... / c)) ,d ...)) ws ... / x))
          (ctx2 ⋱ (('menu `(,@a (,t1 ,(▹ As ... / b)) (,t2 ,(Bs ... / c)) ,@d)) ws ... / x))]
         [x x]))
     (update 'stx (⋱x ctx (/ (transform new-template) xs/ pattern)))]

    ["down"
     (define new-template
       (f/match template
         [(ctx2 ⋱ (('menu `(,a ... (,t1 ,(▹ As ... / b)) (,t2 ,(Bs ... / c)) ,d ...)) ws ... / x))
          (ctx2 ⋱ (('menu `(,@a (,t1 ,(As ... / b)) (,t2 ,(▹ Bs ... / c)) ,@d)) ws ... / x))]
         [x x]))
     (update 'stx (⋱x ctx (/ (transform new-template) xs/ pattern)))]

    ["\r"
     (define new-template
       (f/match template
         [(ctx2 ⋱ (('menu `(,a ... (,transform ,(▹ Bs ... / c)) ,d ...)) ws ... / x))
          (f/match (runtime-match literals transform template)
            [(ctx3 ⋱ (('menu whatever) ws ... / x))
             (ctx3 ⋱ (if (no-⊙? x)
                         ( ws ... / x)
                         (select-first-⊙ (▹ ws ... / x))))]
            [x x])] ; no holes left
         [x x])) ; no menu
     (update 'mode 'nav
             'stx (⋱x ctx new-template))]
    
    [_ (println "no programming for that key") state]))



#| project: stx -> s-expr
   projects cursor and sort info for holes|#
(define (project stx)
  (define @ project)
  (f/match stx
    ; transform mode
    [(▹ template / stx)
     `(▹ [,stx -> ,(project template)])]
    ; label sorts of holes
    [(▹ sort / '⊙)
     `(▹ (⊙ ,sort))]
    [(sort / '⊙)
     `(⊙ ,sort)]
    ; embed cursor
    [(▹ / stx)
     `(▹ ,(@ stx))]
    [(♦ / stx)
     `(♦ ,(@ stx))]
    ; ditch attributes
    [(_ ... / stx)
     (@ stx)]
    [(? list?) (map @ stx)] [x x]))



; mode-loop : key x state -> state
#| determines the effect of key based on mode|#
(define (mode-loop key state)
  (define-from state mode)
  (match mode
    ['menu
     (mode:transform key state)]
    ['nav
     (mode:navigate key state)]))



; debug-output! : world x state x key -> world
(define (debug-output! state key)
  (match-define
    (hash-table ('stx stx)
                ('mode mode)
                ('transforms transforms)
                ('messages messages)) state)
  (displayln `(mode: ,mode  key: ,key))
  #;(displayln (pretty-format (project stx)))
  #;(displayln (pretty-format stx))
  (displayln `(projected: ,(project stx)))
  #;(displayln state))


; output : state -> image
(define (output state)
  (define real-layout-settings
    (hash 'text-size 30
          'form-color (color 0 130 214)
          'literal-color (color 255 131 50)
          'grey-one (color 200 200 200)
          'grey-two (color 184 184 184)
          'pattern-grey-one (color 84 84 84)
          'identifier-color "black"
          'selected-color (color 230 0 0)
          'bkg-color (color 0 47 54)))
  (match state
    [(hash-table ('stx stx))
     (fructure-layout (second stx) real-layout-settings) ; second to skip top
     #;(text (pretty-format (project stx) 100)
             24 "black")]))


(require 2htdp/image)
; bug if true (lol)
(require 2htdp/universe)
; MY LOVE FOR YOU IS LIKE A TRUCK
(big-bang initial-state
  [on-key
   #| This is impure because of printing debug output.
   and ad-hoc pre-processing. This is where we do
   dirty things. |#
   (λ (state key)
     ; state pre-processors
     (apply-in! state 'stx augment)
     ; if there's a transform active, new-candidate
     (apply-in! state
                'stx (λ (stx)
                       (f/match stx
                         [(ctx ⋱ (('transform (ts ... / t)) in-scope as ... / a))
                          (ctx ⋱ (('transform (augment (in-scope ts ... / t))) in-scope as ... / a))]
                         [x x])))
     ; print debugging information
     (debug-output! state key)
     ; transform state based on input and mode
     (mode-loop key state))]
  [to-draw output 800 800])

