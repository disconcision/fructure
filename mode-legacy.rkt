#lang racket

(require racket/hash)

(require "language.rkt"
         "common.rkt"
         "../fructerm/fructerm.rkt")

(provide mode:legacy)


(define (mode:legacy key state)
  (define transform
    (hash-ref (hash-union packaged-alpha-constructors keymap
                          #:combine/key (λ (k v v1) v))
              key identity->))
  (apply-> transform state))







; -------------------------------------------------
; packaged constructors and their helpers


; structure for annotating transformation rules
(struct -> (class props payload) #:transparent)


(define (make-constructor raw-rule)
  (define (select mstx)
    (match mstx
      [`(,y ... / ,d ...)
       `(▹ ,@y / ,@d)]))
  (define (wrap⋱select mstx)
    (for/list ([s mstx])
      (match s
        [`[,a ,b]
         `[⋱ ,(select a) ,(select b)]])))
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




(define keymap
  ; map from keys to functions
  ; this is mostly deprecated by transform mode
  ; but is still useful for editing identifers
  ; pending a more structure solution
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
         '([⋱
             (a ... / (λ (b ... / ((c ... / (id x ... (▹ ys ... / y) z ...)))) e))
             (▹ a ... / (λ (b ... / ((c ... / (id x ... (ys ... / y) z ...)))) e))]
           [⋱
             (As ... / (a ... (▹ Bs ... / b) c ...))
             (▹ As ... / (a ... (Bs ... / b) c ...))]))

   "down" (make-movement
           '([⋱
               (▹ a ... / (λ (b ... / ((c ... / (id (ys ... / y) z ...)))) e))
               (a ... / (λ (b ... / ((c ... / (id (▹ ys ... / y) z ...)))) e))]
             [⋱
               (▹ As ... / (ctx ⋱ (sort Bs ... / b)))
               (As ... / (ctx ⋱ (▹ sort Bs ... / b)))]))

   "left" (make-movement
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

   "right" (make-movement
            '([⋱
                (λ (Cs ... / ((▹ As ... / a))) (Bs ... / b))
                (λ (Cs ... / ((As ... / a))) (▹ Bs ... / b))]
              [⋱
                (a ... (▹ As ... / b) (Bs ... / c) d ...)
                (a ... (As ... / b) (▹ Bs ... / c) d ...)]))  
   ))





(define alphabet
  ; character set for identifiers
  '(a b c d e f g h i j k l m n o p q r s t u v w x y z))


; make constructors for each character
(define packaged-alpha-constructors
  (for/fold ([alpha (hash)])
            ([x alphabet])
    (hash-set alpha
              (symbol->string x)
              (-> 'runtime (set)
                  `([⋱
                      (xs ... / (id as ... (▹ ys ... / b) bs ...))
                      (xs ... / (id as ... ([sort char] / ',x) (▹ ys ... / b) bs ...))])))))





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

