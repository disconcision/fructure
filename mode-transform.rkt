#lang racket

(provide mode:transform
         insert-menu-at-cursor)


(define (mode:transform key state)
  ; transformation major mode
  
  (define-from state stx)
  (define update (curry hash-set* state))
  (match-define (⋱x ctx (/ [transform template] r/ reagent)) stx)
  #;(define template (insert-menu-at-cursor pre-template))


  
  (match key
    
    ["escape"
     ; cancel current transform and restore original syntax
     ; TODO: decide if cursor is conceptually necessary here
     (update 'mode 'nav
             'stx (⋱x ctx (/ r/ (▹ reagent))))]

    ["\r"
     ; perform selected transform, remove menu, and switch to nav mode
     (update 'mode 'nav
             'stx (⋱x ctx (strip-menu (perform-selected-transform template))))]
    
    [" "
     ; if there's a hole after the cursor, advance the cursor+menu to it
     ; BUG? : think about behavior when press space at first-level menu
     (update 'stx (⋱x ctx (/ [transform (move-menu-to-next-hole template stx)]
                             r/ reagent)))]
    
    ["right"
     ; apply selected transform and advance the cursor+menu the next hole     
     (update 'stx (⋱x ctx (/ [transform (move-menu-to-next-hole
                                         (perform-selected-transform template)
                                         stx)]
                             r/ reagent)))]
    
    ["up"
     ; cycle the cursor to the previous menu item
     ; it looks a bit noisy because each menu item is actually
     ; a pair of a (hidden) transformation and its (displayed) result
     (define new-template
       (match template
         [(⋱x c⋱ (/ [menu `((,t1 ,(/ a/ (▹ a))) ,bs ... (,t2 ,(/ c/ c)))] t/ t))
          (⋱x c⋱ (/ [menu `((,t1 ,(/ a/ a)) ,@bs (,t2 ,(/ c/ (▹ c))))] t/ t))]
         [(⋱x c⋱ (/ [menu `(,a ... (,t1 ,(/ b/ b)) (,t2 ,(/ c/ (▹ c))) ,d ...)] t/ t))
          (⋱x c⋱ (/ [menu `(,@a (,t1 ,(/ b/ (▹ b))) (,t2 ,(/ c/ c)) ,@d)] t/ t))]
         [x (println "warning: couldn't find menu cursor") x]))
     (update 'stx (⋱x ctx (/ (transform new-template) r/ reagent)))]

    ["down"
     ; cycle the cursor to the next menu item
     (define new-template
       (match template
         [(⋱x c⋱ (/ [menu `((,t1 ,(/ a/ a)) ,bs ... (,t2 ,(/ c/ (▹ c))))] t/ t))
          (⋱x c⋱ (/ [menu `((,t1 ,(/ a/ (▹ a))) ,@bs (,t2 ,(/ c/ c)))] t/ t))]
         [(⋱x c⋱ (/ [menu `(,a ... (,t1 ,(/ b/ (▹ b))) (,t2 ,(/ c/ c)) ,d ...)] t/ t))
          (⋱x c⋱ (/ [menu `(,@a (,t1 ,(/ b/ b)) (,t2 ,(/ c/ (▹ c))) ,@d)] t/ t))]
         [x (println "warning: couldn't find menu cursor") x]))
     (update 'stx (⋱x ctx (/ (transform new-template) r/ reagent)))]

    
    [_ (println "warning: transform-mode: no programming for that key") state]))



(require "language.rkt"
         "attributes.rkt" ; for local-augment. refactor todo
         "common.rkt")

(require "../fructerm/fructerm.rkt"
         "../fructerm/f-match.rkt"
         "new-syntax.rkt"
         ; temporary renames so as not to intefere with f-match
         (only-in "../containment-patterns/containment-patterns.rkt"
                  (⋱ ⋱x)
                  (⋱1 ⋱1x)
                  (⋱+ ⋱+x)))


; -------------------------------------------------

(define (perform-selected-transform template)
  ; find the transform corresponding to the selected menu item
  ; and apply that transform to the WHOLE template
  (match template
    [(⋱x (/ [menu `(,_ ... (,transform ,(/ _ (▹ _))) ,_ ...)] _ _))
     (runtime-match literals transform template)]
    [x (println "warning: no transform selected") x]))


; -------------------------------------------------
; menu creation

(define (transforms->menu transforms reagent)
  (define (test-match transform stx)
    (match (runtime-match literals transform stx)
      ['no-match #f] [_ #t]))
  (for/fold ([menu '()])
            ([transform transforms])
    (if (test-match transform reagent)
        `(,@menu (,transform
                  ,(f/match (runtime-match literals transform reagent)
                     [(c ⋱ (▹ as ... / a))
                      (as ... / a)])))
        menu)))


(define (make-menu in-scope metavar-transforms current-selection)
  (transforms->menu
   (append base-transforms
           metavar-transforms
           (for/list ([id in-scope])
             `([⋱
                 (▹ [sort expr] xs ... / ⊙)
                 (▹ [sort expr] xs ... /
                    (ref ',id))])))
   current-selection))



; -------------------------------------------------


(module+ test
  (require rackunit)
  (define raw-base-constructor-list
    ; naming idea for attributes
    #;(list '([(/ [sort: expr] a/ ⊙)
               (/ a/ (app (/ [sort: expr] ⊙)
                          (/ [sort: expr] ⊙)))]))
    (list '([([sort expr] xs ... / ⊙)
             ([sort expr] xs ... / (app ([sort expr] / ⊙)
                                        ([sort expr] / ⊙)))])
          '([([sort expr] xs ... / ⊙)
             ([sort expr] xs ... / (λ ( / (( / (id ([sort char] / ⊙)))))
                                     ([sort expr] / ⊙)))])))
  (check-equal?
   (transforms->menu
    raw-base-constructor-list
    '(p/  #hash((sort . expr) (▹ . ▹)) ⊙))
   '((((((sort expr) xs ... / ⊙)
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



(define (extract-scope stx)
  ; extract the variables in scope under the cursor
  ; todo: improve and inline this function
  (f/match stx
    [(c ⋱ (▹ in-scope As ... / a))
     in-scope]    
    ; fallthrough case - current λ params list has no in-scope
    ; TODO: decide if i do/should actually need this case
    [(c ⋱ (▹ As ... / a))
     '()]))



(define (advance-cursor-to-next-hole stx)
  (f/match stx
    [(c ⋱ (▹ ys ... / (d ⋱ (xs ... / '⊙))))
     (c ⋱ (ys ... / (d ⋱ (▹ xs ... / '⊙))))]
    [(c ⋱ (capture-when (or (('▹ _) _ ... / _)
                            (_ ... / '⊙)))
        `(,as ... ,(▹ ws ... / a) ,(zs ... / b) ,bs ...))
     (c ⋱... 
        `(,@as ,(ws ... / a) ,(▹ zs ... / b) ,@bs))]
    [x (println "warning: no hole after cursor") x]))



(define (fruct-to-runtime fr)
  ; done quick... prob hacky
  (match fr
    [`(p/ ,my-hash ,contents)
     (define pairs
       (for/list ([(k v) my-hash])
         `(,k ,(fruct-to-runtime v)))) ;should we recurse into vals?
     `(,@pairs / ,(fruct-to-runtime contents))]
    [(? list?)
     (map fruct-to-runtime fr)]
    [_ fr]))

(define (insert-menu-at-cursor stx ambient-stx)
  (define (extract-metavars ambient-stx)
    (match ambient-stx
      [(⋱+x c⋱ (and m (/ metavar _/ _)))
       m]))
  ; should i be erasing this stuff?
  ; it prevents some bugs, but might just be masking them
  (define (erase-attrs fr)
    (match fr
      [(/ handle in-scope a/ a)
       (/ a/ (erase-attrs a))]
      [(/ handle a/ a)
       (/ a/ (erase-attrs a))]
      [(/ in-scope a/ a)
       (/ a/ (erase-attrs a))]
      [(? list?) (map erase-attrs fr)]
      [_ fr]))
  ; the problem with this approach is runtime-match
  ; doesn't know the literal (chars) for the ids we introduce
  (define metavar-transforms
    (map (λ (x)
           (match (fruct-to-runtime x)
             [`(,a ... / ,b)
              ;  not copying even sort over ...
              `([⋱
                  (▹ / ⊙)
                  (▹ ,@a / ,b)])]))
         (extract-metavars (erase-attrs ambient-stx))))
  (println `(mvt ,metavar-transforms))
  (when (not (empty? metavar-transforms))
    (println `(res ,(runtime-match literals (first metavar-transforms) initial-stx))))
  
  ; problem: need to project metavar contents into
  ; a format runtime-match understands
  (define in-scope
    (extract-scope stx))
  (define menu-stx (make-menu in-scope metavar-transforms stx))
  (f/match stx
    [(c ⋱ (▹ xs ... / x))
     #:when (not (empty? menu-stx))
     (define menu-with-selection
       ; recall that each menu item is a pairing
       ; of a transformation and its result
       (match menu-stx
         [`((,t ,r) ,xs ...)
          `((,t ,(select-▹ r)) ,@xs)]))
     (c ⋱ (▹ ('menu menu-with-selection) xs ... / x))]
    [x (println "warning: no menu inserted")
       (when (empty? menu-stx)
         (println "warning: menu was empty; not handled"))
       x]))


(define (strip-menu stx)
  ; removes up to one menu from stx
  (f/match stx
    [(ctx ⋱ (menu xs ... / x))
     (ctx ⋱ (xs ... / x))]
    [x x]))

(define (local-augment stx)
  (f/match stx
    [(ctx ⋱ (in-scope ts ... / t))
     (ctx ⋱ (augment (in-scope ts ... / t)))]
    [x (println "warning: local-augment no-match") x]))

(define (move-menu-to-next-hole stx ambient-stx)
  ; removes existing menu, advances cursor to next hole,
  ; and inserts a menu at that hole. if there is no such
  ; hole, the returned stx will have no menu
  
  ((if (equal? stx (advance-cursor-to-next-hole stx))
       identity
       (compose (curryr insert-menu-at-cursor ambient-stx)
                local-augment
                advance-cursor-to-next-hole))  
   (strip-menu stx)))




