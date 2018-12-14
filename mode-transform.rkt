#lang racket

(provide mode:transform
         insert-menu-at-cursor)


(define (mode:transform key state)
  ; transformation major mode
  
  (define-from state stx search-buffer)
  (define update (curry hash-set* state))
  (match-define (⋱x ctx (/ [transform template] r/ reagent)) stx)
  #;(define template (insert-menu-at-cursor pre-template))

  #;`(? symbol? (app symbol->string (regexp (string-append "^" ,(symbol->string s) ".*"))))
  
  (match key
    
    ["escape"
     ; cancel current transform and restore original syntax
     ; TODO: decide if cursor is conceptually necessary here
     (update 'mode 'nav
             'search-buffer ""
             'stx (⋱x ctx (/ r/ (▹ reagent))))]

    ["\r"
     ; perform selected transform, remove menu, and switch to nav mode
     (update 'mode 'nav
             'search-buffer ""
             'stx (⋱x ctx (strip-menu (perform-selected-transform template))))]
    
    [" "
     ; if there's a hole after the cursor, advance the cursor+menu to it
     ; BUG? : think about behavior when press space at first-level menu
     (update 'search-buffer ""
             'stx (⋱x ctx (/ [transform (move-menu-to-next-hole template stx
                                                                search-buffer)]
                             r/ reagent)))]
    
    ["right"
     ; apply selected transform and advance the cursor+menu the next hole     
     (update 'search-buffer ""
             'stx (⋱x ctx (/ [transform (move-menu-to-next-hole
                                         (perform-selected-transform template)
                                         stx
                                         search-buffer)]
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

    ["\b"
     ; BUG. to duplicate, type i f in menu and then \b
     ; it will got back to full menu, not i-filtered menu
     ; but the buffer seems to be set right
     (define new-search-buffer (if (equal? "" search-buffer)
                                   ""
                                   (substring search-buffer 0 (sub1 (string-length search-buffer)))))
     (update 'search-buffer new-search-buffer
             'stx (match stx
                    [(⋱x c⋱ (/ [transform template] t/ t))
                     (define new-template
                       (insert-menu-at-cursor (strip-menu template) stx new-search-buffer))
                     (⋱x c⋱ (/ [transform new-template] t/ t))])
             )]

    [(regexp #rx"^[a-z\\]$" c)
     #:when c
     ; hack? otherwise this seems to catch everything?
     ; maybe since we're matching against a key event...
     #;(println `(char ,c pressed))
     (if (equal? "\\" (first c))
         "λ"
         (first c))
     (define buffer-candidate
       (string-append search-buffer (if (equal? "\\" (first c))
                                        "λ"
                                        (first c))))
     (match-define
       (⋱x c⋱ (/ [transform (⋱x d⋱ (/ menu m/ m))] t/ t))
       stx)
     
     (define menu-candidate
       (filter-menu menu buffer-candidate))

     #;(println `(menu-candidate ,menu-candidate))
     
     (if (empty? menu-candidate)
         (update 'stx stx)
         (update 'stx (⋱x c⋱ (/ [transform (⋱x d⋱ (/ [menu menu-candidate]  m/ m))] t/ t))
                 'search-buffer buffer-candidate))
     
     ; we want to add it to buffer, but only if it doesn't make the menu of zero length
     ; so just add it to buffer,
     ; then filter the menu by the buffer
     ; if the length is zero, we remove a character from the buffer

     ; also: we need to send the buffer to the renderer somehow
     ; idea: annotate all menu items with it? seems wasteful

     ]
    
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
   (append metavar-transforms
           base-transforms
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
  ; new: prevent descending into metavar
  ; note that other metavar clause is ill-advised
  (f/match stx
    #;[(c ⋱ (▹ as ... / (and h (or '⊙ '⊙+))))
       (c ⋱ (▹ as ... / h))]
    [(c ⋱ (and (▹ ys ... / (d ⋱ (xs ... / (and h (or '⊙ '⊙+)))))
               (not (('metavar _) _ ... / _))))
     (c ⋱ (ys ... / (d ⋱ (▹ xs ... / h))))]
    [(c ⋱ (capture-when (and (or (('▹ _) _ ... / _)
                                 (_ ... / (or '⊙ '⊙+)))
                             #;(not (('metavar _) _ ... / _))))
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

(define (insert-menu-at-cursor template ambient-stx search-buffer)
  (define (extract-metavars ambient-stx)
    (match ambient-stx
      [(⋱+x c⋱ (and m (/ metavar _/ _)))
       m]))

  
  ; problem: need to project metavar contents into
  ; a format runtime-match understands
  ; soln: fruct-to-runtime

  ; should i be erasing this stuff?
  ; it prevents some bugs, but might just be masking them
  (define (erase-attrs fr)
    (match fr
      [(/ handle in-scope transform a/ a)
       (/ a/ (erase-attrs a))]
      [(/ in-scope transform a/ a)
       (/ a/ (erase-attrs a))]
      [(/ handle transform a/ a)
       (/ a/ (erase-attrs a))]
      [(/ handle in-scope a/ a)
       (/ a/ (erase-attrs a))]
      [(/ handle a/ a)
       (/ a/ (erase-attrs a))]
      [(/ in-scope a/ a)
       (/ a/ (erase-attrs a))]
      [(/ transform a/ a)
       (/ a/ (erase-attrs a))]
      [(? list?) (map erase-attrs fr)]
      [_ fr]))

  ; the problem with this approach is runtime-match
  ; doesn't know the literal (chars) for the ids we introduce
  ; i EXTREME DANGER HACK ""fix"" this
  ; by hacking fructerm to interpret unbound pattern vars as literals
  ; sorry future people
  (define metavar-transforms
    (map (λ (x)
           (match (fruct-to-runtime x)
             [`(,a ... / ,b)
              ;  not copying even sort over ...
              `([⋱
                  (▹ / ⊙)
                  (▹ ,@a / ,b)])]))
         (extract-metavars (erase-attrs ambient-stx))))

  ; hack:
  ; if the template is a metavar
  ; all transformed menu items will be the same metavar
  ; so we strip it
  (define stx
    (match template
      [(/ metavar a/ a) (/ a/ a)]
      [x x]))

  
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
     (define filtered-menu
       (filter-menu menu-with-selection search-buffer))
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

(define (move-menu-to-next-hole stx ambient-stx search-buffer)
  ; removes existing menu, advances cursor to next hole,
  ; and inserts a menu at that hole. if there is no such
  ; hole, the returned stx will have no menu

  (define hole-under-cursor?
    (match-lambda? (⋱x c⋱ (/ _/ (▹ (or '⊙ '⊙+))))))

  (define candidate
    ((compose #;(curryr insert-menu-at-cursor ambient-stx)
              local-augment
              advance-cursor-to-next-hole
              strip-menu)
     stx))

  ; slightly hacky
  ; this case prevents a menu from spawning on an atom
  ; if we press over when there are no other holes
  (if (hole-under-cursor? candidate)
      (insert-menu-at-cursor candidate ambient-stx search-buffer)
      candidate)
  
  )



(define (filter-menu menu search-buffer)
  #;(println `(da menu: ,menu))
  (define matcher
    (match-lambda [`(,t ,r) (stx-str-match r search-buffer)]
                  [a #;(println `(fallthru: ,a)) #f]))
  (define menu-candidate
    (filter matcher menu))
  (define is-cursor-in-menu?
    (match menu-candidate
      [`(, as ... (,t ,(/ b/ (▹ b))) , cs ...) #t] [_ #f]))
  (if is-cursor-in-menu?
      menu-candidate
      (match menu-candidate
        [`((,t ,(/ r/ r)) ,xs ...)
         `((,t ,(/ r/ (▹ r))) ,@xs)]
        [`() `()])))


(define (stx-str-match stx str)
  #;(println `(res ,stx))
  (match stx
    [(/ ref/ `(ref ,(/ id/ `(id ,(/ c/ c) ...))))
     #;(println `(id case: ,(apply string-append (map symbol->string c))))
     (string-prefix? (apply string-append (map symbol->string c)) str)]
    [(/ form/ `(, as ... ,(? form-id? f) ,bs ...))
     ; temp HACK to match lambda literals
     #;(define new-str (if (equal? (first str) "λ") (cons "l" (rest str)) str))
     (string-prefix? (symbol->string f) str)]
    [_ (println `(stx-str-match-fallthrough ,stx)) #f]))
