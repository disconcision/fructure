#lang racket

(provide mode:transform
         insert-menu-at-cursor)

(define init-buffer #;"" '(▹ ""))

(define (mode:transform key state)
  ; transformation major mode
  (define-from state
    stx search-buffer history keypresses)
  #;(define update (curry hash-set* state))
  (define (update . stuff)
    (define base-state
      (hash-set* state
                 'history (cons stx history)
                 'keypresses (cons key keypresses)))
    (apply hash-set* base-state stuff))
  (match-define (⋱x ctx (/ [transform template] r/ reagent)) stx)
  #;(define template (insert-menu-at-cursor pre-template))

  (define hole-selected-in-menu?
    (match-lambda? (⋱x c⋱ (/ [transform (⋱x d⋱ (/ (menu (⋱x (/ h/ (▹ (or '⊙ '⊙+))))) m/ _))] t/ t))))
  
  (match key
    
    ["escape"
     ; cancel current transform and restore original syntax
     ; TODO: decide if cursor is conceptually necessary here
     (update 'mode 'nav
             'search-buffer init-buffer
             'stx (⋱x ctx (/ r/ (▹ reagent))))]

    ["\r"
     ; perform selected transform, remove menu, and switch to nav mode
     (update 'mode 'nav
             'search-buffer init-buffer
             'stx (⋱x ctx (strip-menu (perform-selected-transform template))))]
    
    ["right"
     ; apply selected transform and advance the cursor+menu the next hole     
     (update 'search-buffer init-buffer
             'stx (⋱x ctx (/ [transform (move-menu-to-next-hole
                                         (perform-selected-transform template)
                                         stx init-buffer)] ; empty search buffer
                             r/ reagent)))]

    ["left"
     ; budget undo
     ; do we actually want to change the history in this way?
     ; what are the alternatives?
     (update 'stx (if (empty? history) stx (first history))
             'history (if (empty? history) history (rest history))
             'search-buffer init-buffer)]
    
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
     ; todo: ideally we'd like to retain current menu selection
     ; after pressing bksp
     (define buffer-candidate
       (match search-buffer
         [`(▹ "")
          `(▹ "")]
         [(⋱x c⋱ `((▹ "")))
          (⋱x c⋱ `(▹ ""))]
         [(⋱x c⋱ `(,as ... ,a (▹ "")))
          (⋱x c⋱ `(,@as (▹ ,a)))]
         [(⋱x c⋱ `(▹ ,(and s (? string?) (not (== "")))))
          (⋱x c⋱ `(▹ ,(substring s 0 (sub1 (string-length s)))))]
         [x (error "backspace sux dood duhhhhhh" x)]))
     ; note: need case for backspacing sexpr, and out of sexpr
     (define-values (new-stx-candidate newest-buffer-candidate)
       (menu-filter-in-stx stx search-buffer buffer-candidate))
     (update 'stx new-stx-candidate
             'search-buffer newest-buffer-candidate)]
    ; todo: below should be reinterpreted as a search-buffer operation
    [" "
     (define new-search-buffer
       (match search-buffer
         [(⋱x c⋱ `(,as ... (▹ ,a)))
          (⋱x c⋱ `(,@as ,a (▹ "")))]
         [(⋱x c⋱ `(▹ ,a))
          (⋱x c⋱ `(,a (▹ "")))]))
     (define-values (new-stx-candidate
                     newest-buffer-candidate)
       (menu-filter-in-stx stx search-buffer new-search-buffer))
     (update 'search-buffer newest-buffer-candidate
             'stx new-stx-candidate)
     ; if there's a hole after the cursor, advance the cursor+menu to it
     ; idea for modification to make this feel more natural
     #;(update 'search-buffer init-buffer
               'stx (⋱x ctx (/ [transform (move-menu-to-next-hole template
                                                                  stx init-buffer)]
                               ; above "" is empty search buffer
                               r/ reagent)))]
    ["(" 
     (define new-search-buffer
       (match search-buffer
         [(⋱x c⋱ `(▹ ,a))
          (⋱x c⋱ `((▹ ,a)))])) ; shouldn't need fallthorugh
     (define-values (new-stx-candidate
                     newest-buffer-candidate)
       (menu-filter-in-stx stx search-buffer new-search-buffer))
     (update 'search-buffer newest-buffer-candidate
             'stx new-stx-candidate)]
    [")" 
     (define new-search-buffer
       (match search-buffer
         [(⋱x c⋱ `(,as ... (,bs ... (▹ ,s))))
          (⋱x c⋱ `(,@as (,@bs ,s) (▹ "")))]
         ; otherwise, fallthrough
         ; do we need special force-completeion case?
         ; for totally completeing a form?
         ; no... can always (ish) press space to wrap
         [x x]))
     (define-values (new-stx-candidate
                     newest-buffer-candidate)
       (menu-filter-in-stx stx search-buffer new-search-buffer))
     (update 'search-buffer newest-buffer-candidate
             'stx new-stx-candidate)]
    [(regexp #rx"^[0-9a-z\\]$" c)
     #:when c
     ; hack? otherwise this seems to catch everything?
     ; maybe since we're matching against a key event...

     (define buffer-candidate
       (match search-buffer
         [(⋱x c⋱ `(▹ ,(? string? s)))
          (⋱x c⋱ `(▹ ,(string-append s
                                     (hash-ref (hash "\\" "λ")
                                               (first c) (first c)))))]))

     (define-values (new-stx-candidate newest-buffer-candidate)
       (menu-filter-in-stx stx search-buffer buffer-candidate))
     (update 'stx new-stx-candidate
             'search-buffer newest-buffer-candidate)]
    
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
                  ,(match (runtime-match literals transform reagent)
                     [(⋱x c⋱ (/ as/ (▹ a)))
                      (/ as/ a)])))
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
  (match stx
    [(⋱x c⋱ (/ [in-scope a-scope] a/ (▹ a)))
     a-scope]    
    ; fallthrough case - current λ params list has no in-scope
    ; TODO: decide if i do/should actually need this case
    [(⋱x c⋱ (/ a/ (▹ a)))
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
              ; FIX THIS:
              ;  not copying even sort over ...
              `([⋱
                  (▹ [sort expr] / ⊙)
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
  (match stx
    [(⋱x c⋱ (/ xs/ (▹ x)))
     #:when (not (empty? menu-stx))
     (define menu-with-selection
       ; recall that each menu item is a pairing
       ; of a transformation and its result
       (match menu-stx
         [`((,t ,r) ,xs ...)
          `((,t ,(select-▹ r)) ,@xs)]))
     (define filtered-menu
       (filter-menu menu-with-selection search-buffer))
     #;(println `(fm ,search-buffer ,filtered-menu))
     (⋱x c⋱ (/ [menu filtered-menu] xs/ (▹ x)))]
    [x (println "warning: no menu inserted")
       (when (empty? menu-stx)
         (println "warning: menu was empty; not handled"))
       x]))


(define (strip-menu stx)
  ; removes up to one menu from stx
  (match stx
    [(⋱x ctx⋱ (/ [menu menu] xs/ x))
     (⋱x ctx⋱ (/ xs/ x))]
    [x x]))

(define (local-augment stx)
  (match stx
    [(⋱x ctx⋱ (/ [in-scope in-scope] ts/ t))
     (⋱x ctx⋱ (augment (/ in-scope ts/ t)))]
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
  (define matcher
    (match-lambda [`(,t ,r) (stx-buf-match? r search-buffer)]
                  [a #f]))
  (define menu-annotated
    (map (match-lambda [`(,t ,(/ r/ r)) `(,t ,(/ search-buffer r/ r))]) menu))
  (define menu-candidate
    (filter matcher menu-annotated))
  (define is-cursor-in-menu?
    (match menu-candidate
      [`(, as ... (,t ,(/ b/ (▹ b))) , cs ...) #t] [_ #f]))
  (if is-cursor-in-menu?
      menu-candidate
      (match menu-candidate
        [`((,t ,(/ r/ r)) ,xs ...)
         `((,t ,(/ r/ (▹ r))) ,@xs)]
        [`() `()])))

(define (stx-buf-match? stx buf)
  (match buf
    [`(▹ ,"")
     #t]
    [`(▹ ,(? string? s))
     (stx-str-match? stx s)]
    [(? string? s)
     ; should this case actually be exact match?
     (stx-str-match? stx s #t)]
    [(? list?)
     #:when (and (list? stx) (match stx [(/ a/ (? list? raw-stx)) #t][_ #f]))
     (match-define (/ a/ raw-stx) stx)
     (define res
       (if ((length raw-stx) . < . (length buf))
           #f
           (andmap stx-buf-match?
                   (take raw-stx (length buf))
                   buf)))
     (println `(MUSMATCH (res) ,raw-stx ,buf))
     res]
    ; false fallthrough
    [_ #f]))


(define (stx-str-match? stx str (exact? #f))
  #;(println `(stx-str-match? ,stx ,str))
  (unless (string? str)
    (error "npt string"))
  (define matcher?
    (if exact?
        equal?
        string-prefix?))
  (define (symbols->string c)
    (apply string-append (map ~a c)))
  (define (str-match? str form-string)
    (if (and (not (equal? "" str))
             (equal? " " (substring str (- (string-length str) 1) (string-length str))))
        (equal? form-string (substring str 0 (- (string-length str) 1)))
        (matcher? form-string str)))
  ; basically, a space at the end indicates a terminator
  ; debatably hacky
  (match stx
    [(/ r/ `(ref ,(/ i/ `(id ,(/ c/ c) ...))))
     (str-match? str (symbols->string c))]
    [(/ f/ `(, as ... ,(? form-id? f) ,bs ...))
     (str-match? str (symbol->string f))]
    [(/ c/ (? (disjoin symbol? number?) c)) ; should just be chars, digits
     (matcher? (~a c) str)]
    ; below case is actually necessary
    ; might be hacky due to str-buf-match problems
    [(? (disjoin symbol? number?) c)
     (matcher? (~a c) str)]
    ; note fallthrough is now FALSE!!!
    [_ #f]
    #;[_ (println `(stx-str-match-fallthrough ,stx)) #t]))


(define (single-char-menu? menu-candidate)
  (match-let ([`((,_ ,resultants) ...) menu-candidate])
    (match resultants
      [`(,(/ [sort (or 'digit 'char)] _/ _) ...) #t] [_ #f])))


(define (menu-filter-in-stx init-stx old-buffer buffer-candidate)

  ; this hacky little guy restores the original menu prior to filtering
  (define stx
    (match init-stx
      [(⋱x ctx (/ [transform template] r/ reagent))
       (define new-template
         (insert-menu-at-cursor (strip-menu template) init-stx init-buffer))
       (⋱x ctx (/ [transform new-template] r/ reagent))]))
  
  (match-define
    (⋱x c⋱ (/ [transform (⋱x d⋱ (/ menu m/ m))] t/ t)) stx)
  (match-define
    (⋱x ctx (/ [transform template] r/ reagent)) stx)
  
  (define menu-candidate
    (filter-menu menu buffer-candidate))

  (define template-candidate
    (⋱x d⋱ (/ [menu menu-candidate]  m/ m)))

 
  (cond
    [(empty? menu-candidate)
     (values init-stx
             old-buffer)]
    [(and (equal? 1 (length menu-candidate))
          ; make this an option
          ; todo: case where buffer-cursor is on a hole
          (single-char-menu? menu-candidate))
     (values (⋱x ctx (/ [transform (move-menu-to-next-hole
                                    (perform-selected-transform template-candidate)
                                    stx init-buffer)] ; empty search buffer
                        r/ reagent))
             init-buffer)]
    [else
     (values (⋱x c⋱ (/ [transform template-candidate] t/ t))
             buffer-candidate)])
 
  )
