#lang racket

(require "../../shared/containment-patterns/containment-patterns/main.rkt"
         "../../shared/slash-patterns/slash-patterns.rkt"
         "../../shared/fructerm/fructerm.rkt"
         "../language/syntax.rkt"
         "../language/semantics.rkt" ; for local-augment. TODO: refactor
         "../common.rkt")

(provide mode:transform
         mode:transform-shift
         insert-menu-at-cursor)

(define init-buffer #;"" '(▹ ""))

(define (mode:transform pr key state)
  ; transformation major mode
  (define-from state
    stx search-buffer history)
  (define update (updater state key))

  (match stx
    ; sanity check
    [(⋱ ctx (/ [transform template] r/ reagent)) "cool"]
    [_ (println `(ERROR: TRANSFORM: no match for stx: ,stx))
       (error "see above")])
  
  (match-define (⋱ ctx (/ [transform template] r/ reagent)) stx)

  #;(define hole-selected-in-menu?
      (match-lambda? (⋱ c⋱ (/ [transform (⋱ d⋱ (/ (menu (⋱ (/ h/ (▹ (or '⊙ '⊙+))))) m/ _))] t/ t))))
  
  (if (equal? pr 'release)
      state
      (match key

        ["shift"
         (update 'mode 'transform-shift)]

        ["f1"
         (println `(BEGIN-STX ,stx))
         state]
    
        ["escape"
         ; cancel current transform and restore original syntax
         ; TODO: decide if cursor is conceptually necessary here
         (update 'mode 'nav
                 'search-buffer init-buffer
                 'stx (⋱ ctx (/ r/ (▹ reagent))))]

        ["\r"
         ; perform selected transform, remove menu, and switch to nav mode
         (update 'mode 'nav
                 'search-buffer init-buffer
                 'stx (erase-captures
                       (⋱ ctx (strip-menu (perform-selected-transform template)))))]

        ["f2"
         ; hack to remove menu
         (update 'stx (⋱ ctx (/ [transform (strip-menu template)] r/ reagent)))]
    
        [(or "right" " ")
         ; apply selected transform and advance the cursor+menu the next hole
         ; PROBLEM: when we're inserting through a variadic form
         ; we don't want to move to next hole automatically after transform,
         ; because the effect of that transform may have been to insert a new
         ; hole that we now want to fill.
         ; HOWEVER: the effect of right, according to the ostensible operative
         ; logic, is to 'step into' the current menu selection. if the selection is a hole,
         ; then stepping into it, i.e. doing nothing in most cases, is consistent.
         ; either we can special-case this somehow, or instead use something else,
         ; like space, to move forward in these cases.
         ; proposed logic: right continues to work as before, EXCEPT
         ; we change the move-to-next-hole logic to stay still if we're on a hole
         ; and we used space to skip filling a hole.
         ; BUT: does this work in the general variadic case?
         ; remember that in the single-char case, we force completion
         ; does this change things? let's try it out....
         (update 'search-buffer init-buffer
                 'stx (⋱ ctx (/ [transform (move-menu-to-next-hole
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
             [(⋱ c⋱ (/ [menu `((,t1 ,(/ a/ (▹ a))) ,bs ... (,t2 ,(/ c/ c)))] t/ t))
              (⋱ c⋱ (/ [menu `((,t1 ,(/ a/ a)) ,@bs (,t2 ,(/ c/ (▹ c))))] t/ t))]
             [(⋱ c⋱ (/ [menu `(,a ... (,t1 ,(/ b/ b)) (,t2 ,(/ c/ (▹ c))) ,d ...)] t/ t))
              (⋱ c⋱ (/ [menu `(,@a (,t1 ,(/ b/ (▹ b))) (,t2 ,(/ c/ c)) ,@d)] t/ t))]
             [x (println "warning: couldn't find menu cursor") x]))
         (update 'stx (⋱ ctx (/ (transform new-template) r/ reagent)))]

        ["down"
         ; cycle the cursor to the next menu item
         (define new-template
           (match template
             [(⋱ c⋱ (/ [menu `((,t1 ,(/ a/ a)) ,bs ... (,t2 ,(/ c/ (▹ c))))] t/ t))
              (⋱ c⋱ (/ [menu `((,t1 ,(/ a/ (▹ a))) ,@bs (,t2 ,(/ c/ c)))] t/ t))]
             [(⋱ c⋱ (/ [menu `(,a ... (,t1 ,(/ b/ (▹ b))) (,t2 ,(/ c/ c)) ,d ...)] t/ t))
              (⋱ c⋱ (/ [menu `(,@a (,t1 ,(/ b/ b)) (,t2 ,(/ c/ (▹ c))) ,@d)] t/ t))]
             [x (println "warning: couldn't find menu cursor") x]))
         (update 'stx (⋱ ctx (/ (transform new-template) r/ reagent)))]

        ["\b"
         ; todo: ideally we'd like to retain current menu selection
         ; after pressing bksp
         (define buffer-candidate
           (match search-buffer
             [`(▹ "")
              `(▹ "")]
             [(⋱ c⋱ `((▹ "")))
              (⋱ c⋱ `(▹ ""))]
             [(⋱ c⋱ `(,as ... ,a (▹ "")))
              (⋱ c⋱ `(,@as (▹ ,a)))]
             [(⋱ c⋱ `(▹ ,(and s (? string?) (not (== "")))))
              (⋱ c⋱ `(▹ ,(substring s 0 (sub1 (string-length s)))))]
             [x (error "backspace sux dood duhhhhhh" x)]))
         ; note: need case for backspacing sexpr, and out of sexpr
         (define-values (new-stx-candidate newest-buffer-candidate)
           (menu-filter-in-stx "\b" stx search-buffer buffer-candidate))
         (update 'stx new-stx-candidate
                 'search-buffer newest-buffer-candidate)]
        ; todo: below should be reinterpreted as a search-buffer operation
        ["\t" ; FORMERLY SNEED I MEAN SPACE
         ; new proposed logic for space:
         ; if the search buffer cursor is on (any?) hole,
         ; skip filling that hole and move to the next one.
         ; so: need to refactor search to return cursor-match
         (define new-search-buffer
           (match search-buffer
             [(⋱ c⋱ `(,as ... (▹ ,a)))
              (⋱ c⋱ `(,@as ,a (▹ "")))]
             ; new special case:
             #;[`(▹ "")
                (println `(new special case (▹ "▹ """)))
                '666-not-matchable]
             [(⋱ c⋱ `(▹ ,a))
              (⋱ c⋱ `(,a (▹ "")))]))
         (define-values (new-stx-candidate
                         newest-buffer-candidate)
           ; if pressing space results in there no longer being a match
           ; (might need to ammend that; maybe really want: total match before space?
           ;  actually that would work for hole selected in menu too; it is already total match
           ; total match ==? cursor is last atom in selector pattern, which matches against
           ; last item in replacement.)
           ; then (hypothesis) we should execute current transform ie
           ; advance to next hole AFTER current selection
           ; EVEN IF cur sel is a hole
           (menu-filter-in-stx " " stx search-buffer new-search-buffer))
         (update 'search-buffer newest-buffer-candidate
                 'stx new-stx-candidate)
         ; if there's a hole after the cursor, advance the cursor+menu to it
         ; idea for modification to make this feel more natural
         #;(update 'search-buffer init-buffer
                   'stx (⋱ ctx (/ [transform (move-menu-to-next-hole template
                                                                     stx init-buffer)]
                                  ; above "" is empty search buffer
                                  r/ reagent)))]
        [(or "(" "[" "{") 
         (define new-search-buffer
           (match search-buffer
             [(⋱ c⋱ `(▹ ,a))
              (⋱ c⋱ `((▹ ,a)))])) ; shouldn't need fallthorugh
         (define-values (new-stx-candidate
                         newest-buffer-candidate)
           (menu-filter-in-stx "(" stx search-buffer new-search-buffer))
         (update 'search-buffer newest-buffer-candidate
                 'stx new-stx-candidate)]
        [(or ")" "]" "}") 
         (define new-search-buffer
           (match search-buffer
             [(⋱ c⋱ `(,as ... (,bs ... (▹ ,s))))
              (⋱ c⋱ `(,@as (,@bs ,s) (▹ "")))]
             ; otherwise, fallthrough
             ; do we need special force-completeion case?
             ; for totally completeing a form?
             ; no... can always (ish) press space to wrap
             [x x]))
         (define-values (new-stx-candidate
                         newest-buffer-candidate)
           (menu-filter-in-stx ")" stx search-buffer new-search-buffer))
         (update 'search-buffer newest-buffer-candidate
                 'stx new-stx-candidate)]
        [(regexp #rx"^[0-9A-Za-z?!\\\\-]$" c)
         #:when c
         ; hack? otherwise this seems to catch everything?
         ; maybe since we're matching against a key event...

         (define buffer-candidate
           (match search-buffer
             [(⋱ c⋱ `(▹ ,(? string? s)))
              (⋱ c⋱ `(▹ ,(string-append s
                                        (hash-ref (hash "\\" "λ")
                                                  (first c) (first c)))))]))

         (define-values (new-stx-candidate newest-buffer-candidate)
           (menu-filter-in-stx c stx search-buffer buffer-candidate))
         (update 'stx new-stx-candidate
                 'search-buffer newest-buffer-candidate)]
    
        [_ (println "warning: transform-mode: no programming for that key") state])))


(define (mode:transform-shift pr key state)
  (define-from state
    stx history keypresses layout-settings)
  (define (update . stuff)
    (define base-state
      (hash-set* state
                 'history (cons stx history)
                 'keypresses (cons key keypresses)))
    (apply hash-set* base-state stuff))
  (if (equal? pr 'release)
      (match key
        ["shift" (update 'mode 'menu)]
        [_ state])
      (match key
        ["left" (update 'layout-settings
                        (hash-set layout-settings
                                  'transform-template-only? #true))]
        ["right" (update 'layout-settings
                         (hash-set layout-settings
                                   'transform-template-only? #false))]
        ["up"
         (define current-length (hash-ref layout-settings 'max-menu-length))
         (define new-length (if (equal? 1 current-length) current-length (sub1 current-length)))
         (update 'layout-settings
                 (hash-set* layout-settings
                            'simple-menu? (equal? 1 new-length)
                            'max-menu-length new-length))]
        ["down"
         (define current-length (hash-ref layout-settings 'max-menu-length))
         (update 'layout-settings
                 (hash-set* layout-settings
                            'simple-menu? #false
                            'max-menu-length (add1 current-length)))]
        [_ state])))

; -------------------------------------------------

(define (perform-selected-transform template)
  ; find the transform corresponding to the selected menu item
  ; and apply that transform to the WHOLE template
  (match template
    [(⋱ (/ [menu `(,_ ... (,transform ,(/ _ (▹ _))) ,_ ...)] _ _))
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
                     [(⋱ c⋱ (/ as/ (▹ a)))
                      (/ as/ a)])))
        menu)))


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
    [(⋱ c⋱ (/ [in-scope a-scope] a/ (▹ a)))
     a-scope]    
    ; fallthrough case - current λ params list has no in-scope
    ; TODO: decide if i do/should actually need this case
    [(⋱ c⋱ (/ a/ (▹ a)))
     '()]))



(define (advance-cursor-to-next-hole stx)
  ; new: prevent descending into metavar
  ; note that other metavar clause is ill-advised
  (match stx
    ; if i uncomment this, then i can't use -> to go
    ; past any unfilled hole
    ; HACK: dont skip past variadic holes
    [(⋱ c⋱ (/ [variadic #true] a/ (▹ (and h (or '⊙ '⊙+)))))
     (⋱ c⋱ (/ [variadic #true] a/ (▹ h)))]
    #;[(⋱ c⋱ (/ a/ (▹ (and h (or '⊙ '⊙+)))))
       (⋱ c⋱ (/ a/ (▹ h)))]
    [(⋱ c⋱ (and (/ y/ (▹ (⋱ d⋱ (/ x/ (and h (or '⊙ '⊙+))))))
                (not (/ [metavar _] _ _))))
     (⋱ c⋱ (/ y/ (⋱ d⋱ (/ x/ (▹ h)))))]
    [(⋱+ c⋱ (capture-when (and (or (/ _ (▹ _))
                                   (/ _ (or '⊙ '⊙+)))
                               #;(not (('metavar _) _ ... / _))))
         `(,as ... ,(/ w/ (▹ a)) ,(/ z/ b) ,bs ...))
     (⋱+ c⋱ 
         `(,@as ,(/ w/ a) ,(/ z/ (▹ b)) ,@bs))]
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
      [(⋱+ c⋱ (and m (/ metavar _/ _)))
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

  ; HACK: if the template is a metavar
  ; all transformed menu items will be the same metavar
  ; so we strip it
  (define stx
    (match template
      [(/ metavar a/ a) (/ a/ a)]
      [x x]))

  ; todo: expand holes in menu
  #; (define (expand-menu menu-stx) 0)

  (define in-scope-transforms
    (extract-scope stx)
    ; refactored: scope now consists of actual transforms,
    ; not just identifiers
    #;
    (for/list ([id (extract-scope stx)])
      `([⋱
          (▹ [sort expr] xs ... / ⊙)
          (▹ [sort expr] xs ... / (ref ',id))])))
  
  (define menu-stx
    (transforms->menu
     (append metavar-transforms
             base-transforms
             in-scope-transforms
             base-library-transforms)
     stx))
  
  (match stx
    [(⋱ c⋱ (/ xs/ (▹ x)))
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
     (⋱ c⋱ (/ [menu filtered-menu] xs/ (▹ x)))]
    [x (println "warning: no menu inserted")
       (when (empty? menu-stx)
         (println "warning: menu was empty; not handled"))
       x]))


(define (strip-menu stx)
  ; removes up to one menu from stx
  (match stx
    [(⋱ ctx⋱ (/ [menu menu] xs/ x))
     (⋱ ctx⋱ (/ xs/ x))]
    [x x]))

(define (local-augment stx)
  (match stx
    [(⋱ ctx⋱ (/ [in-scope in-scope] ts/ t))
     (⋱ ctx⋱ (augment (/ in-scope ts/ t)))]
    [x (println "warning: local-augment no-match") x]))

(define (move-menu-to-next-hole stx ambient-stx search-buffer)
  ; removes existing menu, advances cursor to next hole,
  ; and inserts a menu at that hole. if there is no such
  ; hole, the returned stx will have no menu

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
    (match-lambda [`(,t ,r)
                   #;(println `(buffer-cursor: ,(stx-buf-match-new? r search-buffer)))
                   (stx-buf-match-new? r search-buffer)]
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
     #;(println `(MUSMATCH (res) ,raw-stx ,buf))
     res]
    ; false fallthrough
    [_ #f]))

; returns stx matched with buf cursor or #f if no match
(define (stx-buf-match-new? stx buf)
  ; the logic here is convoluted.... need a simpler model
  #;(println `(search-buf ,buf))
  (match buf
    [`(▹ ,(? string? s))
     (match stx
       [(/ a/ `(,(or 'app) ,x ,xs ...))
        ; hacky: skip implicit app
        (stx-buf-match-new? x buf)]
       [_ #:when (stx-str-match? stx s)
          stx]
       [(/ a/ `(,x ,xs ...))
        ; if the current form isn't a match, recurse on first member
        ; this should descend until it hits a pseudo-atom eg. id, num, ref(?)
        (stx-buf-match-new? x buf)][_ #f])
     ]
    [(? string? s)
     ; case: finished strings the cursor has moved one from
     ; should this case actually be exact match?
     (if (stx-str-match? stx s #t) stx #f)]
    ; HACKY special-case for implicit app
    ; BUG: NOTE WELL THIS WILL FUCK UP NON-IMPLICIT APP!!
    ; we need this or this will just return 'app for applications
    ; after we press "("
    [(? list?)
     #:when (and (list? stx)
                 (match stx [(/ a/ `(,(or 'app ) ,raw-stx ...)) #t][_ #f]))
     (match-define (/ a/ `(,implicit-thing ,raw-stx ...)) stx)
     (stx-buf-match-new? (/ a/ raw-stx) buf)]
    [(? list?)
     #:when (and (list? stx)
                 (match stx [(/ a/ `(,(not (or 'num 'id 'ref) ) ,xs ...)) #t][_ #f]))
     (match-define (/ a/ raw-stx) stx)
     (if ((length raw-stx) . < . (length buf))
         #f
         ; note andmap returns last result, which is what we want??
         ; is this true in all cases??
         (andmap stx-buf-match-new?
                 (take raw-stx (length buf))  buf))]
    ; false fallthrough
    [_ #f]))

(define (hole-like-for-menu? fr)
  ; returns true if fr is a hole, or an empty pseudo-atomic element
  ; todo: refactor this to generically identify pseudoatomics
  (match fr
    [(/ _/ (or '⊙ '⊙+)) #t]
    [(/ _/ `(,(or 'num 'id 'ref) ,(/ _/ (or '⊙ '⊙+)))) #t]
    [_ #f]))


(define (stx-str-match? stx str (exact? #f))
  #;(println `(stx-str-match? ,stx ,str))
  (unless (string? str)
    (error "not string"))
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
    [(/ n/ `(num ,(/ d/ d) ...))
     (str-match? str (symbols->string d))]
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


(define (menu-filter-in-stx new-char init-stx old-buffer buffer-candidate)

  ; this hacky little guy restores the original menu prior to filtering
  (define stx
    (match init-stx
      [(⋱ ctx (/ [transform template] r/ reagent))
       (define new-template
         (insert-menu-at-cursor (strip-menu template) init-stx init-buffer))
       (⋱ ctx (/ [transform new-template] r/ reagent))]))
  
  (match-define
    (⋱ c⋱ (/ [transform (⋱ d⋱ (/ menu m/ m))] t/ t)) stx)
  (match-define
    (⋱ ctx (/ [transform template] r/ reagent)) stx)

  (define pre-template-candidate
    (⋱ d⋱ (/ [menu menu]  m/ m)))
  
  (define menu-candidate
    (filter-menu menu buffer-candidate))

  (define template-candidate
    (⋱ d⋱ (/ [menu menu-candidate]  m/ m)))

 
  (cond
    [(and (equal? " " new-char)
          (empty? menu-candidate))
     (values (⋱ ctx (/ [transform (move-menu-to-next-hole
                                   (perform-selected-transform pre-template-candidate)
                                   stx init-buffer)] ; empty search buffer
                       r/ reagent))
             init-buffer)]
    [(empty? menu-candidate)
     (values init-stx
             old-buffer)]
    #;[(and (equal? 1 (length menu-candidate)))]
    ; make temp special case here for length 1 menu with only hole
    ; should make a menu based on hole, and splice it to the length-1 menu
    [(and (equal? 1 (length menu-candidate))
          ; make this an option
          ; todo: case where buffer-cursor is on a hole
          (single-char-menu? menu-candidate))
     (values (⋱ ctx (/ [transform (move-menu-to-next-hole
                                   (perform-selected-transform template-candidate)
                                   stx init-buffer)] ; empty search buffer
                       r/ reagent))
             init-buffer)]
    [else
     (values (⋱ c⋱ (/ [transform template-candidate] t/ t))
             buffer-candidate)])
 
  )
