#lang racket

; bug if true (c)
(provide draw-fruct
         render-menu
         render-transform)

; for tests
#;(provide augment-absolute-offsets)

(require 2htdp/image
         "../../shared/containment-patterns/containment-patterns/main.rkt"
         "../../shared/slash-patterns/slash-patterns.rkt"
         "../language/syntax.rkt"
         "../common.rkt"
         "draw-fruct-common.rkt"
         "common-layout.rkt")


(define (metavar-tint-colors m layout-settings)
  (define-from layout-settings
    capture-pattern-shade-one
    capture-shade-one capture-shade-two capture-color-a
    capture-color-b capture-color-c capture-color-d
    capture-color-x)
  (for/hash ([(k v) (hash-set* layout-settings
                               ; hacky color overrides.
                               'pattern-shade-one capture-pattern-shade-one 
                               'bkg-color capture-shade-one
                               'background-block-color capture-shade-two)])
    (match v
      [(color _ _ _ _)
       ; HACKY HACK TO MAKE SELECTED METAVARS MARGINALLY PRETTIER
       (values k ((per-color-linear-dodge-tint
                   (match m
                     [0 capture-color-a]
                     [1 capture-color-b]
                     [2 capture-color-c]
                     [3 capture-color-d]
                     [_ capture-color-x])
                   0.4) v))]
      [_ (values k v)])))


(define (draw-fruct fruct layout-settings (depth #t) (bkg 0))
  (define-from layout-settings
    popout-transform? popout-menu? implicit-forms
    pattern-identifier-color pattern-shade-one pattern-shade-two
    unit-width unit-height radius
    selected-color +hole-color-pattern +hole-color-number selected-atom-color
    capture-color-a capture-color-b capture-color-c capture-color-d
    capture-color-x capture-atom-color)

  ; replaced threaded depth with precalculated one
  ; to prevent bkg color shifting in popout transform
  ; TODO: remove threaded version
  (define depth (match fruct
                  [(/ [depth d] a/ _) (zero? (modulo d 2))]
                  [_ #f]))
    
  (match fruct

    ; menu render specially
    [(and this-menu
          (/ [menu `((,transforms ,resultants) ...)] m/ m))
     #;(let ([temp-image (draw-fruct '? #;(/ m/ m) layout-settings)])
         (list (first temp-image)
               (match (first temp-image)
                 [(/ a/ a)
                  (/ [bounds `(((0 ,unit-height))
                               ((,unit-width ,unit-height)))]
                     a/ a)])
               (rectangle (image-width (second temp-image))
                          (image-height (second temp-image))
                          "solid" invisible)))
     ; TODO: bounds??
     (match-define (list fruct-with-positions new-image)
       (render-menu this-menu
                    layout-settings))
     (list (match fruct-with-positions
             ; hacky bounds insertion
             ; unit seems to work fine
             ; can't use template bounds, it's usually too big
             [(/ a/ a)
              (/ [bounds `(((0 ,unit-height))
                           ((,unit-width ,unit-height)))]
                 a/ a)])
           (if popout-menu?
               ; whatever is supposed to be there as placeholder
               (let ([temp-image (draw-fruct '? #;(/ m/ m) layout-settings)])
                 (rectangle (image-width (second temp-image))
                            (image-height (second temp-image))
                            "solid" invisible))
               new-image))]

    ; transform renders specially
    [(/ [transform template] t/ target)
     #;(let ([temp-image (draw-fruct (/ t/ target) layout-settings)])
         (list (match (first temp-image)
                 [(/ a/ a)
                  (/ [bounds `(((0 ,unit-height))
                               ((,unit-width ,unit-height)))]
                     a/ a)])
               (rectangle (image-width (second temp-image))
                          (image-height (second temp-image))
                          "solid" invisible)))
     ; TODO: bounds???
     (match-define (list new-fruct new-image)
       (render-transform (/ [transform template] t/ target)
                         layout-settings))
     (list new-fruct
           (if popout-transform?
               (let ([temp-image (draw-fruct (/ t/ target) layout-settings)])
                 (rectangle (image-width (second temp-image))
                            (image-height (second temp-image))
                            "solid"
                            invisible))
               new-image))]

    ; this needs to be after the transform/menu(?) cases
    ; otherwise we lose location information
    ; and popups draw improperly
    ; super hacky; investigate and refactor
    ; probably related to below hack
    [(/ [metavar m] a/ (and a
                            (or `(num ,_ ...)
                                `(ref ,_))))
     ; this is a hack case
     (define metavar-color
       (match m
         [0 capture-color-a]
         [1 capture-color-b]
         [2 capture-color-c]
         [3 capture-color-d]
         [_ capture-color-x]))

     (define tinted-layout-settings
       (metavar-tint-colors m layout-settings))

     (define selected-layout-settings
       (hash-set* tinted-layout-settings
                  ; colors hacks
                  'selected-color selected-color
                  #;#;'selected-atom-color (hash-ref tinted-layout-settings 'identifier-color)
                  #;#;'literal-color metavar-color
                  #;#;'identifier-color metavar-color))
     (define unselected-layout-settings
       (metavar-tint-colors
        m
        (hash-set* layout-settings
                   ; hack to override red bkg for atomic selections
                   'identifier-color capture-atom-color
                   'literal-color capture-atom-color)))
     (define atomic-metavar-background-color
       (hash-ref unselected-layout-settings 'capture-shade-two))
     
     (match-define (list new-fruct id-image)
       (draw-fruct (/ a/ a)
                   (if (selected? fruct)
                       selected-layout-settings
                       unselected-layout-settings)))
     (define id-height (image-height id-image))
     (define id-width (image-width id-image))
     (define radius-adj (div-integer radius 7/5))
     (define new-img
       (overlay/align
        "left" "top"
        ; hack to outline selected atomic metavars
        #;(if #f #;(selected? (/ a/ a))
              (rounded-rectangle-outline
               id-width id-height
               radius-adj selected-color selection-outline-width)
              empty-image)
        id-image
        ; backing
        (rounded-rectangle
         id-width id-height radius-adj
         atomic-metavar-background-color)))
     (list
      (match new-fruct
        [(/ b/ b)
         (/ [metavar m] b/ b)])
      new-img)]
    
    [(/ [metavar m] a/ a)
     (define tinted-layout-settings
       (metavar-tint-colors m layout-settings))
     (match-define (list new-fruct new-img)
       (draw-fruct (/ a/ a)
                   (hash-set* tinted-layout-settings
                             ; HACK TO PREVENT OVERRIDE OF SELECTION COLOR
                             'selected-color selected-color
                             'selected-atom-color (hash-ref tinted-layout-settings 'identifier-color))))
     (list
      ; hack
      ; this took forever to figure out
      ; otherwise this is taking the metavar attribute off
      ; so when we try to render the item in a menu...
      ; actually what exactly is going on?
      ; in any case, we need to make sure we're not loosing attributes..
      ; update: attribute issue should be taken care of by below:
      ; (BOUNDS should be taken care of by this)
      (match new-fruct
        [(/ b/ b)
         (/ [metavar m] b/ b)])
      new-img)]

    [(/ ref/ `(ref ,id))
     #:when (member 'ref implicit-forms)
     (define-from layout-settings radius)
     (match-define (list id-fruct id-image)
       (if (selected? (/ ref/ `(ref ,id)))
           ; hacky - properly seperate selected and metavar logic
           (draw-fruct id (hash-set layout-settings
                                    'identifier-color selected-atom-color))
           (draw-fruct id layout-settings)))
     ; TODO: properly add position information here
     (define id-height (image-height id-image))
     (define id-width (image-width id-image))
     (define new-bounds
       `(((0  ,id-height))
         ((,id-width ,id-height))))
     ; hacky smaller radii for looks
     (define radius-adj (div-integer radius 7/5))
     (list (/ [bounds new-bounds] ref/ `(ref ,id-fruct))
           (cond
             [(selected? (/ ref/ `(ref ,id)))
              (overlay/align
               "left" "top"
               ; layout goes inbetween
               id-image
               ; backing
               (rounded-rectangle
                id-width id-height radius-adj
                selected-color #;(if depth  )))]
             [else id-image]))]

    #;[(/ hole/ '⊙)
       ; HACK: added in this case for padding purposes only
       ; BUG: causes arrow to shift over if transform template is hole
       ; same issue exists for symbol, and would for num without +...
       ; NOTE: this MASKS a bug in how generalized rounded backings
       ; glitch out in the one-unit size case
       (match-define (list (/ new-hole/ '⊙) hole-image)
         (render-atom fruct (selected? fruct) layout-settings))
       (define new-bounds
         `(((0
             ,(image-height hole-image)))
           ((,(image-width hole-image)
             #;,(+ unit-width (image-width hole-image))
             ; unit-wdith is the hack
             ,(image-height hole-image)))))
       (list (/ [bounds new-bounds] new-hole/ 'hole) hole-image)]
    
    [(/ id/ `(id ,xs ...))
     ; id itself is not drawn
     ; draw the letters xs with no spaces
     (define children
       (map (curryr draw-fruct layout-settings) xs))
     (define-values (new-frs my-new-image _)
       (layout-row (list 0 0) 0 children))
     
     (define new-bounds
       `(((0
           ,(image-height my-new-image)))
         ((,(image-width my-new-image)
           ,(image-height my-new-image)))))
     (list (/ [bounds new-bounds] id/ `(id ,@new-frs)) my-new-image)]

    [(/ n/ `(num ,xs ...))
     (define children
       (map (curryr draw-fruct (if (selected? (/ n/ `(num ,xs ...)))
                                   (hash-set* layout-settings
                                              '+hole-color +hole-color-number
                                              'literal-color selected-atom-color)
                                   layout-settings))
            xs))
     (define-values (new-frs between-image _)
       (layout-row (list 0 0) 0 children
                   #;(if (empty? children) '()
                         (drop-right children 1))))
     ; HACK: above: skip + hole at end
     ; UPDATE: commented out. doesn't work right, rethink
     (define new-bounds
       `(((0
           ,(image-height between-image)))
         ((,(image-width between-image)
           ,(image-height between-image)))))
     (define radius-adj (div-integer radius 7/5))
     (define my-new-image
       (if (selected? (/ n/ `(num ,xs ...)))
           (overlay/align
            "left" "top"
            ; outline
            #;(rounded-rectangle-outline
               id-width id-height
               radius-adj selected-color selection-outline-width)
            ; layout goes inbetween
            between-image
            ; backing
            (rounded-rectangle
             (image-width between-image) (image-height between-image) radius-adj
             selected-color #;(if depth  )))
           between-image))
     (list (/ [bounds new-bounds] n/ `(num ,@new-frs)) my-new-image)]

    [(and ps (/ (sort 'params) a/ a))
     #;(println `(params case activates))
     (define local-layout-settings
       (hash-set* layout-settings
                  ; TODO magic colors
                  'identifier-color pattern-identifier-color
                  ; HACK need to update when patterns move beyond chars
                  '+hole-color +hole-color-pattern
                  'bkg-color pattern-shade-one
                  'background-block-color pattern-shade-two))
     (render-list ps
                  ; HACK reset depth for params-list
                  #t
                  bkg local-layout-settings (selected? fruct))]

    [(/ a/ (? list? a))
     (render-list (/ a/ a)
                  depth bkg layout-settings (selected? fruct))]

    [(/ a/ a)
     (render-atom (/ a/ a) (selected? fruct) layout-settings)]
    
    [(? (disjoin form-id? symbol?) a)
     ; TODO: MAGIC symbol include, investigate this
     ; this case SHOULD be only for form headers
     #;(println `(fallthru ,a))
     ; but actually catching at least:
     #; (? → ▹ λ app)
     ; TODO: factor out all these cases
     ; slight hack; adding blank attributes to a
     ; WARNING: DO NOT REPLACE THE LIST WITH JUST THE RENDER CALL
     ; form-ids will get wrapped and layout will get screwy
     ; BOUNDS: gonna try not doing anything here, hope for the best...
     (list
      a
      (second (render-atom (/ a) (selected? fruct) layout-settings)))]
    
    [_ (error (~v `(,fruct "error: layout: render: not a fruct")))]))






(define (render-horizontal layout-settings children)
  (define-from layout-settings
      unit-width )

  (define-values (new-children new-image _)
    (layout-row (list unit-width 0)
                unit-width
                children))

  ; experimental : show parentheses option
  ; misses a parens when we skip the space after a terminal non-atom
  (define newest-image
    new-image
    #;(if show-parens?
        (overlay/align "left" "top"
                       (render-symbol "(" identifier-color layout-settings)
                       (overlay/align "right" "bottom"
                                      (render-symbol ")" identifier-color layout-settings)
                                      new-image))
        new-image))

  (list new-children
        ; special case?
        ; if last child is atom, we leave a space on the right
        newest-image))


(define (render-vertical indent-width header-items layout-settings children)
  (define-from layout-settings 
    unit-width unit-height line-spacing)
  
  (define indent-image
    (rectangle indent-width unit-height
               "solid" invisible))

  (define line-spacer-image
    (rectangle unit-width line-spacing
               "solid" invisible))

  (define-values (header-fruct header-image _)
    (layout-row (list unit-width 0)
                unit-width
                (take children header-items)))

  (define-values (body-fruct body-image __)
    (layout-column (list indent-width
                         (+ line-spacing
                            (image-height header-image)))
                   line-spacing
                   (drop children header-items)))
  
  (list (append header-fruct body-fruct)
        (above/align*
         "left" header-image line-spacer-image
         (beside/align*
          "top" indent-image body-image))))







(define (render-list fruct depth bkg init-layout-settings selected?)
  (define-from init-layout-settings
    unit-width implicit-forms length-conditional-layout?
    length-conditional-cutoff force-horizontal-layout?)
  
  (match-define (/ a/ `(,first-stx ,rest-stx ...)) fruct)

  (define layout-settings
    (if selected?
        init-layout-settings
        #;(hash-set* init-layout-settings)
        init-layout-settings))

  ; figure this out here in case we truncate the names below
  (define if-like? (if-like-id? first-stx))
  (define lambda-like? (lambda-like-id? first-stx))
  (define cond-like? (cond-like-id? first-stx))
  (define mapp-like? (equal? 'mapp first-stx))
  (define params-like?
    (match fruct
      [(/ [sort 'params] _/ _) #t][_ #f]))

  (define ends-in-atom?
    (match rest-stx
      ; HACK: properly abstract atomic definition
      [`(,xs ... ,(/ _/ (or (? (negate list?))
                            '⊙ `(,(or 'id 'ref 'num) ,_ ...)))) #t]
      [_ #f]))
  
  ; forms may lose their identities here
  (define possibly-truncated-stx
    (if (member first-stx implicit-forms)
        rest-stx
        (cons first-stx rest-stx)))

  ; render the children
  (define children
    (for/list ([s possibly-truncated-stx])
      ; THIS IS THE ONLY USE OF DRAW-FRUCT OUTSIDE OF ITSELF
      ; REFACTOR SO ALL OF THE RENDER-LIST FNS CAN BE SPLIT OFF
      (draw-fruct s layout-settings (not depth) bkg)
      #;(if (and selected? (form-id? s))
            ; if selected highlight form-name; hacky
            ; todo: refactor render-symbol to dehack
            `(,(first (draw-fruct s layout-settings (not depth) bkg))
              ,(render-symbol s selected-color layout-settings))
            (draw-fruct s layout-settings (not depth) bkg)))) 
  
  ; decide if we're laying this out horizontally or vertically
  (define render-this-horizontally?
    (cond
      [force-horizontal-layout? #t]
      [(not (or if-like? lambda-like? cond-like?)) #t]
      [(not length-conditional-layout?) #f]
      [else (match-define `((,_ ,child-images) ...) children)
            (define total-length
              (div-integer (apply + (map image-width child-images))
                           unit-width))
            (define horiz-width-approx total-length)
            (define vertical-width-approx
              (apply max (map image-width child-images)))
            (and (total-length . < . length-conditional-cutoff)
                 ; below is broken
                 ; suppose to prevent vertical layout
                 ; in case where savings is small
                 #;(vertical-width-approx . > . (+ -5 horiz-width-approx)))]))

  (define (local-render-vertical num-header-items indent stright-left? header-exception?)
    (match-define (list new-kids-local new-layout-local)
      (render-vertical indent num-header-items layout-settings children))
    (add-vertical-backing (/ a/ new-kids-local) children layout-settings
                          indent num-header-items  new-layout-local selected? depth
                          stright-left? header-exception?))
  
  (match-define (list almost-final-stx final-image)
    (cond
      [render-this-horizontally?
       (match-define (list new-kids-local new-layout-local)
         (render-horizontal layout-settings children))
       ; HACK
       (if (or params-like?
               mapp-like?
               (not ends-in-atom?))
           ; omit trailing space
           (add-horizontal-backing 
            (/ a/ new-kids-local) children new-layout-local #f selected? depth layout-settings)
           (add-horizontal-backing 
            (/ a/ new-kids-local) children new-layout-local #t selected? depth layout-settings))]

      ; (local-render-vertical num-header-items ident-width straight-left? header-exception?)
      [lambda-like?
       (local-render-vertical 2 (* 2 unit-width) #f #t)]    
      [(and if-like? (member first-stx implicit-forms))
       (local-render-vertical 1 unit-width #t #f)]      
      [if-like?
       (local-render-vertical 2 (+ (image-width (second (first children))) (* 2 unit-width)) #f #f)]
      [cond-like?
       (local-render-vertical 1 (* 2 unit-width) #t #f)]))

  ; possible BUG
  ; put back in the implicit form
  ; dunno if this is working properly and or necessary at all
  (list (if (member first-stx implicit-forms)
            (match almost-final-stx
              [(/ a/ thing)
               (/ a/ (cons first-stx thing))])
            almost-final-stx)
        final-image)
  
  ; experimental parens option
  #;(define newest-image
      (if show-parens?
          (overlay/align "left" "top"
                         (render-symbol "(" identifier-color layout-settings)
                         (overlay/align "right" "bottom"
                                        (render-symbol ")" identifier-color layout-settings)
                                        newer-image))
          newer-image)))

(define (add-vertical-backing fruct init-children layout-settings
                              indent num-header-items
                              new-layout-local selected? depth
                              straight-left? header-exception?)
  (define-from layout-settings
    unit-width radius outline-block-width background-block-width
    outline-block-color selected-color bkg-color alternate-bkg-vertical?
    selection-outline-width background-block-color)
  ; the following is a somewhat hacky way of adding a unit
  ; of right-padding to lines ending in atomic forms
  ; there might be a better way of doing this, but it
  ; should be done in this function; trying to move it
  ; outside creates issues that need vicious compensation
  (define children
    (for/list ([bc init-children])
      (match bc
        [`(,(and fr (/ [bounds `(,lb ,rb)] a/ a)) ,img)
         (define new-rb
           (if (real-atomic? fr)
               (match rb
                 [`((,w ,h))
                  `((,(+ w unit-width) ,h))])
               rb))
         `(,(and fr (/ [bounds `(,lb ,new-rb)] a/ a)) ,img)]
        ; fallthrough here should just be form-identifiers
        [x x])))
  (define header-children (take children num-header-items))
  (define body-children (drop children num-header-items))
  ; assuming header isn't empty
  (define last-header-child (last header-children))
  ; assuming body-children isn't empty
  (define middle-rows-children (drop-right body-children 1))
  (define last-row-child (last body-children))

  (match-define `(,params-left-bounds ,params-right-bounds)
    (match last-header-child
      [`(,(/ [bounds b] _ _) ,_) b]
      ; case for only thing in header is form-id e.g. cond:
      [`(,(? symbol?) ,img) `(((0 ,(image-height img)))
                              ((,(+ unit-width (image-width img)) ,(image-height img))))]))
  
  #;(match-define `(,(/ [bounds `(,params-left-bounds
                                  ,params-right-bounds)] _ _) ,_)
      last-header-child)
  ; hacky error checking
  (match last-row-child
    [(not `(,(/ [bounds `(,last-left-bounds
                          ,last-right-bounds)] _ _) ,_))
     ; todo: remove if this isn't getting triggered
     (println `(warning: vertical backing bounds issue: ,children))] [_ 0])
  
  (match-define (/ [bounds last-row-bounds] _ _)
    (first last-row-child))

  (match-define `(,(/ [bounds middle-row-bounds] _ _) ...)
    (map first middle-rows-children))
  
  (define last-left-bounds (append #;(apply append (map first middle-row-bounds)) (first last-row-bounds) ))
  (define last-right-bounds (append #;(apply append (map second middle-row-bounds)) (second last-row-bounds) ))
  ; assume for now that the id always has unit height
  ; and that params have at least unit height

  (define total-params-height
    (apply + (second (apply map list params-left-bounds))))
  (when (not (equal? total-params-height
                     (apply + (second (apply map list params-right-bounds)))))
    (error "left and right parameter profiles have different total heights"))
  (define first-row-left-bounds
    (for/list ([r params-left-bounds])
      (match r
        [`(,x ,y) `(0 ,y)])))
  (define offset
    ; is this the indent we actually want? or constant...?
    (+ (apply + (map (compose image-width second) (drop-right header-children 1)))
       (* unit-width (length header-children))))
  (define first-row-right-bounds
    (for/list ([r params-right-bounds])
      (match r
        [`(,x ,y) `(,(+ x offset) ,y)])))

  #;(when (not (empty? middle-rows-children))
      (error "bounds: not empty middle rows case not implemented"))
  
  (define middle-left-bounds (apply append (map first middle-row-bounds)))
  (define middle-right-bounds (apply append (map second middle-row-bounds)))

  (define middle-rows-left-bounds
    (for/list ([r middle-left-bounds])
      (match r
        [`(,x ,y) `(0 #;,(+ x indent) ,y)])))
  (define middle-rows-right-bounds
    (for/list ([r middle-right-bounds])
      (match r
        [`(,x ,y) `(,(+ x indent) ,y)])))
     
  (define almost-last-row-left-bounds
    (for/list ([r last-left-bounds])
      (match r
        [`(,x ,y) `(,(+ x indent) ,y)])))
  (define last-row-left-bounds

    ; note this currently masks a rendering bug affecting
    ; left profiles which involve one-unit steps (needs two for smooth transition)
    (if straight-left?
        (map (match-lambda [`(,x ,y) `(0 ,y)]) almost-last-row-left-bounds)
        ; first should be in-line with header
        ; which is currently fixed at 0
        (match almost-last-row-left-bounds
          [`((,x ,y) ,as ...) `((0 ,y) ,@as)])))
  
  (define last-row-right-bounds
    (for/list ([r last-right-bounds])
      (match r
        [`(,x ,y) `(,(+ x indent) ,y)])))   
  (define final-left-bounds
    (append first-row-left-bounds
            middle-rows-left-bounds
            last-row-left-bounds))
  (define final-right-bounds
    (append first-row-right-bounds
            middle-rows-right-bounds
            last-row-right-bounds))
  
  (define basic-image
    (overlay/align "left" "top"
                   ; SUPER HACKY way of changing form symbol / function-ref color
                   ; only works if symbol is first thing in form
                   (head-overlay-hack (first (first header-children))
                                      selected? new-layout-local layout-settings)
                   (if selected?
                       empty-image
                       (if alternate-bkg-vertical?
                           (if depth
                               (overlay
                                ; todo: rewfactor rounded-backing to avoid double-call
                                (rounded-backing
                                 final-right-bounds
                                 final-left-bounds
                                 radius outline-block-color "outline" outline-block-width
                                 header-exception?)
                                (rounded-backing
                                 final-right-bounds
                                 final-left-bounds
                                 radius bkg-color "solid" background-block-width
                                 header-exception?))
                               (rounded-backing
                                final-right-bounds
                                final-left-bounds
                                radius background-block-color "solid" background-block-width
                                header-exception?))
                           (overlay
                            ; todo: rewfactor rounded-backing to avoid double-call
                            (rounded-backing
                             final-right-bounds
                             final-left-bounds
                             radius outline-block-color "outline" outline-block-width
                             header-exception?)
                            (rounded-backing
                             final-right-bounds
                             final-left-bounds
                             radius bkg-color "solid" background-block-width
                             header-exception?)))
                       )))
  
  (define possibly-selected-image
    (if selected?
        (overlay/align "left" "top"
                       (rounded-backing
                        final-right-bounds
                        final-left-bounds
                        radius selected-color "outline" selection-outline-width
                        header-exception?)
                       basic-image
                       ; if we want red backgrounds
                       (rounded-backing
                        final-right-bounds
                        final-left-bounds
                        radius selected-color "solid" 0
                        header-exception?))
        basic-image))
     
  (list (match fruct
          [(/ a/ a)
           (/ [bounds `(,final-left-bounds
                        ,final-right-bounds)] a/ a)])
        possibly-selected-image))


(define (add-horizontal-backing whole-stx children new-layout rear-padded? selected? depth layout-settings)
  (define-from layout-settings
    radius unit-width alternate-bkg-horizontal?
    outline-block-width selection-outline-width background-block-color
    selected-color outline-block-color bkg-color)
  (define width
    (+ (image-width new-layout)
       (if rear-padded? unit-width 0)))
  (define height
    (image-height new-layout))
  (define new-layout-local
    (overlay/align
     "left" "top"
     (if selected?
         (rounded-rectangle-outline
          width height radius selected-color selection-outline-width)
         ; todo: create arg here for width
         ; THIS IS WHERE HORIZONTAL OUTLINES ARE CREATED
         (rounded-rectangle-outline
          width height radius outline-block-color outline-block-width))
     ; layout goes inbetween
     new-layout
     ; backing
     (if selected?
         (rounded-rectangle width height radius
                            selected-color)
         ; WTF. if i change below colors, things go screwy
         ; in weird ways... like it changes pattern background color???
         (rounded-rectangle width height radius
                            (if alternate-bkg-horizontal?
                                (if depth bkg-color background-block-color)
                                bkg-color))
         )))
  ; HACK to RECOLOR head if form-id or function-call-ref
  (define new-image
    (head-overlay-hack (first (first children))
                       selected? new-layout-local layout-settings))
  (define new-bounds
    `(((0 ,height)) ((,width ,height))))
  (list (match whole-stx
          [(/ a/ a) (/ [bounds new-bounds] a/ a)])
        new-image))

(define (head-overlay-hack the-head selected? new-layout-local layout-settings)
  ; SUPER HACKY way of changing form-id/function-ref color
  (define-from layout-settings selected-atom-color)
  (if (and selected? (or (symbol? the-head)
                         (match? the-head (/ ref/ `(ref ,id)))))
      (let ([my-symbol (if (symbol? the-head)
                           the-head
                           (match the-head
                             [(/ ref/ `(ref ,id)) (ref->symbol the-head)]
                             [_ '||]))])
        (overlay/align
         "left" "top"
         (beside (render-symbol '| | selected-atom-color layout-settings)
                 (render-symbol my-symbol selected-atom-color layout-settings))
         new-layout-local))
      new-layout-local))


; --------------------------------------------
; move to seperate file when resolve circular dependencies

(define (render-menu stx layout-settings)
  (define-from layout-settings
    text-size typeface max-menu-length max-menu-length-chars
    implicit-forms simple-menu? custom-menu-selector?
    selection-outline-width line-spacing selected-color
    menu-bkg-color menu-search-color menu-secondary-color radius
    menu-outline-color menu-outline-width simple-menu-background-color
    menu-expander-height)
  (match-define (/ [menu `((,transforms ,resultants) ...)] p/ place) stx)

  #| this function currently renders ALL menu items
     not just the ones displayed.
     tagged: performance, optimization |#

  (define (single-char-menu? resultants)
    (match resultants
      [`(,(/ [sort (or 'digit 'char)] _/ _) ...) #t] [_ #f]))

  (define local-max-menu-length
    (if (single-char-menu? resultants)
        max-menu-length-chars
        max-menu-length))

  (define resultants-wraparound
    (match resultants
      [`(,as ... ,(/ b/ (▹ b)) ,cs ...)
       `(,(/ b/ (▹ b)) ,@cs ,@as)]))
    
  (define truncated-menu
    (if ((length resultants-wraparound) . < . local-max-menu-length)
        resultants-wraparound
        (take resultants-wraparound local-max-menu-length)))
  
  (match-define fruct-image-pairs
    (for/list ([item truncated-menu])
      (define override-layout-settings
        ; slight hack to remove form backings
        ; another hack for metavars
        ; need to do this again here because
        ; menu breaks layout-settings inheritance
        (match item
          ; shouldnt need special case
          ; render should pick this up
          ; but it doesn't work either way...
          [(/ [metavar m] b/ b)
           (hash-set* layout-settings
                      'force-horizontal-layout? #f)]
          [_ (hash-set* layout-settings
                        'force-horizontal-layout? #f)]))
      (define search-buffer (match item [(/ [search-buffer search-buffer] a/ a)
                                         search-buffer]
                              ; todo: fix hardcoded init-buffer here:
                              [_ '(▹ "")]))

      (define (strip▹ buf)
        (match buf
          [`(▹ ,s) s]
          [(? list?) (map strip▹ buf)]
          [x x]))
      
      (define (overlay-search-buffer stx image)
        (match stx
          ; sort of hacky exception for ref, num
          ; still not really right, r is still going to select refs maybe?
          ; THE REAL ISSUE HERE is that it displays eg. overlaid "r" for all refs
          ; but they shouldn't be here to begin with; fix on the tranform-mode side...
          #;[(/ a/ `(,(or 'ref 'num 'mapp #;(? (curryr member implicit-forms))) ,xs ...))
             image]
          [_ (overlay/align
              "left" "top"
              (match search-buffer
                [`(▹ ,(? string? s))
                 (render-symbol (string->symbol (string-append " " s))
                                menu-search-color layout-settings)]
                [x (render-symbol (string->symbol
                                   ; todo: once implemented ")" properly
                                   ; then make below work
                                   #;(substring (~a (strip▹ x)) 0 (+ -1 (string-length (~a (strip▹ x)))))
                                   (string-replace (~a (strip▹ x)) ")" " "))
                                  menu-search-color layout-settings)]
                [_ (error "complex search buffer layout not implemented")])
              
              image)]))
      (cond
        [custom-menu-selector?
         (match item
           [(/ b/ (▹ b))
            (list (first (draw-fruct (/ b/ (▹ b)) override-layout-settings))
                  ; HACK, BUG: get (wrong) positioning data
                  ; also special-cases char menus
                  (overlay-search-buffer
                   (/ b/ (▹ b))
                   (if (single-char-menu? resultants)
                       (second (draw-fruct (/ b/ b) override-layout-settings))
                       (overlay/align
                        "left" "top"
                        (space text-size typeface)
                        (if (or (not (list? b))
                                (or (and (member 'ref implicit-forms)
                                         (match b [`(ref ,_) #t][_ #f]))
                                    (and (member 'num implicit-forms)
                                         (match b [`(num ,_) #t][_ #f]))))
                            ; HACK extra spacing for atoms
                            ; HACK for implicit refs
                            ; HACK below throws off offset alignment
                            ; need to refactor as popped layer
                            (let ([temp (beside (space text-size typeface)
                                                (second (draw-fruct (/ b/ b)  override-layout-settings))
                                                (space text-size typeface))])
                              (overlay (rounded-rectangle-outline
                                        (image-width temp)
                                        ; slightly hacky adjustment
                                        ; to make outline entirely inside line-height
                                        (max 0 (- (image-height temp) 1))
                                        radius
                                        selected-color selection-outline-width)
                                       temp))
                            (let ([temp (second (draw-fruct (/ b/ b) override-layout-settings))])
                              (overlay (rounded-rectangle-outline
                                        (image-width temp)
                                        ; slightly hacky see above
                                        (max 0 (- (image-height temp) 1))
                                        radius
                                        selected-color selection-outline-width)
                                       temp)))))))]
           [(/ b/ b)
            (list (first (draw-fruct item override-layout-settings))
                  (overlay-search-buffer
                   (/ b/ b)
                   (if (or (not (list? b)) (or (and (member 'ref implicit-forms)
                                                    (match b [`(ref ,_) #t][_ #f]))
                                               (and (member 'num implicit-forms)
                                                    (match b [`(num ,_) #t][_ #f]))))
                       (beside (space text-size typeface)
                               (second (draw-fruct item override-layout-settings))
                               (space text-size typeface))
                       ; below is call that should have tint when metavar 666
                       (second (draw-fruct item override-layout-settings)))))])]
        [else (draw-fruct item override-layout-settings)])))


  (define-values (truncated-menu-fruct
                  truncated-menu-image _)
    (layout-column '(0 0) line-spacing fruct-image-pairs))
  
  (define expander-ellipses-color
    menu-outline-color)

  ; stylish backing
  (define cool-menu
    (if simple-menu?
        (overlay truncated-menu-image
                 (rounded-rectangle
                  (image-width truncated-menu-image)
                  (image-height truncated-menu-image)
                  radius
                  simple-menu-background-color)) ; magic color
        (overlay
         (above (ellipses menu-expander-height expander-ellipses-color)
                truncated-menu-image
                (ellipses menu-expander-height expander-ellipses-color))
         (rounded-rectangle
          (image-width truncated-menu-image)
          (image-height truncated-menu-image)
          radius
          menu-bkg-color)
         (rounded-rectangle
          (+ 0 (image-width truncated-menu-image))
          (+ (* 2 menu-expander-height)
             (image-height truncated-menu-image))
          radius
          menu-secondary-color)))) ; magic color

  ; all this is just to put the rewritten fructs back in the right place
  ; TODO: check to make sure the data is going in the right place!!
  ; It's not... at least, the outlines don't render
  ; TODO: simplify this entire approach.
  (define-values (menu-before-cursor menu-from-cursor)
    (match resultants
      [`(,as ... ,(/ b/ (▹ b)) ,cs ...)
       (values as
               `(,(/ b/ (▹ b)) . ,cs))]))
  (define num-items-before-cursor
    (length menu-before-cursor))
  (define num-items-from-cursor-to-end
    (length menu-from-cursor))
  (define replace-in-wrapparound
    (append truncated-menu-fruct
            (list-tail resultants-wraparound
                       (length truncated-menu-fruct))))
  
  (define new-fruct
    (append (list-tail replace-in-wrapparound num-items-from-cursor-to-end)
            (take replace-in-wrapparound num-items-from-cursor-to-end)))
  
  
  (list (/ [menu `(,@(map list transforms new-fruct))] p/ place)
        (overlay (rounded-rectangle-outline (image-width cool-menu)
                                            (image-height cool-menu)
                                            radius menu-outline-color menu-outline-width)
                 cool-menu)))



(define (render-transform fruct layout-settings)
  (define-from layout-settings
    text-size typeface selected-color transform-template-only?
    tint-template? transform-tint-color radius
    selection-outline-width)
  
  (match-define (/ [transform template] t/ target) fruct)

  (match-define (list target-fruct target-image)
    (draw-fruct (/ t/ target) (hash-set* layout-settings
                                         ; BUG 663832872:
                                         ; below only triggers horizontal bkgcolors
                                         ; figure out why not vertical
                                         #;'gry-one 
                                         #;'gry-two )))
  
  (match-define (/ new-t/ new-target)
    (match target-fruct
      [(? (disjoin symbol? number?)) template]
      [(/ a/ a)
       (/ [display-offset (list 0 0)]
          [display-box (list (image-width target-image)
                             (image-height target-image))]
          a/ a) ]))
  
  (define arrow-image
    (second (draw-fruct '→ layout-settings)))

  (match-define (list template-fruct template-image)
    (if tint-template?
        ; debtably a hack:
        ; apply a color tint to every color in layout-settings
        (draw-fruct template
                    (for/hash ([(k v) layout-settings])
                      (match v
                        [(color _ _ _ _)
                         #:when (not (set-member? (set 'capture-shade-one
                                                       'capture-shade-two
                                                       'capture-color-a
                                              'capture-color-b
                                              'capture-color-c
                                              'capture-color-d
                                              'capture-color-x)k))
                         (values k ((per-color-linear-dodge-tint
                                     transform-tint-color 0.0) v))]
                        [_ (values k v)])))
        (draw-fruct template layout-settings)))


  ; determines menu offset
  (define template-offset
    (list (if transform-template-only?
              ; whether to show only the template
              ; (no target or arrow)
              0
              (apply + (map image-width
                            (list target-image
                                  (space text-size typeface)
                                  arrow-image
                                  (space text-size typeface)))))
          0))
 
  (define new-template
    (match template-fruct
      [(? (disjoin symbol? number?)) template]
      [(/ a/ a)
       (/ [display-offset
           template-offset]
          [display-box
           (list (image-width template-image)
                 (image-height template-image))]
          a/ a) ]))

  (define template-bounds
    (match template-fruct
      [(/ [bounds b] _ _) b]
      ; todo: remove if this isn't being triggered
      [_ (println "warning: render-transform: template bounds fallthough")
         `(((0 0)) ((,(image-width template-image) ,(image-height template-image))))]))

  (define target-bounds
    (match target-fruct
      [(/ [bounds b] _ _) b]
      ; todo: remove if this isn't being triggered
      [_ (println "warning: render-transform: target bounds fallthough")
         `(((0 0)) ((,(image-width target-image) ,(image-height target-image))))]))

  (define target-backing
    ; HACK: this conditional is specifically for 1-unit forms
    ; since rounded-backing is currently bugged for 1-unit (differences)
    (if (real-atomic? target-fruct)
        empty-image
        (rounded-backing
         (second target-bounds)
         (first target-bounds)
         radius selected-color "outline" selection-outline-width #f)))

  (define template-backing
    (rounded-backing
     (second template-bounds)
     (first template-bounds)
     radius selected-color "outline" selection-outline-width #f))
  
  (define transform-backing
    (make-transform-backing
     target-bounds template-bounds layout-settings))
  
  (define new-image
    (if transform-template-only?
        ; show only template
        ; HACK: since rounded-backing is currently bugged
        ; for scales < 2xradius. remove when this is fixed
        (if (< (first (first (second template-bounds))) text-size)
            template-image
            (overlay/align "left" "top"
                         template-backing
                         template-image))
        
        ; show target - >template
        (overlay/align
         "left" "top"
         (beside/align
          "top"     
          (overlay/align "left" "top"
                         target-backing
                         target-image)
          (beside (space text-size typeface)
                  arrow-image 
                  (space text-size typeface))
          (overlay/align "left" "top"
                         template-backing
                         template-image))
         transform-backing)))
  
  (list (/ [transform new-template] new-t/ new-target)
        new-image))


(define (make-transform-backing target-bounds template-bounds
                                layout-settings)
  (define-from layout-settings
    unit-width unit-height radius
    transform-color
    transform-outline-width)
  #| better transform backing plan:
     mw = max width of right profile of target
     mh = min height of target and template profiles
     left backing left profile is target left profile
     left backing right profile is constant mw+4 up to
       mh and then constant mw below that
       (actually instead of 4 do min width of template right profile)
     we will overlay/position this with right backing
     actually we shoulsn't even need a right backing...
   |#
  (define target-right-profile (second target-bounds))
  (define template-left-profile (first template-bounds))
  (define template-right-profile (second template-bounds))
  (define min-width-template
    (apply min (map first template-right-profile)))
  (define max-target-width
    (apply max (map first target-right-profile)))
  (define min-profile-height
    (min (length target-right-profile)
         (length template-right-profile))) ; this is in chars
  (define new-right-profile-init
    ; below 2 should be 3 for 2 spaces and arrow
    ; but taking off one to prevent any overshoot
    (make-list min-profile-height
               `(,(+ (* 3 unit-width) min-width-template max-target-width)
                 ,unit-height)))
  
  (define template-target-height-diff
    (- (length target-right-profile)
       (length template-right-profile)))
  (define left-backing-left-bounds
    (first target-bounds))
  (define left-backing-right-bounds
    (if (> template-target-height-diff 0)
        (append new-right-profile-init
                (make-list template-target-height-diff
                           `(,max-target-width ,unit-height)))
        new-right-profile-init))
  ; this next bit is hacky and might screw up for
  ; complex template profiles?
  (define right-backing-right-bounds
    `((,(* 4 unit-width) ,unit-height) (,(* 4 unit-width) ,unit-height)))
  (define right-backing-left-bounds
    `((0 ,unit-height) (,(* 2 unit-width) ,unit-height)))
  (overlay/align/offset
   "left" "top"
   (overlay (rounded-backing
             left-backing-right-bounds
             left-backing-left-bounds
             radius transform-color "solid" 0 #f)
            ; todo: refactor rounded-backing
            ; to avoid double-call here
            (rounded-backing
             left-backing-right-bounds
             left-backing-left-bounds
             radius transform-color "outline" transform-outline-width #f))
   (+ (* 1 unit-width) max-target-width) 0

   (if (>= template-target-height-diff 0)
       empty-image
       (rounded-backing
        right-backing-right-bounds
        right-backing-left-bounds
        radius transform-color "solid" 0 #f))))
