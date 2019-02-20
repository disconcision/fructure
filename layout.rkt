#lang racket

(require 2htdp/image)
(require rackunit
         "layout-tests.rkt"
         "new-syntax.rkt"
         "language.rkt"
         "common.rkt"
         "../containment-patterns/containment-patterns.rkt"
         "common-graphics.rkt")

; bug if true (c)
(provide fructure-layout
         display-keypresses)

; for tests
(provide render
         test-settings
         augment-absolute-offsets)

(define (div-integer x y)
  (inexact->exact (round (div x y))))



; TODO: rework as default settings?
(define test-settings-init 
  (hash 'text-size 34
        'typeface "Iosevka, Light"
        'max-menu-length 3
        'max-menu-length-chars 1
        'popout-transform? #t
        'popout-menu? #t
        'custom-menu-selector? #t
        'force-horizontal-layout? #f
        'length-conditional-layout? #t
        'length-conditional-cutoff 14
        'dodge-enabled? #t
        'implicit-forms '(ref num app)
        'line-spacing 0 ; 1
        'char-padding-vertical 0 ; 5
        'show-parens? #f

        'hole-bottom-color (color 252 225 62)
        'hole-side-color (color 193 115 23)
        'background-block-color (color 25 80 84)
        'transform-tint-color (color 160 0 0) ; selected-color
        'selected-atom-color (color 255 255 255)
        'menu-bkg-color (color 112 112 112)
        'form-color (color 0 130 214)
        'literal-color (color 228 150 34)
        'grey-one (color 230 230 230)
        'grey-two (color 215 215 215)
        'identifier-color (color 0 0 0)
        'selected-color (color 230 0 0)
        '+hole-color (color 25 80 84)
        'transform-arrow-color (color 255 255 255)
        'bkg-color (color 0 47 54)
        'pattern-bkg-color (color 230 230 230)
        'pattern-grey-one (color 17 39 46)#;(color 76 76 76)
        'pattern-grey-two (color 110 110 110)

        ))

; COPIED from fructure.rkt TODO REFACTOR
(define (add-dynamic-settings layout)
  (define-from layout
    text-size typeface
    char-padding-vertical)
  (define (div-integer x y)
    (inexact->exact (round (div x y))))
  (define space-image
    (text/font " " text-size "black"
               typeface 'modern 'normal 'normal #f))
  (hash-set* layout
             'radius (sub1 (div-integer text-size 2))
             'margin (div-integer text-size 5)
             'unit-width (image-width space-image)
             'unit-height (+ char-padding-vertical (image-height space-image))))

; add dynamic settings to layout
(define test-settings (add-dynamic-settings test-settings-init))


(define (display-keypresses keypresses)
  (define (key-remap k)
    (match k
      [" " "SPACE"]
      ["\r" "↪"]
      ["\b" "BACK"]
      ["right" "→"]
      ["left" "←"]
      ["up" "↑"]
      ["down" "↓"]
      [else k]))
  (if (empty? keypresses)
      empty-image
      (above/align
       "left"
       (text/font " " 20 (color 255 255 255) #f 'modern 'normal 'normal #f)
       (beside/align
        "top"
        (text/font " " 20 (color 255 255 255) #f 'modern 'normal 'normal #f)
        (text/font (string-append " " (key-remap (first keypresses)))
                   20 (color 255 255 255 220) #f 'modern 'normal 'bold #f)
        (text/font (apply string-append
                          (map (λ (x) (string-append " " (key-remap x)))
                               (if (< (length (rest keypresses)) 10)
                                   (rest keypresses) (take (rest keypresses) 10))))
                   20 (color 255 255 255 160) #f 'modern 'normal 'normal #f)
        (text/font " ..."
                   20 (color 255 255 255 220) #f 'modern 'normal 'bold #f)))))


; fructure-layout : syntax -> pixels
(define (fructure-layout fruct layout-settings (screen-x 800) (screen-y 400))
  (define-from layout-settings
    bkg-color text-size popout-transform? popout-menu?)
 
  (define expander-height
    (round (* 1/4 text-size))) ; magic af
  
  ; sanity checks
  (match fruct
    [`(◇ ,x) (error "strip top before calling")] [_ 0])

  (match-define `(,x-offset ,y-offset) `(,text-size ,(* 3 text-size)))
  ; magic 3 above leaves room for key display

  (match-define `(,new-fruct ,scene-image)
    (render
     (match fruct
       [(/ fr/ stx)
        (/ [display-offset `(,x-offset ,y-offset)]
           [display-box `(,screen-x ,screen-y)]
           fr/ stx)])
     layout-settings))

  ; calculate absolute positioning from relative
  (define newest-fruct (augment-absolute-offsets new-fruct))
  
 
  (define (render-in renderer foreground background x-align y-align)
    (match-define (list _ foreground-image)
      (renderer foreground
                layout-settings))
    (place-image/align foreground-image
                       x-align y-align
                       "left" "top"
                       background))

 

  (define (stick-transform-in backing-image)
    (match newest-fruct
      [(or (and this-transform
                (/ [transform _]
                   [display-absolute-offset
                    `(,(app (curry + x-offset) x)
                      ,(app (curry + y-offset) y))] t/ _))
           (⋱ _ (and this-transform
                     (/ [transform _]
                        [display-absolute-offset `(,x ,y)] t/ _))))
       (render-in render-transform
                  this-transform backing-image
                  ; leaving this placeholder in case we
                  ; decide to re-pad transforms
                  (+ x #;(- margin)) y)]
      [_ backing-image]))

  ; from mode-transform:
  (define (single-char-menu? menu-candidate)
    (match-let ([`((,_ ,resultants) ...) menu-candidate])
      (match resultants
        [`(,(/ [sort (or 'digit 'char)] _/ _) ...) #t] [_ #f])))
  
  (define (stick-menu-in backing-image)
    (match newest-fruct
      [(or (/ [transform
               (⋱ _ (and this-menu
                         (/ [menu menu]
                            [display-absolute-offset
                             `(,(app (curry + x-offset) x)
                               ,(app (curry + y-offset) y))]
                            m/ _)))] t/ _)
           (⋱ _ (/ [transform
                    (⋱ _ (and this-menu
                              (/ [menu menu]
                                 [display-absolute-offset `(,x ,y)]
                                 m/ _)))] t/ _)))
       (render-in render-menu
                  this-menu backing-image
                  ; 666
                  (+ x #;(if (single-char-menu? menu)
                             ; ULTRA MAGIC NUMBER
                             ; empirically -12 works for text-size 30 (width is 18)
                             (* -12/30 text-size) 0))
                  (+ y (- expander-height)))]
      [_ backing-image]))
 
          

  (define (>>if ? f a)
    (if ? (f a) a))

  (define new-image
    (overlay/align/offset
     "left" "top"
     scene-image
     (- x-offset) (- y-offset)
     (rectangle screen-x screen-y "solid" bkg-color)))
  
  (list newest-fruct
        (>>if popout-menu? stick-menu-in
              (>>if popout-transform? stick-transform-in
                    new-image))))




(define (render fruct layout-settings (depth #t) (bkg 0))
  (define-from layout-settings
    text-size popout-transform? popout-menu? implicit-forms
    pattern-bkg-color pattern-grey-one pattern-grey-two
    bkg-color background-block-color unit-width unit-height
    selected-color grey-one grey-two unit-width radius)
    
  (match fruct

    ; menu render specially
    [(and this-menu
          (/ [menu `((,transforms ,resultants) ...)] m/ m))
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
               (let ([temp-image (render '? #;(/ m/ m) layout-settings)])
                 (rectangle (image-width (second temp-image))
                            (image-height (second temp-image))
                            "solid" invisible))
               new-image))]

    ; transform renders specially
    [(/ [transform template] t/ target)
     ; TODO: bounds???
     (match-define (list new-fruct new-image)
       (render-transform (/ [transform template] t/ target)
                         layout-settings))
     (list new-fruct
           (if popout-transform?
               (let ([temp-image (render (/ t/ target) layout-settings)])
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
    [(/ [metavar m] a/ a)
     (define (metavar-tint-colors m layout-settings)
       (for/hash ([(k v) (hash-set* layout-settings
                                    ; hacky color overrides
                                    'pattern-grey-one (color 0 0 0)
                                    'grey-one bkg-color
                                    'grey-two background-block-color)])
         (match v
           [(color _ _ _ _)
            (values k ((per-color-linear-dodge-tint
                        (match m
                          [0 (color 0 255 255)]
                          [1 (color 0 255 0)]
                          [2 (color 255 0 255)]
                          [3 (color 255 255 0)]
                          [_ (color 0 255 0)])
                        0.4) v))]
           [_ (values k v)])))
     (match-define (list new-fruct new-img)
       (render (/ a/ a) (metavar-tint-colors m layout-settings)))
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
           (render id (hash-set layout-settings
                                ; todo: magic color
                                'identifier-color "white"))
           (render id layout-settings)))
     ; TODO: properly add position information here
     (define id-height (image-height id-image))
     (define id-width (image-width id-image))
     (define new-bounds
       `(((0  ,id-height))
         ((,id-width ,id-height))))
     ; hacky smaller radii for looks
     (define radius-adj (div-integer radius 7/5))
     (list (/ [bounds new-bounds] ref/ `(ref ,id-fruct))
           (if (selected? (/ ref/ `(ref ,id)))
               (overlay/align
                "left" "top"
                ; outline if selected
                #;(rounded-rectangle-outline
                   id-width id-height
                   radius-adj selected-color 2)
                ; layout goes inbetween
                id-image
                ; backing
                (rounded-rectangle
                 id-width id-height radius-adj
                 selected-color #;(if depth grey-one grey-two)))
               id-image))]

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
       (list (/ [bounds new-bounds] new-hole/ '⊙) hole-image)]
    
    [(/ id/ `(id ,xs ...))
     ; id itself is not drawn
     ; draw the letters xs with no spaces
     (define children
       (map (curryr render layout-settings) xs))
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
       (map (curryr render (if (selected? (/ n/ `(num ,xs ...)))
                               (hash-set* layout-settings
                                          ; todo: magic colors
                                          '+hole-color (color 130 0 0)
                                          'literal-color "white")
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
               radius-adj selected-color 2)
            ; layout goes inbetween
            between-image
            ; backing
            (rounded-rectangle
             (image-width between-image) (image-height between-image) radius-adj
             selected-color #;(if depth grey-one grey-two)))
           between-image))
     (list (/ [bounds new-bounds] n/ `(num ,@new-frs)) my-new-image)]

    [(and ps (/ (sort 'params) a/ a))
     #;(println `(params case activates))
     (define local-layout-settings
       (hash-set* layout-settings
                  ; TODO magic colors
                  'identifier-color pattern-bkg-color
                  ; HACK need to update when patterns move beyond chars
                  '+hole-color (color 170 170 170 120)
                  'grey-one pattern-grey-one
                  'grey-two pattern-grey-two))
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



(define (ellipses expander-height color)
  (beside (circle (div-integer expander-height 6) "solid" color)
          (rectangle 1 expander-height "solid" invisible)
          (circle (div-integer expander-height 6) "solid" color)
          (rectangle 1 expander-height "solid" invisible)
          (circle (div-integer expander-height 6) "solid" color)))


(define (space text-size typeface)
  (text/font " " text-size "black"
             typeface 'modern 'normal 'normal #f))


; hacky, rework this
(define (real-atomic? fruct)
  (or (atomic? fruct)
      (match fruct [(/ _/ `(ref ,_)) #t][_ #f])
      (match fruct [(/ _/ `(num ,_)) #t][_ #f])))



(define (render-symbol s my-color layout-settings)
  (define-from layout-settings
    typeface text-size char-padding-vertical unit-width unit-height
    hole-bottom-color hole-side-color)

  (overlay
   (cond
     [(equal? s '⊙+)
      (text/font "+"
                 (round (* 7/12 text-size))
                 my-color
                 typeface 'modern 'normal 'normal #f)
      #;(circle (* 1/15 text-size) "solid"
                (color 180 180 180))]
     [(equal? s '⊙)
      (define my-radius
        ; TODO: magic numbers
        ; TODO: render transform arrow, menu cursor in this way
        (min (* 3/11 text-size)
             (* 1/2 unit-width)))
      (overlay/align
       "right" "bottom"
       (circle (+ -0.5 (div-integer my-radius 29/26)) "solid"
               hole-bottom-color)
       (circle my-radius "solid"
               hole-side-color))
      #;(overlay
         (circle my-radius "outline"
                 (color 74 241 237))
         (circle my-radius "solid"
                 "white"))]
     [else
      (text/font (string-append (~a s))
                 ; HACK: hole char is slightly weird
                 ; at least on windows?
                 (if (equal? s '⊙)
                     (round (* 0.8 text-size))
                     text-size)
                 my-color
                 typeface #;#f
                 'modern 'normal 'normal #f)])
   ; padding
   (rectangle unit-width
              unit-height
              "solid" invisible)))


(define (layout-row intial-offset space-width pairs)
  (match-define `((,stx ,images) ...) pairs)
  (define spacer-image
    (rectangle space-width 1 "solid" invisible))
  (define new-image
    (apply beside/align* "top"
           (for/fold ([acc '()])
                     ([i images])
             `(,@acc ,spacer-image ,i))))
  (define-values (new-fruct final-offset)
    (for/fold ([output '()]
               [running-offset intial-offset])
              ([s stx] [i images])
      (match-define (list offset-x offset-y) running-offset)
      (define new-child
        (match s
          [(? (disjoin symbol? number?)) s]
          [(/ a/ a)
           (/ [display-offset (list offset-x offset-y)]
              [display-box (list (image-width i) (image-height i))]
              a/ a) ]))
      (define new-running-offset
        (list (+ offset-x space-width (image-width i)) offset-y))
      (values `(,@output ,new-child)
              new-running-offset)))
  (values new-fruct
          new-image
          final-offset))


(define (layout-column intial-offset line-spacing pairs)
  (match-define `((,stx ,images) ...) pairs)
  (define line-spacer-image
    (rectangle line-spacing line-spacing "solid" invisible))
  (define new-image
    (apply above/align* "left"
           (let ([temp
                  (for/fold ([output '()])
                            ([i images])
                    `(,@output ,i ,line-spacer-image))])
             (if (empty? temp)
                 temp
                 (drop-right temp 1)))))
  (define-values (new-fruct final-offset)
    (for/fold ([output '()]
               [running-offset intial-offset])
              ([s stx] [i images])
      (match-define (list offset-x offset-y) running-offset)
      (define new-child
        (match s
          [(? (disjoin symbol? number?)) s]
          [(/ a/ a)
           (/ [display-offset (list offset-x offset-y)]
              [display-box (list (image-width i)
                                 (image-height i))]
              a/ a) ]))
      (define new-running-offset
        (list offset-x
              (+ offset-y
                 line-spacing
                 (image-height i))))
      (values `(,@output ,new-child)
              new-running-offset)))
  (values new-fruct
          new-image
          final-offset))



(define (render-horizontal layout-settings children)
  (define-from layout-settings
    typeface text-size unit-width show-parens?)

  (define-values (new-children new-image _)
    (layout-row (list unit-width 0)
                unit-width
                children))

  ; experimental : show parentheses option
  ; misses a parens when we skip the space after a terminal non-atom
  (define newest-image
    (if show-parens?
        (overlay/align "left" "top"
                       (render-symbol "(" (color 255 255 255 90) layout-settings)
                       (overlay/align "right" "bottom"
                                      (render-symbol ")" (color 255 255 255 90) layout-settings)
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





(define (render-menu stx layout-settings)
  (define-from layout-settings
    text-size typeface max-menu-length max-menu-length-chars implicit-forms unit-width
    custom-menu-selector? line-spacing selected-color menu-bkg-color radius)
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
                      'force-horizontal-layout? #f
                      ; just turned this off for giggles
                      #;#;'form-color (color 255 255 255))]
          [_ (hash-set* layout-settings
                        'force-horizontal-layout? #t
                        'grey-one menu-bkg-color
                        'pattern-grey-one (color 76 76 76)
                        'form-color (color 255 255 255))])) ; magic color
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
          [(/ a/ `(,(or 'ref 'num #;(? (curryr member implicit-forms))) ,xs ...))
           image]
          [_ (overlay/align
              "left" "top"
              (match search-buffer
                [`(▹ ,(? string? s))
                 (render-symbol (string->symbol (string-append " " s))
                                selected-color layout-settings)]
                [x (render-symbol (string->symbol
                                   ; todo: once implemented ")" properly
                                   ; then make below work
                                   #;(substring (~a (strip▹ x)) 0 (+ -1 (string-length (~a (strip▹ x)))))
                                   (string-replace (~a (strip▹ x)) ")" " "))
                                  selected-color layout-settings)]
                [_ (error "complex search buffer layout not implemented")])
              
              image)]))
      (cond
        [custom-menu-selector?
         (match item
           [(/ b/ (▹ b))
            (list (first (render (/ b/ (▹ b)) override-layout-settings))
                  ; HACK, BUG: get (wrong) positioning data
                  ; also special-cases char menus
                  (overlay-search-buffer
                   (/ b/ (▹ b))
                   (if (single-char-menu? resultants)
                       ; HACK: color change... is it even working?
                       (second (render (/ b/ b) (hash-set override-layout-settings
                                                          'identifier-color "white")))
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
                                                (second (render (/ b/ b) (hash-set override-layout-settings
                                                                                   'identifier-color "white")))
                                                (space text-size typeface))])
                              (overlay (rounded-rectangle-outline
                                        (image-width temp)
                                        ; slightly hacky adjustment
                                        ; to make outline entirely inside line-height
                                        (max 0 (- (image-height temp) 1))
                                        radius
                                        selected-color 2)
                                       temp))
                            (let ([temp (second (render (/ b/ b) override-layout-settings))])
                              (overlay (rounded-rectangle-outline
                                        (image-width temp)
                                        ; slightly hacky see above
                                        (max 0 (- (image-height temp) 1))
                                        radius
                                        selected-color 2)
                                       temp)))))))]
           [(/ b/ b)
            (list (first (render item override-layout-settings))
                  (overlay-search-buffer
                   (/ b/ b)
                   (if (or (not (list? b)) (or (and (member 'ref implicit-forms)
                                                    (match b [`(ref ,_) #t][_ #f]))
                                               (and (member 'num implicit-forms)
                                                    (match b [`(num ,_) #t][_ #f]))))
                       (beside (space text-size typeface)
                               (second (render item (hash-set override-layout-settings
                                                              'identifier-color "white")))
                               (space text-size typeface))
                       ; below is call that should have tint when metavar 666
                       (second (render item override-layout-settings)))))])]
        [else (render item override-layout-settings)])))


  (define-values (truncated-menu-fruct
                  truncated-menu-image _)
    (layout-column '(0 0) line-spacing fruct-image-pairs))
  

  ; magic number, colors
  (define expander-height
    (round (* 1/4 text-size)))
  (define expander-ellipses-color
    (color 200 200 200))

  ; stylish backing
  (define cool-menu
    (overlay
     (above (ellipses expander-height expander-ellipses-color)
            truncated-menu-image
            (ellipses expander-height expander-ellipses-color))
     (rounded-rectangle
      (image-width truncated-menu-image)
      (image-height truncated-menu-image)
      radius
      menu-bkg-color)
     (rounded-rectangle
      (+ 0 (image-width truncated-menu-image))
      (+ (* 2 expander-height)
         (image-height truncated-menu-image))
      radius
      (color 125 125 125)))) ; magic color

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
        cool-menu))







(define (render-transform fruct layout-settings)
  (define-from layout-settings
    text-size typeface selected-color
    dodge-enabled? transform-tint-color radius)
  
  (match-define (/ [transform template] t/ target) fruct)

  (match-define (list target-fruct target-image)
    (render (/ t/ target) layout-settings))
  
  (match-define (/ new-t/ new-target)
    (match target-fruct
      [(? (disjoin symbol? number?)) template]
      [(/ a/ a)
       (/ [display-offset (list 0 0)]
          [display-box (list (image-width target-image)
                             (image-height target-image))]
          a/ a) ]))
  
  (define arrow-image
    (second (render '→ layout-settings)))

  (match-define (list template-fruct template-image)
    (if dodge-enabled?
        ; debtably a hack:
        ; apply a color tint to every color in layout-settings
        (render template
                (for/hash ([(k v) layout-settings])
                  (match v
                    [(color _ _ _ _)
                     (values k ((per-color-linear-dodge-tint
                                 transform-tint-color 0.5) v))]
                    [_ (values k v)])))
        (render template layout-settings)))
 
  (define new-template
    (match template-fruct
      [(? (disjoin symbol? number?)) template]
      [(/ a/ a)
       (/ [display-offset
           (list (apply + (map image-width
                               (list target-image
                                     (space text-size typeface)
                                     arrow-image
                                     (space text-size typeface))))
                 0)]
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
         radius selected-color "outline" 2 #f)))

  (define template-backing
    (rounded-backing
     (second template-bounds)
     (first template-bounds)
     radius selected-color "outline" 2 #f))

  (define transform-backing
    (make-transform-backing
     target-bounds template-bounds layout-settings))
  
  (define new-image
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
     transform-backing))
  
  (list (/ [transform new-template] new-t/ new-target)
        new-image))


(define (make-transform-backing target-bounds template-bounds
                                layout-settings)
  (define-from layout-settings
    unit-width unit-height radius selected-color)
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
             radius selected-color "solid" 0 #f)
            ; todo: refactor rounded-backing
            ; to avoid double-call here
            (rounded-backing
             left-backing-right-bounds
             left-backing-left-bounds
             radius selected-color "outline" 2 #f))
   (+ (* 1 unit-width) max-target-width) 0

   (if (>= template-target-height-diff 0)
       empty-image
       (rounded-backing
        right-backing-right-bounds
        right-backing-left-bounds
        radius selected-color "solid" 0 #f))))


(define (render-atom a selected? layout-settings)
  
  (match-define (/ s/ s) a)
  (define-from layout-settings
    selected-color literal-color
    form-color +hole-color transform-arrow-color 
    identifier-color selected-atom-color
    radius unit-width)
  (define literal? (disjoin boolean? number? string?))
  
  (define candidate
    (render-symbol
     (match (/ s/ s) [(or (/ (sort 'char) _/ '⊙)
                          (/ (sort 'digit) _/ '⊙)
                          (/ _/ '⊙+))
                      '⊙+] [_ s])
     (if selected?
         (if (form-id? s)
             selected-color
             selected-atom-color)
         (cond
           [(equal? s '→) transform-arrow-color]
           [(equal? s '⊙) +hole-color]
           [(equal? s '⊙+) +hole-color]
           [(equal? s '+) +hole-color]
           [(literal? s) literal-color]
           [(form-id? s) form-color]
           [(equal? s '▹) selected-color]
           [else identifier-color]))
     layout-settings))
  
  (define new-image
    (cond
      [(and selected? (not (form-id? s)))
       (overlay
        candidate
        (rounded-rectangle
         (image-width candidate)
         (image-height candidate)
         radius
         selected-color))]
      [else candidate]))
  
  (define new-bounds
    `(((0 ,(image-height new-image)))
      ((,(image-width new-image) ,(image-height new-image)))))
  
  (list (/ [bounds new-bounds] s/ s)
        new-image))




(define (render-list fruct depth bkg init-layout-settings selected?)
  (define-from init-layout-settings
    radius
    selected-color unit-width implicit-forms
    length-conditional-layout? length-conditional-cutoff
    force-horizontal-layout? show-parens?)
  
  (match-define (/ a/ `(,first-stx ,rest-stx ...)) fruct)

  (define layout-settings
    (if selected?
        (hash-set* init-layout-settings
                   'grey-one (color 230 230 230)
                   'grey-two (color 215 215 215)
                   )
        init-layout-settings))

  ; figure this out here in case we truncate the names below
  (define if-like? (if-like-id? first-stx))
  (define lambda-like? (lambda-like-id? first-stx))
  (define cond-like? (cond-like-id? first-stx))
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
      (if (and selected? (form-id? s))
          ; if selected highlight form-name; hacky
          ; todo: refactor render-symbol to dehack
          `(,(first (render s layout-settings (not depth) bkg))
            ,(render-symbol s selected-color layout-settings))
          (render s layout-settings (not depth) bkg)))) 
  
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
               (not ends-in-atom?))
           ; omit trailing space
           (add-horizontal-backing 
            (/ a/ new-kids-local) new-layout-local #f selected? depth layout-settings)
           (add-horizontal-backing 
            (/ a/ new-kids-local) new-layout-local #t selected? depth layout-settings))]

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
                         (render-symbol "(" (color 255 255 255 90) layout-settings)
                         (overlay/align "right" "bottom"
                                        (render-symbol ")" (color 255 255 255 90) layout-settings)
                                        newer-image))
          newer-image))
  )



(define (add-vertical-backing fruct init-children layout-settings
                              indent num-header-items
                              new-layout-local selected? depth
                              straight-left? header-exception?)
  (define-from layout-settings
    unit-width radius
    selected-color grey-one grey-two bkg-color background-block-color)
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
                              ((,(image-width img) ,(image-height img))))]))
  
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
                   (if (and selected? (symbol? (first (first header-children))))
                       (begin
                         ; SUPER HACKY way of changing form symbol color
                         ; only works if symbol is first thing in form
                         (overlay/align
                          "left" "top"
                          (beside (render-symbol '| |
                                                 "white" ; todo: magic colors
                                                 layout-settings)
                                  (render-symbol (first (first header-children))
                                                 "white"
                                                 layout-settings))
                          new-layout-local))
                       new-layout-local)
                   (if selected?
                       empty-image
                       (if depth
                           (overlay
                            ; todo: rewfactor rounded-backing to avoid double-call
                            (rounded-backing
                             final-right-bounds
                             final-left-bounds
                             radius background-block-color "outline" 1
                             header-exception?)
                            (rounded-backing
                             final-right-bounds
                             final-left-bounds
                             radius bkg-color "solid" 2
                             header-exception?))
                           (rounded-backing
                            final-right-bounds
                            final-left-bounds
                            radius background-block-color "solid" 2
                            header-exception?)))))
  
  (define possibly-selected-image
    (if selected?
        (overlay/align "left" "top"
                       (rounded-backing
                        final-right-bounds
                        final-left-bounds
                        radius selected-color "outline" 2
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





(define (add-horizontal-backing whole-stx new-layout rear-padded? selected? depth layout-settings)
  (define-from layout-settings
    text-size typeface radius unit-width unit-height
    selected-color background-block-color bkg-color grey-one grey-two)
  (define width
    (+ (image-width new-layout)
       (if rear-padded? unit-width 0)))
  (define height
    (image-height new-layout))
  (define new-image
    (overlay/align
     "left" "top"
     ; outline if selected
     (if selected?
         (rounded-rectangle-outline width height radius selected-color 2)
         ; todo: create arg here for width
         empty-image)
     ; layout goes inbetween
     new-layout
     ; backing
     (rounded-rectangle width height radius
                        (if depth grey-one grey-two))
     #;(if #t #;selected?
           (rounded-rectangle width height radius
                              (if depth grey-one grey-two))
           (overlay (rounded-rectangle-outline
                     width height radius
                     background-block-color 1)
                    (rounded-rectangle
                     width height radius
                     (if depth background-block-color bkg-color))))))
  
  (define new-bounds
    `(((0 ,height)) ((,width ,height))))
  
  (list (match whole-stx
          [(/ a/ a) (/ [bounds new-bounds] a/ a)])
        new-image))





(define (augment-absolute-offsets fruct
                                  (running-offset (list 0 0)))
  (match-define `(,offset-x ,offset-y) running-offset)
  (match fruct

    [(/ [display-box `(,width ,height)]
        [display-offset `(,x ,y)]
        a/ as)

     (define new-offset
       `(,(+ x offset-x),(+ y offset-y)))
     
     (define new-as
       (if (list? as)
           (for/list ([a as])
             (augment-absolute-offsets a new-offset))
           as))

     (match (/ [display-box `(,width ,height)]
               [display-offset `(,x ,y)]
               [display-absolute-offset new-offset]
               a/ as)

       ; special case for menus
       [(/ [menu ms] local-a/ _)
        (define augmented-menu
          (for/list ([m ms])
            (match m
              [`(,t ,r)
               `(,t ,(augment-absolute-offsets r new-offset))])))
        (/ [menu augmented-menu]
           local-a/ new-as)]

       ; special case for transforms
       [(/ [transform template] local-a/ _)
        (/ [transform (augment-absolute-offsets template new-offset)]
           local-a/ new-as)]

       [(/ local-a/ _)
        (/ local-a/ new-as)])]

    ; menu items which aren't drawn
    [(/ a/ a) (/ a/ a)] 
    [_ fruct]))



(define (draw-outlines-abs fruct (running-scene empty-image))
  (match fruct

    [(/ [display-box `(,width ,height)]
        [display-absolute-offset `(,x ,y)]
        a/ as)
     
     (define new-scene
       (place-image/align
        (rectangle width height "outline" "white")
        x y "left" "top" running-scene))

     (match fruct
       [(/ [menu `((,transforms ,resultants) ...)] a/ as)
        ; the way i have to process menus feels hacky...
        (for/fold ([scene new-scene])
                  ([r resultants])
          (draw-outlines-abs r scene))]
       [(/ [transform template]  a/ as)
        (draw-outlines-abs template new-scene)]
       [(/ a/ (? list? as))
        (for/fold ([scene new-scene])
                  ([a as])
          (draw-outlines-abs a scene))]
       [(/ a/ a) new-scene])]
    
    [(/  a/ a)
     (println `(warning: draw-outlines-unhandled-fruct ,(/  a/ a)))
     ; this case currently occurs for menu items which don't get
     ; drawn due to cutoffs/cursor position
     running-scene]
    [_
     ; should just be bare form-header symbols
     running-scene]))





#;(second
   (render
    data-0
    test-settings))

#;(second
   (render
    data-1
    test-settings))

#;(second
   (render
    data-2
    test-settings))

; THIS IS THE ONE WE WERE TESTING UNITL 2019.jan31
#;(second
   (render
    data-3
    test-settings))

#;(second
   (render
    data-4
    test-settings))

#;(second
   (render
    data-5
    test-settings))

#;(second
   (render
    (match data-6
      [(/ a/ a)
       (/ [display-box `(800 240)]
          [display-offset `(0 0)]
          a/ a)])
    test-settings))

#;(draw-outlines-abs
   (augment-absolute-offsets
    (first (render (match data-6
                     [(/ a/ a)
                      (/ [display-box `(800 240)]
                         [display-offset `(0 0)]
                         a/ a)])
                   test-settings))))

#;(second
   (fructure-layout data-7 test-settings))

(draw-outlines-abs (first (fructure-layout data-8 test-settings))
                   (second (fructure-layout data-8 test-settings)))


(second
 (fructure-layout data-8 test-settings))

(second
 (fructure-layout data-9 test-settings))

(second
 (fructure-layout data-10 test-settings))

(second
 (render data-11 test-settings))

(second
 (render data-12 test-settings))

(second
 (fructure-layout data-13 test-settings))

(second
 (fructure-layout data-14 test-settings))

(second
 (fructure-layout data-15 test-settings))

(second
 (fructure-layout data-16 test-settings))

(second
 (fructure-layout transforming-λ-dog-dog-dog-to-hole
                  test-settings))

(second
 (fructure-layout transforming-three-liner-to-hole
                  test-settings))

(second
 (fructure-layout transforming-3-line-to-4-line
                  test-settings))

(second
 (fructure-layout transforming-hole-to-2-line
                  test-settings))


         
; temp work on search

(define (my-matches? prefix-string form-name)
  (regexp-match (regexp (string-append "^" prefix-string ".*"))
                (symbol->string form-name)))

(define (add-hooks prefix-string fruct)
  (define AH (curry add-hooks prefix-string))
  (match fruct
    [(/ a/ `(,(? (conjoin symbol? (curry my-matches? prefix-string)) form-name) ,xs ...))
     (/ [hook prefix-string] a/ (map AH `(,form-name ,@xs)))]
    [(/ a/ (? list? as))
     (/ a/ (map AH as))]
    [(/ a/ (? symbol? thing))
     (if (my-matches? prefix-string thing)
         (/ [hook prefix-string] a/ thing)
         (/ a/ thing))]
    [_ fruct]))

#;(add-hooks "x" (stx->fruct
                  '(lambda (x)
                     (and x (▹ (and true false)))
                     x)))


