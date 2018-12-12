#lang racket

(require lang/posn)
(require 2htdp/image)
(require rackunit
         "layout-tests.rkt"
         "new-syntax.rkt"
         "language.rkt"
         "common.rkt"
         "../containment-patterns/containment-patterns.rkt"
         "common-graphics.rkt")

; bug if true (c)
(provide fructure-layout)

; for tests
(provide render
         test-settings
         augment-absolute-offsets)

(define (div-integer x y)
  (inexact->exact (round (div x y))))


; TODO: rework as default settings?
(define test-settings
  (hash 'text-size 30
        'max-menu-length 3
        'max-menu-length-chars 1
        'popout-transform? #t
        'popout-menu? #t
        'custom-menu-selector? #t
        'force-horizontal-layout? #f
        'length-conditional-layout? #t
        'length-conditional-cutoff 14
        'dodge-enabled? #t
        'implicit-forms '(ref app)
        'line-spacing 1 ; 1
        'char-padding-vertical 4 ; 5

        'selected-atom-color (color 255 255 255)
        'menu-bkg-color (color 112 112 112)
        'form-color (color 0 130 214)
        'literal-color (color 255 131 50)
        'grey-one (color 230 230 230)
        'grey-two (color 215 215 215)
        'pattern-grey-one (color 84 84 84)
        'identifier-color (color 0 0 0)
        'selected-color (color 230 0 0)
        'hole-color (color 0 180 140)
        'transform-arrow-color (color 255 255 255)
        'bkg-color (color 0 47 54)
        'pattern-bkg-color (color 230 230 230)
        'pattern-grey-one (color 76 76 76)
        'pattern-grey-two (color 110 110 110)

        #;#;'radius (λ (text-size) (sub1 (div-integer text-size 2)))
        #;#;'margin (λ (text-size) (div-integer text-size 5))
        ))

; fructure-layout : syntax -> pixels
(define (fructure-layout fruct layout-settings (screen-x 800) (screen-y 400))
  (define-from layout-settings
    bkg-color text-size popout-transform? popout-menu?)
  (define margin (div-integer text-size 5))
  
  ; sanity checks
  (match fruct
    [`(◇ ,x) (error "strip top before calling")] [_ 0])

  (match-define `(,x-offset ,y-offset) `(,text-size ,text-size))

  (match-define `(,new-fruct ,scene-image)
    (render
     (match fruct
       [(/ fr/ stx)
        (/ [display-offset `(,x-offset ,y-offset)]
           [display-box `(,screen-x ,screen-y)]
           fr/ stx)])
     layout-settings))

  ; calculate absolute positioning from relative
  (define newest-fruct
    (augment-absolute-offsets
     new-fruct))

  
  (define new-image
    (overlay/align/offset "left" "top"
                          scene-image
                          (- x-offset) (- y-offset)
                          (rectangle screen-x screen-y "solid" bkg-color)))

  ; overlay transform if in popout mode
  (define post-procd-image
    (cond
      [popout-transform?
       (match newest-fruct
         
         ; HACK: case copied from below but special-cased
         ; to stop TOP-LEVEL transforms from losing the
         ; initial pane offset
         [(and this-transform
               (/ [transform _]
                  [display-absolute-offset `(,x ,y)]
                  t/ _))
          (match-define (list transform-fr transform-image)
            (render-transform this-transform #t layout-settings))
          (place-image/align transform-image
                             (+ x-offset (- x margin))
                             (+ y-offset y) "left" "top" new-image)]
         
         [(⋱ c⋱ (and this-transform
                     (/ [transform _]
                        [display-absolute-offset `(,x ,y)]
                        t/ _)))
          (match-define (list transform-fr transform-image)
            (render-transform this-transform #t layout-settings))
          (place-image/align transform-image
                             (- x margin)
                             y "left" "top" new-image)]
         [_ new-image])]
      [else new-image]))


  ; overlay menu if in popout mode
  (define post-post-procd-image
    (cond
      [popout-menu?
       (define expander-height
         (round (* 1/4 text-size))) ; magic af
       (match newest-fruct

         ; HACK, see above
         [(/ [transform
              (⋱ d⋱ (and this-menu
                         (/ [menu _]
                            [display-absolute-offset `(,x ,y)]
                            m/ _)))] t/ _)
          (match-define (list menu-fr menu-image)
            (render-menu this-menu layout-settings))
          ;todo: factor out to property
          (place-image/align menu-image
                             (+ x x-offset (- (+ (* 2 margin)))) ;only slightly magic
                             (+ y y-offset (- expander-height)) "left" "top"
                             post-procd-image)]
         
         [(⋱ c⋱ (/ [transform
                    (⋱ d⋱ (and this-menu
                               (/ [menu _]
                                  [display-absolute-offset `(,x ,y)]
                                  m/ _)))] t/ _))
          (match-define (list menu-fr menu-image)
            (render-menu this-menu layout-settings))
          (place-image/align menu-image
                             (+ x (- (* 2 margin))) ;only slightly magic
                             (+ y (- expander-height)) "left" "top"
                             post-procd-image)]
         [_ post-procd-image])]
      [else post-procd-image]))  

  
  (list newest-fruct
        post-post-procd-image))






(define (render fruct layout-settings (depth #t) (bkg 0))
  (define-from layout-settings
    text-size selected-color hole-color
    popout-transform? popout-menu? implicit-forms
    grey-one grey-two pattern-bkg-color
    pattern-grey-one pattern-grey-two)
    
  (match fruct

    [(and this-menu
          (/ [menu `((,transforms ,resultants) ...)] m/ m))
     (match-define (list fruct-with-positions new-image)
       (render-menu this-menu
                    layout-settings))
     (list fruct-with-positions
           (if popout-menu?
               ; whatever is supposed to be there as placeholder
               (let ([temp-image (render '? #;(/ m/ m) layout-settings)])
                 (rectangle (image-width (second temp-image))
                            (image-height (second temp-image))
                            "solid" invisible))
               new-image))]
    
    [(/ [transform template] t/ target)
     (match-define (list new-fruct new-image)
       (render-transform (/ [transform template] t/ target)
                         depth layout-settings))
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
       (for/hash ([(k v) layout-settings])
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
     (list
      ; hack
      ; this took forever to figure out
      ; otherwise this is taking the metavar attribute off
      ; so when we try to render the item in a menu...
      ; actually what exactly is going on?
      ; in any case, we need to make sure we're not loosing attributes..
      (/ [metavar m] a/ a)
      (second (render (/ a/ a) (metavar-tint-colors m layout-settings))))]
     
    
    [(/ ref/ `(ref ,id))
     #:when (member 'ref implicit-forms)
     ; bug: this is losing location information
     ; reproduce: try to transform a letter in a reference
     ; selected highlight is... not great
     ; need to make the selector overlaid so can
     ; draw this like parens, but space it out a bit
     ; it's too tight as-is
     (define margin (div-integer text-size 5))
     (define radius (sub1 (div-integer text-size 2)))
     (match-define (list id-fruct id-image)
       (if (selected? (/ ref/ `(ref ,id)))
           (render id layout-settings)
           (render id layout-settings)))
     ; need to add position information here
     (list (/ ref/ `(ref ,id-fruct))
           (if (selected? (/ ref/ `(ref ,id)))
               (overlay id-image
                        #;(rounded-rectangle
                           (image-width id-image)
                           (image-height id-image)
                           radius
                           selected-color)
                        (overlay
                         (rounded-rectangle
                          (+ (- 0 margin) (image-width id-image))
                          (image-height id-image)
                          radius
                          ; hacky: skip depth level
                          (if (not depth) grey-one grey-two))
                         (rounded-rectangle
                          (image-width id-image)
                          (image-height id-image)
                          radius
                          selected-color))
                        )
               id-image))]
    
    [(/ id/ `(id ,xs ...))
     ; id itself is not drawn
     ; draw the letters xs with no spaces
     (define children
       (map (curryr render layout-settings) xs))
     (define-values (new-frs my-new-image _)
       (layout-row (list 0 0) 0 children))
     (list (/ id/ `(id ,@new-frs)) my-new-image)]
    
    [(/ a/ (? list? a))
     (define local-layout-settings
       (match (/ a/ a)
         [(/ (sort 'params) _/ _)
          (hash-set* layout-settings
                     ; TODO magic colors
                     'identifier-color pattern-bkg-color
                     ; hack, need to update when patterns move beyond chars
                     'hole-color (color 170 170 170 120)
                     'grey-one pattern-grey-one
                     'grey-two pattern-grey-two)]
         [_ layout-settings]))
     (render-list (/ a/ a)
                  ; hack to reset depth for params-list
                  (match (/ a/ a)
                    [(/ (sort 'params) _/ _) #t] [_ depth])
                  bkg local-layout-settings (selected? fruct))]

    [(/ a/ a)
     (render-atom (/ a/ a) (selected? fruct) layout-settings)]
    
    [(? (disjoin form-id? symbol?) a)
     ; TODO: MAGIC symbol include, investigate this
     ; this case SHOULD be only for form headers
     #; (println `(fallthru ,a))
     ; but actually catching at least:
     #; (? → ▹ λ app)
     ; TODO: factor out all these cases
     ; slight hack; adding blank attributes to a
     ; WARNING: DO NOT REPLACE THE LIST WITH JUST THE RENDER CALL
     ; form-ids will get wrapped and layout will get screwy
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


(define (space text-size)
  (text/font " " text-size "black"
             #f 'modern 'normal 'normal #f))
  

(define (get-atomic-children stx child-images)  
  (foldl (λ (x y acc)
           (if (or (atomic? x)
                   (match x [(/ _/ `(ref ,_)) #t][_ #f]))
               (cons y acc)
               acc))
         '() stx child-images))


(define (get-non-atomic-children stx child-images)
  (foldl (λ (x y acc)
           (if (not (or (atomic? x)
                        (match x [(/ _/ `(ref ,_)) #t][_ #f])))
               (cons y acc)
               acc))
         '() stx child-images))


(define (shortest img-list)
  (if (empty? img-list)
      0
      (apply min (map image-width img-list))))


(define (longest img-list)
  (if (empty? img-list)
      0
      (apply max (map image-width img-list))))





(define (render-symbol s my-color layout-settings)
  (define-from layout-settings
    text-size char-padding-vertical)

  ; todo: factor out
  (define unit-width (image-width (space text-size)))
  (define unit-height
    (+ char-padding-vertical
       (image-height (space text-size))))

  (overlay
   (cond
     [(equal? s '⊙)
      (define my-radius
        ; TODO: magic numbers
        ; TODO: render transform arrow, menu cursor in this way
        ; TODO (aside): try red outline instead of arrow for menu selection
        (min (* 3/11 text-size)
             (* 1/2 unit-width))) 
      (overlay
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
                 #f 'modern 'normal 'normal #f)])
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
  (define-from layout-settings text-size)
  (define unit-width (image-width (space text-size)))

  (define-values (new-children new-image _)
    (layout-row (list unit-width 0)
                unit-width
                children))

  (list new-children
        ; note special case
        ; if last child is atom, we leave a space on the right
        (beside new-image
                (match (first (last children))
                  ; feels slightly hacky
                  ; should bellow be single 'pattern case?
                  [(/ _/ `(,(not (or 'id 'ref)) ,xs ...)) empty-image]
                  [_ (space text-size)]))))



(define (render-vertical indent-width header-items layout-settings children)
  (define-from layout-settings
    text-size line-spacing char-padding-vertical)

  ; todo: factor out
  (define unit-width (image-width (space text-size)))
  (define unit-height
    (+ char-padding-vertical
       (image-height (space text-size))))
  
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
    text-size max-menu-length max-menu-length-chars implicit-forms
    custom-menu-selector? selected-color menu-bkg-color)
  (define radius (sub1 (div-integer text-size 2)))
  (define margin (div-integer text-size 5))
  (match-define (/ [menu `((,transforms ,resultants) ...)] p/ place) stx)

  #| this function currently renders ALL menu items
     not just the ones displated.
     tagged: performance, optimization |#

  (define char-menu?
    (match resultants
      [`(,(/ (sort 'char) _/ _) ...) #t] [_ #f]))

  (define local-max-menu-length
    (if char-menu?
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
  
  (match-define `((,child-fructs ,child-images) ...)
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
                      'force-horizontal-layout? #t
                      #;#;'form-color (color 255 255 255))]
          [_ (hash-set* layout-settings
                        'force-horizontal-layout? #t
                        'grey-one menu-bkg-color
                        'form-color (color 255 255 255))])) ; magic color
      (cond
        [custom-menu-selector?
         (match item
           [(/ b/ (▹ b))
            (list (first (render (/ b/ (▹ b)) override-layout-settings))
                  ; HACK, BUG: get (wrong) positioning data
                  ; also special-cases char menus
                  (if char-menu?
                      ; hacky color change... is it even working?
                      ; it's interpreting chars as identifiers for some reason?
                      (second (render (/ b/ b) (hash-set override-layout-settings
                                                         'identifier-color "white")))
                      (overlay/align "left" "top"
                                     (space text-size)
                                     #;(second (render '▹ override-layout-settings))
                                     (if (or (not (list? b)) (and (member 'ref implicit-forms)
                                                                  (match b [`(ref ,_) #t][_ #f])))
                                         ; hacky extra spacing for atoms
                                         ; extra hacky for implicit refs
                                         ; HACK below throws off offset alignment
                                         ; need to refacto as popped layer
                                         (let ([temp (beside (space text-size)
                                                             (second (render (/ b/ b) (hash-set override-layout-settings
                                                                                                'identifier-color "white")))
                                                             (space text-size))])
                                           (overlay temp
                                                    (rounded-rectangle
                                                     (+ 2 (image-width temp))
                                                     (+ 2 (image-height temp))
                                                     radius
                                                     selected-color)))
                                         (let ([temp (second (render (/ b/ b) override-layout-settings))])
                                           (overlay temp
                                                    (rounded-rectangle
                                                     (+ 2 (image-width temp))
                                                     (+ 2 (image-height temp))
                                                     radius
                                                     selected-color)))))))]
           [(/ b/ b)
            (list (first (render item override-layout-settings))
                  (if (or (not (list? b)) (and (member 'ref implicit-forms)
                                               (match b [`(ref ,_) #t][_ #f])))
                      ; hacky extra spacing for atoms
                      ; extra hacky for implicit refs
                      (beside (space text-size)
                              (second (render item (hash-set override-layout-settings
                                                             'identifier-color "white")))
                              (space text-size))
                      ; below is call that should have tint when metavar 666
                      (second (render item override-layout-settings))))])]
        [else (render item override-layout-settings)])))
  
  (define truncated-menu-image
    (apply above/align* "left"
           child-images))

  ; magic number, colors
  (define expander-height
    (round (* 1/4 text-size)))
  (define expander-color
    (color 125 125 125))
  (define expander-ellipses-color
    (color 200 200 200))

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
      (color 125 125 125))))
  
  (define new-image
    ; if char menu, supress drawing of left-side
    ; highlight 'cause jankiness.
    (if char-menu?
        (overlay/align/offset
         "left" "top"
         cool-menu
         (- (* 2 margin)) 0 
         empty-image)
        (overlay/align/offset
         "right" "top"
         cool-menu
         (- (* 2 margin)) 0
         ; lets skip out on adornment while we get the basics down
         ; makes offset more complicated
         ; espc forces special-casing of offset for char menu
         #;#;(- (div-integer margin 2)) 0 ; slighly magic number
         empty-image
         #;(rounded-rectangle
            (+ 0 (image-width truncated-menu-image))
            (+ (* 2 expander-height)
               (image-height truncated-menu-image))
            radius
            selected-color))))

  ; calculate REAL position data
  (define-values (post-new-fruct _)
    (for/fold ([output '()]
               [running-offset `(,(- (* 2 margin)) 0)])
              ; BUG, HACK: dubious initial offset above!
              ; alignment not quite right AND conceptually suspect
              ([s child-fructs] [i child-images])
      (match-define (list offset-x offset-y) running-offset)
      (define new-child
        (match s
          [(? (disjoin symbol? number?)) s]
          [(/ a/ a)
           (/ [display-offset (list offset-x offset-y)]
              [display-box (list (image-width i) (image-height i))]
              a/ a) ]))
      (define new-running-offset
        (list offset-x
              (+ offset-y
                 0 #;line-spacing
                 (image-height i))))
      (values `(,@output ,new-child)
              new-running-offset)))
  
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
    (append post-new-fruct
            (list-tail resultants-wraparound
                       (length post-new-fruct))))
  
  (define new-fruct
    (append (list-tail replace-in-wrapparound num-items-from-cursor-to-end)
            (take replace-in-wrapparound num-items-from-cursor-to-end)))

  
  
  
  (list (/ [menu `(,@(map list transforms new-fruct))] p/ place)
        new-image))







(define (render-transform fruct depth layout-settings)
  (define-from layout-settings
    text-size grey-one grey-two selected-color
    dodge-enabled?)
  (define radius (sub1 (div-integer text-size 2)))
  (define margin (div-integer text-size 5))
  (match-define (/ [transform template] t/ target) fruct)
  (define pat-tem-bkg-color
    (if depth grey-one grey-two))
  
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
                                 selected-color 0.5) v))]
                    [_ (values k v)])))
        (render template layout-settings)))
 
  (define new-template
    (match template-fruct
      [(? (disjoin symbol? number?)) template]
      [(/ a/ a)
       (/ [display-offset (list (apply + (map image-width
                                              (list target-image
                                                    (space text-size)
                                                    arrow-image
                                                    (space text-size))))
                                0)]
          [display-box (list (image-width template-image)
                             (image-height template-image))]
          a/ a) ]))

  (define new-layout
    (beside/align
     "top"
     (overlay/align "right" "top"
                    target-image
                    (rounded-rectangle
                     (+ margin (image-width target-image))
                     (image-height target-image)
                     radius
                     selected-color))
     (space text-size)
     arrow-image 
     (space text-size)
     (overlay/align "left" "top"
                    template-image
                    (rounded-rectangle
                     (+ margin (image-width template-image))
                     (image-height template-image)
                     radius
                     selected-color))))
  
  (define new-backing
    (rounded-rectangle
     (image-width new-layout)
     (min (image-height target-image)
          (image-height template-image))
     radius
     selected-color))  
  (define new-image
    (overlay/align "middle" "top"
                   new-layout
                   new-backing))
  
  (list (/ [transform new-template] new-t/ new-target)
        new-image))


(define (render-atom a selected? layout-settings)
  
  (match-define (/ s/ s) a)
  (define-from layout-settings
    text-size selected-color literal-color
    form-color hole-color transform-arrow-color 
    identifier-color selected-atom-color)
  (define radius (sub1 (div-integer text-size 2)))
  (define margin (div-integer text-size 5))
  (define literal? (disjoin boolean? number? string?))
  
  (define candidate
    (render-symbol
     (match (/ s/ s) [(or (/ (sort 'char) _/ '⊙)
                          (/ _/ '⊙+)) '+] [_ s])
     (if selected?
         (if (form-id? s)
             selected-color
             selected-atom-color)
         (cond
           [(equal? s '→) transform-arrow-color]
           [(equal? s '⊙) hole-color]
           [(equal? s '⊙+) hole-color]
           [(equal? s '+) hole-color]
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
  
  (list (/ s/ s)
        new-image))





(define (render-list fruct depth bkg layout-settings selected?)
  (define-from layout-settings
    selected-color text-size implicit-forms
    length-conditional-layout? length-conditional-cutoff
    force-horizontal-layout?)
  (define unit-width (image-width (space text-size)))
  
  (match-define (/ a/ `(,first-stx ,rest-stx ...)) fruct)

  ; figure this out here in case we truncate the names below
  (define if-like? (if-like-id? first-stx))
  (define lambda-like? (lambda-like-id? first-stx))
  (define cond-like? (cond-like-id? first-stx))

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
            (define total-length (div-integer (apply + (map image-width child-images))
                                              unit-width))
            (total-length . < . length-conditional-cutoff)]))

  ; choose an algorithm to layout the children
  (define layout-renderer
    (cond
      [(and lambda-like? (not render-this-horizontally?))
       (curry render-vertical (* 2 unit-width) 2)]         
      [(and if-like? (not render-this-horizontally?)
            (member first-stx implicit-forms))
       ; if the form is implicit and we're rendering it vertically
       ; the header is just child 1, and the indent-width is space-width
       (curry render-vertical unit-width 1)]
      [(and if-like? (not render-this-horizontally?))
       ; indent length is 2 plus the length of the form name
       (curry render-vertical
              (+ (* 2 unit-width)
                 (image-width (second (first children))))
              2)]
      [(and cond-like? (not render-this-horizontally?))
       (curry render-vertical unit-width 1)]
      [else render-horizontal]))

  ; choose an algorithm to back the layout
  (define backer
    (cond
      [render-this-horizontally?
       (λ (x) (add-horizontal-backing x selected? depth layout-settings))]
      [else
       (λ (x) (add-vertical-backing possibly-truncated-stx selected? depth
                                    children x layout-settings))]))

  (match-define (list new-kids-local new-layout-local)
    (layout-renderer layout-settings children))

  ; possible BUG
  ; put back in the implicit form
  ; dunno if this is working properly and or necessary at all
  (list (/ a/ (if (member first-stx implicit-forms)
                  (cons first-stx new-kids-local)
                  new-kids-local))
        (backer new-layout-local)))



(define (render-vertical-backing stx new-layout depth child-images layout-settings)
  (define-from layout-settings
    text-size grey-one grey-two implicit-forms
    line-spacing char-padding-vertical)

  ; todo: factor out
  (define radius (sub1 (div-integer text-size 2)))
  (define unit-height (+ char-padding-vertical (image-height (space text-size))))
  (define unit-width (image-width (space text-size)))

  (define (calculate-height effective-stx effective-child-images)
    (apply +
           (if (not (empty? (get-atomic-children (list (last effective-stx))
                                                 (list (last effective-child-images)))))
               (map image-height effective-child-images)
               (cons unit-height
                     (map image-height (drop-right effective-child-images 1))))))
  
  (match stx
    [`(,(? cond-like-id? id) ,xs ...)
          
     (define atomic-children
       (get-atomic-children (rest stx) (rest child-images)))
     
     (rounded-rectangle
      (+ (* 2 unit-width)
         ; front and back margins
         (max (+ (longest atomic-children)
                 unit-width)
              (image-width (first child-images))))
      (apply + unit-height
             (map image-height (rest (rest child-images))))
      radius
      (if depth grey-one grey-two))]
    
    [`(,(? if-like-id? id) ,xs ...)
          
     (define atomic-children
       (get-atomic-children (rest stx) (rest child-images)))
     
     (define header-length
       ; width of form-name e.g. 'and'
       ; plus spaces before/after
       (+ (image-width (first child-images))
          (* 2 unit-width)))
     
     (rounded-rectangle
      (+ header-length  
         (max (+ (longest atomic-children)
                 unit-width)
              unit-width))
      (calculate-height stx (rest child-images))
      radius
      (if depth grey-one grey-two))]
         
    [`(,(? lambda-like-id?) ,x ,xs ...)

     (define atomic-children
       (get-atomic-children (rest (rest stx))
                            (rest (rest child-images))))
          
     (define header-length
       (+ (image-width (first child-images))
          (image-width (second child-images))
          (* 2 unit-width)))

     (define header-height
       (max (image-height (first child-images))
            (image-height (second child-images))))
          
     (define backing-candidate
       (overlay/align
        "left" "top"
        ; todo: make local-atomic. use to condition
        ; below rectangle width of if last child is atomic
        (rounded-rectangle (+ -2 header-length) ; hack magic -2, prevents aliasing
                           header-height
                           radius (if depth grey-one grey-two))
        (rounded-rectangle (+ (* 2 unit-width) ; indentation                              
                              (+ unit-width ; just looks good
                                 (longest atomic-children))) ; cover any atoms 
                           (+ header-height
                              ; accounts for line-spacing   
                              (* line-spacing
                                 (length (rest (rest child-images))))
                              (calculate-height stx (rest (rest child-images))))
                           radius (if depth grey-one grey-two))))
     backing-candidate]
    [_
     ; this case is only implicit app
     ; refactor to avoid potential bugs
     (define atomic-children
       (if (and (list? stx) (cond-like-id? (first stx)))
           (get-atomic-children stx child-images)
           (get-atomic-children stx child-images))
       )
     (define header-length
       unit-width)
     
     (rounded-rectangle
      (+ header-length  
         (max (+ (longest atomic-children)
                 (* 2 unit-width)) ; MAGIC 2 for debatable looks
              unit-width))
      (calculate-height stx child-images)
      radius
      (if depth grey-one grey-two))]))




(define (add-vertical-backing stx selected? depth children new-layout layout-settings)
  (define new-backing
    (render-vertical-backing stx new-layout depth (map second children) layout-settings))
  (define-from layout-settings
    selected-color text-size grey-one grey-two)
  (define radius (sub1 (div-integer text-size 2)))
  (define margin (div-integer text-size 5))
  
  (overlay/align
   "left" "top"
   new-layout
   (if selected?
       (overlay (overlay
                 (rounded-rectangle
                  (max 0 (+ (- margin)
                            (image-width new-backing)))
                  (image-height new-backing)
                  radius
                  (if depth grey-one grey-two))
                 (rounded-rectangle
                  (image-width new-backing)
                  (image-height new-backing)
                  radius
                  selected-color)
                 )
                new-backing)
       new-backing)))


(define (add-horizontal-backing new-layout selected? depth layout-settings)
  (define-from layout-settings
    selected-color text-size grey-one grey-two)
  (define radius (sub1 (div-integer text-size 2)))
  (define margin (div-integer text-size 5))
  (define new-backing
    (rounded-rectangle
     (image-width new-layout)
     (image-height new-layout)
     radius
     (if depth grey-one grey-two)))
  (overlay/align
   "left" "top"
   new-layout
   (if selected?
       (overlay (overlay
                 (rounded-rectangle
                  (+ (- 0 margin) (image-width new-backing))
                  (image-height new-layout)
                  radius
                  (if depth grey-one grey-two))
                 (rounded-rectangle
                  (image-width new-backing)
                  (image-height new-backing)
                  radius
                  selected-color)
                 )
                new-backing)
       new-backing)))





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
        (rectangle width height "outline" "blue")
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

(second
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



; color filter





