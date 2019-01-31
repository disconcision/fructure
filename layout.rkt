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

        'transform-tint-color (color 160 0 0) ; selected-color
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
    margin bkg-color text-size popout-transform? popout-menu?)
 
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
                  (+ x (- margin))
                  (+ y))]
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
                  (+ x (if (single-char-menu? menu)
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
    text-size  popout-transform? popout-menu? implicit-forms
    pattern-bkg-color pattern-grey-one pattern-grey-two
    selected-color grey-one grey-two)
    
  (match fruct

    ; menu render specially
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

    ; transform renders specially
    [(/ [transform template] t/ target)
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
     (define-from layout-settings radius margin)
     (match-define (list id-fruct id-image)
       (if (selected? (/ ref/ `(ref ,id)))
           (render id layout-settings)
           (render id layout-settings)))
     ; need to add position information here
     (list (/ ref/ `(ref ,id-fruct))
           (if (selected? (/ ref/ `(ref ,id)))
               (overlay id-image
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

    [(/ n/ `(num ,xs ...))
     (define children
       (map (curryr render layout-settings) xs))
     (define-values (new-frs my-new-image _)
       (layout-row (list 0 0) 0 children))
     (list (/ n/ `(num ,@new-frs)) my-new-image)]

    [(and ps (/ (sort 'params) a/ a))
     #;(println `(params case activates))
     (define local-layout-settings
       (hash-set* layout-settings
                  ; TODO magic colors
                  'identifier-color pattern-bkg-color
                  ; HACK need to update when patterns move beyond chars
                  'hole-color (color 170 170 170 120)
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


(define (space text-size typeface)
  (text/font " " text-size "black"
             typeface 'modern 'normal 'normal #f))
  

(define (get-atomic-children stx child-images)  
  (foldl (λ (x y acc)
           (if (or (atomic? x)
                   (match x [(/ _/ `(ref ,_)) #t][_ #f])
                   (match x [(/ _/ `(num ,_)) #t][_ #f]))
               (cons y acc)
               acc))
         '() stx child-images))


#;(define (get-non-atomic-children stx child-images)
    (foldl (λ (x y acc)
             (if (not (or (atomic? x)
                          (match x [(/ _/ `(ref ,_)) #t][_ #f])
                          (match x [(/ _/ `(num ,_)) #t][_ #f])))
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
    typeface text-size char-padding-vertical unit-width unit-height)

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
        ; note special case
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
    text-size typeface max-menu-length max-menu-length-chars implicit-forms
    custom-menu-selector? line-spacing selected-color menu-bkg-color radius margin)
  (match-define (/ [menu `((,transforms ,resultants) ...)] p/ place) stx)

  #| this function currently renders ALL menu items
     not just the ones displated.
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
                      'force-horizontal-layout? #t
                      #;#;'form-color (color 255 255 255))]
          [_ (hash-set* layout-settings
                        'force-horizontal-layout? #t
                        'grey-one menu-bkg-color
                        'form-color (color 255 255 255))])) ; magic color
      (define search-buffer (match item [(/ [search-buffer search-buffer] a/ a)
                                         search-buffer]
                              ; todo: fix hardcoded init-buffer here:
                              [_ '(▹ "")]))
      #;(println `(in layout search-buffer is: ,search-buffer))

      (define (strip▹ buf)
        (match buf
          [`(▹ ,s) s]
          [(? list?) (map strip▹ buf)]
          [x x]))
      
      (define (overlay-search-buffer stx image)
        (match stx
          ; sort of hacky exception for ref
          ; still not really right, r is still going to select refs maybe?
          [(/ a/ `(,(and (not 'ref) (not 'num) (? (curryr member implicit-forms)) _) ,xs ...))
           #;(println "curry member implicit case") image]
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
                  (overlay-search-buffer (/ b/ (▹ b))
                                         (if (single-char-menu? resultants)
                                             ; hacky color change... is it even working?
                                             ; it's interpreting chars as identifiers for some reason?
                                             (second (render (/ b/ b) (hash-set override-layout-settings
                                                                                'identifier-color "white")))
                                             (overlay/align "left" "top"
                                                            (space text-size typeface)
                                                            (if (or (not (list? b)) (or (and (member 'ref implicit-forms)
                                                                                             (match b [`(ref ,_) #t][_ #f]))
                                                                                        (and (member 'num implicit-forms)
                                                                                             (match b [`(num ,_) #t][_ #f]))))
                                                                ; hacky extra spacing for atoms
                                                                ; extra hacky for implicit refs
                                                                ; HACK below throws off offset alignment
                                                                ; need to refacto as popped layer
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
                                                                            selected-color)
                                                                           temp))
                                                                (let ([temp (second (render (/ b/ b) override-layout-settings))])
                                                                  (overlay (rounded-rectangle-outline
                                                                            (image-width temp)
                                                                            ; slightly hacky see above
                                                                            (max 0 (- (image-height temp) 1))
                                                                            radius
                                                                            selected-color)
                                                                           temp)))))))]
           [(/ b/ b)
            (list (first (render item override-layout-settings))
                  (overlay-search-buffer (/ b/ b)
                                         (if (or (not (list? b)) (or (and (member 'ref implicit-forms)
                                                                          (match b [`(ref ,_) #t][_ #f]))
                                                                     (and (member 'num implicit-forms)
                                                                          (match b [`(num ,_) #t][_ #f]))))
                                             ; hacky extra spacing for atoms
                                             ; extra hacky for implicit refs
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
  (define expander-color
    (color 125 125 125))
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
  
  (define new-image
    ; if char menu, supress drawing of left-side
    ; highlight 'cause jankiness.
    (overlay/align/offset
     (if (single-char-menu? resultants) "left" "right") "top"
     cool-menu (* -2 margin) 0  empty-image))

  
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
        new-image))







(define (render-transform fruct layout-settings)
  (define-from layout-settings
    text-size typeface grey-one grey-two selected-color
    dodge-enabled? transform-tint-color radius margin)
  (match-define (/ [transform template] t/ target) fruct)
  #;(define pat-tem-bkg-color
      grey-one #;(if depth grey-one grey-two))
  
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
       (/ [display-offset (list (apply + (map image-width
                                              (list target-image
                                                    (space text-size typeface)
                                                    arrow-image
                                                    (space text-size typeface))))
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
     (space text-size typeface)
     arrow-image 
     (space text-size typeface)
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
    identifier-color selected-atom-color
    radius margin)
  (define literal? (disjoin boolean? number? string?))
  
  (define candidate
    (render-symbol
     (match (/ s/ s) [(or (/ (sort 'char) _/ '⊙)
                          (/ (sort 'digit) _/ '⊙)
                          (/ _/ '⊙+)) '⊙+] [_ s])
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





(define (render-list fruct depth bkg init-layout-settings selected?)
  (define-from init-layout-settings
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
  #;(when params-like? (println 'paramslike!!!!!!))

  (define ends-in-atom?
    (match rest-stx
      ; HACK: properly abstract atomic definition
      [`(,xs ... ,(/ _/ (or (? (negate list?))
                            '⊙ `(,(or 'id 'ref 'num) ,_ ...)))) #t]
      [_ #f]))
  #;(when ends-in-atom? (println 'ends-in-atom!!))
  
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
            (define horiz-width-approx total-length)
            (define vertical-width-approx
              (apply max (map image-width child-images)))
            (and (total-length . < . length-conditional-cutoff)
                 ; below is broken
                 ; suppose to prevent vertical layout
                 ; in case where savings is small
                 #;(vertical-width-approx . > .
                                          (+ -5 horiz-width-approx)))]))

  ; choose an algorithm to layout the children
  (define layout-renderer
    (cond
      [(and lambda-like? (not render-this-horizontally?))
       (curry render-vertical (* 2 unit-width) 2)]         
      [(and if-like?
            (not render-this-horizontally?)
            (member first-stx implicit-forms))
       ; if the form is implicit and we're rendering it vertically
       ; the header is just child 1, and the indent-width is space-width
       (curry render-vertical unit-width 1)]
      [(and if-like? (not render-this-horizontally?))
       ; indent length is 2 plus the length of the form name
       (define image-from second)
       (curry render-vertical
              (+ (* 2 unit-width)
                 (image-width (image-from (first children))))
              2)]
      [(and cond-like? (not render-this-horizontally?))
       (curry render-vertical unit-width 1)]
      [else render-horizontal]))

  ; START OF BACKING REBUILD
  #; (rounded-backing source-right-profile
                      source-left-profile
                      r my-color mode outline-w)
  #; (rounded-backing
      (list '(300 50)
            '(100 50)
            '(400 50)
            '(400 50)
            '(300 50)
            '(150 50)
            '(250 50))
      (list '(0   50)
            '(0   50)
            '(300 50)
            '(100 50)
            '(150 50)
            '(150 50)
            '(150 50))
      10 "red" "outline" 2)
  ; org option: (constant) line-height
  ; plus pairs of start/end posns on that line
  #; (list 50
           (list '(300 0)
                 '(100 0)
                 '(400 300)
                 '(400 100)
                 '(300 150)
                 '(150 150)
                 '(250 150)))

  ; choose an algorithm to back the layout
  (define backer
    (cond
      [render-this-horizontally?
       (if (or params-like?
               (not ends-in-atom?))
           ; omit trailing space
           (λ (x y) (add-horizontal-backing 
                     x y #f selected? depth layout-settings))
           (λ (x y) (add-horizontal-backing 
                     x y #t selected? depth layout-settings)))
       ]
      [else
       (λ (x y) (add-vertical-backing-new x y selected? depth
                                          children layout-settings))]))

  (match-define (list new-kids-local new-layout-local)
    (layout-renderer layout-settings children))


  #;(define newer-image (backer new-layout-local))

  (match-define (list almost-final-stx final-image)
    (backer (/ a/ new-kids-local) new-layout-local))

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

  #;(list new-stx-before-backer
          newer-image))



(define (render-vertical-backing stx new-layout depth child-images layout-settings)
  (define-from layout-settings
    unit-height unit-width line-spacing radius
    implicit-forms grey-one grey-two )

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
          ; lambda-headers (only) front-padded by a space
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
    ; note map second below: this is where we strip of stx
    (render-vertical-backing stx new-layout depth (map second children) layout-settings))
  (define-from layout-settings
    selected-color grey-one grey-two radius margin)
  
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

(define (render-vertical-backing-new init-stx selected? depth children layout-settings)
  (define-from layout-settings
    unit-height unit-width line-spacing radius
    implicit-forms grey-one grey-two )

  (define child-images
    (map second children))
  
  (define stx
    (match init-stx
      [(/ a/ this) this]))

  (define (calculate-height effective-stx effective-child-images)
    (apply +
           (if (not (empty? (get-atomic-children (list (last effective-stx))
                                                 (list (last effective-child-images)))))
               (map image-height effective-child-images)
               (cons unit-height
                     (map image-height (drop-right effective-child-images 1))))))
  
  (define new-image
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
          ; lambda-headers (only) front-padded by a space
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

  
  (list init-stx
        new-image))



(define (add-vertical-backing-new stx new-layout-layout selected? depth children layout-settings)
  (match-define (list new-stx new-backing)
    (render-vertical-backing-new stx selected? depth children layout-settings))
  (define-from layout-settings
    selected-color grey-one grey-two radius margin)
  ; if selected?, outline backing in selected-color, and possibly use selected-color as bkg
  (list new-stx
        (overlay/align "left" "top" new-layout-layout new-backing)))


(define (add-horizontal-backing new-stx new-layout rear-padded? selected? depth layout-settings)
  (define-from layout-settings
    text-size typeface radius margin
    selected-color grey-one grey-two unit-width)
  (define new-backing
    (rounded-rectangle
     ; hacky padding option for ids
     (+ (image-width new-layout)
        (if rear-padded?
            unit-width
            0))
     (image-height new-layout)
     radius
     (if depth grey-one grey-two)))
  (define new-image
    (overlay/align
     "left" "top"
     new-layout
     (if selected?
         (overlay (overlay
                   (rounded-rectangle
                    (+ (- margin) (image-width new-backing))
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
  (list new-stx new-image))





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





