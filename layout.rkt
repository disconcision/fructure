#lang racket

(require lang/posn)
(require 2htdp/image)
(require rackunit
         "layout-tests.rkt"
         "new-syntax.rkt"
         "utility.rkt"
         "../containment-patterns/containment-patterns.rkt"
         "utility-graphics.rkt")

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
        'length-conditional-layout? #t
        'length-conditional-cutoff 10
        'dodge-enabled? #t
        
        'menu-bkg-color (color 112 112 112)
        'form-color (color 0 130 214)
        'literal-color (color 255 131 50)
        'grey-one (color 200 200 200)
        'grey-two (color 184 184 184)
        'pattern-grey-one (color 84 84 84)
        'identifier-color "black"
        'selected-color (color 230 0 0)
        'hole-color (color 0 180 140)
        'transform-arrow-color (color 255 255 255)
        'bkg-color (color 0 47 54)

        #;#;'radius (λ (text-size) (sub1 (div-integer text-size 2)))
        #;#;'margin (λ (text-size) (div-integer text-size 5))
        ))

; fructure-layout : syntax -> pixels
(define (fructure-layout fruct layout-settings)
  (define-from layout-settings
    bkg-color text-size popout-transform? popout-menu?)
  (define margin (div-integer text-size 5))
  
  ; sanity checks
  (match fruct
    [`(◇ ,x) (error "strip top before calling")] [_ 0])

  (match-define `(,x-offset ,y-offset) '(20 20))

  (match-define `(,new-fruct ,scene-image)
    (render
     (match fruct
       [(/ fr/ stx)
        (/ [display-offset `(,x-offset ,y-offset)]
           [display-box '(800 800)]
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
                          (rectangle 800 800 "solid" bkg-color)))

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
                             (+ x x-offset (- (* 2 margin))) ;only slightly magic
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
    popout-transform? popout-menu?)
    
  (match fruct
   
    [(and this-menu
          (/ [menu `((,transforms ,resultants) ...)] m/ _))
     (match-define (list fruct-with-positions new-image)
       (render-menu this-menu
                    layout-settings))
     (list fruct-with-positions
           (if popout-menu?
               ; fill in dummy placeholder
               (let ([temp-image (render '? layout-settings)])
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
    
    [(/ id/ `(id ,xs ...))
     ; id itself is not drawn
     ; draw the letters xs with no spaces
     
     (match-define `((,child-fructs ,child-images) ...)
       (map (curryr render layout-settings) xs))
     (define-values (new-frs _)
       (layout-row (list 0 0) 0 xs child-images))
     (define my-new-image
       (apply beside* child-images))
     (list (/ id/ `(id ,@new-frs)) my-new-image)]
    [(/ a/ a)
     (define local-layout-settings
       (match (/ a/ a)
         [(/ (sort 'params) _/ _)
          (hash-set* layout-settings
                     'identifier-color (color 230 230 230)
                     'grey-one (color 76 76 76)
                     'grey-two (color 110 110 110))]
         [_ layout-settings]))
     (if (list? a)
         (render-list (/ a/ a)
                      ; hack to reset depth for params-list
                      (match (/ a/ a)
                        [(/ (sort 'params) _/ _)  #t] [_ depth])
                      bkg local-layout-settings (selected? fruct))
         (list fruct
               (cond
                 ; char holes look different
                 ; TODO: refactor render-atom and relocate this there
                 [(and (equal? a '⊙)
                       (match (/ a/ a) [(/ (sort 'char) _/ _) #t][_ #f]))
                  (render-atom '+ (selected? fruct) local-layout-settings)]
                 [else (render-atom a (selected? fruct) layout-settings)])))]
    
    [(? (disjoin form-id? symbol?) a)
     ; TODO: MAGIC symbol include, investigate this
     ; this case SHOULD be only for form headers
     (list fruct
           (render-atom a (selected? fruct) layout-settings))]
    [_ (error (~v `(,fruct "error: layout: render: not a fruct")))]))







(define (ellipses expander-height color)
  (beside (circle 3/2 "solid" color)
          (rectangle 1 expander-height "solid" invisible)
          (circle 3/2 "solid" color)
          (rectangle 1 expander-height "solid" invisible)
          (circle 3/2 "solid" color)))


(define (space text-size)
  (text/font " " text-size "black"
             #f 'modern 'normal 'normal #f))
  

(define (get-atomic-children stx child-images)  
  (foldl (λ (x y acc)
           (if (atomic? x)
               (cons y acc)
               acc))
         '() stx child-images))


(define (get-non-atomic-children stx child-images)
  (foldl (λ (x y acc)
           (if (not (atomic? x))
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
    text-size)

  (overlay
   (cond
     [(equal? s '⊙)
      (define my-radius
        ; TODO: magic numbers
        (min (* 3/11 text-size)
             (* 1/2 (image-width (space text-size))))) 
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
   (rectangle (image-width (space text-size))
              ; TODO: magic number 5
              ; additional headroom for chars
              ; aka default line spacing in pixels
              (+ 5 (image-height (space text-size)))
              "solid" invisible)))


(define (layout-row intial-offset space-width stx images)
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


(define (layout-column intial-offset line-spacing stx images)
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
      (list offset-x
            (+ offset-y
               line-spacing
               (image-height i))))
    (values `(,@output ,new-child)
            new-running-offset)))



(define (render-horizontal-default selected? layout-settings children)
  (match-define `((,stx ,child-images) ...) children)
  (define-from layout-settings
    text-size)

  (define-values (new-children final-offset)
    (layout-row (list (image-width (space text-size)) 0)
                (image-width (space text-size))
                stx
                child-images))
  
  (define new-image
    (apply beside/align
           "top"
           (space text-size)
           ; cursor display disabled for the moment
           #;(if selected? (second (render '▹ layout-settings)) (space text-size))
           (first child-images)
           (for/fold ([acc '()])
                     ([a (rest child-images)])
             `(,@acc ,(space text-size) ,a))))
  
  (list new-children
        ; note special case
        ; if last child is atom, we leave a space on the right
        (beside new-image
                (match (last children)
                  [(/ _/ (? list?)) empty-image]
                  [_ (space text-size)]))))



(define (render-vertical-if-like selected? layout-settings children)
  (match-define `((,stx ,child-images) ...) children)
  (define-from layout-settings
    text-size)

  (define new-first-children
    (match (first stx)
      [(? (disjoin symbol? number?)) (first stx)]
      [(/ a/ a)
       (/ [display-offset (list (image-width (space text-size)) 0)]
          [display-box (list (image-width (first child-images))
                             (image-height (first child-images)))]
          a/ a)]))

  (define offset-after-first ; note two spaces
    (list (+ (image-width (space text-size))
             (image-width (first child-images))
             (image-width (space text-size)))
          0))

  (define-values (new-rest-children final-offset)
    (layout-column offset-after-first
                   1
                   (rest stx)
                   (rest child-images)))

  (define new-image
    (beside/align
     "top"
     (space text-size)
     #;(if selected? (second (render '▹ layout-settings)) (space text-size))
     (first child-images)
     (space text-size)
     (apply above/align* "left"
            (for/fold ([acc '()])
                      ([i (rest child-images)])
              `(,@acc ,i ,1px)))
     #;(space text-size)))
  
  (list (cons new-first-children new-rest-children) new-image))



(define (render-vertical-lambda-like selected? layout-settings children)
  (match-define `((,stx ,child-images) ...) children)
  (define-from layout-settings
    text-size)

  (define-values (new-header-children offset-after-header-children)
    (layout-row (list (image-width (space text-size)) 0)
                (image-width (space text-size))
                (take stx 2)
                (take child-images 2)))

  (define header-image
    (beside/align "top"
                  (space text-size)
                  #;(if selected? (second (render '▹ layout-settings)) (space text-size))
                  (first child-images)
                  (space text-size)
                  (second child-images)))

  (define indent-image
    (beside (space text-size) (space text-size)))

  (define-values (new-rest-children final-offset)
    (layout-column (list (image-width indent-image)
                         (image-height header-image))
                   1
                   (rest (rest stx))
                   (rest (rest child-images))))

 
  (define new-image
    (above/align* "left"
                  header-image
                  (beside/align* "top"
                                 indent-image
                                 (apply above/align* "left"
                                        (for/fold ([acc '()])
                                                  ([i (rest (rest child-images))])
                                          `(,@acc ,i ,1px))))))


  (list (append new-header-children new-rest-children) new-image))




(define (render-menu stx layout-settings)
  (define-from layout-settings
    text-size max-menu-length max-menu-length-chars
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
        (hash-set* layout-settings
                   'grey-one invisible
                   'form-color "white")) ; magic color
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
                                     (second (render '▹ override-layout-settings))
                                     (if (not (list? b))
                                         ; hacky extra spacing for atoms
                                         (beside (space text-size)
                                                 (second (render (/ b/ b) override-layout-settings))
                                                 (space text-size))
                                         (second (render (/ b/ b) override-layout-settings))))))]
           [_ (render item override-layout-settings)])]
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
  
  (define truncated-menu-backed
    (overlay/align/offset "right" "top"
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
                            (color 125 125 125)))
                          
                          (- (div-integer margin 2)) 0
                          ; slighly magic number
                          (rounded-rectangle
                           (+ 0 (image-width truncated-menu-image))
                           (+ (* 2 expander-height)
                              (image-height truncated-menu-image))
                           radius
                           selected-color)))

  ; hackish. crops menu in char case to alleviate overlap
  (define new-image
    (if (not char-menu?)
        truncated-menu-backed
        (overlay/align/offset
         "left" "top"
         (crop (div-integer margin 2)
               0
               (image-width (space text-size))
               (image-height truncated-menu-backed)
               truncated-menu-backed)
         (- (* 2 margin)) 0 
         empty-image)))

  ; calculate REAL position data
  (define-values (post-new-fruct _)
    (for/fold ([output '()]
               [running-offset `(,(- margin) 0)])
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
                                 selected-color 0.3) v))]
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


(define (render-atom s selected? layout-settings)
  (define-from layout-settings
    text-size selected-color literal-color
    form-color hole-color transform-arrow-color 
    identifier-color grey-one)
  (define radius (sub1 (div-integer text-size 2)))
  (define margin (div-integer text-size 5))
  (define literal? (disjoin boolean? number? string?))
  (define candidate
    (render-symbol
     s
     (if selected?
         selected-color
         (cond
           [(equal? s '→) transform-arrow-color]
           [(equal? s '⊙) hole-color]
           [(equal? s '+) hole-color]
           [(equal? s '▹) selected-color]
           [(literal? s) literal-color]
           [(form-id? s) form-color]
           [else identifier-color]))
     layout-settings))
  
  (cond
    [selected?
     (define selected-candidate
       (overlay/align
        "middle" "middle"
        (render-symbol
         s
         "white"
         layout-settings)
        (rounded-rectangle
         (image-width candidate)
         (image-height candidate)
         radius
         selected-color)
        ))
     ; special-case the size of single-character selections
     (if (and (symbol? s) (equal? 1 (string-length (symbol->string s))))
         (crop 0
               0
               (image-width (space text-size))
               (image-height candidate)
               selected-candidate)
         selected-candidate)
     
     
     #;(beside* #;(second (render '▹ layout-settings)) candidate)]
    [else candidate]))





(define (render-list fruct depth bkg layout-settings selected?)
  (define-from layout-settings
    selected-color text-size grey-one grey-two
    length-conditional-layout? length-conditional-cutoff)
  (define radius (sub1 (div-integer text-size 2)))
  (match-define (/ a/ stx) fruct)
  
  (define candidate
    (for/list ([s stx])
      (render
       s
       layout-settings
       (not depth)
       (match stx
         [(or `(,(? lambda-like-id?) ,xs ...)
              `(,(? if-like-id?) ,xs ...))
          bkg]
         [_ depth]
         ))))
     
  (define children
    (if selected?
        (match stx
          [`(,(? symbol? x) ,xs ...)
           `((,(first (first candidate))
              ,(render-symbol
                x
                selected-color
                layout-settings)) ,@(rest candidate))]
          [_ candidate])
        candidate))
  
  ; decide if we're rendering horizontally or vertically
  (define render-this-horizontally?
    (cond
      [(not length-conditional-layout?) #f]
      [else (match-define `((,_ ,child-images) ...) children)
            (define total-length (div-integer (apply + (map image-width child-images))
                                              (image-width (space text-size))))
            (total-length . < . length-conditional-cutoff)]))
     
  (match stx
    

    ; TODO: refactor duplication below by spliting out render-this-horizontally?
      
    [`(,(? lambda-like-id?) ,xs ...)
     (match-define (list new-kids-local new-layout-local)
       (if render-this-horizontally?
           (render-horizontal-default selected? layout-settings children)
           (render-vertical-lambda-like selected? layout-settings children)))
     (list (/ a/ new-kids-local)
           (if render-this-horizontally?
               (add-simple-horizontal-backing new-layout-local selected?
                                              depth layout-settings)
               (add-backing stx selected? depth children
                            new-layout-local layout-settings)))
     ]         
    [`(,(? if-like-id?) ,xs ...)
     (match-define (list new-kids-local new-layout-local)
       (if render-this-horizontally?
           (render-horizontal-default selected? layout-settings children)
           (render-vertical-if-like selected? layout-settings children)))
     (list (/ a/ new-kids-local)
           (if render-this-horizontally?
               (add-simple-horizontal-backing new-layout-local selected?
                                              depth layout-settings)
               (add-backing stx selected? depth children
                            new-layout-local layout-settings)))
     ]
    [_
     (match-define (list new-kids-local new-layout-local)
       (render-horizontal-default selected? layout-settings children))
     (list (/ a/ new-kids-local)
           (add-simple-horizontal-backing new-layout-local selected?
                                          depth layout-settings))])
  )



(define (render-backing stx new-layout depth child-images layout-settings)
  (define-from layout-settings
    text-size
    grey-one
    grey-two)
  (define radius (sub1 (div-integer text-size 2)))
  (match stx
    [`(,(? if-like-id?) ,xs ...)
          
     (define atomic-children
       (get-atomic-children (rest stx) (rest child-images)))
     (define non-atomic-children
       (get-non-atomic-children (rest stx) (rest child-images)))
          
     (rounded-rectangle
      (+ (image-width (first child-images)) 
         ; width of form-name e.g. 'and'
         (* 2 (image-width (space text-size)))
         ; add pixels for each row (what? this is width...)
         #;(length (rest child-images))
         ; spaces before/after above form-name
         (max (+ (longest atomic-children)
                 (image-width (space text-size)))
              (shortest non-atomic-children)))
      (image-height new-layout)
      radius
      (if depth grey-one grey-two))]
         
    [`(,(? lambda-like-id?) ,x ,xs ...)

     (define atomic-children
       (get-atomic-children (rest (rest stx))
                            (rest (rest child-images))))
     (define non-atomic-children
       (get-non-atomic-children (rest (rest stx))
                                (rest (rest child-images))))
     (define shortest-non-atomic-child-width
       (shortest non-atomic-children))
     (define longest-atomic-child-width
       (longest atomic-children))
          
     (define header-length
       (+ (image-width (first child-images))
          (image-width (second child-images))
          (* 2 (image-width (space text-size)))))

     (define header-height
       (max (image-height (first child-images))
            (image-height (second child-images))))
          
     (define minimum-child-length
       (apply min (map image-width
                       (filter (negate list?)
                               (rest (rest child-images))))))

     (define backing-candidate
       (overlay/align
        "left" "top" 
        (rounded-rectangle header-length
                           header-height
                           radius (if depth grey-one grey-two))
        (rounded-rectangle (+ (* 2 (image-width (space text-size))) ; indentation                              
                              (image-width (space text-size)) ; just looks good
                              (longest atomic-children)  ; cover any atoms
                              )
                           (apply +
                                  header-height
                                  ; add pixels for each row
                                  (+ 0 (length (rest (rest child-images))))
                                  (map image-height (rest (rest child-images))))
                           radius (if depth grey-one grey-two))))
     backing-candidate]
    [_
     (rounded-rectangle
      (image-width new-layout)
      (image-height new-layout)
      radius
      (if depth grey-one grey-two))]))




(define (add-backing stx selected? depth children new-layout layout-settings)
  (define new-backing
    (render-backing stx new-layout depth (map second children) layout-settings))
  (define-from layout-settings
    selected-color text-size grey-one grey-two)
  (define radius (sub1 (div-integer text-size 2)))
  (define margin (div-integer text-size 5))
  
  (overlay/align
   "left" "middle"
   new-layout
   (if selected?
       (overlay (overlay
                 (rounded-rectangle
                  (max 0 (+ (- margin)
                            (image-width new-backing)))
                  (image-height new-layout)
                  radius
                  (if depth grey-one grey-two))
                 (rounded-rectangle
                  (image-width new-backing)
                  (image-height new-layout)
                  radius
                  selected-color)
                 )
                new-backing)
       new-backing)))


(define (add-simple-horizontal-backing new-layout selected? depth layout-settings)
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
   "left" "middle"
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
                  (image-height new-layout)
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
    [(/  a/ a) (/  a/ a)] 
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





(second
 (render
  data-0
  test-settings))

(second
 (render
  data-1
  test-settings))

(second
 (render
  data-2
  test-settings))

(second
 (render
  data-3
  test-settings))

(second
 (render
  data-4
  test-settings))

(second
 (render
  data-5
  test-settings))

(second
 (render
  (match data-6
    [(/ a/ a)
     (/ [display-box `(800 800)]
        [display-offset `(0 0)]
        a/ a)])
  test-settings))

(draw-outlines-abs
 (augment-absolute-offsets
  (first (render (match data-6
                   [(/ a/ a)
                    (/ [display-box `(800 800)]
                       [display-offset `(0 0)]
                       a/ a)])
                 test-settings))))

(second
 (fructure-layout data-7 test-settings))

(draw-outlines-abs (first (fructure-layout data-8 test-settings))
                   (second (fructure-layout data-8 test-settings)))


(second
 (fructure-layout data-8 test-settings))

(second
 (fructure-layout data-9 test-settings))



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


(add-hooks "x" (stx->fruct
                '(lambda (x)
                   (and x (▹ (and true false)))
                   x)))



; color filter





