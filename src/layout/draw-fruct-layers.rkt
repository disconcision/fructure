#lang racket

(provide draw-fruct-layers
         draw-outlines-abs)

(require 2htdp/image
         "../../shared/containment-patterns/containment-patterns/main.rkt"
         "../../shared/slash-patterns/slash-patterns.rkt"
         "../common.rkt"
         "draw-fruct.rkt")


; fructure-layout : syntax -> pixels
(define (draw-fruct-layers fruct layout-settings
                           (screen-x 800) (screen-y 400))
  (define-from layout-settings
     text-size menu-expander-height menu-outline-width
    popout-transform? popout-menu? simple-menu?)

  ; sanity check
  (match fruct
    [`(◇ ,x) (error "strip top before calling")] [_ 0])
  
  ; prevent cut-off of overlapping menu
  (define offset
    (+ menu-expander-height menu-outline-width))
  (match-define `(,x-offset ,y-offset) `(,offset ,offset))
  ; MAGIC AF PIXEL OFFSET TO AVOID CUTOFF FOR MENU
  ; BUT ALSO FOR RED OUTLINES

  (match-define `(,new-fruct ,scene-image)
    (draw-fruct
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
  
  (define (stick-selection-in backing-image)
    (match newest-fruct
      [(or #;(and this-selection
                (/ [display-absolute-offset
                    `(,(app (curry + x-offset) x)
                      ,(app (curry + y-offset) y))] s/ (▹ _)))
           (⋱ _ (and this-selection
                     (/ [display-absolute-offset `(,x ,y)] s/ (▹ _)))))
      (render-in draw-fruct
                  this-selection backing-image
                  x y)]
      [_ backing-image]))
 

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
  #;(define (single-char-menu? menu-candidate)
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
                  (+ y (if simple-menu? 0 (- offset))))]
      [_ backing-image]))
 
          

  (define (>>if ? f a)
    (if ? (f a) a))

  (define new-image
    (overlay/align/offset
     "left" "top"
     scene-image
     (- x-offset) (- y-offset)
     (rectangle screen-x screen-y "solid" (color 0 0 0 0))))
  
  (list newest-fruct
        (>>if #true stick-selection-in
              (>>if popout-menu? stick-menu-in
                    (>>if popout-transform? stick-transform-in
                          new-image)))))


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