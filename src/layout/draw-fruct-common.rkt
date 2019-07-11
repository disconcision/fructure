#lang racket

(provide layout-row
         layout-column
         real-atomic?
         render-symbol
         render-atom
         ellipses
         space)

(require 2htdp/image
         "../../shared/containment-patterns/containment-patterns/main.rkt"
         "../../shared/slash-patterns/slash-patterns.rkt"
         "../language/syntax.rkt"
         "../common.rkt"
         "common-layout.rkt")

; hacky, rework this
(define (real-atomic? fruct)
  (or (atomic? fruct)
      (match fruct [(/ _/ `(ref ,_)) #t][_ #f])
      (match fruct [(/ _/ `(num ,_)) #t][_ #f])))

(define (space text-size typeface)
  (text/font " " text-size "black"
             typeface 'modern 'normal 'normal #f))

(define (ellipses expander-height color)
  (beside (circle (div-integer expander-height 6) "solid" color)
          (rectangle 1 expander-height "solid" invisible)
          (circle (div-integer expander-height 6) "solid" color)
          (rectangle 1 expander-height "solid" invisible)
          (circle (div-integer expander-height 6) "solid" color)))

(define (render-symbol s my-color layout-settings)
  (define-from layout-settings
    typeface text-size unit-width unit-height
    hole-bottom-color hole-side-color)

  (overlay
   (cond
     [(equal? s '⊙+)
      (text/font "+"
                 (round (* 7/12 text-size))
                 my-color
                 typeface 'modern 'normal 'normal #f)]
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
               hole-side-color))]
     [else
      (text/font (string-append (~a s))
                 ; HACK: hole char is slightly weird
                 ; at least on windows?
                 (if (equal? s '⊙)
                     (round (* 0.8 text-size))
                     text-size)
                 my-color
                 typeface
                 'modern 'normal 'normal #f)])
   ; padding
   (rectangle unit-width
              unit-height
              "solid" invisible)))



(define (render-atom a selected? layout-settings)
  
  (match-define (/ s/ s) a)
  (define-from layout-settings
    selected-color literal-color
    form-color +hole-color transform-arrow-color 
    identifier-color selected-atom-color
    radius unit-width text-size typeface hole-as-sort?)
  (define literal? (disjoin boolean? number? string?))

  (define candidate
    (match a
      [(/ [sort sort] _/ (and h (or '⊙ '⊙+)))
       #:when hole-as-sort?
       ; render sort name instead of hole
       (define blank
         (render-symbol " " (color 0 0 0 0) layout-settings))
       (define sort-image
         (text/font (string-upcase (if (or (equal? h '⊙+) (equal? sort 'char))
                                       (string-append (~a sort) "+")
                                       (string-append (~a sort))))
                    (round (* 0.4 text-size))
                    (if selected? selected-atom-color literal-color)
                    typeface
                    'modern 'normal 'normal #f))
       (overlay/align "middle" "middle"
                      sort-image
                      blank)]
      [_
       (render-symbol
          (match (/ s/ s)
            [(or (/ (sort 'char) _/ '⊙)
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
          layout-settings)]))

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