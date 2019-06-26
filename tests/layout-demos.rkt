#lang racket

(require 2htdp/image
         "../shared/slash-patterns/slash-patterns.rkt"
         ; only for div in add-dynamic. TODO: refactor
         "../src/common.rkt"
         "../src/layout/layout.rkt"
         "layout-demos-data.rkt")


; TODO: rework as default settings?
(define test-settings-init 
  (hash 'text-size 34
        'typeface "Iosevka, Light"
        'max-menu-length 3
        'max-menu-length-chars 1
        'transform-template-only #f
        'popout-transform? #t
        'popout-menu? #t
        'custom-menu-selector? #t
        'simple-menu? #f
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
        'menu-secondary-color (color 40 40 40)
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


         