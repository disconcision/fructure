#lang racket

(provide display-keypresses)

(require 2htdp/image
         "common-layout.rkt")

(define (display-keypresses keypresses text-size)
  (define keypress-text-size text-size)
  (define keypress-typeface "Iosevka, Light")
  (define keypress-radius 8)
  (define keypress-color (color 0 118 172))
  (define keypress-nonlit-color (color 0 47 54))
  (define keypress-nonlit-outline-color (color 0 47 54))
  (define keypress-nonlit-background-color (color 0 74 84))
  (define keypress-num 36)
  (define keypress-axis 'horizontal)
  (define keypress-direction "bottom")
  (define (render-key-core str typeface text-color outline-color bkg-color)
    (define text-image
      (text/font str keypress-text-size text-color
                 typeface 'modern 'normal 'normal #;'bold #f))
    (overlay (rounded-rectangle-outline
              (image-width text-image) (image-height text-image)
              keypress-radius outline-color 2)
             text-image
             (rounded-rectangle
              (image-width text-image) (image-height text-image)
              keypress-radius bkg-color)))
  (define (render-keypress str)
    (render-key-core str keypress-typeface keypress-color
                     (color 0 57 66)
                     (color 0 0 0 0)))
  (define (render-keypress-nonlit str)
    (render-key-core str #f keypress-nonlit-color
                     keypress-nonlit-outline-color
                     keypress-nonlit-background-color))
  (define (render-key k)
    (match k
      ; nonliterals
      ["right"  (render-keypress-nonlit "→")]
      ["left"   (render-keypress-nonlit "←")]
      ["up"     (render-keypress-nonlit "↑")]
      ["down"   (render-keypress-nonlit "↓")]
      ["\r"     (render-keypress-nonlit "↪")]
      ["\b"     (render-keypress-nonlit "⌫")]
      ["\t"     (render-keypress-nonlit "↹")]
      ["shift"  (render-keypress-nonlit "⇧")]
      ["control"(render-keypress-nonlit "✲")]
      ["escape" (render-keypress-nonlit "⎋")]
      ; pseudoliterals
      [" "      (render-keypress " ")]
      ["["      (render-keypress "(")]
      ["]"      (render-keypress ")")]
      [else (render-keypress k)]))
  (define last-num-keypresses
    (reverse (if (< (length keypresses) keypress-num)
                 keypresses
                 (take keypresses keypress-num))))
  (match keypresses
    ['() empty-image]
    [(? list?)
     (apply (case keypress-axis
              [('vertical) above/align*]
              [('horizontal) beside/align*]
              [else beside/align*])
            keypress-direction
            (map render-key last-num-keypresses))]))