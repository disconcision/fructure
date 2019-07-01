#lang racket

(provide display-keypresses
         display-command-line)

(require 2htdp/image
         "../common.rkt"
         "common-layout.rkt")


(define (display-command-line state)
  (define-from state
    command-buffer command-pointer layout-settings mode)
  (define-from layout-settings
    selected-color
    text-size typeface background-block-color form-color outline-block-color  #;radius)
  (define command-display-radius 6)
  (define-values (text-color outline-color bkg-color)
    (values (color 200 200 200)
            (color 0 57 66)
            (color 0 0 0 0)))
  (define text-image
    (beside
     (overlay/align
      "left" "top"
      (text/font command-buffer text-size (if (equal? mode 'command)
                                              "red"
                                              (color 80 80 80))
                 typeface 'modern 'normal 'normal #;'bold #f)
      (text/font (symbol->string command-pointer) text-size (color 80 80 80)
                 typeface 'modern 'normal 'normal #;'bold #f))
     (text/font " : " text-size "white"
                typeface 'modern 'normal 'normal #;'bold #f)
     (text/font (~a (hash-ref layout-settings command-pointer "null")) text-size (if (equal? mode 'command)
                                                                                     "white"
                                                                                     (color 80 80 80))
                typeface 'modern 'normal 'normal #;'bold #f)))
  (overlay (rounded-rectangle-outline
            (image-width text-image)
            (image-height text-image)
            command-display-radius
            (if (equal? mode 'command)
                selected-color
                outline-color)
            2)
           text-image
           (rounded-rectangle
            (image-width text-image) (image-height text-image)
            command-display-radius bkg-color)))

(define (display-keypresses state)
  (define-from state
    layout-settings keypresses)
  (define-from layout-settings
    text-size typeface background-block-color form-color outline-block-color bkg-color #;radius)
  (define keypress-radius 6)
  (define keypress-color (color 70 70 70))
  (define keypress-nonlit-color (color 90 90 90))
  (define keypress-nonlit-outline-color outline-block-color)
  (define keypress-nonlit-background-color background-block-color)
  (define keypress-num 36)
  (define keypress-axis 'horizontal)
  (define keypress-direction "bottom")
  (define (render-key-core str typeface text-color outline-color bkg-color)
    (define text-image
      (text/font str text-size text-color
                 typeface 'modern 'normal 'normal #;'bold #f))
    (overlay (rounded-rectangle-outline
              (image-width text-image) (image-height text-image)
              keypress-radius outline-color 2)
             text-image
             (rounded-rectangle
              (image-width text-image) (image-height text-image)
              keypress-radius bkg-color)))
  (define (render-keypress str)
    (render-key-core str typeface keypress-color
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