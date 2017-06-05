#lang racket

(require racket/gui/base)
(require fancy-app)


(define my-frame (new frame%
                      [label "fructure"]
                      [width 600]
                      [height 300]))


(define my-canvas (new editor-canvas%
                       [parent my-frame]))


(define fruct-ed% (class text% (super-new)
                    (init parent)
                    (define parent-editor parent)
                    (init-field pos)
                    (field [containing-snip (void)])
                    (define/public (get-parent-editor) parent-editor)
                    (define/public (get-pos) pos)
                    (define/public (set-snip! a-snip)
                      (set! containing-snip a-snip))                     
                    ))


(define fruct-sn% (class editor-snip%
                    (init parent)
                    (define parent-editor parent)
                    (init-field pos)
                    (define children '())
                    (define border-color "blue")
                          
                    (super-new [with-border? #f])

                    (define/public (change-color color)
                      (set! border-color color))

                    (define/override (draw dc x y left top right bottom dx dy draw-caret)
                      (send dc set-brush (make-color (* 200 (/ (length pos) 5)) 100 150) 'solid)
                      (send dc set-pen border-color 1 'solid)
                      (define bottom-x (box 10))
                      (define bottom-y (box 10))

                      (send parent-editor get-snip-location this bottom-x bottom-y #t)                            
                      (send dc draw-rectangle (+ x 1) (+ y 1) (unbox bottom-x) (unbox bottom-y))
                            
                      (super draw dc x y left top right bottom dx dy draw-caret))))


(define my-board (new fruct-ed%
                      [parent "none"]
                      [pos '(0)]))

(send my-canvas
      set-editor my-board)

(send my-frame
      show #t)

(send my-board
      insert "top")


(define (build-gui code [parent-ed my-board] [position '()])
  (let* ([ed (new fruct-ed% [parent parent-ed] [pos position])]
         [sn (new fruct-sn% [editor ed] [parent parent-ed] [pos position])])
    (send ed set-snip! sn)
    (send parent-ed insert sn)
    (if (list? code)
        (map (λ (sub pos) (build-gui sub ed (append position `(,pos)))) code (range 0 (length code)))
        (send ed insert (~v code)))))


(define source '(define my-board (new fruct-ed%
                      [parent "none"]
                      [pos '(0)])))

(build-gui source)