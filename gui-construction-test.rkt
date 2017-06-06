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
                  
                    (init-field parent-editor)
                    (init-field position)
                    (field [containing-snip (void)])
                    
                    (define/public (format-vertical)
                      (let ([num-items (send this last-position)])
                        (for ([pos (range 1 (sub1 (* 2 num-items)) 2)])
                          (send this insert "\n" pos))))
                    
                    (define/public (format-horizontal)
                      (remove-text-snips))

                    (define/public (remove-text-snips)
                      (for ([pos (range 0 (send this last-position))])
                        (when (is-a? (send this find-snip pos 'before) string-snip%)
                          (send this release-snip (send this find-snip pos 'before))
                          (remove-text-snips)) ))

                    (define/public (format-vertical-fixed-indent-after start-at)
                      (let ([num-items (send this last-position)])
                        (for ([pos (range start-at (* 2 (sub1 num-items)) 2)])
                          (send this insert "\n" pos))
                        (for ([line-num (range 1 (sub1 num-items))])
                          (send this insert "    " (send this line-start-position line-num)))))
                    
                    (define/public (get-parent-editor) parent-editor)
                    (define/public (get-pos) position)
                    (define/public (set-snip! a-snip) (set! containing-snip a-snip))
                    
                    ))


(define fruct-sn% (class editor-snip%
                    
                    (init-field parent-editor)
                    (init-field position)

                    (define children '())
                    (define border-color "MediumVioletRed")
                    (define background-color (make-color (modulo (* 200 (/ (length position) 5)) 256) 100 150))
                          
                    (super-new [with-border? #f])
                    
                    (define/public (change-color color)
                      (set! border-color color))

                    (define/override (draw dc x y left top right bottom dx dy draw-caret)
                      (send dc set-brush background-color 'solid)
                      (send dc set-pen border-color 1 'solid)
                      (define bottom-x (box 2))
                      (define bottom-y (box 2))
                      ; (send dc set-background "blue") ;ineffective
                      (send parent-editor get-snip-location this bottom-x bottom-y #t)                            
                      (send dc draw-rectangle (+ x 0) (+ y 0) (+ (unbox bottom-x) 0) (+(unbox bottom-y) 0))
                            
                      (super draw dc x y left top right bottom dx dy draw-caret))))






(define my-board (new fruct-ed%
                      [parent-editor "none"]
                      [position '(0)]))

(send my-canvas
      set-editor my-board)

(send my-frame
      show #t)

(send my-board
      insert "top")


(define my-style-delta (make-object style-delta%))
(send my-style-delta set-delta-background (make-object color% 100 100 200))
(send my-style-delta set-transparent-text-backing-on #f)


(define stylesheet '((default
                       (background-color "blue")
                       (format format-horizontal))
                     (define
                       (background-color "green")
                       (format format-vertical-fixed-indent-after 2))))

(define (find-style atom)
  (let ([style (filter (λ (style) (equal? atom (first style))) stylesheet)])
    (if (empty? style)
        'default
        (first (first style)))))

(define (build-style-tree code)
  (if (list? code)
      (map build-style-tree code)
      (find-style code)))

; notes on styling
; so to start, we decide the style for a list-node based on the first element
; when that element is an atom. we'll ignore the case where the first element is a list


(define (build-gui code [parent-ed my-board] [position '()])
  (let* ([ed (new fruct-ed% [parent-editor parent-ed] [position position])]
         [sn (new fruct-sn% [editor ed] [parent-editor parent-ed] [position position])])
    (send ed set-snip! sn)
    (send parent-ed insert sn)
    (send ed change-style my-style-delta)
    (if (list? code)
        (begin (map (λ (sub pos) (build-gui sub ed (append position `(,pos)))) code (range 0 (length code)))
               #; (insert test stuff here))
        (send ed insert (~v code)))))


(define source '(define (format-vertical)
                  (let ([num-items (send this last-position)])
                    (for ([pos (range 1 (sub1 (* 2 num-items)) 2)])
                      (send this insert "\n" pos)))))

(build-gui source)

(build-style-tree source)

