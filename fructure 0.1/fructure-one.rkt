#lang racket

(require racket/gui/base)
(require fancy-app)
(require "transform-engine.rkt")
(require "utility-fns.rkt")

; -------------------------------------------------------
; source structure data

(define original-source '(define (build-gui-block code [parent-ed my-board] [position '()])
                           (let* ([ed (new fruct-ed% [parent-editor parent-ed] [position position])]
                                  [sn (new fruct-sn% [editor ed] [parent-editor parent-ed] [position position])]
                                  [style (make-style position code)])
                             (send ed set-snip! sn)
                             (send parent-ed insert sn)
                             (if (list? code)
                                 (let* ([builder (λ (sub pos) (build-gui-block sub ed (append position `(,pos))))]
                                        [kids (map builder code (range 0 (length code)))])
                                   (set-style! style sn ed)
                                   `(,(block-data position 'list style ed sn) ,@kids))
                                 (begin (set-style! style sn ed)
                                        (send ed insert (~v code))
                                        `(,(block-data position 'atom style ed sn)))))))
#; (define original-source '(let ([a b]
                                  [c d])
                              e
                              f))

#; (define original-source '("0" "1" "a" ("20" "21" ("220")) "3"))

; -------------------------------------------------------
; structures and objects for gui

(struct block-data (position parent-ed type style ed sn))

(define pos '(1))
(define mode 'nav)
(define num-chars 0)

(define fruct-ed% (class text% (super-new [line-spacing 0]) ; line spacing changes something.. padding?

                    #; (send this set-sticky-styles #t)
                    
                    (init-field parent-editor)
                    (init-field position)
                    
                    (field [is-atomic (void)])
             
                    (field [containing-snip (void)])

                    (define/public (get-parent-editor) parent-editor)
                    (define/public (get-pos) position)
                    (define/public (atomic?) is-atomic)
                    (define/public (set-atomic! bool) (set! is-atomic bool))

                    (define/public (set-snip! a-snip) (set! containing-snip a-snip))
                    (define/public (get-snip) containing-snip)
                    
                    
                    (define/public (remove-text-snips)
                      (for ([pos (range 0 (send this last-position))])
                        (when (is-a? (send this find-snip pos 'before) string-snip%)
                          (send this release-snip (send this find-snip pos 'before))
                          (remove-text-snips))))

                    (define/public (format-horizontal)
                      (remove-text-snips))
                    
                    (define/public (format-vertical)
                      (remove-text-snips)
                      (let ([num-items (send this last-position)])
                        (for ([pos (range 1 (- (* 2 num-items) 2) 2)])
                          (send this insert "\n" pos))))
                    
                    (define/public (format-indent-after start-at)
                      (remove-text-snips)
                      (let ([num-items (send this last-position)])
                        (for ([pos (range start-at (* 2 (sub1 num-items)) 2)])
                          (send this insert "\n" pos))
                        (for ([line-num (range 1 (sub1 num-items))])
                          (send this insert "    " (send this line-start-position line-num)))))

                    (define (before-linebreak? sn parent-ed)
                      (let* ([snip-pos (send parent-ed get-snip-position sn)]
                             [next-snip (send parent-ed find-snip (add1 snip-pos) 'after-or-none)])
                        (if (equal? next-snip #f)
                            #f
                            (equal? (send next-snip get-text 0 1) "\n"))))

                    (define (after-linebreak? sn parent-ed)
                      (let* ([snip-pos (send parent-ed get-snip-position sn)]
                             [prev-snip (send parent-ed find-snip snip-pos 'before-or-none)])
                        (if (equal? prev-snip #f)
                            #f
                            (or (equal? (send prev-snip get-text 0 1) "\n")
                                (equal? (send prev-snip get-text 0 1) " ")
                                #|this hacky second case deals with indents|#))))
                    
                    (define/override (on-default-char event)
                      (match-let* ([key-code (send event get-key-code)]
                                   [pos (sel-to-pos source)]
                                   [obj (obj-at-pos gui pos)]
                                   [(block-data position parent-ed type style ed sn) obj])
                        (when (not (equal? key-code 'release))
                          (cond
                            [(equal? mode 'nav)
                             (match key-code
                               [#\space (send parent-ed set-caret-owner sn 'global)
                                        (send ed set-position 0)
                                        (if (equal? mode 'nav) (set! mode 'text-entry) (set! mode 'nav))]                                     
                               [_ (define (relativize-direction key-code)
                                    (cond
                                      [(and (before-linebreak? sn parent-ed) (equal? key-code #\s)) #\d]
                                      [(and (after-linebreak? sn parent-ed) (equal? key-code #\w)) #\a]
                                      [else key-code]))
                                  (set! key-code (relativize-direction key-code))
                                  (set! source (update source key-code))
                                  (let* ([new-board (new fruct-ed% [parent-editor "none"] [position '(0)])])
                                    (set! gui (build-gui-block source new-board))
                                    (send my-canvas set-editor new-board))
                                  ])]
                            [(equal? mode 'text-entry)
                             (match (string key-code)
                               [" " (if (equal? mode 'nav) (set! mode 'text-entry) (set! mode 'nav))
                                    (define form-name (send ed get-text 0 num-chars))
                                    (set! num-chars 0)
                                    (set! source ((insert-form form-name) source))
                                    ;following is verbatim update code from above; refactor
                                    (let* ([new-board (new fruct-ed% [parent-editor "none"] [position '(0)])]
                                           )
                                      (set! gui (build-gui-block source new-board))
                                      (send my-canvas set-editor new-board))
                                    ]
                               [(regexp #rx"[A-Za-z0-9_]") (set! num-chars (add1 num-chars))
                                                           (send ed insert key-code)])
                             ]
                            ))))))



(define fruct-sn% (class editor-snip%
                    
                    (init-field parent-editor)
                    (init-field position)
                    (init-field editor)

                    (super-new [with-border? #f] [editor editor])
                    
                    (define border-color "MediumVioletRed")
                    (define background-color "red")
                    
                    (define/public (set-background-color color)
                      (set! background-color color))

                    (define/public (set-format format)
                      (match format
                        ['horizontal (send editor format-horizontal)]
                        ['vertical (send editor format-vertical)]
                        ['indent (send editor format-indent-after 2)]))
                    
                    (define/override (draw dc x y left top right bottom dx dy draw-caret)
                      
                      (define a-dc dc)
                      (define a-x x)
                      (define a-y y)
                      (define a-w (box 2))
                      (define a-h (box 2))
                      (define a-descent (box 2))                     
                      (send this get-extent a-dc a-x a-y a-w a-h a-descent)
                      (send dc set-brush background-color 'solid)
                      (send dc set-pen background-color 1 'solid)
                      (define bottom-x (box 2))
                      (define bottom-y (box 2))
                      #; (send dc set-text-mode 'transparent) ; ineffective
                      #; (send dc set-background "blue") ; ineffective
                      #; (send editor get-extent width height) ; try this instead?
                      (send parent-editor get-snip-location this bottom-x bottom-y #t)                            
                      (send dc draw-rectangle (+ x 0) (+ y 0) (+ (unbox bottom-x) 0) (+ (unbox bottom-y) 0))

                      (super draw dc x y left top right bottom dx dy draw-caret))))


; -------------------------------------------------------
; build gui and return tree with references to relevant objects

(define (build-gui-block code [parent-ed my-board] [position '()])
  (let* ([ed (new fruct-ed% [parent-editor parent-ed] [position position])]
         [sn (new fruct-sn% [editor ed] [parent-editor parent-ed] [position position])]
         [style (make-style position code)])
    (send ed set-snip! sn)
    (unless (equal? code selector) ; hack
      (send parent-ed insert sn))
    (if (list? code)
        (let* ([builder (λ (sub pos) (build-gui-block sub ed (append position `(,pos))))]
               [kids (map builder code (range 0 (length code)))])
          (set-style! style sn ed) ; need to set style after children are inserted
          (send ed set-atomic! #f)
          `(,(block-data position parent-ed 'list style ed sn) ,@kids))
        (begin (set-style! style sn ed) ; styler must be  first else deletes text
               (send ed set-atomic! #f)
               (unless (equal? code selector) ; hack
                 (send ed insert (~v code)))
               `(,(block-data position parent-ed 'atom style ed sn))))))


; -------------------------------------------------------
; styling functions


#; (define stylesheet '((default
                          (background-color "blue")
                          (format format-horizontal))
                        (define
                          (background-color "green")
                          (format format-vertical-fixed-indent-after 2))))


#; (define (find-style atom)
     (let ([style (filter (λ (style) (equal? atom (first style))) stylesheet)])
       (if (empty? style)
           'default 
           (first (first style)))))


#; (define/match (show-style-tree gui-tree)
     [(`(,(block-data _ _ style _ _) ,ls ...)) 
      `(,style ,@(map show-style-tree ls))])


(define (make-style position code)
  `(my-style
    (background-color ,(match code
                         [`(,(== selector) ,a ...) (make-color 200 200 0)]
                         [_ (make-color (modulo (exact-round (* 255 (sqrt (/ (length position) (tree-depth original-source))))) 256)
                                        60
                                        100)]))
    (format ,(match code
               [`((,(== selector) ,a) ,ls ...) (second (third (make-style position `(,a ,@ls))))] ; hack
               [`(,form ,ls ...) #:when (member form '(let let* if begin define for)) 'indent]              
               [`([,a ...] ...) 'vertical]
               [_ 'horizontal]))))


(define/match (set-style! style sn ed)
  [(`(,name (background-color ,color)
            (format ,format)) _ _)
   (begin (send sn set-background-color color)
          (send sn use-style-background #t)
          (send sn set-format format)
          (send sn set-margin 2 2 2 2)
          #;(send sn set-inset 0 0 0 0) ; ???
          #;(send sn set-align-top-line #t) ; don't exactly understand this
          (begin (define my-style-delta (make-object style-delta%))
                 (send my-style-delta set-delta-background color)
                 (send my-style-delta set-delta-foreground (make-color 255 255 255))
                 (send my-style-delta set-alignment-on 'top) ; ???
                 #;(send my-style-delta set-transparent-text-backing-on #f) ; doesn't work
                 (send ed change-style my-style-delta)))])


#; (define (build-gui code [parent-ed my-board] [position '()])
     (let* ([ed (new fruct-ed% [parent-editor parent-ed] [position position])]
            [sn (new fruct-sn% [editor ed] [parent-editor parent-ed] [position position])])
       (send ed set-snip! sn)
       (send parent-ed insert sn)
       #;(send ed change-style my-style-delta)
       (if (list? code)
           (begin (map (λ (sub pos) (build-gui sub ed (append position `(,pos)))) code (range 0 (length code)))
                  #; (insert test stuff here))
           (send ed insert (~v code)))))


; -------------------------------------------------------
; gui setup

(define my-frame (new frame%
                      [label "fructure"]
                      [width 1300]
                      [height 900]))

(send my-frame set-alignment 'center 'center) ; ineffective?


(define my-canvas (new editor-canvas%
                       [parent my-frame]))


(define my-board (new fruct-ed%
                      [parent-editor "none"]
                      [position '()]))

(send my-board set-atomic! #f) ; hack

; append selector
(define source (simple-select original-source))

; build gui
(define gui (build-gui-block source my-board))


(define (traverse gui)
  (if (list? gui)
      (map traverse gui)
      (block-data-sn gui)))

#;(traverse gui)

(send my-canvas set-editor my-board)
(send my-board set-caret-owner #f 'global)
(send my-frame show #t)




; note on nav fixing

; things nav might depend on:
; style (direction, child-num, linebreaks)
; whether or not it's an atom
; style/atomicity of parent/children/siblings

; if linebreak after, down selects next child
; if linebreak before, up selects prev child, left selects parent, right does nothing (or parent?)
; if linebreak before and at end: same but down selects ?? parent ??

; make left/right (or up/down inside vertical block) at beginning/end of block exit block instead of wrapping
; or if last

; more advanced
; 'sibling' cases for identically shaped siblings which can be moved around in like a grid
; must be both horizontal or both vertical
; identical shaped := something like: if sibling has same 'list shape' up to the level of depth where the selector is


; if horizontal, should right go: ($new (f ...) -> (new ($f)) ie skip to first child?
; makes it slower to get to second child (as well as selecting child instead of child.first obviously)
; but to alleviate bracketted case: when press space to edit an atom at beginning of list, could automatically select parent list




; notes on modes

; what if the key used to quit insert mode picked the action
; like instead of insert, exiting with SHIFT searched on entered string instead
; could use as main command entry mode
; would need to actually build an intermediary structure though, not just an intermeidary string
; and draw that structure as an overlay somehow (to start, let's try a different pane)


; notes on panes

; three to start
; stage, kit, and pattern



; pattern autoselect:
; when select node, automatch it's subtree to available patterns
; generate hashmap of pattern vars to selected's children
; for first child (interpreted as symbol literal), color grey
; for other childern, re-color according to color map (in correspondance with hashmap)
; populate kit.environment with hashmap (in list form?)



; affordances:
; write <...> into tree, select it to create new child of relevant type (when parent form takes list args)
; affordances are contextual patterns; they depend on (their position in) their parent form


; transformers:
; as close to possible as exactly what meets the eye