#lang racket

(require racket/gui/base)
(require fancy-app)

(define my-frame (new frame% [label "Simple Edit"]
                      [width 400]
                      [height 400]))
(define my-canvas (new editor-canvas% [parent my-frame]))
(define my-board (new text%))
(send my-canvas
      set-editor my-board)
(send my-frame show #t)

#|
(define mb (new menu-bar% [parent my-frame]))
(define m-edit (new menu% [label "Edit"] [parent mb]))
(define m-font (new menu% [label "Font"] [parent mb]))
(append-editor-operation-menu-items m-edit #f)
(append-editor-font-menu-items m-font)
(send my-board set-max-undo-history 100)
|#



(define my-style-delta (make-object style-delta% 'change-bold))
(send my-style-delta
      set-delta-background (make-object color% 255 100 0))
(define my-style-list (new style-list%)) ; create root style 'Basic'
(send my-style-list
      find-or-create-style
      (send my-style-list basic-style)
      my-style-delta)


; both of these overrides have double effects?? -fixed, supressed key release
#;(define my-text% (class text% (super-new)
                     (define/override (on-local-char event)
                       (send this show-border (not (send this border-visible?)))
                       (println "flipped border state")
                       #;(super on-local-char event))))

(define my-editor-snip% (class editor-snip% (super-new)
                          (define/override (on-char dc x y edx edy event)
                            (let ([key-code (send event get-key-code)])
                              (println key-code)
                              (match key-code
                                ['release void]
                                [#\e (send this show-border (not (send this border-visible?)))]
                                [#\d (begin (println "next")
                                            (send my-board set-caret-owner (send this next)) 'global)]
                                [#\a (begin (println "previous")
                                            (send my-board set-caret-owner (send this previous) 'global))]
                                [#\q (begin (println "select all")
                                            (send (send this get-editor) select-all))])
                              #;(super on-char dc x y edx edy event)))))
; commenting out last line makes top level editor border toggle only (else whole hierarchy toggles)

;------------------------------

(define sub-board1 (new text%))
(define sub-board2 (new text%))
(define sub-board3 (new text%))
;(send sub-board change-style my-style-delta)

(define sub-board1-editor-snip (make-object my-editor-snip% sub-board1))
(define sub-board2-editor-snip (make-object my-editor-snip% sub-board2))
(define sub-board3-editor-snip (make-object my-editor-snip% sub-board3))

(send sub-board1 insert "sb1")
(send sub-board2 insert "sb2")
(send sub-board3 insert "sb3")

(send my-board insert sub-board1-editor-snip)
(send my-board insert sub-board2-editor-snip)
(send my-board insert sub-board3-editor-snip)

;-----------------------------

(define sub-board3-1 (new text%))
(define sub-board3-2 (new text%))
(define sub-board3-3 (new text%))

(define sub-board3-1-editor-snip (make-object my-editor-snip% sub-board3-1))
(define sub-board3-2-editor-snip (make-object my-editor-snip% sub-board3-2))
(define sub-board3-3-editor-snip (make-object my-editor-snip% sub-board3-3))

(send sub-board3-1 insert "sb3-1")
(send sub-board3-2 insert "sb3-2")
(send sub-board3-3 insert "sb3-3")

(send sub-board3 insert sub-board3-1-editor-snip)
(send sub-board3 insert sub-board3-2-editor-snip)
(send sub-board3 insert sub-board3-3-editor-snip)

;-----------------------------


; method (send an-editor release-snip snip) 
; Requests that the specified snip be deleted and released from the editor.




;(define sub-board-editor-snip (make-object my-editor-snip% sub-board))
#;(send my-board insert sub-board-editor-snip)

;(send sub-board-editor-snip get-editor)
; returns a my-text%


;(define sub-sub-board (new text%))
;(define sub-sub-board-editor-snip (make-object my-editor-snip% sub-sub-board))
#;(send sub-board insert sub-sub-board-editor-snip)
#;(send sub-sub-board insert "blah")


(send my-board set-caret-owner #f 'global)


(define testcode-0 '(104 324 494))
(define testcode-1 '(11 51 61 (1 1) ((1 1) (1 (1 1)))))


(define (make-obj-tree code)
  (cond [(list? code) (let ([new-text-snip (new text%)])
                        (list* new-text-snip (map make-obj-tree code)))]
        [else (let ([new-text-snip (new text%)])
                (send new-text-snip
                      insert (~v code))
                new-text-snip)]))


(define (make-editor-tree obj-tree)
  (cond [(empty? obj-tree) '()]
        [(list? obj-tree) (let ([parent-board (first obj-tree)])
                            (map (λ (x) (if (list? x)
                                            (let ([new-ed (make-object my-editor-snip% (first x))])
                                              (send parent-board
                                                    insert new-ed)
                                              (list* new-ed (make-editor-tree x)))
                                            (let ([new-ed (make-object my-editor-snip% x)])
                                              (send parent-board
                                                    insert new-ed)
                                              new-ed)))
                                 (rest obj-tree)))
                          #;(map (λ (x) (if (list? x) (make-editor-tree x) "err")) (rest obj-tree))]
        [else "errrrrr"]))



(define obj-tree (list my-board (make-obj-tree testcode-0)))

obj-tree

(define ed-tree (make-editor-tree obj-tree))

ed-tree


; trying to change style; why doesn't this work??
;(send (second (second obj-tree)) change-style my-style-delta)

; Shows or hides the snip’s border.
(send (send my-board find-first-snip) show-border #t)


; Returns the first snip in the editor, or #f if the editor is empty. To get all of the snips in the editor, use the next in snip% on the resulting snip.
(send my-board find-first-snip)

; Gets the current extent of the editor’s graphical representation.
(define width (box 0))
(define height (box 0))
(send (second (second obj-tree)) get-extent width height)
(unbox width)
(unbox height)

; Returns the contents of the editor in text form.
(send (second (second obj-tree)) get-flattened-text)

; Gets the location of the given snip.
(define x-coord (box 0))
(define y-coord (box 0))
(send my-board get-snip-location (send my-board find-first-snip) x-coord y-coord)
(unbox x-coord)
(unbox y-coord)

; Inserts a box (a sub-editor) into the editor by calling on-new-box
;(send my-board insert-box 'text)




;(send an-editor on-default-char event)






;(send sub-board-editor resize 200 200)

;(send t insert s)

;(define x (box 1))
;(define y (box 0))
;(send t get-snip-location s x y)
;(unbox x)
;(unbox y)

;(define subsub (new text%))
;(define subsubed (make-object editor-snip% subsub))
;(send t insert subsubed)