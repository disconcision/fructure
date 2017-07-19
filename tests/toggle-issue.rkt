#lang racket
(require racket/gui/base)

; setup frame
(define my-frame (new frame% [label "fructure"]
                      [width 400]
                      [height 400]))
(define my-canvas (new editor-canvas% [parent my-frame]))
(define my-board (new text%))
(send my-canvas
      set-editor my-board)
(send my-frame show #t)


; override on-char to add a border toggle key
(define my-editor-snip% (class editor-snip% (super-new)
                          (define/override (on-char dc x y edx edy event)
                            (define focus-snip (send (send this get-editor) get-focus-snip))
                            (if (and focus-snip (is-a? focus-snip editor-snip%))
                                (super on-char dc x y edx edy event)
                                (let ([key-code (send event get-key-code)])
                                  (match key-code
                                    ['release void]
                                    [#\e (send this show-border (not (send this border-visible?)))])
                                  )))))
; uncommenting super call makes whole hierarchy toggle


; make embedded editor
(define sub-board1 (new text%))
(define sub-board1-editor-snip (make-object my-editor-snip% sub-board1))
(send sub-board1 insert "sb1")
(send my-board insert sub-board1-editor-snip)

; make doubly embedded editor
(define sub-board1-1 (new text%))
(define sub-board1-1-editor-snip (make-object my-editor-snip% sub-board1-1))
(send sub-board1-1 insert "sb1-1")
(send sub-board1 insert sub-board1-1-editor-snip)


; set caret focus to doubly embedded editor
(send sub-board1 set-caret-owner sub-board1-1-editor-snip 'global)

