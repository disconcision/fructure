#lang racket

(require racket/gui/base)
(require fancy-app)

(define my-frame (new frame% [label "fructure"]
                      [width 400]
                      [height 400]))
(define my-canvas (new editor-canvas% [parent my-frame]))
(define my-board (new text%))
(send my-canvas
      set-editor my-board)
(send my-frame show #t)


;-----------------------------



(define (position-first-child pos)
  (append pos (list 0)))

(define (position-parent pos)
  (reverse (rest (reverse pos))))

(define (position-next pos)
  (append (position-parent pos) (list (add1 (last pos)))))

(define (position-prev pos)
  (append (position-parent pos) (list (sub1 (last pos)))))

(position-first-child '(1 4 71))


(define/match (sub-at-pos tree pos)
  [(tree `()) tree]
  [(_ `(,a . ,as)) (let ([cand (sub-at-pos (list-ref tree (modulo a (length tree))) as)])
                     (if (list? cand)
                         (first cand)
                         cand))])





(define (parent editor-snip)
  my-board)


(define my-editor-snip% (class editor-snip% (super-new)
                          (define/override (on-char dc x y edx edy event)
                            (let ([key-code (send event get-key-code)])
                              (println key-code)
                              (match key-code
                                ['release void]
                                [#\r (println (send (send this get-editor) get-focus-snip))] ; for testing
                                [#\t (println (send (send this get-editor) find-first-snip))] ; for testing
                                [#\e (send this show-border (not (send this border-visible?)))]
                                [#\d (begin (println "next")
                                            (set! pos (position-next pos))
                                            (println (sub-at-pos editor-tree pos))
                                            (println (sub-at-pos snip-tree pos))
                                            (println (sub-at-pos editor-tree (position-parent pos)))
                                            (send (sub-at-pos editor-tree (position-parent pos)) set-caret-owner (sub-at-pos snip-tree pos) 'global)
                                            )]
                                [#\a (begin (println "previous")
                                            (set! pos (position-prev pos))
                                            (send (sub-at-pos editor-tree (position-parent pos)) set-caret-owner (sub-at-pos snip-tree pos) 'global)
                                            )]
                                [#\q (begin (println "select all inside")
                                            (send (send this get-editor) select-all))]
                                [#\w (send (parent this) set-caret-owner (send this next))]
                                [#\s 
                                     (println (send (send this get-editor) find-snip 0 'before)
                                              #;(send (send this get-editor) find-next-non-string-snip this))
                                     (send my-board set-caret-owner (send (send this get-editor) find-snip 0 'before))])
                              #;(super on-char dc x y edx edy event)))))
; commenting out last line makes top level editor border toggle only (else whole hierarchy toggles)

;-----------------------------

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

(define editor-tree `(,my-board
                      ,sub-board1
                      ,sub-board2
                      (,sub-board3 ,sub-board3-1
                                   ,sub-board3-2
                                   ,sub-board3-3)))

(define snip-tree `(,void
                    ,sub-board1-editor-snip
                    ,sub-board2-editor-snip
                    (,sub-board3-editor-snip ,sub-board3-1-editor-snip
                                             ,sub-board3-2-editor-snip
                                             ,sub-board3-3-editor-snip)))

(sub-at-pos snip-tree '(0))
(sub-at-pos snip-tree '(1))
(sub-at-pos snip-tree '(2))
(sub-at-pos snip-tree '(3))
(sub-at-pos snip-tree '(3 0))
(sub-at-pos snip-tree '(3 1))
(sub-at-pos snip-tree '(3 2))
(sub-at-pos snip-tree '(3 3))

;-----------------------------

(println (send sub-board2-editor-snip next))
(send my-board set-caret-owner (send sub-board3-editor-snip next) 'global)

(println (send sub-board3-1-editor-snip next))
(send my-board set-caret-owner (send sub-board3-1-editor-snip next) 'global) ; not effective

(println (send sub-board3-1-editor-snip next))
(send sub-board3 set-caret-owner (send sub-board3-1-editor-snip next) 'global) ; effective


;-----------------------------


; initialize
(define pos '(0))
(send (sub-at-pos editor-tree pos) set-caret-owner (sub-at-pos snip-tree '(2)) 'global)
(send (sub-at-pos editor-tree '(1)) select-all)






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
  (let ([parent-board (first obj-tree)])
    (map (λ (x) (if (list? x)
                    (let ([new-ed (make-object my-editor-snip% (first x))])
                      (send parent-board
                            insert new-ed)
                      (list* new-ed (make-editor-tree x)))
                    (let ([new-ed (make-object my-editor-snip% x)])
                      (send parent-board
                            insert new-ed)
                      new-ed)))
         (rest obj-tree))))



(define obj-tree (list my-board (make-obj-tree testcode-0)))

obj-tree

(define ed-tree (make-editor-tree obj-tree))

ed-tree

