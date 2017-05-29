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
(send my-board insert "mb")

;-----------------------------


(define (position-first-child pos)
  (append pos (list 1)))

(define (position-parent pos)
  (reverse (rest (reverse pos))))

(define (position-next pos)
  (append (position-parent pos) (list (add1 (last pos)))))

(define (position-prev pos)
  (append (position-parent pos) (list (sub1 (last pos)))))


(module+ test (require rackunit)
  (check-equal? (position-first-child '(1 2 3)) '(1 2 3 1))
  (check-equal? (position-parent '(1 2 3)) '(1 2))
  (check-equal? (position-next '(1 2 3)) '(1 2 4))
  (check-equal? (position-prev '(1 2 3)) '(1 2 2)))

(define/match (sub-at-pos tree pos)
  [(tree `()) (first tree)]
  [(_ `(,a . ,as)) (sub-at-pos (list-ref tree (modulo a (length tree))) as)])


;-----------------------------


(define my-editor-snip% (class editor-snip% (super-new)
                          (define/override (on-char dc x y edx edy event)
                            (let ([key-code (send event get-key-code)])
                              (println key-code)
                              (match key-code
                                ['release void]
                                [#\r (println (send (send this get-editor) get-focus-snip))] ; for testing
                                [#\t (println (send (send this get-editor) find-first-snip))] ; for testing
                                [#\e (send this show-border (not (send this border-visible?)))]
                                [#\q (begin (println "select all inside")
                                            (send (send this get-editor) select-all))]
                                [#\d (begin (println "next")
                                            (set! pos (position-next pos))
                                            (send (sub-at-pos editor-tree (position-parent pos)) set-caret-owner (sub-at-pos snip-tree pos) 'global)
                                            )]
                                [#\a (begin (println "previous")
                                            (set! pos (position-prev pos))
                                            (send (sub-at-pos editor-tree (position-parent pos)) set-caret-owner (sub-at-pos snip-tree pos) 'global)
                                            )]
                                [#\w (set! pos (position-parent pos))
                                     (send (sub-at-pos editor-tree (position-parent pos)) set-caret-owner (sub-at-pos snip-tree pos) 'global)]
                                [#\s 
                                 (println (send (send this get-editor) find-snip 0 'before)
                                          #;(send (send this get-editor) find-next-non-string-snip this))
                                 (send my-board set-caret-owner (send (send this get-editor) find-snip 0 'before) 'global)])
                              #;(super on-char dc x y edx edy event)))))
; commenting out last line makes top level editor border toggle only (else whole hierarchy toggles)

;-----------------------------

(define sub-board1 (new text%))
(define sub-board2 (new text%))
(define sub-board3 (new text%))

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


(define test-tree `("" ("1")
                       ("2")
                       ("3" ("31")
                            ("32")
                            ("33"))))

(define editor-tree `(,my-board (,sub-board1)
                                (,sub-board2)
                                (,sub-board3 (,sub-board3-1)
                                             (,sub-board3-2)
                                             (,sub-board3-3))))

(define snip-tree `(,void (,sub-board1-editor-snip)
                          (,sub-board2-editor-snip)
                          (,sub-board3-editor-snip (,sub-board3-1-editor-snip)
                                                   (,sub-board3-2-editor-snip)
                                                   (,sub-board3-3-editor-snip))))

(module+ test (require rackunit)
  (check-equal? (sub-at-pos test-tree '()) "")
  (check-equal? (sub-at-pos test-tree '(1)) "1")
  (check-equal? (sub-at-pos test-tree '(2)) "2")
  (check-equal? (sub-at-pos test-tree '(3)) "3")
  (check-equal? (sub-at-pos test-tree '(3 1)) "31")
  (check-equal? (sub-at-pos test-tree '(3 2)) "32")
  (check-equal? (sub-at-pos test-tree '(3 3)) "33"))




;-----------------------------

;(println (send sub-board2-editor-snip next))
;(send my-board set-caret-owner (send sub-board2-editor-snip next) 'global)

;(println (send sub-board3-1-editor-snip next))
;(send my-board set-caret-owner (send sub-board3-1-editor-snip next) 'global) ; not effective

;(println (send sub-board3-1-editor-snip next))
;(send sub-board3 set-caret-owner (send sub-board3-1-editor-snip next) 'global) ; effective


;-----------------------------


; initialize caret
(define pos '(3 1))
(println "caret tests")
(println pos)
(println (sub-at-pos snip-tree pos))
(println (sub-at-pos snip-tree (position-parent pos)))
(send (sub-at-pos editor-tree (position-parent pos)) set-caret-owner (sub-at-pos snip-tree pos) 'global)

;(set! pos (position-prev pos))
;(println pos)
;(println (sub-at-pos snip-tree pos))
;(println (sub-at-pos snip-tree (position-parent pos)))
;(send (sub-at-pos editor-tree (position-parent pos)) set-caret-owner (sub-at-pos snip-tree pos) 'global)


(set! pos (position-parent pos))
(println pos)
(println (sub-at-pos snip-tree pos))
;(println (sub-at-pos snip-tree (position-parent pos)))
;(println (sub-at-pos editor-tree (position-parent pos)))
;(send (sub-at-pos editor-tree (position-parent pos)) set-caret-owner (sub-at-pos snip-tree pos) 'global)
;(send (sub-at-pos editor-tree pos) select-all)



; structure data
(define testcode-0 '(104 324 494))
(define testcode-1 '(11 51 61 (1 1) ((1 1) (1 (1 1)))))


(define (make-ed-tree code)
  (cond [(list? code) (let ([new-text-ed (new text%)])
                        (list* new-text-ed (map make-ed-tree code)))]
        [else (let ([new-text-ed (new text%)])
                (send new-text-ed
                      insert (~v code))
                new-text-ed)]))


(define (make-sn-tree ed-tree)
  (let ([parent-board (first ed-tree)])
    (map (λ (x) (if (list? x)
                    (let ([new-snip (make-object my-editor-snip% (first x))])
                      (send parent-board
                            insert new-snip)
                      (list* new-snip (make-sn-tree x)))
                    (let ([new-snip (make-object my-editor-snip% x)])
                      (send parent-board
                            insert new-snip)
                      new-snip)))
         (rest ed-tree))))



(define ed-tree (list my-board (make-ed-tree testcode-0)))

;ed-tree

(define sn-tree (make-sn-tree ed-tree))

;sn-tree

