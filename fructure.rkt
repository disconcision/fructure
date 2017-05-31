#lang racket

(require racket/gui/base)
;(require fancy-app)
(require lens/common)
(require lens/data/list)

(define my-text-ed% (class text% (super-new)
                      #;(define/override (get-focus-snip)
                          #f)))


(define my-frame (new frame% [label "fructure"]
                      [width 400]
                      [height 400]))
(define my-canvas (new editor-canvas% [parent my-frame]))
(define my-board (new my-text-ed%))
(send my-canvas
      set-editor my-board)
(send my-frame show #t)
(send my-board insert "mb")

;-----------------------------


(define (position-first-child pos)
  (append pos (list 1)))

(define/match (position-parent pos)
  [(`()) pos]
  [(_) (reverse (rest (reverse pos)))])

(define (position-next pos)
  (append (position-parent pos) (list (add1 (last pos)))))

(define (position-prev pos)
  (append (position-parent pos) (list (sub1 (last pos)))))


(module+ test (require rackunit)
  (check-equal? (position-first-child '(1 2 3))'(1 2 3 1))
  (check-equal? (position-parent '(1 2 3)) '(1 2))
  (check-equal? (position-next '(1 2 3)) '(1 2 4))
  (check-equal? (position-prev '(1 2 3)) '(1 2 2)))


(define/match (sub-at-pos tree pos)
  [(tree `()) (first tree)]
  [(_ `(,a . ,as)) (sub-at-pos (list-ref tree (add1 (modulo (sub1 a) (sub1 (length tree))))) as)])


(define (delete-node tree pos)
  (let* ([child-positions (range 1 (length tree))]
         [filt-positions (filter (λ (child-pos) (not (and (equal? (first pos) child-pos) (equal? (rest pos) '())))) child-positions)])
    (list* (first tree) (map (λ (child-pos) (cond [(not (equal? (first pos) child-pos)) (list-ref tree child-pos)]
                                                  [else (delete-node (list-ref tree child-pos) (rest pos))]))
                             filt-positions))))

(define (replace-node tree pos new-node)
  (let ([child-positions (range 1 (length tree))])
    (list* (first tree) (map (λ (child-pos) (cond [(not (equal? (first pos) child-pos)) (list-ref tree child-pos)]
                                                  [(and (equal? (first pos) child-pos) (equal? (rest pos) '())) new-node]
                                                  [else (replace-node (list-ref tree child-pos) (rest pos) new-node)]))
                             child-positions))))


(module+ test (require rackunit)
  (check-equal? (replace-node '(0 (1) (2) (3 (4) (5))) '(3 1) "replace") '(0 (1) (2) (3 "replace" (5))))
  (check-equal? (delete-node '(0 (1) (2) (3 (4) (5))) '(3 1)) '(0 (1) (2) (3 (5)))))


(define (insert-first-child tree pos new-node)
  0)

(define (insert-sibling-before tree pos new-node)
  0)

(define (insert-sibling-after tree pos new-node)
  0)


;-----------------------------


(define my-editor-snip% (class editor-snip% (super-new)
                          (define/override (on-char dc x y edx edy event)
                            (let ([key-code (send event get-key-code)]
                                  [etree ed-tree]
                                  [stree sn-tree])
                              (println key-code)
                              (match key-code
                                ['release void]
                                [#\r (println (send (send this get-editor) get-focus-snip))] ; for testing
                                [#\t (println (send (send this get-editor) find-first-snip))] ; for testing
                                [#\e (send this show-border (not (send this border-visible?)))]
                                [#\q (send (sub-at-pos etree pos) select-all)]
                                [#\y ; delete node at pos, set pos to parent
                                 (send (sub-at-pos etree (position-parent pos)) release-snip (sub-at-pos stree pos))
                                 (set! ed-tree (delete-node ed-tree pos))
                                 (set! sn-tree (delete-node sn-tree pos))
                                 (set! pos (position-parent pos))]
                                [#\u ; insert new child
                                 (send (sub-at-pos etree pos) insert (make-object my-editor-snip% (new text%)))]
                                [#\d ; select next sibling
                                 (set! pos (position-next pos))
                                 (send (sub-at-pos etree (position-parent pos)) set-caret-owner (sub-at-pos stree pos) 'global)
                                 (send (sub-at-pos etree pos) select-all)]
                                [#\a ; select previous sibling
                                 (set! pos (position-prev pos))
                                 (send (sub-at-pos etree (position-parent pos)) set-caret-owner (sub-at-pos stree pos) 'global)
                                 (send (sub-at-pos etree pos) select-all)]
                                [#\w ; select parent
                                 (set! pos (position-parent pos))
                                 (send (sub-at-pos etree pos) set-caret-owner (send (sub-at-pos etree pos) find-first-snip) 'global)
                                 (send (sub-at-pos etree pos) select-all)]
                                [#\s ; select first child
                                 #;(println (send (send this get-editor) find-snip 0 'before))
                                 (set! pos (position-first-child pos))
                                 (send (sub-at-pos etree (position-parent pos)) set-caret-owner (sub-at-pos stree pos) 'global)
                                 (send (sub-at-pos etree pos) select-all)])
                              #;(super on-char dc x y edx edy event)))))
; commenting out last line makes top level editor border toggle only (else whole hierarchy toggles)


;-----------------------------

(define sub-board1 (new my-text-ed%))
(define sub-board2 (new my-text-ed%))
(define sub-board3 (new my-text-ed%))

(define sub-board1-editor-snip (make-object my-editor-snip% sub-board1))
(define sub-board2-editor-snip (make-object my-editor-snip% sub-board2))
(define sub-board3-editor-snip (make-object my-editor-snip% sub-board3))

(send sub-board1 insert "sb1")
(send sub-board2 insert "sb2")
(send sub-board3 insert "sb3")

;(send my-board insert sub-board1-editor-snip)
;(send my-board insert sub-board2-editor-snip)
;(send my-board insert sub-board3-editor-snip)

;-----------------------------

(define sub-board3-1 (new my-text-ed%))
(define sub-board3-2 (new my-text-ed%))
(define sub-board3-3 (new my-text-ed%))

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

; structure data
(define testcode-0 '(104 324 494))
(define testcode-1 '(11 51 61 (1 1) ((1 1) (1 (1 1)))))


(define (make-ed-tree code)
  (if (list? code) `(,(new text%) ,@(map make-ed-tree code))
      (let ([new-text-ed (new text%)])
        (send new-text-ed insert (~v code))
        (list new-text-ed))))


(define (make-sn-tree ed-tree)
  `(,(make-object my-editor-snip% (first ed-tree)) ,@(map make-sn-tree (rest ed-tree))))


(define (insert-snips sn-tree ed-tree)
  (if (list? sn-tree)
      (begin (map (λ (x) (send (first ed-tree) insert (first x))) (rest sn-tree))
             (map insert-snips sn-tree ed-tree))
      '()))

(define ed-tree (make-ed-tree testcode-1))

(define sn-tree (make-sn-tree ed-tree))

(insert-snips sn-tree ed-tree)

(send my-board insert (first sn-tree))



;-----------------------------

;(println (send sub-board2-editor-snip next))
;(send my-board set-caret-owner (send sub-board2-editor-snip next) 'global)

;(println (send sub-board3-1-editor-snip next))
;(send my-board set-caret-owner (send sub-board3-1-editor-snip next) 'global) ; not effective

;(println (send sub-board3-1-editor-snip next))
;(send sub-board3 set-caret-owner (send sub-board3-1-editor-snip next) 'global) ; effective


;-----------------------------


; initialize caret
(define pos '(3))
(println "caret tests")
ed-tree
sn-tree
;(println (sub-at-pos sn-tree pos))
;(println (sub-at-pos ed-tree (position-parent pos)))

;(send my-board set-caret-owner (send my-board find-first-snip) 'global)
(send (sub-at-pos ed-tree (position-parent pos)) set-caret-owner (sub-at-pos sn-tree pos) 'global)
(send (sub-at-pos ed-tree pos) select-all)

;(println pos)
;(println (position-parent pos))
;(println (sub-at-pos snip-tree pos))
;(println (sub-at-pos editor-tree (position-parent pos)))
;(send (sub-at-pos editor-tree (position-parent pos)) set-caret-owner (sub-at-pos snip-tree pos) 'global)
;(send (sub-at-pos editor-tree pos) select-all)

;(send (sub-at-pos editor-tree '()) set-caret-owner (sub-at-pos snip-tree '(2)) 'global)
;(send (sub-at-pos editor-tree '(3)) set-caret-owner (sub-at-pos snip-tree '(3 2)) 'global)

;(send (sub-at-pos editor-tree '(3)) get-focus-snip)
;(send (sub-at-pos editor-tree '()) set-caret-owner (sub-at-pos snip-tree '(3)) 'global) ; not effective (already 'at' 3)
;(send (sub-at-pos editor-tree '()) set-caret-owner (sub-at-pos snip-tree '(2)) 'global) ;effective

;(send (sub-at-pos editor-tree '(3)) set-caret-owner (send (sub-at-pos editor-tree '(3)) find-first-snip) 'global)





;(send an-editor clear) → void?
; Deletes the currently selected items.


; (send an-editor insert snip)
; Inserts data into the editor. 




#|
(set! pos (position-first-child pos))
(println pos)
(println (sub-at-pos snip-tree pos))
(send (sub-at-pos editor-tree (position-parent pos)) set-caret-owner (sub-at-pos snip-tree pos) 'global)
(send (sub-at-pos editor-tree pos) select-all)

(set! pos (position-next pos))
(println pos)
(println (sub-at-pos snip-tree pos))
(send (sub-at-pos editor-tree (position-parent pos)) set-caret-owner (sub-at-pos snip-tree pos) 'global)
(send (sub-at-pos editor-tree pos) select-all)

(set! pos (position-parent pos))
(println pos)
(println (sub-at-pos snip-tree pos))
(send (sub-at-pos editor-tree (position-parent pos)) set-caret-owner (sub-at-pos snip-tree pos) 'global)
(send (sub-at-pos editor-tree pos) select-all)
|#





