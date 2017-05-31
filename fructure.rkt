#lang racket

(require racket/gui/base)
;(require fancy-app)
;(require lens/common)
;(require lens/data/list)

(define my-text-ed% (class text% (super-new) ))


(define my-frame (new frame% [label "fructure"]
                      [width 600]
                      [height 300]))
(define my-canvas (new editor-canvas% [parent my-frame]))
(define my-board (new my-text-ed%))
(send my-canvas
      set-editor my-board)
(send my-frame show #t)
(send my-board insert "struct")

;-----------------------------


(define (position-first-child pos)
  (append pos (list 1)))

(define/match (position-parent pos)
  [(`()) "fuck"] 
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


(define/match (sub-list-at-pos tree pos)
  [(tree `()) tree]
  [(_ `(,a . ,as)) (sub-list-at-pos (list-ref tree (add1 (modulo (sub1 a) (sub1 (length tree))))) as)])


(module+ test (require rackunit)
  (define test-tree `("" ("1")
                         ("2")
                         ("3" ("31")
                              ("32")
                              ("33"))))
  (check-equal? (sub-at-pos test-tree '()) "")
  (check-equal? (sub-at-pos test-tree '(1)) "1")
  (check-equal? (sub-at-pos test-tree '(2)) "2")
  (check-equal? (sub-at-pos test-tree '(3)) "3")
  (check-equal? (sub-at-pos test-tree '(3 1)) "31")
  (check-equal? (sub-at-pos test-tree '(3 2)) "32")
  (check-equal? (sub-at-pos test-tree '(3 3)) "33"))


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

(define (no-children-at? tree pos)
  (<= (length (sub-list-at-pos tree pos)) 1))

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
                                 (if (<= (length pos) 1)
                                     "do nothing"
                                     (begin (set! pos (position-parent pos))
                                            (send (sub-at-pos etree pos) set-caret-owner (send (sub-at-pos etree pos) find-first-snip) 'global)
                                            (send (sub-at-pos etree pos) select-all)))]
                                [#\s ; select first child
                                 #;(println (send (send this get-editor) find-snip 0 'before))
                                 (if (no-children-at? etree pos)
                                     "do nothing"
                                     (begin (set! pos (position-first-child pos))
                                            (send (sub-at-pos etree (position-parent pos)) set-caret-owner (sub-at-pos stree pos) 'global)
                                            (send (sub-at-pos etree pos) select-all)))])
                              #;(super on-char dc x y edx edy event)))))
; commenting out last line makes top level editor border toggle only (else whole hierarchy toggles)


;-----------------------------

; tree initialization functions

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


;-----------------------------

; structure data
(define testcode-1 '(alp bea gma (flf pop) ((brk grl) (zwl (nkp flp)))))

; initialize tree
(define ed-tree (make-ed-tree testcode-1))
(define sn-tree (make-sn-tree ed-tree))
(insert-snips sn-tree ed-tree)
(send my-board insert (first sn-tree))


(define pos '(1))
(send (sub-at-pos ed-tree (position-parent pos)) set-caret-owner (sub-at-pos sn-tree pos) 'global)
(send (sub-at-pos ed-tree pos) select-all)


; let's take a look
ed-tree
sn-tree
;(println (sub-at-pos sn-tree pos))
;(println (sub-at-pos ed-tree (position-parent pos)))



;(send an-editor clear) → void?
; Deletes the currently selected items.








