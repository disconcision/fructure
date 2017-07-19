#lang racket

(require racket/gui/base)
(require fancy-app)
;(require lens/common)
;(require lens/data/list)



(define my-frame (new frame% [label "fructure"]
                      [width 600]
                      [height 300]))
(define my-canvas (new editor-canvas% [parent my-frame]))

(define pos '(1))

;-----------------------------


(define (position-first-child pos)
  (append pos (list 1)))

(define/match (position-parent pos)
  [(`()) "no"] 
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




(define my-text-ed% (class text% (super-new)
                      (init parent)
                      (define parent-editor parent)
                      (init-field name)
                      (field [in-snip (void)])
                      (define/public (get-parent)
                        parent-editor)
                      (define/public (pretty-print)
                        name)
                      (define/public (set-snip! a-snip)
                        (set! in-snip a-snip))
                      (define/override (own-caret own?)
                        (println "owned"))
                      (define/override (on-focus own?)
                        (println "focused"))                      
                      (define/override (on-default-char event)
                        (let* ([key-code (send event get-key-code)]
                               [etree ed-tree]
                               [stree sn-tree]
                               [no-children? (< (length (sub-list-at-pos etree pos)) 2)]
                               [toggle-border (λ () (send (sub-at-pos stree pos) show-border (not (send (sub-at-pos stree pos) border-visible?))))]
                               [move-to (λ (move)
                                          (toggle-border)
                                          (set! pos (move pos))
                                          (toggle-border))]
                               [get-caret (λ ()
                                            (send this own-caret (cons 0 0))
                                            (send (sub-at-pos ed-tree (position-parent pos)) set-caret-owner (sub-at-pos sn-tree pos) 'global))])                          
                          (when (not (equal? key-code 'release))
                            (println key-code)
                            (println name)
                            
                            (match key-code
                              ['release void]
                              [#\g (get-caret)]
                              [#\i (send (sub-at-pos etree (position-parent pos)) set-caret-owner this 'global)]
                              [#\r (println (send (send this get-editor) get-focus-snip))] ; for testing
                              [#\t (println (send (send this get-editor) find-first-snip))] ; for testing
                              [#\e (send (sub-at-pos stree pos) change-color "yellow")]
                              [#\q (send (sub-at-pos etree pos) select-all)]
                              [#\y ; delete node at pos, set pos to parent
                               (send (sub-at-pos etree (position-parent pos)) release-snip (sub-at-pos stree pos))
                               (set! ed-tree (delete-node ed-tree pos))
                               (set! sn-tree (delete-node sn-tree pos))
                               (set! pos (position-parent pos))]
                              [#\u ; insert new child
                               (send (sub-at-pos etree pos) insert (make-object my-editor-snip% (new text%)))]

                              [#\d (move-to position-next)]
                              [#\a (move-to position-prev)]
                              [#\w (if (<= (length pos) 1)
                                       (println "no")
                                       (move-to position-parent))]
                              [#\s (if no-children?
                                       (println "no")
                                       (move-to position-first-child))])
                            (println pos)
                            #; (build-gui testcode-1)
                            ; rebuilding interface every time -- works for now speedwise
                            )
                          
                          #;(super on-char dc x y edx edy event)))))




(define my-editor-snip% (class editor-snip%
                          (init parent-ed)

                          (define parent-editor parent-ed)
                          (define children '())
                          (define border-color "blue")

                          (define/public (pretty-print)
                            parent-editor)
                          
                          (super-new [with-border? #f])

                          (define/public (change-color color)
                            (set! border-color color))

                          (define/override (draw dc x y left top right bottom dx dy draw-caret)
                            (send dc set-brush "green" 'solid)
                            (send dc set-pen border-color 1 'solid)
                            (define bottom-x (box 10))
                            (define bottom-y (box 10))

                            (send parent-editor get-snip-location this bottom-x bottom-y #t)                            
                            (send dc draw-rectangle (+ x 1) (+ y 1) (unbox bottom-x) (unbox bottom-y))
                            
                            (super draw dc x y left top right bottom dx dy draw-caret))))



; -----------------------------


(define my-board (new my-text-ed% [parent "none"][name '(0)]))
(send my-canvas
      set-editor my-board)
(send my-frame
      show #t)
(send my-board
      insert "top")

;-----------------------------

; tree initialization functions

(define (make-ed-tree code [parent my-board] [position '()])
  (let ([ed (new my-text-ed% [parent parent] [name position])])
    (if (list? code)        
        `(,ed ,@(map (λ (node pos) (make-ed-tree node ed (append position `(,pos)))) code (range 0 (length code))))              
        (begin (send ed insert (~v code))
               `(,ed)))))


(define (make-sn-tree ed-tree)
  (let ([sn (new my-editor-snip% [editor (first ed-tree)] [parent-ed (send (first ed-tree) get-parent)])])
    `(,sn ,@(map (make-sn-tree _) (rest ed-tree)))))


(define (insert-snips sn-tree ed-tree)
  (if (list? sn-tree)
      (begin (map (λ (sn) (send (first ed-tree) insert (first sn))) (rest sn-tree))
             (map insert-snips sn-tree ed-tree))
      '()))


(define (set-snips sn-tree ed-tree)
  (if (list? sn-tree)
      (map set-snips sn-tree ed-tree)
      (begin (send ed-tree set-snip! sn-tree)
             (send ed-tree pretty-print))))


(define (print-sn-tree sn-tree)
  (if (list? sn-tree)
      (map print-sn-tree sn-tree)
      (send sn-tree pretty-print)))





;-----------------------------


(define (build-gui code)
  (define (make-ed-tree code [parent my-board] [position '()])
    (let ([ed (new my-text-ed% [parent parent] [name position])])
      (if (list? code)        
          `(,ed ,@(map (λ (node pos) (make-ed-tree node ed (append position `(,pos)))) code (range 0 (length code))))              
          (begin (send ed insert (~v code))
                 `(,ed)))))
  (define (make-sn-tree ed-tree)
    (let ([sn (new my-editor-snip% [editor (first ed-tree)] [parent-ed (send (first ed-tree) get-parent)])])
      `(,sn ,@(map (make-sn-tree _) (rest ed-tree)))))
  (define (insert-snips sn-tree ed-tree)
    (if (list? sn-tree)
        (begin (map (λ (sn) (send (first ed-tree) insert (first sn))) (rest sn-tree))
               (map insert-snips sn-tree ed-tree))
        '()))
  (define (set-snips sn-tree ed-tree)
    (if (list? sn-tree)
        (map set-snips sn-tree ed-tree)
        (begin (send ed-tree set-snip! sn-tree)
               (send ed-tree pretty-print))))
  (define my-board (new my-text-ed% [parent "none"][name '(0)]))
  (send my-canvas
        set-editor my-board)
  (define ed-tree (make-ed-tree code))
  (define sn-tree (make-sn-tree ed-tree))
  (insert-snips sn-tree ed-tree)
  (set-snips sn-tree ed-tree)
  (send my-board insert (first sn-tree))
  (send (sub-at-pos sn-tree pos) show-border (not (send (sub-at-pos sn-tree pos) border-visible?)))
  (send (sub-at-pos ed-tree (position-parent pos)) set-caret-owner (sub-at-pos sn-tree pos) 'global)
  (println ed-tree)
  (println sn-tree)
  )


(define testcode-2 '(alp bea gma (flf pop) ((brk grl) (zwl (nkp flp)))))
(define testcode-3 '(234523453245 23452345 2345 (flf gdfsgdf) ((brk 2345345) (3453245 (nkp flp)))))


;-----------------------------
; algorthmically mapping your extant program to the minimal program displaying some bug or conceptual issue you are having or want to highlight
; for peer commentary, self-reflection, etc.


; structure data
(define testcode-1 '(alp bea gma (flf pop) ((brk grl) (zwl (nkp flp)))))


; initialize tree

(define ed-tree (make-ed-tree testcode-1))
(define sn-tree (make-sn-tree ed-tree))
(insert-snips sn-tree ed-tree)
(send my-board insert (first sn-tree))

(set-snips sn-tree ed-tree)

(print-sn-tree sn-tree)

(send (sub-at-pos sn-tree pos) show-border (not (send (sub-at-pos sn-tree pos) border-visible?)))
(send (sub-at-pos ed-tree (position-parent pos)) set-caret-owner (sub-at-pos sn-tree pos) 'global)

;ed-tree
;sn-tree


#;(build-gui testcode-3)

#;(define (loop [ls '("blah")])
    (build-gui ls)
    (read)
    (loop `(,ls ,ls)))
#;(loop)


;(println (sub-at-pos sn-tree pos))
;(println (sub-at-pos ed-tree (position-parent pos)))


;(send an-editor clear) → void?
; Deletes the currently selected items.











