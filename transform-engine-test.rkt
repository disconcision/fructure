#lang racket

(require racket/gui/base)
(require fancy-app)

(define-syntax define/↦
  (syntax-rules (↦)
    [(define/↦ transform [pattern ↦ result] ...)
     (define/match (transform source) [(pattern) result] ...
       [(_) (if (list? source)
                (map transform source)
                source)])]))

(require (rename-in racket (#%app call)))
(define-syntax #%app
  (syntax-rules (↦ ↓)
    [[#%app pattern ↦ result] (#%app [pattern ↦ result])]
    [(#%app [pattern ↦ result] ...) (letrec ([transform (λ (source)
                                                          (match source
                                                            [pattern result] ...
                                                            [_ (if (list? source)
                                                                   (map transform source)
                                                                   source)]))])
                                      transform)]
    [(#%app f-expr arg-expr ...) (call f-expr arg-expr ...)]))

(define-syntax-rule (↓ [pattern ↦ result] ...)
  ([pattern ↦ result] ...))

; -------------------------------------------------------

(define (simple-select ls) `(S ,ls))

(define (update source input)
  (let ([transform (match input
                     [#\s first-child]
                     [#\z last-child]
                     [#\w parent]
                     [#\a next-sibling]
                     [#\d prev-sibling]
                     [#\i delete]
                     [#\o insert-child-r]
                     [#\u insert-child-l]
                     [#\l new-sibling-r]
                     [#\j new-sibling-l]
                     [#\k wrap])])
    (transform source)))

(define (loop source stream)
  (unless (empty? stream)
    (let* ([input (first stream)]
           [new-source (update source input)])
      (map display `(,input " : " ,new-source)) (newline)
      (loop new-source (rest stream)))))


; move selector -----------------------------------------

(define first-child
  [`(S (,a ,b ...)) ↦ `((S ,a) ,@b)])

(define last-child
  [`(S (,a ... ,b)) ↦ `(,@a (S ,b))])

(define parent
  [`(,a ... (S ,b ...) ,c ...) ↦ `(S (,@a ,@b ,@c))])

(define next-sibling
  (↓ [`(,a ... (S ,b) ,c ,d ...) ↦ `(,@a ,b (S ,c) ,@d)]
     [`(,a ,b ... (S ,c)) ↦ `((S ,a) ,@b ,c)]))

(define prev-sibling
  (↓ [`(,a ... ,b (S ,c) ,d ...) ↦ `(,@a (S ,b) ,c ,@d)]
     [`((S ,a) ,b ... ,c) ↦ `(,a ,@b (S ,c))]))


; simple transforms -------------------------------------

(define delete
  [`(,a ... (S ,b ...) ,c ...) ↦ `(S (,@a ,@c ))])

(define insert-child-r
  [`(S (,a ...)) ↦ `(,@a (S (new)))])

(define insert-child-l
  [`(S (,a ...)) ↦ `((S (new)) ,@a)])

(define new-sibling-r
  [`(,a ... (S (,b ...)) ,c ...) ↦ `(,@a ,@b (S (new)) ,@c)])

(define new-sibling-l
  [`(,a ... (S (,b ...)) ,c ...) ↦ `(,@a (S (new)) ,@b ,@c)])

(define wrap
  [`(S (,a ...)) ↦ `(S ((,@a)))])


; secondary transforms -----------------------------------

(define push-sibling-r
  (↓ [`(,a ... (S ,b ...) ,c ,d ...) ↦ `(,@a ,c (S ,@b) ,@d)]
     [`(,a ... (S ,b ...)) ↦ `((S ,@b) ,@a)]))

(define push-sibling-l
  (↓ [`(,a ... ,b (S ,c ...) ,d ...) ↦ `(,@a (S ,@c) ,b ,@d)]
     [`((S ,a ...) ,b ...) ↦ `(,@b (S ,@a))]))

(define merge
  [`(S (,a ...) (,b ...)) ↦ `(S (,@a ,@b))])

(define pop/splice
  [`(,a ... (S (,b ...)) ,c ...) ↦ `(S (,@a ,@b ,@c))])

(define slurp-r
  [`(,a ... (S (,b ...)) ,c ,d ...) ↦ `(,@a (S (,@b ,c)) ,@d)])

(define slurp-l
  [`(,a ... ,b (S (,c ...)) ,d ...) ↦ `(,@a (S (,b ,@c)) ,@d)])

(define barf-r
  [`(,a ... (S (,b ... ,c)) ,d ...) ↦ `(,@a (S (,@b)) ,c ,@d)])

(define barf-l
  [`(,a ... (S (,b ,c ...)) ,d ...) ↦ `(,@a ,b (S (,@c)) ,@d)])


; mathy transforms --------------------------------------

(define comm
  [`(S (,op ,a ,b)) ↦ `(S (,op ,b ,a))])

(define assoc
  [`(S (,op ,a (,op ,b ,c))) ↦ `(S (,op (,op ,a ,b) ,c))])


; -------------------------------------------------------

(module+ test (require rackunit)
  
  ; movement
  (check-equal? (first-child '(S (0 1 2 3)))
                '((S 0) 1 2 3))
  (check-equal? (last-child '(S (0 1 2 3)))
                '(0 1 2 (S 3)))
  (check-equal? (parent '(0 1 (S 2) 3))
                '(S (0 1 2 3)))
  (check-equal? (next-sibling '(0 1 (S 2) 3))
                '(0 1 2 (S 3)))
  (check-equal? (prev-sibling '((S 0) 1 2 3))
                '(0 1 2 (S 3)))

  ; simple
  (check-equal? (delete '("a" "b" (S "c") "d"))
                '(S ("a" "b" "d")))
  (check-equal? (insert-child-r '(S ("a" "b")))
                '("a" "b" (S (new))))
  (check-equal? (insert-child-l '(S ("a" "b")))
                '((S (new)) "a" "b"))
  (check-equal? (new-sibling-r '("a" (S ("b")) "d"))
                '("a" "b" (S (new)) "d"))
  (check-equal? (new-sibling-l '("a" (S ("b")) "d"))
                '("a" (S (new)) "b" "d"))
  (check-equal? (wrap '(S ("a" "b")))
                '(S (("a" "b"))))

  ; secondary
  (check-equal? (push-sibling-r '(1 (S 2) 3 4))
                '(1 3 (S 2) 4))
  (check-equal? (push-sibling-l '(1 (S 2) 3 4))
                '((S 2) 1 3 4))
  (check-equal? (merge '(S (1 2) (3 4)))
                '(S (1 2 3 4)))
  (check-equal? (pop/splice '(1 (S (2 21 22)) 4))
                '(S (1 2 21 22 4)))
  (check-equal? (slurp-r '((S (1 2)) 3 4))
                '((S (1 2 3)) 4))  
  (check-equal? (slurp-l '(1 (S (2 3)) 4))
                '((S (1 2 3)) 4))
  (check-equal? (barf-r '((S (1 2 3)) 4))
                '((S (1 2)) 3 4))  
  (check-equal? (barf-l '((S (1 2 3)) 4))
                '(1 (S (2 3)) 4))  
  
  ; composition
  (check-equal? ((compose wrap insert-child-l) '(S (a b)))
                '((S ((new))) a b)))


; -------------------------------------------------------

(define original-source '(1 (2 21 22) 3))

(define input-stream '(#\s #\d #\d #\z #\w #\a #\w))

(define source (simple-select original-source))

(loop source input-stream)



; app-get override for [ ↦ ] and ([ ↦ ] ... )
; make function to remove selector from tree
; make function to insert selector at provided position
; utility fns

(define (pos-to-sel tree pos)
  'tree-with-selection)

(define (sel-to-tree sel-tree)
  'tree-without-selection)

(define (sel-to-pos sel-tree)
  'position-of-selection)


; make: given a pattern, return first result
; given a pattern, return all results
; maximal/minimal ? results for depth patterns?
; (backwards from S-match or forwards from root)



