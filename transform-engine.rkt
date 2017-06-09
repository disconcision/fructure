#lang racket

(require racket/gui/base)
(require fancy-app)

(provide selector)
(provide simple-select)
(provide update)

; -------------------------------------------------------

(define-syntax define/↦
  (syntax-rules (↦)
    [(define/↦ transform [pattern ↦ result] ...)
     (define/match (transform source) [(pattern) result] ...
       [(_) (if (list? source)
                (map transform source)
                source)])]))

(require (rename-in racket (#%app call)))
(define-syntax #%app
  (syntax-rules (↦ ↓ ⇒)
    [(#%app f ⇒ g)
     (compose f g)]
    [[#%app pattern ↦ result]
     (#%app [pattern ↦ result])]
    [(#%app [pattern ↦ result] ...)
     (letrec ([transform (λ (source)
                           (match source
                             [`pattern `result] ...
                             [_ (if (list? source)
                                    (map transform source)
                                    source)]))])
       transform)]
    [(#%app f-expr arg-expr ...) (call f-expr arg-expr ...)]))

(define-syntax-rule (↓ [pattern ↦ result] ...)
  ([pattern ↦ result] ...))


; -------------------------------------------------------

(define selector '▹)

(define simple-select [,a ↦ (▹ ,a)])

(define (update source input)
  (let ([transform (match input
                     [#\s first-child]
                     [#\z last-child]
                     [#\w parent]
                     [#\d next-sibling]
                     [#\a prev-sibling]
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
  [(▹ (,a ,b ...)) ↦ ((▹ ,a) ,@b)])

(define last-child
  [(▹ (,a ... ,b)) ↦ (,@a (▹ ,b))])

(define parent
  [(,a ... (▹ ,b ...) ,c ...) ↦ (▹ (,@a ,@b ,@c))])

(define next-sibling
  (↓ [(,a ... (▹ ,b) ,c ,d ...) ↦ (,@a ,b (▹ ,c) ,@d)]
     [(,a ,b ... (▹ ,c)) ↦ ((▹ ,a) ,@b ,c)]))

(define prev-sibling
  (↓ [(,a ... ,b (▹ ,c) ,d ...) ↦ (,@a (▹ ,b) ,c ,@d)]
     [((▹ ,a) ,b ... ,c) ↦ (,a ,@b (▹ ,c))]))


; simple transforms -------------------------------------

(define delete
  [(,a ... (▹ ,b ...) ,c ...) ↦ (▹ (,@a ,@c ))])

(define insert-child-r
  [(▹ (,a ...)) ↦ (,@a (▹ (new)))])

(define insert-child-l
  [(▹ (,a ...)) ↦ ((▹ (new)) ,@a)])

(define new-sibling-r
  [(,a ... (▹ (,b ...)) ,c ...) ↦ (,@a ,@b (▹ (new)) ,@c)])

(define new-sibling-l
  [(,a ... (▹ (,b ...)) ,c ...) ↦ (,@a (▹ (new)) ,@b ,@c)])

(define wrap
  [(▹ (,a ...)) ↦ (▹ ((,@a)))])


; secondary transforms -----------------------------------

(define push-sibling-r
  (↓ [(,a ... (▹ ,b ...) ,c ,d ...) ↦ (,@a ,c (▹ ,@b) ,@d)]
     [(,a ... (▹ ,b ...)) ↦ ((▹ ,@b) ,@a)]))

(define push-sibling-l
  (↓ [(,a ... ,b (▹ ,c ...) ,d ...) ↦ (,@a (▹ ,@c) ,b ,@d)]
     [((▹ ,a ...) ,b ...) ↦ (,@b (▹ ,@a))]))

(define merge
  [(▹ (,a ...) (,b ...)) ↦ (▹ (,@a ,@b))])

(define pop/splice
  [(,a ... (▹ (,b ...)) ,c ...) ↦ (▹ (,@a ,@b ,@c))])

(define slurp-r
  [(,a ... (▹ (,b ...)) ,c ,d ...) ↦ (,@a (▹ (,@b ,c)) ,@d)])

(define slurp-l
  [(,a ... ,b (▹ (,c ...)) ,d ...) ↦ (,@a (▹ (,b ,@c)) ,@d)])

(define barf-r
  [(,a ... (▹ (,b ... ,c)) ,d ...) ↦ (,@a (▹ (,@b)) ,c ,@d)])

(define barf-l
  [(,a ... (▹ (,b ,c ...)) ,d ...) ↦ (,@a ,b (▹ (,@c)) ,@d)])


; mathy transforms --------------------------------------

(define comm
  [(▹ (,op ,a ,b)) ↦ (▹ (,op ,b ,a))])

(define assoc
  [(▹ (,op ,a (,op ,b ,c))) ↦ (▹ (,op (,op ,a ,b) ,c))])


; -------------------------------------------------------

#;(module+ test (require rackunit)
  
  ; movement
  (check-equal? (first-child '(▹ (0 1 2 3)))
                '((▹ 0) 1 2 3))
  (check-equal? (last-child '(▹ (0 1 2 3)))
                '(0 1 2 (▹ 3)))
  (check-equal? (parent '(0 1 (▹ 2) 3))
                '(▹ (0 1 2 3)))
  (check-equal? (next-sibling '(0 1 (▹ 2) 3))
                '(0 1 2 (▹ 3)))
  (check-equal? (prev-sibling '((▹ 0) 1 2 3))
                '(0 1 2 (▹ 3)))

  ; simple
  (check-equal? (delete '("a" "b" (▹ "c") "d"))
                '(▹ ("a" "b" "d")))
  (check-equal? (insert-child-r '(▹ ("a" "b")))
                '("a" "b" (▹ (☺))))
  (check-equal? (insert-child-l '(▹ ("a" "b")))
                '((▹ (☺)) "a" "b"))
  (check-equal? (new-sibling-r '("a" (▹ ("b")) "d"))
                '("a" "b" (▹ (☺)) "d"))
  (check-equal? (new-sibling-l '("a" (▹ ("b")) "d"))
                '("a" (▹ (☺)) "b" "d"))
  (check-equal? (wrap '(▹ ("a" "b")))
                '(▹ (("a" "b"))))

  ; secondary
  (check-equal? (push-sibling-r '(1 (▹ 2) 3 4))
                '(1 3 (▹ 2) 4))
  (check-equal? (push-sibling-l '(1 (▹ 2) 3 4))
                '((▹ 2) 1 3 4))
  (check-equal? (merge '(▹ (1 2) (3 4)))
                '(▹ (1 2 3 4)))
  (check-equal? (pop/splice '(1 (▹ (2 21 22)) 3))
                '(▹ (1 2 21 22 3)))
  (check-equal? (slurp-r '((▹ (1 2)) 3 4))
                '((▹ (1 2 3)) 4))  
  (check-equal? (slurp-l '(1 (▹ (2 3)) 4))
                '((▹ (1 2 3)) 4))
  (check-equal? (barf-r '((▹ (1 2 3)) 4))
                '((▹ (1 2)) 3 4))  
  (check-equal? (barf-l '((▹ (1 2 3)) 4))
                '(1 (▹ (2 3)) 4))  
  
  ; composition
  (check-equal? ((wrap ⇒ insert-child-l) '(▹ (a b)))
                '((▹ ((new))) a b)))


; -------------------------------------------------------

(define original-source '(1 (2 21 22) 3))

(define input-stream '(#\s #\d #\d #\z #\w #\a #\w))

(define source (simple-select original-source))

; uncomment to run through a simple test scenario
#; (loop source input-stream)


; -------------------------------------------------------


; utility fns

(define (pos-to-sel tree pos)
  'tree-with-selection)

(define (sel-to-tree sel-tree)
  'tree-without-selection)

(define (sel-to-pos sel-tree)
  'position-of-selection)


; (⋱ pat) pattern e.g. (⋱ (S a)) matches first found a just like my macro type above
; (let inits (⋱ (S a))) matches most local let to (S a)
; (let (⋯ [a init]) (⋱ (S a))) - given a, find most local binding
; (⋱ *(S a)) matches all occurences of (S a) (multiple return values) ??
; version of above for above above is complicated??

; default: given a pattern, return first result
; make: given a pattern, return all results
; maximal/minimal ? results for depth patterns?
; (backwards from S-match or forwards from root)



