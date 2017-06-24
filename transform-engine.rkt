#lang racket

(require racket/gui/base)
(require fancy-app)
(require "utility-fns.rkt")

(provide selector
         simple-select
         simple-deselect
         update)

(provide forms
         insert-form)

(provide pos-to-sel
         sel-to-pos
         obj-at-pos)

(provide atomic?
         proper-list?
         tree-depth)

; -------------------------------------------------------

(define-match-expander atom
  (syntax-rules ()
    [(atom <name>)
     (? (compose not pair?) <name>)]))

#; (define-syntax define/↦
     (syntax-rules (↦)
       [(define/↦ transform [pattern ↦ result] ...)
        (define/match (transform source) [(pattern) result] ...
          [(_) (if (list? source)
                   (map transform source)
                   source)])]))

(require (rename-in racket (#%app call)))
(define-syntax #%app
  (syntax-rules (↦ ↓ ⇐ ∘ ≡)
    [(#%app f ⇐ g)
     (compose f g)]
    [(#%app f ∘ g)
     (compose f g)]
    [[#%app pattern ≡]
     (match-lambda
       [`pattern #true]
       [_ #false])]
    [[#%app pattern ↦ result]
     (#%app [pattern ↦ result])]
    [(#%app [pattern ↦ result] ...)
     (letrec ([transform (match-lambda
                           [`pattern `result] ...
                           [a #:when (not (pair? a)) a]
                           [ls (map transform ls)])])
       transform)]
    [(#%app f-expr arg-expr ...) (call f-expr arg-expr ...)]))

(define-syntax-rule (↓ [pattern ↦ result] ...)
  ([pattern ↦ result] ...)) ; explicit fallthrough annotation

(define ∘ compose)
(define ⇐ compose)


; -------------------------------------------------------

(define selector '▹)

(define simple-select
  [,a ↦ (▹ ,a)])

(define simple-deselect
  [(▹ ,a ...) ↦ (,@a)])

(define (update source input)
  (let ([transform (match input
                     [#\space identity]
                     [#\e #;#\s first-child]
                     [#\z last-child]
                     [#\q #;#\w parent]
                     [#\d next-escape]
                     [#\a prev-escape]
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


; simple nav --------------------------------------------

(define first-child
  [(▹ (,a ,b ...)) ↦ ((▹ ,a) ,@b)])

(define last-child
  [(▹ (,a ... ,b)) ↦ (,@a (▹ ,b))])

(define parent
  [(,a ... (▹ ,b ...) ,c ...) ↦ (▹ (,@a ,@b ,@c))])

(define next-sibling-wrap
  (↓ [(,a ... (▹ ,b) ,c ,d ...) ↦ (,@a ,b (▹ ,c) ,@d)]
     [(,a ,b ... (▹ ,c)) ↦ ((▹ ,a) ,@b ,c)]))

(define prev-sibling-wrap
  (↓ [(,a ... ,b (▹ ,c) ,d ...) ↦ (,@a (▹ ,b) ,c ,@d)]
     [((▹ ,a) ,b ... ,c) ↦ (,a ,@b (▹ ,c))]))


; escape nav --------------------------------------------
; disadvantage: next compose prev != identity

(define next-escape
  (↓ [(,a ... (▹ ,b) ,c ,d ...) ↦ (,@a ,b (▹ ,c) ,@d)]
     [(,x ... (,a ... (▹ ,b)) ,y ,z ...) ↦ (,@x (,@a ,b) (▹ ,y) ,@z)]
     [(,s ... (,x ... (,a ... (▹ ,b))) ,r ,t ...) ↦ (,@s (,@x (,@a ,b)) (▹ ,r) ,@t)]))

(define prev-escape
  (↓ [(,a ... ,b (▹ ,c) ,d ...) ↦ (,@a (▹ ,b) ,c ,@d)]
     [(,x ... ,y ((▹ ,a) ,b ...) ,z ...) ↦ (,@x (▹ ,y) (,a ,@b) ,@z)]
     [(,s ... ,r (((▹ ,a) ,b ...) ,z ...) ,t ...) ↦ (,@s (▹ ,r) ((,a ,@b) ,@z) ,@t)]))

; atomic nav --------------------------------------------


(define/match (first-contained-atom source)
  [(`(▹ ,(atom a))) `(▹ ,a)]
  [(`(▹ ,ls)) (first-contained-atom-inner ls)]
  [((atom a)) a] 
  [(ls) (map first-contained-atom ls)])

(define/match (first-contained-atom-inner source)
  [((atom a)) `(▹ ,a)]
  [(`(,a ,b ...)) `(,(first-contained-atom-inner a) ,@b)])

(define/match (next-atom source)
  [(`(,a ... (▹ ,b) ,c ,d ...)) (println "1")`(,@a ,b ,(first-contained-atom `(▹ ,c)) ,@d)]
  [(`(,x ... (,a ... (▹ ,b)) ,y ...)) (println "2") (next-atom `(,@x (▹ (,@a ,b)) ,@y))]
  [((atom a)) (println "3") a] 
  [(ls) (println "4") (map next-atom ls)])

; note: templates that are dynamic: when update the num of args in define/match
; should also update the num of args in the patterns in each pattern-result pair

#; (first-contained-atom '(▹ ((9) 8)))

#; (first-contained-atom '(1 2 (▹ (((((9) 7) 8) 5) 6))))


#; (next-atom '((▹ 7)))
#; (next-atom '((▹ 7) 8))
#; (next-atom '(((▹ 7)) 8))
#; (next-atom '((6 (▹ 7)) 8))
(next-atom '((((▹ 7))) 8))

(define next-atomic
  (↓ [(,a ... (▹ ,b) ,c ,d ...) ↦ ,(if (list? c)
                                       `(,@a ,b ((▹ ,(first c)) ,@(rest c)) ,@d)
                                       `(,@a ,b (▹ ,c) ,@d))]
     [(,x ... (,a ... (▹ ,b)) ,y ,z ...) ↦ ,(if (list? y)
                                                `(,@x (,@a ,b) ((▹ ,(first y)) ,@(rest y)) ,@z)
                                                `(,@x (,@a ,b) (▹ ,y) ,@z))]
     [(,s ... (,x ... (,a ... (▹ ,b))) ,r ,t ...) ↦ ,(if (list? r)
                                                         `(,@s (,@x (,@a ,b)) ((▹ ,(first r)) ,@(rest r)) ,@t)
                                                         `(,@s (,@x (,@a ,b)) (▹ ,r) ,@t))]))

; works sometimes, waaay too hacky, need to actually recurse
; get next-escape
; if selected is atomic, return
; if selected is list, recurse, kind-of?

; with \\\ pattern (and macro-style ...):
#; (define next-atomic
     (↓ [(,a ... (▹ ,b) ,(⋱ (? atomic? c)) ,d ...) ↦ (,a ... ,b ,(⋱ (▹ ,c)) ,d ...)]
        [((⋱ (,a ... (▹ ,b))) ,(⋱ (? atomic? y)) ,z ...) ↦  ((⋱ (,a ... ,b)) ,(⋱ (▹ ,y)) ,z ...)]))

; with above plus implicit pattern variables
#; (define next-atomic
     (↓ [(... (▹ ,a) ,(⋱ (? atomic? b)) ...) ↦ (... ,a ,(⋱ (▹ ,b)) ...)]
        [((⋱ (... (▹ ,a))) ,(⋱ (? atomic? b)) ...) ↦  ((⋱ (... ,a)) ,(⋱ (▹ ,b)) ...)]))

; simple transforms -------------------------------------

(define delete
  [(,a ... (▹ ,b ...) ,c ...) ↦ (▹ (,@a ,@c ))])

(define insert-child-r
  [(▹ (,a ...)) ↦ (,@a (▹ (☺)))])

(define insert-child-l
  [(▹ (,a ...)) ↦ ((▹ (☺)) ,@a)])

(define new-sibling-r
  [(,a ... (▹ (,b ...)) ,c ...) ↦ (,@a ,@b (▹ (☺)) ,@c)])

(define new-sibling-l
  [(,a ... (▹ (,b ...)) ,c ...) ↦ (,@a (▹ (☺)) ,@b ,@c)])

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


; -------------------------------------------------------

(module+ test (require rackunit)

  ; selection

  (check-equal? (simple-deselect '((((▹ 1)))))
                '((((1)))))
  
  ; movement
  (check-equal? (first-child '(▹ (0 1 2 3)))
                '((▹ 0) 1 2 3))
  (check-equal? (last-child '(▹ (0 1 2 3)))
                '(0 1 2 (▹ 3)))
  (check-equal? (parent '(0 1 (▹ 2) 3))
                '(▹ (0 1 2 3)))
  (check-equal? (next-sibling-wrap '(0 1 (▹ 2) 3))
                '(0 1 2 (▹ 3)))
  (check-equal? (prev-sibling-wrap '((▹ 0) 1 2 3))
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
  (check-equal? ((wrap ∘ insert-child-l) '(▹ (a b)))
                '((▹ ((☺))) a b)))



; mathy transforms --------------------------------------

(define comm
  [(▹ (,op ,a ,b)) ↦ (▹ (,op ,b ,a))])

(define assoc
  [(▹ (,op ,a (,op ,b ,c))) ↦ (▹ (,op (,op ,a ,b) ,c))])


; form inserts ------------------------------------------
; todo: generate directly from grammar in docs

(define forms #hash(("define" . (define name expr))
                    ("define ()" . (define (name variable ⋯) expr))
                    ("begin" . (begin expr expr ⋯))
                    ("λ" . (λ (variable ⋯) expr))
                    ("let" . (let ([name expr] ⋯) expr))
                    ("letrec" . (letrec ([name expr] ⋯) expr))
                    ("cond" . (cond [expr expr] ⋯ [expr expr]))
                    ("quote" . (quote expr))
                    ("unquote" . (unquote expr))
                    ("match" . (match expr [pattern expr] ⋯))
                    ("if" . (if expr expr expr))
                    ("map" . (map fn ls ⋯))))

(define forms+ #hash(("define" . (define (▹ /name/) /expr/)) ; how about this notation?
                     ("λ" . (λ ((▹ variable) ⋯) expr))
                     ("define ()" . (define ((▹ name) variable ⋯) expr))
                     ("let" . (let ([(▹ name) expr] ⋯) expr))
                     ("letrec" . (letrec ([(▹ name) expr] ⋯) expr))
                     ("begin" . (begin (▹ expr) expr ⋯))
                     ("if" . (if (▹ expr) expr expr))
                     ("cond" . (cond [(▹ expr) expr] ⋯ [expr expr]))
                     ("match" . (match (▹ expr) [pattern expr] ⋯))
                     ("quote" . (quote (▹ expr)))
                     ("unquote" . (unquote (▹ expr)))
                     ("map" . (map (▹ fn) ls ⋯))))

(define (insert-form name)
  [(▹ ,a) ↦ ,(hash-ref forms+ name)])

#; ((insert-form "define") '(0 1 2 (▹ 3)))


; idea: slight tint to functions depending on arity (also struct size?)
; just enough to make it obvious when nearby variables/fns are much more light/heavyweight

; -------------------------------------------------------

(define original-source '(1 (2 21 22) 3))

(define input-stream '(#\s #\d #\d #\z #\w #\a #\w))

(define source (simple-select original-source))

; uncomment to run through a simple test scenario
#; (loop source input-stream)


; utility fns - trees -----------------------------------

(define atomic?
  (compose not pair?))

(define proper-list?
  [(,x ,xs ...) ≡])

#; (define proper-list?
     (conjoin list? (compose not empty?)))

(define (tree-depth source)
  (if (atomic? source)
      1
      (add1 (apply max (map tree-depth source)))))


; utility fns -------------------------------------------

(define (tree-update tree pos fn)
  (if (empty? pos)
      (fn tree)
      (list-update tree (first pos) (λ (a) (tree-update a (rest pos) fn)))))

(define (pos-to-sel tree pos)
  (tree-update tree pos simple-select))

(define/match (sel-to-pos sel-tree [pos '()])
  [(_ _) #:when (not (list? sel-tree)) #f]
  [(`(▹ ,a) _) '()]
  [(_ _) (let ([result (filter identity
                               (map (λ (sub num)
                                      (let ([a (sel-to-pos sub pos)])
                                        (if a `(,num ,@a) #f)))
                                    sel-tree
                                    (range 0 (length sel-tree))))])
           (if (empty? result) #f (first result)))])

(define/match (obj-at-pos obj-tree pos)
  [(_ `()) (first obj-tree #;(third obj-tree))] ; use third if you don't want the selector itself
  [(_ `(,a . ,as)) (obj-at-pos (list-ref (rest obj-tree) a) as)])


; do proper tests!!
#; (sel-to-pos '((▹ "sdf") 0 1 3))




; (⋱ pat) pattern e.g. (⋱ (S a)) matches first found a just like my macro type above
; (let inits (⋱ (S a))) matches most local let to (S a)
; (let (⋯ [a init]) (⋱ (S a))) - given a, find most local binding
; (⋱ *(S a)) matches all occurences of (S a) (multiple return values) ??
; version of above for above above is complicated??

; default: given a pattern, return first result
; make: given a pattern, return all results
; maximal/minimal ? results for depth patterns?
; (backwards from S-match or forwards from root)



