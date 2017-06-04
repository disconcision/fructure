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


; -------------------------------------------------------

(define (simple-select ls) `(S ,ls))

(define (update src input)
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
                     [#\k wrap-single])])
    (transform src)))

(define (loop source stream)
  (unless (empty? stream)
    (let* ([input (first stream)]
           [new-source (update source input)])
      (map display `(,input " : " ,new-source)) (newline)
      (loop new-source (rest stream)))))


; move selector -----------------------------------------

(define/↦ first-child
  [`(S (,a ,b ...)) ↦ `((S ,a) ,@b)])

(define/↦ last-child
  [`(S (,a ... ,b)) ↦ `(,@a (S ,b))])

(define/↦ parent
  [`(,a ... (S ,b ...) ,c ...) ↦ `(S (,@a ,@b ,@c))])

(define/↦ next-sibling
  [`(,a ... (S ,b) ,c ,d ...) ↦ `(,@a ,b (S ,c) ,@d)]
  [`(,a ,b ... (S ,c)) ↦ `((S ,a) ,@b ,c)])

(define/↦ prev-sibling
  [`(,a ... ,b (S ,c) ,d ...) ↦ `(,@a (S ,b) ,c ,@d)]
  [`((S ,a) ,b ... ,c) ↦ `(,a ,@b (S ,c))])


; simple transforms -------------------------------------

(define/↦ delete
  [`(,a ... (S ,b ...) ,c ...) ↦ `(S (,@a ,@c ))])

(define/↦ insert-child-r
  [`(S (,a ...)) ↦ `(,@a (S (new)))])

(define/↦ insert-child-l
  [`(S (,a ...)) ↦ `((S (new)) ,@a)])

(define/↦ new-sibling-r
  [`(,a ... (S (,b ...)) ,c ...) ↦ `(,@a ,@b (S (new)) ,@c)])

(define/↦ new-sibling-l
  [`(,a ... (S (,b ...)) ,c ...) ↦ `(,@a (S (new)) ,@b ,@c)])

(define/↦ wrap-single
  [`(S (,a ...)) ↦ `(S ((,@a)))])


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
  (check-equal? (wrap-single '(S ("a" "b")))
                '(S (("a" "b"))))

  ; composition
  (check-equal? ((compose wrap-single insert-child-l) '(S (a b)))
                '((S ((new))) a b)))


; -------------------------------------------------------

(define original-source '(1 (2 21 22) 3))

(define input-stream '(#\s #\d #\d #\z #\w #\a #\w))

(define source (simple-select original-source))

(loop source input-stream)




; make function to remove selector from tree
; make function to insert selector at provided position






