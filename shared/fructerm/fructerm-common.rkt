#lang racket

(require memoize)
(provide match-lambda?
         multi-split
         multi-containment
         first-containment)

(define-syntax-rule (match-lambda? <pat>)
  (match-lambda [<pat> #t] [_ #f]))

; splits ls into segments of lengths
(define (multi-split ls lengths)
  (unless (equal? (length ls)
                  (apply + lengths))
    (error "length of list doesn't partition"))
  (define-values (actual extra)
    (for/fold ([acc '()]
               [ls ls])
              ([l lengths])
      (define-values (this others)
        (split-at ls l))
      (values (cons this acc) others)))
  (reverse actual))

(define/memo (multi-containment match? xs (until? (λ (x) #f)))
  ; this returns a list of two elements
  ; the first element is the multi-holed context as a fn
  ; the second is a list of the contents of those holes
  (cond
    [(match? xs)
     (list (λ (x) x) `(,xs))]
    [(or (not (list? xs)) (until? xs))
     (list (λ () xs) `())]
    [else
     (define subpairs
       (for/list ([x xs])
         (multi-containment match? x until?)))
     (define subcontexts
       (map first subpairs))
     (define submatches
       (apply append (map second subpairs)))
     (define subcontext-arities
       (map procedure-arity subcontexts))
     (define (context-candidate . args)
       (for/list ([subfn subcontexts]
                  [arg-list (multi-split args
                                         subcontext-arities)])
         (apply subfn arg-list)))
     (define new-context
       (procedure-reduce-arity context-candidate
                               (apply + subcontext-arities)))
     (list new-context
           submatches)]))

(define/memo (first-containment match? xs (until? (λ (x) #f)))
  ; this returns a list of two elements
  ; the first element is a one-holed context as a fn
  ; the second is a one-element list of the content of that hole
  ; this currently is just a gloss for mult-containment
  ; it could be implemented more efficiently separately
  (match-define (list context matches)
    (multi-containment match? xs until?))
  (if (empty? matches)
      (list context matches)
      (list (λ (x) (apply context x (rest matches))) (list (first matches)))))


(module+ test
  (require rackunit)

  (check-equal? ((first (multi-containment (curry equal? 1) '(0 0 1 0 1))) 3 4)
                '(0 0 3 0 4))
  
  (check-equal? ((first (multi-containment (curry equal? 1) '(0 1 (0 1 (1 0)) 0 1) )) 3 4 5 6)
                '(0 3 (0 4 (5 0)) 0 6))

  (check-equal? ((first (multi-containment (curry equal? 1)
                                           '(0 1 (0 1 (1 0)) 0 1)
                                           (curry equal? '(1 0)))) 3 4 5)
                '(0 3 (0 4 (1 0)) 0 5))

  (check-equal? ((first (first-containment (curry equal? 1) '(0 0 1 0 1))) 3)
                '(0 0 3 0 1))

  (check-equal? ((first (first-containment (curry equal? 1) '(0 0 0 0 0))))
                '(0 0 0 0 0))


  )

