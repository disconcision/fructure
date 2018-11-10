#lang racket


(require (only-in racket/base
                  (/ div)))

(provide / anno div)


(require racket/hash)
(require racket/struct)

(struct anno (attributes stx)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     ; pretty printer for annotated syntax
     ; TODO: why does this recurse to doing write
     ; instead of print for children?
     (make-constructor-style-printer
      (lambda (obj) '/)
      (lambda (obj)
        (define nicer-attrs
          (for/list ([(k v) (anno-attributes obj)])
            `[,k ,v]))
        `(,nicer-attrs ,(anno-stx obj)))))])

#;(define (helper stx)
    (match stx
      [(? anno?) stx]
      [(? (negate list?)) stx]
      [_ (map helper stx)]))

(define-match-expander /
  (λ (stx)
    (syntax-case stx (▹)
      [(/ <whatever> ... (▹ <stx>))
       #'(/ (▹ _) <whatever> ... <stx>)]

      [(/ (<ats> <pat>) ... <stx>)
       #'(/ (<ats> <pat>) ... _ <stx>)]

      [(/ <a/> <stx>)
       ; need to check that <a/> is a symbol
       ; need this seperate case because empty hash-table
       ; matches only empty hash-tables
       #'`(p/ ,<a/> ,<stx>)]

      [(/ (<ats> <pat>) ... <a/> <stx>)
       ; need to check that <a/> is a symbol
       ; but should be guarded by previous clause
       #'`(p/ ,(and <a/> (hash-table ('<ats> <pat>) ...))
              ,<stx>)]
      
      [(/ <bare-ats> ... <a/> <stx>)
       ; need to check that all are symbols
       ; but can initially self-enforce and rely on above to guard
       ; (this is probably a mistake)
       ; don't need to quote attr, will get quoted in above clause
       #' (/ (<bare-ats> <bare-ats>) ... <a/> <stx>)] 
      ))
  (λ (stx)
    (syntax-case stx (▹)
      [(/ <whatever> ... (▹ <stx>))
       ; 0 is dummy
       #'(/ (▹ 0) <whatever> ... <stx>)]

      [(/ (<atrs> <pat>) ... <stx>)
       #'(/ (<atrs> <pat>) ... (hash) <stx>)]
      
      [(/ (<ats> <pat>) ... <a/> <stx>)

       #'`(p/ ,(hash-union <a/> (hash (~@ '<ats> <pat>) ...)
                           #:combine/key (λ (k v v1) v1))
              ,<stx>)]

      [(/ <bare-ats> ... <a/> <stx>)
       ; don't need to quote attr, will get quoted in above clause
       #'(/ (<bare-ats> <bare-ats>) ... <a/> <stx>)] 
      )))



(module+ test
  (require rackunit)

  #;(check-equal? (match (/ 0)
                    [(/ anns/ 0)
                     (/ anns/ 0)])
                  (/ 0))

  (check-equal? (match (/ (in-scope '()) 0)
                  [(/ in-scope top-rest/
                      0)
                   (/ in-scope top-rest/
                      0)])
                (/ (in-scope '()) 0))

  (check-equal? (match (/ (in-scope '()) 0)
                  [(/ anns/ 0)
                   (/ anns/ 0)])
                (/ (in-scope '()) 0))

  ; this doesnt work... the ' does something but what
  (check-equal? (match (/ [sort: 'expr] '⊙)
                  [(/ sort: a/ '⊙)
                   (/ a/ 0)])
                (/ [sort: 'expr] 0))

  (check-equal? (match (/ [sort: 'expr] '⊙)
                  [(/ [sort: 'expr] a/ '⊙)
                   (/ a/ 0)])
                (/ [sort: 'expr] 0))

  (check-equal? (match (/ [sort: 'expr]
                          [whatever: 'dude] '⊙)
                  [(/ [sort: 'expr] a/ '⊙)
                   (/ a/ `(app ,(/ [sort: 'expr] '⊙)
                               ,(/ [sort: 'expr] '⊙)))])
                (/ [sort: 'expr]
                   [whatever: 'dude]
                   `(app ,(/ [sort: 'expr] '⊙)
                         ,(/ [sort: 'expr] '⊙))))

  (check-equal? (match (/ [sort: 'expr] '⊙)
                  [(/ [sort: 'expr] a/ '⊙)
                   (/ a/ `(λ ,(/ [sort: 'params]
                                 `(,(/ [sort: 'pat]
                                       `(id ,(/ [sort: 'char] '⊙)))))
                            (/ (sort: 'expr) '⊙)))])
                (/ [sort: 'expr] `(λ ,(/ [sort: 'params]
                                         `(,(/ [sort: 'pat]
                                               `(id ,(/ [sort: 'char] '⊙)))))
                                    (/ (sort: 'expr) '⊙))))

  )



#;'([(/ [sort: expr] a/ ⊙)
     (/ a/ 0)])
#;'([(/ [sort: expr] a/ ⊙)
     (/ a/ (app (/ [sort: expr] ⊙)
                (/ [sort: expr] ⊙)))])
#;'([(/ [sort: expr] a/ ⊙)
     (/ a/ (λ (/ [sort: params]
                 `(,(/ [sort: pat]
                       `(id ,(/ [sort: char] ⊙)))))
             (/ (sort: expr) ⊙)))])

#| need to codify selectability pattern
             to start: only selectables are sort: exprs|#

#; (struct anno (attributes syntax))
; can we get structs to print in errors via a custom pretty-print?

; add / as match-lambda for parens. possibly as λ for ident
          
          
#; (/ whatever ... (▹ <stx>))
#; (/ ('▹ _) whatever ... <stx>)

; unary / in pattern: wildcard or empty?
; unary / in template: empty attributes
#; (/ <stx>)
#; (/ _ <stx>)
#; (anno (hash-table ))

#; (/ <a/> <stx>)
#; (special-case-of-below)
          
#; (/ <bare-atrs> ... <a/> <stx>) ; if all bare, no pair: must be a a/
#; (/ ('<bare-atrs> <bare-atrs>) ... <a/> <stx>)
          
#; (/ <bare-atrs> ... (<atrs> <pat>) ..1 <a/> <stx>)
#; (/ ('<bare-atrs> <bare-atrs>) ... (<atrs> <pat>) ..1 <a/> <stx>)
          
#; (/ <bare-atrs> ... (<atrs> <pat>) ..1 <stx>)
#; (/ ('<bare-atrs> <bare-atrs>) ... (<atrs> <pat>) ..1 <stx>)
          
#; (/ (<atrs> <pat>) ..1 <a/> <stx>)
#; (anno (and <a/> (hash-table ('<atrs> <pat>) ...))
         <stx>)
          
#; (/ (<atrs> <pat>) ..1 <stx>)
#; (anno (hash-table ('<atrs> <pat>) ...)
         <stx>)
          
