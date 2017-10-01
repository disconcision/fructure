#lang racket

(provide atom?
         proper-list?
         transpose
         map-rec
         undotdotdot
         redotdotdot
         eval-match-λ
         eval-match-?
         \\
         //
         /@)

(define atom? (compose not pair?))

(define proper-list? (conjoin list? (compose not empty?)))

(define transpose (curry apply map list))


; pre-applies fn into source tree
(define (map-rec fn source) 
  (match (fn source)
    [(? list? ls) (map (curry map-rec fn) ls)]
    [(? atom? a) a]))


; desugars _ ... into (ooo _)
(define/match (undotdotdot source)
  [((list a ... b '... c ...)) `(,@(undotdotdot a) (ooo ,b) ,@c)]
  [(_) source])

  
; resugars (ooo _) into _ ...
(define/match (redotdotdot source)
  [(`(,a ... (ooo ,b) ,c ...)) `(,@(redotdotdot a) ,b ... ,@c)]
  [(_) source])


; meta-quotation
(define-values (\\
                //
                /@) (values ((curry list) 'quasiquote)
                            ((curry list) 'unquote)
                            ((curry list) 'unquote-splicing)))



(define-namespace-anchor an)
(define ns (namespace-anchor->namespace an))
(define (eval-match-λ pat-tem)
  (match-let ([`(,pat ,tem) pat-tem])
    (eval `(match-lambda [,pat ,tem] [x x]) ns)))
(define (eval-match-? pat)
  (eval `(match-lambda [(and x ,pat) x] [_ #f]) ns))