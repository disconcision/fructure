#lang racket

(require (for-syntax racket/match racket/list racket/syntax))
(begin-for-syntax 



  (define-syntax-rule (map-into source [<pattern> <payload> ...] ...)
    (letrec ([recursor (match-lambda
                         [<pattern> <payload> ...] ...
                         [(? list? x) (map recursor x)]
                         [(? (compose not pair?) x) x])])
      (recursor source)))

  (define (undotdotdot-into source)
    (map-into source
              [(? (compose not pair?)) source]
              [(list a ... b '... c ...) `(,@(undotdotdot-into a) (... ,b) ,@c)]
              [_ source]))
   
  (define (redotdotdot-into source)
    (map-into source
              [`(,a ... (... ,b) ,c ...) `(,@(redotdotdot-into a) ,b ... ,@c)]
              [_ source]))

    #;(define (undotdotdot source)
    (match source
      [(? (compose not pair?)) source]
      [(list a ... b '... c ...) `(,@(undotdotdot a) (... ,b) ,@c)]
      [_ source]))
    
  #;(define (undotdotdot-into source)
      (match (undotdotdot source)
        [`(... ,a) `(... ,(undotdotdot-into a))] ; is this necessary?
        [(? list? ls) (map undotdotdot-into ls)]
        [(? (compose not pair?) a) a]))

  #;(define (redotdotdot source)
    (match source
      [`(,a ... (... ,b) ,c ...) `(,@(redotdotdot a) ,b ... ,@c)]
      [_ source]))

  #;(define (redotdotdot-into source)
    (match (redotdotdot source)
      #; [`(... ,a) `(... ,(redotdotdot-into a))] ; is this (or something else) necessary?
      [(? list? ls) (map redotdotdot-into ls)]
      [(? (compose not pair?) a) a]))
  

  (define (tandem-holes pattern template)
    (match* (pattern template)
      [('expr _)
       (let ([new-var (gensym)])
         `(,(list 'unquote new-var) expr))]
      [(`(... expr) _)
       (let ([new-var (gensym)])
         `((... ,(list 'unquote new-var)) ,(list 'unquote-splicing `(make-list (length ,new-var) `expr))))]
      [('if _)
       '(if if)]
      [((? list?) _)
       (map tandem-holes pattern template)])))

(define-syntax (get-pat-macro stx)
  (syntax-case stx ()
    [(_ <source> <form>)
     (match-let* ([prepro (undotdotdot-into (eval (syntax->datum #'<form>)))]
                  [`(,pat ,temp) (map redotdotdot-into (apply map list (tandem-holes prepro prepro)))])
       (with-syntax* ([<new-pat> (datum->syntax #'<source> pat)]
                      [<new-temp> (datum->syntax #'<source> temp)])
         #'(match <source>
             [`<new-pat> `<new-temp>])))]))




(get-pat-macro '(if 1 2) '(if expr expr ...))






