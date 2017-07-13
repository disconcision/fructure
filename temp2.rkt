#lang racket

(require (for-syntax racket/match racket/list racket/syntax))
(begin-for-syntax
  (define (better-holes source)
    (match source
      [(list '... a) 5]
      [(? list?) (map better-holes (undotdotdot-into source))]
      ['if 'if]
      ['expr (list 'unquote (gensym))]))

  (define (undotdotdot source)
    (match source
      [(? (compose not pair?)) source]
      [(list a ... b '... c ...) `(,@(undotdotdot a) (... ,b) ,@c)]
      [_ source]))

  (define (undotdotdot-into source)
    (match (undotdotdot source)
      [`(... ,a) `(... ,(undotdotdot-into a))]
      [(? list? ls) (map undotdotdot-into ls)]
      [(? (compose not pair?) a) a]))

  (define (redotdotdot source)
  (match source
    [`(,a ... (... ,b) ,c ...) `(,@(redotdotdot a) ,b ... ,@c)]
    [_ source]))

(define (redotdotdot-into source)
    (match (redotdotdot source)
      #;[`(... ,a) `(... ,(redotdotdot-into a))]
      [(? list? ls) (map redotdotdot-into ls)]
      [(? (compose not pair?) a) a]))
  
  (define (make-holes source)
    (match source
      [(? list?) (map make-holes source)]
      ['if 'if]
      ['... '...]
      ['expr (list 'unquote (gensym))]))

  (define (tandem-holes pattern template)
    #;(println (undotdotdot-into pattern))
    (let ([clean-pat (undotdotdot-into pattern)]
          [clean-temp (undotdotdot-into template)])
      (match* (clean-pat clean-temp)
        [(`(... expr) _) (let ([new-var (gensym)])
                           `((... ,new-var) ,(list 'unquote-splicing '(make-list (length new-var) `expr))))]
        [((? list?) _) (map tandem-holes clean-pat clean-temp)]
        [('if _) '(if if)]
        [('expr _) (let ([new-var (gensym)])
                     `(,(list 'unquote new-var) expr))]))))

(define-syntax (get-pat-macro stx)
  (syntax-case stx ()
    [(_ <source> <form>)
     (let ([pat-temp (tandem-holes (eval (syntax->datum #'<form>)) (eval (syntax->datum #'<form>)))])
       (with-syntax* ([<new-pat> (datum->syntax #'<source> (redotdotdot (first pat-temp)))]
                      [<new-temp> (datum->syntax #'<source> (redotdotdot (second pat-temp)))])
         #'(match <source>
             [`<new-pat> <new-temp>])))]))




(get-pat-macro '(if 1 2 3) '(if expr ...))







(require (for-syntax racket/match))
(begin-for-syntax
  #;(define (better-holes source)
      (match source
        [(list '... a) 0]
        [(? list?) (map better-holes source)]
        ['if 'if]
        ['expr (list 'unquote (gensym))])))

#|

(define (better-holes source)
    (match source
      [(list '... a) 5]
      [(? list?) (map better-holes (undotdotdot-into source))]
      ['if 'if]
      ['expr (list 'unquote (gensym))]))

(define (undotdotdot source)
  (match source
    [(? (compose not pair?)) source]
    [(list a ... b '... c ...) `(,@(undotdotdot a) (... ,b) ,@c)]
    [_ source]))

(define (undotdotdot-into source)
  (println source)
  (match (undotdotdot source)
    [`(... ,a) `(... ,(undotdotdot-into a))]
    [(? list? ls) (map undotdotdot-into ls)]
    [(? (compose not pair?) a) a]))

|#
