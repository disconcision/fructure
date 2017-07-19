#lang racket

(require (for-syntax racket/match))
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
    (println source)
    (match (undotdotdot source)
      [`(... ,a) `(... ,(undotdotdot-into a))]
      [(? list? ls) (map undotdotdot-into ls)]
      [(? (compose not pair?) a) a]))
  
  (define (make-holes source)
    (match source
      [(? list?) (map make-holes source)]
      ['if 'if]
      ['... '...]
      ['expr (list 'unquote (gensym))])))

(define-syntax (get-pat-macro stx)
  (syntax-case stx ()
    [(_ <source> <form>)
     (with-syntax ([<new-pat> (datum->syntax #'<source> (make-holes (eval (syntax->datum #'<form>))))])
       #'(match <source>
           [`<new-pat> <form>]))]))



(get-pat-macro '(if 1 2 3) '(if expr expr ...))





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
