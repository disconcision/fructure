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






