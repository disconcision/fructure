#lang racket

(provide lookup-style
         apply-style!)

; stylesheet ------------------------------------------

(require (for-syntax racket/match racket/list racket/syntax racket/function))
(begin-for-syntax
 
  (define L1-form-names '(if begin define let lambda send new env kit meta))
  (define L1-terminal-names '(name name-new name-ref literal))
  (define L1-sort-names '(expr name))
  (define L1-affo-names '(▹ selector))

  (define atom? (compose not pair?))
  (define transpose (curry apply map list))

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

  
  (define-values  (†quote
                   †quasiquote
                   †unquote
                   †unquote-splice) (values ((curry list) 'quote)
                                            ((curry list) 'quasiquote)
                                            ((curry list) 'unquote)
                                            ((curry list) 'unquote-splicing)))


  (define (make-pattern pattern)
    (match pattern
      [(? (λ (x) (member x L1-form-names)))
       pattern]
      [(? (λ (x) (member x L1-sort-names)))
       (†unquote (gensym))]
      ['◇ '◇]
      [`(ooo ,(app make-pattern new-pat))
       `(ooo ,new-pat)]
      ; the above is sort of a hack. the test for first not equalling unquote detects when new-pat is actually a list of pats
      ; but maybe not robustly? to clarify, when the first is unquote we're assuming it's just a quoted, unquoted variable name
      [(? list? ls)
       (map make-pattern ls)]))

  
  (define form-list->pattern
    (compose (curry map-rec redotdotdot)
             make-pattern
             (curry map-rec undotdotdot)))

  #;(println (form-list->pattern '(if expr ... (◇ expr) expr ...)))
  
  (define stylesheet->parse-stylesheet
    (match-lambda
      [`((,form-pat ,style) ...)
       (transpose `(,(map form-list->pattern form-pat) ,style))])))


(define-syntax (form+stylesheet->style stx)
  (syntax-case stx ()
    [(_ <form> <stylesheet>)  
     (let ([proc-sheet (stylesheet->parse-stylesheet (eval (syntax->datum #'<stylesheet>)))])
       (println proc-sheet)
       (with-syntax* ([((<pat> <style>) ...) (datum->syntax #'<source> proc-sheet)])
         #'(match <form>
             [`<pat> `<style>] ...)))]))


(define (lookup-style form)
  (form+stylesheet->style form '(((◇ (if expr expr expr))
                                  ((background-color (color 65 160 130))
                                   (format indent)
                                   (border-style square-brackets)
                                   (border-color (color 71 60 99) #;parent-form-bkg)))
                     
                                 (((◇ if) expr expr expr)
                                  ((background-color (color 65 160 130))
                                   (text-color (color 132 243 223))
                                   (border-style square-brackets)
                                   (border-color (color 49 175 135))))

                                 
                                 ((◇ (define (name name ...) expr ...))
                                  ((format indent)
                                   (background-color (color 71 60 99))
                                   (border-style square-brackets)
                                   (border-color (color 255 255 255) #;parent-form)))
                                 
                                 (((◇ define) (name name ...) expr ...)
                                  ((background-color (color 71 60 99) #;wrapper-bkg)
                                   (text-color (color 211 196 253))
                                   (border-style square-brackets)
                                   (border-color (color 71 60 99) #;wrapper-bkg)))
                                 
                                 ((define ((◇ name) name ...) expr ...)
                                  ((background-color (color 132 255 251))
                                   (text-color (color 45 156 188))
                                   (border-style square-brackets)
                                   (border-color (color 71 60 99) #;wrapper-bkg)))
                                 
                                 ((define (◇ (name name ...)) expr ...)
                                  ((background-color (color 71 60 99) #;wrapper-bkg)
                                   (border-style square-brackets)
                                   (border-color (color 71 60 99) #;wrapper-bkg)))
                                 
                                 ((define (name name ... (◇ name) name ...) expr ...)
                                  ((background-color (color 119 57 99))
                                   (text-color (color 253 218 219))
                                   (border-style square-brackets)
                                   (border-color (color 71 60 99) #;wrapper-bkg))) 


                                 ((◇ (let ([name expr] ...) expr ...))
                                  ((background-color (color 98 59 99))
                                   (format indent)
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))

                                 (((◇ let) ([name expr] ...) expr ...)
                                  ((background-color (color 98 59 99))
                                   (text-color (color 239 165 241))
                                   (border-style square-brackets)
                                   (border-color (parent background-color) #;wrapper-bkg)))

                                 ((let (◇ ([name expr] ...)) expr ...)
                                  ((background-color (color 98 59 99) #;wrapper-bkg)
                                   (format vertical)
                                   (border-style square-brackets)
                                   (border-color (color 132 255 251))))

                                 ((let ([name expr] ... (◇ [name expr]) [name expr] ...) expr ...)
                                  ((background-color (color 98 59 99) #;wrapper-bkg)
                                   (format horizontal)
                                   (border-style square-brackets)
                                   (border-color (color 98 59 99))))

                                 ((let ([name expr] ... [(◇ name) expr] [name expr] ...) expr ...)
                                  ((background-color (color 132 255 251))
                                   (text-color (color 45 156 188))
                                   (border-style square-brackets)
                                   (border-color (color 98 59 99) #;wrapper-bkg)))

                                 
                                 (expr
                                  ((format horizontal)
                                   (background-color (color 255 255 255))
                                   (text-color (color 128 128 128))
                                   (border-style none)
                                   (border-color (color 255 255 255)))))))




#;(define stylesheet '((env
                        (wrapper
                         (background-color (color 0 0 0))
                         (format vertical)
                         (border-style square-brackets)
                         (border-color (color 255 255 255) #;parent-form))
                        (head
                         (background-color (color 0 0 0) #;wrapper-bkg)
                         (text-color (color 255 255 255))
                         (border-style square-brackets)
                         (border-color (color 0 0 0) #;wrapper-bkg)))

                       (meta
                        (wrapper
                         (background-color (color 0 0 0))
                         (format vertical)
                         (border-style square-brackets)
                         (border-color (color 255 255 255) #;parent-form))
                        (head
                         (background-color (color 0 0 0) #;wrapper-bkg)
                         (text-color (color 255 255 255))
                         (border-style square-brackets)
                         (border-color (color 0 0 0) #;wrapper-bkg)))

                       (kit
                        (wrapper
                         (background-color (color 0 0 0))
                         (format vertical)
                         (border-style square-brackets)
                         (border-color (color 255 255 255) #;parent-form))
                        (head
                         (background-color (color 0 0 0) #;wrapper-bkg)
                         (text-color (color 255 255 255))
                         (border-style square-brackets)
                         (border-color (color 0 0 0) #;wrapper-bkg)))

                       (selector
                        (wrapper
                         (background-color (color 124 252 0))
                         (border-style square-brackets)
                         (border-color (color 255 255 255) #;parent-form))
                        (head
                         (background-color (color 124 252 0) #;wrapper-bkg)
                         (text-color (color 0 0 0))
                         (border-style square-brackets)
                         (border-color (color 124 252 0) #;wrapper-bkg)))
                                                  
                       (send
                        (wrapper
                         (format horizontal)
                         (background-color (color 119 57 99) #;parent-form)
                         (border-style square-brackets)
                         (border-color (color 255 255 255) #;parent-form))
                        (head
                         (background-color (color 213 135 29))
                         (text-color (color 255 254 212))
                         (border-style square-brackets)
                         (border-color (color 255 255 255) #;parent-form))
                        (target
                         (background-color (color 252 218 167))
                         (text-color (color 213 135 29))
                         (border-style square-brackets)
                         (border-color (color 255 255 255) #;parent-form))
                        (method
                         (background-color (color 247 0 114) #;parent)
                         (text-color (color 253 238 189))
                         (border-style square-brackets)
                         (border-color (color 255 255 255))))
                     

                       (begin
                         (wrapper
                          (background-color (color 130 65 160 ))
                          (format indent)
                          (border-style square-brackets)
                          (border-color (color 71 60 99) #;parent-form-bkg))
                         (head
                          (background-color (color 130 65 160 ))
                          (text-color (color 132 243 223))
                          (border-style square-brackets)
                          (border-color (color 49 175 135))))
                     
                       (new
                        (wrapper
                         (background-color (color 71 60 99) #;parent-form-bkg)
                         (format horizontal)
                         (border-style square-brackets)
                         (border-color (color 71 60 99) #;parent-form-bkg))
                        (head
                         (background-color (color 184 118 27))
                         (text-color (color 255 254 212))
                         (border-style square-brackets)
                         (border-color (color 71 60 99) #;wrapper-bkg))
                        (obj-type
                         (background-color (color 254 245 168) #;wrapper-bkg)
                         (text-color (color 213 135 29))
                         (border-style square-brackets)
                         (border-color (color 250 185 84)))
                        (pair-wrapper
                         #; (background-color (color 98 59 99) #;wrapper-bkg)
                         #; (format horizontal)
                         #; (border-style square-brackets)
                         #; (border-color (color 98 59 99)))                      
                        (name
                         (text-color (color 252 217 128))
                         (border-style square-brackets)
                         (border-color (color 250 185 84) #;wrapper-bkg)))
                     
                       (atom
                        (atom
                         (background-color (color 119 57 99))
                         (text-color (color 211 196 253))
                         (border-style square-brackets)
                         (border-color (color 255 255 255))))
                     
                       (function
                        (wrapper
                         (background-color (color 71 60 99) #;parent-form-bkg)
                         (border-style square-brackets)
                         (border-color (color 255 255 255) #;parent-form-bkg))
                        (head
                         (background-color (color 247 0 114) #;parent)
                         (text-color (color 253 238 189))
                         (border-style square-brackets)
                         (border-color (color 255 255 255))))))






; style fns ------------------------------------------

(define (apply-style! style sn ed)
  (let ([get (λ (property) (get-property style property))])
    (send sn set-background-color (get  'background-color))
    (send sn set-border-color (get 'border-color))
    (send sn set-border-style (get 'border-style))
    (send ed set-format (get 'format))
    (send ed set-text-color (get 'text-color))
    (send sn set-margins 4 2 2 2)))


(define (get-property style property)
  (let ([result (assoc property style)])
    (if result
        (second result)
        (second (assoc property (lookup-style 'expr)))))) ; this lookup-style argument is a hack


; old style fns --------------------------------------

#;(define (lookup-style fruct-name fruct-type)
    '((format horizontal)
      (background-color (color 247 0 114) #;parent)
      (text-color (color 253 238 189))
      (border-style square-brackets)
      (border-color (color 255 255 255)))
    #;(let ([result (rest (assoc fruct-type (rest (assoc fruct-name stylesheet))))])
        (if result result (println "style lookup error"))))

#; (define (make-style position code)
     `(my-style
       (background-color ,(match code
                            [`(,(== selector) ,a ...) '(color 200 200 0)]
                            [_ `(color ,(modulo (exact-round (* 255 (sqrt (/ (length position) (tree-depth original-source))))) 256)
                                       60
                                       100)]))
       (format ,(match code
                  [`((,(== selector) ,a) ,ls ...) (second (third (make-style position `(,a ,@ls))))] ; hacky
                  [`(,form ,ls ...) #:when (member form '(let let* if begin define for)) 'indent]              
                  [`([,a ...] ...) 'vertical]
                  [_ 'horizontal]))))


#; (define/match (set-style! style sn ed)
     [(`(,name (background-color ,color)
               (format ,format)) _ _)
      (begin (send sn set-background-color color)
             (send sn set-border-color '(color 30 205 90))
             (send sn set-border-style 'square-brackets)
             (send ed set-format format)
             (send ed set-text-color '(color 255 255 210))
             (send sn set-margins 4 2 2 2))])