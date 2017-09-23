#lang racket

(provide lookup-style
         apply-style!
         (struct-out gui))

; stylesheet ------------------------------------------

(require (for-syntax racket/match racket/list racket/syntax racket/function
                     "fructure-language.rkt"
                     "fructure-utility.rkt"))

(begin-for-syntax
 
  (define (make-style-pattern pattern)
    (match pattern
      [(? (disjoin form-name? affo-name?))
       pattern]
      [(? sort-name?)
       (// (gensym))]
      ['◇ '◇]
      [`(ooo ,(app make-style-pattern new-pat))
       `(ooo ,new-pat)]
      ; the above is sort of a hack. the test for first not equalling unquote detects when new-pat is actually a list of pats
      ; but maybe not robustly? to clarify, when the first is unquote we're assuming it's just a quoted, unquoted variable name
      [(? list? ls)
       (map make-style-pattern ls)]))

  
  (define form-list->pattern
    (compose (curry map-rec redotdotdot)
             make-style-pattern
             (curry map-rec undotdotdot)))

  
  (define stylesheet->parse-stylesheet
    (match-lambda
      [`((,form-pat ,style) ...)
       (transpose `(,(map form-list->pattern form-pat) ,style))])))


(define-syntax (form+stylesheet->style stx)
  (syntax-case stx ()
    [(_ <form> <stylesheet>)  
     (let ([proc-sheet (stylesheet->parse-stylesheet (eval (syntax->datum #'<stylesheet>)))])
       (with-syntax* ([((<pat> <style>) ...) (datum->syntax #'<source> proc-sheet)])
         #'(match <form>
             [`<pat> `<style>] ...)))]))


(define (lookup-style form)
  (form+stylesheet->style form '(((◇ (▹ hole))
                                  ((background-color (color 168 255 0))
                                   (format horizontal)
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))
                                 
                                 (((◇ ▹) hole)
                                  ((background-color (parent background-color))
                                   (text-color (color 0 0 0))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))


                                 ((◇ (c▹ hole hole))
                                  ((background-color (color 255 105 180))
                                   (format horizontal)
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))

                                 (((◇ c▹) hole hole)
                                  ((background-color (parent background-color))
                                   (text-color (color 0 0 0))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))


                                 ((◇ (s▹ hole hole))
                                  ((background-color (color 48 244 228))
                                   (format horizontal)
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))

                                 (((◇ s▹) hole hole)
                                  ((background-color (parent background-color))
                                   (text-color (color 0 0 0))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))

                                 
                                 ((◇ (▹▹ hole))
                                  ((background-color (parent background-color))
                                   (format horizontal)
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))
                                 
                                 (((◇ ▹▹) hole)
                                  ((background-color (parent background-color))
                                   (text-color (color 48 244 228))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))
                                 

                                 ((◇ (c▹▹ hole))
                                  ((background-color (parent background-color))
                                   (format horizontal)
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))
                                 
                                 (((◇ c▹▹) hole)
                                  ((background-color (parent background-color))
                                   (text-color (color 0 0 255))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))


                                 ((◇ (⋈ name hole))
                                  ((background-color (parent background-color))
                                   (format horizontal)
                                   (border-style full-box)
                                   (border-color (color 0 255 0))))

                                 (((◇ ⋈) name hole)
                                  ((background-color (parent background-color))
                                   (text-size 0)
                                   (text-color (color 0 255 0))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))

                                 ((⋈ (◇ name) hole)
                                  ((background-color (parent background-color))
                                   (text-size 8)
                                   (text-align center)
                                   (text-color (color 0 255 0))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))

                                 
                                 ((◇ (if expr expr expr))
                                  ((background-color (color 65 160 130))
                                   (format indent)
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))
                     
                                 (((◇ if) expr expr expr)
                                  ((background-color (parent background-color))
                                   (text-color (color 132 243 223))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))


                                 ((◇ (cond [expr expr] ...))
                                  ((background-color (color 85 140 160))
                                   (format (indent-after 1))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))

                                 (((◇ cond) [expr expr] ...)
                                  ((background-color (parent background-color))
                                   (text-color (color 152 233 243))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))

                                 ((cond [expr expr] ... (◇ [expr expr]) [expr expr] ...)
                                  ((background-color (parent background-color))
                                   (border-style square-brackets)
                                   (border-color (color 152 233 243))))


                                 ((◇ (define name expr))
                                  ((format indent)
                                   (background-color (color 71 60 99))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))


                                 (((◇ define) name expr)
                                  ((background-color (parent background-color))
                                   (text-color (color 211 196 253))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))

                                 ((define (◇ name) expr)
                                  ((background-color (color 132 255 251))
                                   (text-color (color 45 156 188))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))


                                 ((◇ (local (expr ...) expr ...))
                                  ((format indent)
                                   (background-color (color 31 40 69))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))

                                 (((◇ local) (expr ...) expr ...)
                                  ((background-color (parent background-color))
                                   (background-color (color 91 110 149))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))
                                 
                                 
                                 ((◇ (define (name name ...) expr ...))
                                  ((format indent)
                                   (background-color (color 71 60 99))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))
                                 
                                 (((◇ define) (name name ...) expr ...)
                                  ((background-color (parent background-color))
                                   (text-color (color 211 196 253))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))
                                 
                                 ((define ((◇ name) name ...) expr ...)
                                  ((background-color (color 132 255 251))
                                   (text-color (color 45 156 188))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))
                                 
                                 ((define (◇ (name name ...)) expr ...)
                                  ((background-color (parent background-color))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))
                                 
                                 ((define (name name ... (◇ name) name ...) expr ...)
                                  ((background-color (color 119 57 99))
                                   (text-color (color 253 218 219))
                                   (border-style square-brackets)
                                   (border-color (parent background-color)))) 


                                 #;((◇ (let ([name expr] ...) expr ...))
                                    ((background-color (color 98 59 99))
                                     (format indent)
                                     (border-style square-brackets)
                                     (border-color (parent background-color))))

                                 ((◇ (let ([name expr] ...) expr ...))
                                  ((background-color (color 98 59 99))
                                   (format indent)
                                   (margins 80 20 2 2)

                                   ))

                                 (((◇ let) ([name expr] ...) expr ...)
                                  ((background-color (parent background-color))
                                   (text-color (color 239 165 241))

                                   (text-size 8)
                                   (text-family symbol)
                                   (italic? #true)
                                   (bold? #true)
                                   (smooth? #true)
                                   
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))

                                 ((let (◇ ([name expr] ...)) expr ...)
                                  ((background-color (parent background-color))
                                   (format vertical)
                                   (border-style square-brackets)
                                   (border-color (color 132 255 251))))

                                 ((let ([name expr] ... (◇ [name expr]) [name expr] ...) expr ...)
                                  ((background-color (parent background-color))
                                   (format horizontal)
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))

                                 ((let ([name expr] ... [(◇ name) expr] [name expr] ...) expr ...)
                                  ((background-color (color 132 255 251))
                                   (text-color (color 45 156 188))
                                   (border-style square-brackets)
                                   (border-color (parent background-color))))

                                 
                                 ; danger! since we're not checking sorts, this is a fallthrough!!
                                 #; ((◇ (free ...))
                                     ((background-color (color 128 128 128))
                                      (text-color (color 0 0 0))
                                      (border-style both-square-brackets)
                                      (border-color (color 255 0 0))))

                                 
                                 ((◇ (expr ...))
                                  ((background-color (parent background-color))
                                   (border-style both-square-brackets)
                                   (border-color (color 100 200 255))
                                   (format horizontal)
                                   (text-size 24)
                                   (text-family base)
                                   (italic? #true)))

                                 ; this doesn't work
                                 (((◇ expr) expr ...)
                                  ((background-color (color 30 67 82))
                                   (border-style both-square-brackets)
                                   (border-color (parent background-color))
                                   (format horizontal)
                                   (text-color (color 100 200 0))
                                   (text-size 24)
                                   (text-family base)
                                   (italic? #true)))

                                 
                                 (expr
                                  ((visible #true)
                                   (background-color (parent background-color))
                                   (border-style none)
                                   (border-color (color 200 32 200))
                                   (format horizontal)
                                   (margins 4 2 2 2)
                                   (text-color (color 247 224 23))
                                   (text-color-add)
                                   (text-color-mult)
                                   (text-size 24)
                                   (text-family base)
                                   (text-align top) ; doesn't work for some reason
                                   (italic? #false)
                                   (bold? #false)
                                   (smooth? #false))))))





; style fns ------------------------------------------

(struct gui (sn ed parent-ed))


(define (get-property style property)
  (let ([result (assoc property style)])
    (if result
        (second result)
        (second (assoc property (lookup-style 'expr)))))) ; this lookup-style argument is a hack


(define (apply-style! style g)
  (match-let ([(gui sn ed parent-ed) g]
              [get (λ (property) (get-property style property))])
    
    (send sn set-background-color (get 'background-color))
    (send sn set-border-color (get 'border-color))
    (send sn set-border-style (get 'border-style))
    (send sn set-margins 4 2 2 2)

    (send ed set-format (get 'format))
    
    #;(when #true #;(get 'visible) (send parent-ed insert sn))
    #;(send ed set-text-color (get 'text-color))
    
    (send ed set-text-style
          (get 'text-color)
          #; (get 'text-color-add)
          #; (get 'text-color-mult)
          (get 'text-size)
          (get 'text-family)
          (get 'text-align)
          (get 'italic?)
          (get 'bold?)
          (get 'smooth?))))




; old style fns --------------------------------------


#; (define (make-style position code)
     `(my-style
       (background-color ,(match code
                            [`(,(== selector) ,a ...) '(color 200 200 0)]
                            [_ `(color ,(modulo (exact-round (* 255 (sqrt (/ (length position) (tree-depth original-source))))) 256)
                                       60
                                       100)]))))



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
