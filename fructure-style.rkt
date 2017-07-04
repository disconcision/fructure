#lang racket

(provide lookup-style
         apply-style!)

; stylesheet ------------------------------------------

(define stylesheet '(("default"
                      (default
                        (format horizontal)
                        (background-color (color 255 255 255))
                        (text-color (color 128 128 128))
                        (border-style none)
                        (border-color (color 255 255 255))))
                     
                     ("unidentified"
                      (unidentified
                       (background-color (color 237 177 77))
                       (text-color (color 128 128 128))
                       (format horizontal)
                       (border-style none)
                       (border-color (color 255 255 255))))
                     
                     ("selector"
                      (wrapper
                       (background-color (color 124 252 0))
                       (border-style square-brackets)
                       (border-color (color 255 255 255) #;parent-form))
                      (head
                       (background-color (color 124 252 0) #;wrapper-bkg)
                       (text-color (color 0 0 0))
                       (border-style square-brackets)
                       (border-color (color 124 252 0) #;wrapper-bkg)))
                                      
                     ("define"
                      (wrapper
                       (format indent)
                       (background-color (color 71 60 99))
                       (border-style square-brackets)
                       (border-color (color 255 255 255) #;parent-form))
                      (head
                       (background-color (color 71 60 99) #;wrapper-bkg)
                       (text-color (color 211 196 253))
                       (border-style square-brackets)
                       (border-color (color 71 60 99) #;wrapper-bkg))
                      (name
                       (background-color (color 132 255 251))
                       (text-color (color 45 156 188))
                       (border-style square-brackets)
                       (border-color (color 71 60 99) #;wrapper-bkg))
                      (fn-wrapper
                       (background-color (color 71 60 99) #;wrapper-bkg)
                       (border-style square-brackets)
                       (border-color (color 71 60 99) #;wrapper-bkg))
                      (var
                       (background-color (color 119 57 99))
                       (text-color (color 253 218 219))
                       (border-style square-brackets)
                       (border-color (color 71 60 99) #;wrapper-bkg)) 
                      (child))
                     
                     ("send"
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
                     
                     ("let"
                      (wrapper
                       (background-color (color 98 59 99))
                       (format indent)
                       (border-style square-brackets)
                       (border-color (color 255 255 255) #;parent))
                      (head
                       (background-color (color 98 59 99))
                       (text-color (color 239 165 241))
                       (border-style square-brackets)
                       (border-color (color 98 59 99) #;wrapper-bkg))
                      (inits-wrapper
                       (background-color (color 98 59 99) #;wrapper-bkg)
                       (format vertical)
                       (border-style square-brackets)
                       (border-color (color 132 255 251)))
                      (pair-wrapper
                       (background-color (color 98 59 99) #;wrapper-bkg)
                       (format horizontal)
                       (border-style square-brackets)
                       (border-color (color 98 59 99)))                      
                      (name
                       (background-color (color 132 255 251))
                       (text-color (color 45 156 188))
                       (border-style square-brackets)
                       (border-color (color 98 59 99) #;wrapper-bkg))
                      #;(expr-for-let
                         (background-color (color 132 255 251))
                         (text-color (color 45 156 188))
                         (border-style square-brackets)
                         (border-color (color 71 60 99) #;parent))                    
                      (child))

                     ("if"
                      (wrapper
                       (background-color (color 65 160 130))
                       (format indent)
                       (border-style square-brackets)
                       (border-color (color 71 60 99) #;parent-form-bkg))
                      (head
                       (background-color (color 65 160 130))
                       (text-color (color 132 243 223))
                       (border-style square-brackets)
                       (border-color (color 49 175 135))))

                     ("begin"
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
                     
                     ("new"
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
                     
                     ("atom"
                      (atom
                       (background-color (color 119 57 99))
                       (text-color (color 211 196 253))
                       (border-style square-brackets)
                       (border-color (color 255 255 255))))
                     
                     ("function"
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

(define (lookup-style fruct-name fruct-type)
  (let ([result (rest (assoc fruct-type (rest (assoc fruct-name stylesheet))))])
    (if result result (println "style lookup error"))))


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
        (second (assoc property (lookup-style "default" 'default))))))


; old style fns --------------------------------------

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