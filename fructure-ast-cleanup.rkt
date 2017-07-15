#lang racket

(require lens/common)
(require lens/data/list)
(require "transform-engine.rkt")
(require "fructure-style.rkt")

; ----------------------------------------------------------------------------

(define atom? (compose not pair?))

(define proper-list? (conjoin list? (compose not empty?)))

(require (rename-in racket (#%app call)))
(define-syntax #%app
  (syntax-rules (⋱↦ ↓ ⇐ ∘ ≡)
    [[#%app pattern ≡]
     (match-lambda
       [`pattern #true]
       [_ #false])]
    [[#%app pattern ⋱↦ result]
     (#%app [pattern ⋱↦ result])]
    [(#%app [pattern ⋱↦ result] ...)
     (letrec ([transform (match-lambda
                           [`pattern `result] ...
                           [a #:when (not (pair? a)) a]
                           [ls (map transform ls)])])
       transform)]
    [(#%app f-expr arg-expr ...) (call f-expr arg-expr ...)]))


(define-syntax-rule (map-into source [<pattern> <payload> ...] ...)
  (letrec ([recursor (match-lambda
                       [<pattern> <payload> ...] ...
                       [(? list? x) (map recursor x)]
                       [(? atom? x) x])])
    (recursor source)))

(define-syntax-rule (map-into-fruct source [<pattern> <payload> ...] ...)
  (letrec ([recursor (match-lambda
                       [<pattern> <payload> ...] ...
                       [`(,x . ,xs) (map recursor xs)]
                       [(? atom? x) x])])
    (recursor source)))


; ----------------------------------------------------------------------------

(require (for-syntax racket/match racket/list racket/syntax))
(begin-for-syntax
  
  (define L1 '((atom (|| (name
                          literal))) ; attach predicates
               (expr (|| (if expr expr expr)
                         (begin expr ...)
                         (define (name name ...) expr ...)
                         (let ([name expr] ...) expr ...)
                         atom))))
  
  (define L1-form-names '(if begin define let send new env kit meta)) ; copy with stuff added
  (define L1-sort-names '(expr name)) ; copy
  (define L1-affo-names '(▹ selector)) ;copy

  (define atom? (compose not pair?)) ; copy

  (define (transpose x) (apply map list x))

  (define (map-rec fn source)
    (match (fn source)
      [(? list? ls) (map (λ (x) (map-rec fn x)) ls)]
      [(? atom? a) a]))

  (define (undotdotdot source)
    (match source
      [(list a ... b '... c ...) `(,@(undotdotdot a) (ooo ,b) ,@c)]
      [_ source]))
    
  (define (redotdotdot source)
    (match source
      [`(,a ... (ooo ,b) ,c ...) `(,@(redotdotdot a) ,b ... ,@c)]
      [_ source]))
  
  (define (make-parse-pair pattern)
    (match pattern
      [(? (λ (x) (member x L1-form-names)))
       `(,pattern ,pattern)]
      [(? (λ (x) (member x L1-sort-names)))
       `(,(list 'unquote (gensym)) ,pattern)]
      [`(ooo ,(app make-parse-pair `(,new-pat ,new-temp)))
       `((ooo ,new-pat) ,(list 'unquote-splicing `(make-list (length ,(list 'quasiquote (if (equal? 'unquote (first new-pat)) new-pat (first new-pat)))) ,(list 'quote new-temp))))]
      ; the above is sort of a hack. the test for first not equalling unquote detects when new-pat is actually a list of pats
      ; but maybe not robustly? to clarify, when the first is unquote we're assuming it's just a quoted, unquoted variable name
      [(? list? ls)
       (transpose (map make-parse-pair ls))]))

  (define (add-ignores source)
    (match source
      ['... '...]
      [(list 'unquote x) source]
      [(? symbol? x) (list 'unquote `(ignore-affo ,source))]
      [(? list?) (list 'unquote `(ignore-affo ,(map add-ignores source)))])))



(define-match-expander ignore-affo
  (syntax-rules ()
    [(ignore-affo <pat>)
     (app (λ (source) (match source
                        [`(,(? (λ (x) (member x L1-affo-names))) ,a) a]
                        [_ source])) `<pat>)]))



(define-syntax (get-pat-macro-list stx)
  (syntax-case stx ()
    [(_ <source> <forms>)
     (match-let* ([form-list (map-rec undotdotdot (eval (syntax->datum #'<forms>)))]
                  [parse-pairs (map make-parse-pair form-list)]
                  ; note that below app adds ignore-affo around whole pat as well as sub-components
                  [`((,(app add-ignores pat) ,temp) ...) (map (λ (x) (map-rec redotdotdot x)) parse-pairs)])
       (with-syntax* ([(<new-pat> ...) (datum->syntax #'<source> pat)]
                      [(<new-temp> ...) (datum->syntax #'<source> temp)])
         #'(match <source>
             #; [`(,(? (λ (x) (member x L1-affo-names)) affo-name) ,x) `(,affo-name expr)] ; no
             [`<new-pat> `<new-temp>] ...
             [(? atom? a) a])))])) ; atom case (hack)




; parsing --------------------------------------------------------------------

(define L1-sort-names '(atom hole expr))
(define L1-form-names '(if begin define let))
(define L1-terminal-names '(name name-ref literal))
(define L1-affo-names '(▹ selector)) ;copy

(define (get-form source)
  (get-pat-macro-list source '((if expr expr expr)
                               (begin expr ...)
                               (define (name name ...) expr ...)
                               (define name expr)
                               (let ([name expr] ...) expr ...)
                               (send expr expr expr ...)
                               (new expr [name expr] ...)
                               (env expr ...)
                               (kit expr ...)
                               (meta expr ...)
                               (expr expr ...))))


(define (sel◇ source) `(◇ ,source))

(define/match (get◇ source)
  [(`(◇ ,a)) a]
  [((? atom? a)) #f]
  [(_) (let ([result (filter identity (map get◇ source))])
         (if (empty? result) #f (first result)))])

(define (apply-ith i fn source)
  (lens-transform (list-ref-lens i) source fn))

(define (select◇-ith i)
  [(◇ (,ls ...)) ⋱↦ ,(apply-ith i sel◇ ls)])

(define (get-child-contexts temp src)
  (map (λ (i) ((select◇-ith i) temp)) (range 0 (length src))))


(define (parse source [ctx `(top (◇ expr))])
  (match source
    [`(,(? (λ (x) (member x L1-affo-names)) affo-name) ,x) 
     `(,(hash 'self `(,affo-name hole) 'context ctx) ,(hash 'self affo-name 'context `((◇ ,affo-name) hole)) ,(parse x ctx))]
    ; the above is a hack. it supports only single selections and simply passes the context along to the selectee
    [_
     (match (get◇ ctx)
       [(? (λ (x) (member x L1-terminal-names)))
        (hash 'self source 'context ctx)]
       [(? (λ (x) (member x L1-form-names)))
        (hash 'self source 'context ctx)]
       ['expr
        (let* ([form (get-form source)]
               [hash (hash 'self form 'context ctx)])
          (if (list? form)
              `(,hash ,@(map parse source (get-child-contexts `(◇ ,form) source)))
              hash))]
       [(? list?)
        `(,(hash 'self '() 'context ctx) ,@(map parse source (get-child-contexts ctx source)))])]))


(define/match (get-symbol fruct)
  [(`(,(hash-table ('self s)) ,ls ...)) s])


(define/match (project-symbol fruct)
  [(`(,x ,xs ...)) (map project-symbol xs)]
  [((hash-table ('self s))) s])



; testing ---------------------------------------------------------------------

(define test-src2 '(define (fn a) a (define (g q r) 2)))
(define test-src '(define (selector (fn a)) 7))

(get-pat-macro-list  '((selector let) (▹ ([f a][f a][k a][g a])) 4 4 4) '((if expr expr expr)
                                                                          (begin expr ...)
                                                                          (define (name name ...) expr ...)
                                                                          (let ([name expr] ...) expr ...)))


(pretty-print (parse test-src))
test-src
(project-symbol (parse test-src))
(map-into-fruct (parse test-src) [(hash-table ('self s)) s])

#;(map-into test-src ['a 'b])

; need to do:
; simple lookup style pass
; ed-sn pass
; insert pass
; set style pass
; insert text pass

; these are enough to re-implement new-gui

; other places fructs are used:
; update-kit and utilities: need to redo these anyway
; char-input: just a restructuring

#; (define (atom->string source)
  (cond [(symbol? source) (symbol->string source)]
        [else (~a source)]))

#; (define (gui-pass:object source [parent-ed "no default"])
  (let* ([ed (new fruct-ed%)]
         [sn (new fruct-sn% [editor ed] [parent-editor parent-ed])]
         [mt (meta sn ed parent-ed)])
    (if (list? source)
        `(,(fruct 0 0 0 "?" 0 mt) ,@(map (λ (sub) (gui-pass:object sub ed)) source))
        `(,(fruct 0 0 0 (atom->string source) 0 mt)))))


#; (define (new-gui source parent-ed)
  (let ([obj-source (gui-pass:object source parent-ed)])
        
    (set! obj-source (gui-pass:forms source obj-source))    
    (set! obj-source ((gui-pass [(fruct sort type name text style mt)
                                 (fruct sort type name text (lookup-style name type) mt)]) obj-source))
    
    #;(set! obj-source (gui-pass:cascade-style obj-source))
    
    ((gui-pass [(fruct _ _ _ text _ (meta sn _ parent-ed)) ; changes behavior is done after forms pass??
                (unless #f #;(equal? text "▹")
                  (send parent-ed insert sn))]) obj-source) 
    ((gui-pass [(fruct _ _ _ _ style (meta sn ed _))
                (apply-style! style sn ed)]) obj-source)    
    ((gui-pass [(fruct _ type _ text _ (meta sn ed _)) ; must be after style cause style deletes text
                (when (not (equal? text "?"))
                  (send ed insert text))]) obj-source) 
    
    obj-source))

