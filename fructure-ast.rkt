#lang racket

(require lens/common)
(require lens/data/list)
(require "transform-engine.rkt")

; ----------------------------------------------------------------------------


#; (lang def name form) ; sorts & terminals

#; ((lang (def ...)))
#; (def (name form))


#; (hole atom head affo form fruct) ; sorts & terminals

#; (sig (|| hole (sig)) ...)
#; (form (|| atom (head sig)))
#; (fruct (|| form affo))


; metas: () (... hole) (|| hole )
; (middle is reader rewrite of "hole ...")


; literals: if begin define let
; terminals: atom
; sorts: hole
(define L0 '((hole (|| (if hole hole hole)
                       (begin hole ...)
                       (define (hole hole ...) hole ...)
                       (let ([hole hole] ...) hole ...)
                       atom))))


; literals: (atoms are excepted) if begin define let
; terminals : name literal
; sorts: atom expr 
(define L1 '((atom (|| (name
                        literal))) ; attach predicates
             (expr (|| (if expr expr expr)
                       (begin expr ...)
                       (define (name name ...) expr ...)
                       (let ([name expr] ...) expr ...)
                       atom))))

; literals: code
; terminals: lang
; sorts: atom file folder top def expr
(define LX '((atom lang)
             (file (code lang top ...))
             (folder ((|| file folder) ...))
             (top ((|| def expr) ...))
             (def ooo)
             (expr ooo)))

; ----------------------------------------------------------------------------

(define L0-sort-names '(hole))
(define L0-form-names '(if begin define let))
(define L0-terminal-names '(atom))

(define L1-sort-names '(atom hole expr))
(define L1-form-names '(if begin define let))
(define L1-terminal-names '(name name-ref literal))

; L0 : simplest sort model: a single non-atomic sort called 'hole'
(define lang-expr-holes '(|| (if hole hole hole)
                             (begin hole ...)
                             (define (hole hole ...) hole ...)
                             (let ([hole hole] ...) hole ...)
                             atom))

; L1 : sort system: multi-sort
(define lang-expr-sorts '(|| (if expr expr expr)
                             (begin expr ...)
                             (define (name name ...) expr ...)
                             (let ([name expr] ...) expr ...)))

; sortless patterns to match against
(define lang-expr-spats '(|| (if ,a ,b ,c)
                             (begin ,as ...)
                             (define (,a ,bs ...) ,cs ...)
                             (let ([,as ,bs] ...) ,cs ...)))

; used in get-pat
(define lang-expr-xpand '(|| `(if expr expr expr)
                             `(begin ,@(make-list (length as) `expr))
                             `(define (name ,@(make-list (length bs) `name)) ,@(make-list (length cs) `expr))
                             `(let (,@(make-list (length as) `[name expr])) ,@(make-list (length cs) `expr))))
     
; sort-checking patterns
(define lang-expr-cksrt '(|| (if ,(? (sort: expr) a) ,(? (sort: expr) b) ,(? (sort: expr) c))
                             (begin ,(? (sort: expr) a) ...)
                             (define (,(? (sort: name) a) ,(? (sort: name) b) ...) ,(? (sort: expr) c) ...)
                             (let ([,(? (sort: name) a) ,(? (sort: name) b)] ...) ,(? (sort: expr) c) ...)))


; ----------------------------------------------------------------------------

; defaults for holes
(define lang-expr-defts '(|| (if #true void void)
                             (begin void ...)
                             (define (,(gensym) ,(gensym) ...) void ...)
                             (let ([,(gensym) void] ...) void ...)))

; human-readable names for holes
(define lang-expr-names '(|| (if condition consequence alternative)
                             (begin body ...)
                             (define (function-name parameter ...) body ...)
                             (let ([new-name initializer] ...) body ...)))


; ----------------------------------------------------------------------------


(define test-src '(define (fn a) a (define (g q r) 2)))




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
  
  (define L1-form-names '(if begin define let)) ; this is a copy!!!!
  (define L1-sort-names '(expr name)) ; ditto

  (define atom? (compose not pair?)) ; ditto
  
  (define-syntax-rule (map-into source [<pattern> <payload> ...] ...) ; ditto
    (letrec ([recursor (match-lambda
                         [<pattern> <payload> ...] ...
                         [(? list? x) (map recursor x)]
                         [(? (compose not pair?) x) x])])
      (recursor source)))

  #;(define (undotdotdot-into source)
      (map-into source
                [(list a ... b '... c ...) `(,@(undotdotdot-into a) (... ,b) ,@c)]
                [_ source]))
   
  #;(define (redotdotdot-into source)
      (map-into source
                [`(,a ... (... ,b) ,c ...) `(,@(redotdotdot-into a) ,b ... ,@c)]
                [_ source]))

  (define (undotdotdot source)
    (match source
      [(? (compose not pair?)) source]
      [(list a ... b '... c ...) `(,@(undotdotdot a) (... ,b) ,@c)]
      [_ source]))
    
  (define (undotdotdot-into source)
    (match (undotdotdot source)
      [`(... ,a) `(... ,(undotdotdot-into a))] ; is this necessary?
      [(? list? ls) (map undotdotdot-into ls)]
      [(? (compose not pair?) a) a]))

  (define (redotdotdot source)
    (match source
      [`(,a ... (... ,b) ,c ...) `(,@(redotdotdot a) ,b ... ,@c)]
      [_ source]))

  (define (redotdotdot-into source)
    (match (redotdotdot source)
      [`(... ,a) `(... ,(redotdotdot-into a))] ; is this (or something else) necessary?
      [(? list? ls) (map redotdotdot-into ls)]
      [(? (compose not pair?) a) a]))

  (println (undotdotdot-into '(define (name name ...) expr ...)))
  
  (define (transpose x) (apply map list x))
  
  (define (tandem-holes pattern)
    (match* (pattern)
      [((? (λ (x) (member x L1-form-names))))
       `(,pattern ,pattern)]
      [((? (λ (x) (member x L1-sort-names))))
       (let ([new-var (gensym)])
         `(,(list 'unquote new-var) ,pattern))]
      [(`(... ,x))
       (match-let ([`(,new-pat ,new-temp) (tandem-holes x)])
         `((... ,new-pat) ,(list 'unquote-splicing `(make-list (length ,(list 'quasiquote new-pat)) ,(list 'quote new-temp)))))]
      ; the above doesn't quite work for the let case. length is taking the list for a binding pair, ie 2. we need to
      ; conditionally get the length of a single component of the list
      #;[(`(... ,(? (λ (x) (member x L1-sort-names)) sort-name))) ; note this only goes one level deep, needs work to go deeper
         (let ([new-var (gensym)])
           `((... ,(list 'unquote new-var)) ,(list 'unquote-splicing `(make-list (length ,new-var) ,(list 'quote sort-name)))))]
      [((? list?))
       (transpose (map tandem-holes pattern))])))

(define-syntax (get-pat-macro stx)
  (syntax-case stx ()
    [(_ <source> <form>)
     (match-let* ([form-datum (undotdotdot-into (eval (syntax->datum #'<form>)))]
                  [pat-temp (tandem-holes form-datum)]
                  [`(,pat ,temp) (map redotdotdot-into pat-temp)])
       (with-syntax* ([<new-pat> (datum->syntax #'<source> pat)]
                      [<new-temp> (datum->syntax #'<source> temp)])
         #'(match <source>
             [`<new-pat> `<new-temp>])))]))



(get-pat-macro '(define (f a b c ) a 4 4) '(define (name name ...) expr ...))

(define-syntax (get-pat-macro-list stx)
  (syntax-case stx ()
    [(_ <source> <forms>)
     (match-let* ([form-list (map undotdotdot-into (eval (syntax->datum #'<forms>)))]
                  [pat-temp (map tandem-holes form-list)]
                  [`((,pat ,temp) ...) (map (λ (x) (map redotdotdot-into x)) pat-temp)])
       (with-syntax* ([(<new-pat> ...) (datum->syntax #'<source> pat)]
                      [(<new-temp> ...) (datum->syntax #'<source> temp)])
         #'(match <source>
             [`<new-pat> `<new-temp>] ...)))]))

(get-pat-macro-list '(let ([f a][f a][f a]) 4 4 4) '((if expr expr expr)
                                                     (begin expr ...)
                                                     (define (name name ...) expr ...)
                                                     (let ([name expr] ...) expr ...)
                                                     ))

; ----------------------------------------------------------------------------

#; (define (hole-pat pat)
     (if (atom? pat)
         (if (member pat L1-sort-names) 'hole pat)
         (map hole-pat pat)))


; start with (quote (if expr expr expr))
; need (quote (quasiquote (if (unquote a) (unquote b) (unquote c)))
(define (hole-if source)
  (match source
    [(? list?) (map hole-if source)]
    ['if 'if]
    ['expr (list 'unquote (gensym))]))

(hole-if '(if expr expr expr))


(define (get-pat source)
  (match source
    [`(if ,a ,b ,c)
     `(if expr expr expr)]
    [`(begin ,as ...)
     `(begin ,@(make-list (length as) `expr))]
    [`(define (,a ,bs ...) ,cs ...)
     `(define (name ,@(make-list (length bs) `name)) ,@(make-list (length cs) `expr))]
    [`(let ([,as ,bs] ...) ,cs ...)
     `(let (,@(make-list (length as) `[name expr])) ,@(make-list (length cs) `expr))]
    [(? atom? a) 
     (if (symbol? a) a a #;#;
         `(name-ref ,a)
         `(literal ,a))]))

; ----------------------------------------------------------------------------


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
  (match (get◇ ctx)
    [(? (λ (x) (member x L1-terminal-names)))
     (hash 'self source 'context ctx)]
    [(? (λ (x) (member x L1-form-names)))
     (hash 'self source 'context ctx)]
    ['expr
     (let ([pat (get-pat source)])
       (if (list? pat)
           `(,(hash 'self pat 'context ctx) ,@(map parse source (get-child-contexts `(◇ ,pat) source)))
           (hash 'self pat 'context ctx)))]
    [(? list?)
     `(,(hash 'self '() 'context ctx) ,@(map parse source (get-child-contexts ctx source)))]))

(pretty-print (parse test-src))


(define/match (get-symbol fruct)
  [(`(,(hash-table ('self s)) ,ls ...)) s])

(define/match (project-symbol fruct)
  [(`(,x ,xs ...)) (map project-symbol xs)]
  [((hash-table ('self s))) s])


test-src
(project-symbol (parse test-src))
(map-into-fruct (parse test-src) [(hash-table ('self s)) s])

#;(map-into test-src ['a 'b])



#|
(match '(if 1 2 3)
     [`(if ,a ,b ,c)
      `(begin ,b ,c)])

(parse `(if 1 2 3))
(parse `(begin 2 3))

(match (parse `(if 1 2 3))
     [`(,(hash-table ('context ctx)) ,(hash-table ('self s)) ,a ,b ,c)
      `(,(hash 'self '(begin expr expr) 'context ctx) ,(hash 'self 'begin) ,b ,c)])
|#
; ----------------------------------------------------------------------------

(define-syntax-rule (style-match source obj-src
                                 [<pat> <style-pat>] ...)
  (letrec
      ([recursor
        (λ (source obj-src [form-context "none"])
          (match-let ([`(,(fruct _ _ _ text style mt) ,obj-kids (... ...)) obj-src])
            (match form-context
              [_ #:when (and (proper-list? source)
                             (equal? selector (first source)))
                 ; hack: styles selector, passes on form-context
                 ; note: hack currently does not support list-selections
                 `(,(fruct 'afford 'wrapper 'selector text style mt) (,(fruct 'comp 'head 'selector text style mt)) ,(recursor (second source) (second obj-kids) form-context))]
              ["none"
               (match source
                 [<pat>
                  (match-let* ([`((,sort ,name ,type) ,xs (... ...)) <style-pat>]
                               [kids (if (empty? xs) xs (map recursor source obj-kids xs))])
                    `(,(fruct sort type name text style mt) ,@kids))] ...
                 [ls `(,(fruct 'unidentified 'unidentified 'unidentified text style mt) ,@(map recursor source obj-kids))])]
              [`(,(? atom? sort) ,name ,type)
               (if (list? source) ; bit of a hack to escape-hatch things unaccounted-for in the form grammar
                   `(,(fruct sort type name text style mt) ,@(map recursor source obj-kids))
                   `(,(fruct sort type name text 0 mt)))]
              [`((,sort ,name ,type) ,xs (... ...))
               `(,(fruct sort type name text style mt) ,@(map recursor source obj-kids xs))])
            ))])
    (recursor source obj-src)))


