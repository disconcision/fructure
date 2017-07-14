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


; kinds of atoms
; symbol-literal (quoted symbol)
; form-name (symbol-literal)
; name-new (symbol)
; name-ref (symbol)
; literal

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
  (define L1-sort-names '(expr name)) ; copy

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
    (match* (pattern)
      [((? (λ (x) (member x L1-form-names))))
       `(,pattern ,pattern)]
      [((? (λ (x) (member x L1-sort-names))))
       `(,(list 'unquote (gensym)) ,pattern)]
      [(`(ooo ,(app make-parse-pair `(,new-pat ,new-temp))))
       `((ooo ,new-pat) ,(list 'unquote-splicing `(make-list (length ,(list 'quasiquote (if (equal? 'unquote (first new-pat)) new-pat (first new-pat)))) ,(list 'quote new-temp))))]
      ; the above is sort of a hack. the test for first not equalling unquote detects when new-pat is actually a list of pats
      ; but maybe not robustly? to clarify, when the first is unquote we're assuming it's just a quoted, unquoted variable name
      [((? list?))
       (transpose (map make-parse-pair pattern))])))


(define-syntax (get-pat-macro-list stx)
  (syntax-case stx ()
    [(_ <source> <forms>)
     (match-let* ([form-list (map-rec undotdotdot (eval (syntax->datum #'<forms>)))]
                  [parse-pairs (map make-parse-pair form-list)]
                  [`((,pat ,temp) ...) (map (λ (x) (map-rec redotdotdot x)) parse-pairs)])
       (with-syntax* ([(<new-pat> ...) (datum->syntax #'<source> pat)]
                      [(<new-temp> ...) (datum->syntax #'<source> temp)])
         #'(match <source>
             [`<new-pat> `<new-temp>] ...
             [(? atom? a) a])))]))



(get-pat-macro-list '(let ([f a][f a][k a][g a]) 4 4 4) '((if expr expr expr)
                                                          (begin expr ...)
                                                          (define (name name ...) expr ...)
                                                          (let ([name expr] ...) expr ...)
                                                          ))

(define (get-form source)
  (get-pat-macro-list source '((if expr expr expr)
                               (begin expr ...)
                               (define (name name ...) expr ...)
                               (let ([name expr] ...) expr ...))))


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
     (let* ([form (get-form source)]
            [hash (hash 'self form 'context ctx)])
       (if (list? form)
           `(,hash ,@(map parse source (get-child-contexts `(◇ ,form) source)))
           hash))]
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


(define-match-expander ♥
  (syntax-rules ()
    [(♥ <thing>)
     (app (λ (source) (match source
                        [`(,(== selector) ,a) a]
                        [_ source])) `<thing>)]))


#; (define (gui-pass:forms source obj-src)
     (style-match
      source obj-src
      #;[`(,(== selector) ,a)
         `((selector wrapper) (selector head) "none")]
      [(atom a)
       `((id atom atom))] ; need to make literal case
      [`(,(♥ if) ,a ,b ,c)
       `((expr if wrapper) (comp if head) "none" "none" "none")]
      [`(,(♥ begin) ,expr ...)
       `((expr begin wrapper) (comp begin head) ,@(make-list (length expr) "none"))]
      [`(,(♥ send) ,target ,method ,args ...)
       `((expr send wrapper) (comp send head) (comp send target) (comp send method) ,@(make-list (length args) "none"))]
      [`(,(♥ define) ,(atom id) ,expr ...)
       `((expr define wrapper) (comp define head) (new-id define name) ,@(make-list (length expr) "none"))]
      [`(,(♥ define) ,(♥ (,id ,vars ...)) ,expr ...)
       `((expr define wrapper) (comp define head) ((comp define fn-wrapper) (new-id define name) ,@(make-list (length vars) "none")) ,@(make-list (length expr) "none"))]
      [`(,(♥ let) ,(♥ (,(♥ (,id ,expr-for-let)) ...)) ,expr ...)
       `((expr let wrapper) (comp let head) ((comp let inits-wrapper) ,@(make-list (length id) '((comp let pair-wrapper) (new-id let name) "none"))) ,@(make-list (length expr) "none"))]
      [`(,(♥ new) ,obj [,prop ,val] ...)
       `((expr new wrapper) (comp new head) (comp new obj-type) ,@(make-list (length prop) '((comp new pair-wrapper) "none" "none")))]
      [`(,(♥ env) ,binds ...)
       `((expr env wrapper) (comp env head) ,@(make-list (length binds) "none"))]
      [`(,(♥ kit) ,sections ...)
       `((expr kit wrapper) (comp kit head) ,@(make-list (length sections) "none"))]
      [`(,(♥ meta) ,props ...)
       `((expr meta wrapper) (comp meta head) ,@(make-list (length props) "none"))]
      ; remember that the following pattern is a catch-all and should be last
      [`(,(♥ ,(atom function)) ,args ...)
       `((expr function wrapper) (id function head) ,@(make-list (length args) "none"))]
      ))

