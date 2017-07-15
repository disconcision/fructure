#lang racket


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
(define L1-affo-names '(▹ selector)) ;copy

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