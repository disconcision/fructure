#lang racket

(require lens/common)
(require lens/data/list)
(require "transform-engine.rkt")

; ----------------------------------------------------------------------------

(define lang '((atom (|| (name
                          literal))) ; attach predicates
               (expr (|| (if hole hole hole)
                         (begin hole ...)
                         (define (hole hole ...) hole ...)
                         (let ([hole hole] ...) hole ...)
                         atom))))

(define sort-names '(atom hole expr name literal))


; simplest sort model: a single non-atomic sort called 'hole'
(define lang-expr-holes '(|| (if hole hole hole)
                             (begin hole ...)
                             (define (hole hole ...) hole ...)
                             (let ([hole hole] ...) hole ...)))

; sortless patterns to match against
(define lang-expr-spats '(|| (if ,a ,b ,c)
                             (begin ,as ...)
                             (define (,a ,bs ...) ,cs ...)
                             (let ([,as ,bs] ...) ,cs ...)))

; after parse check against this?
(define lang-expr-sorts '(|| (if expr expr expr)
                             (begin expr ...)
                             (define (name name ...) expr ...)
                             (let ([name expr] ...) expr ...)))

; sort-checking patterns
(define lang-expr-cksrt '(|| (if ,(? (sort? expr) a) ,(? (sort? expr) b) ,(? (sort? expr) c))
                             (begin ,(? (sort? expr) a) ...)
                             (define (,(? (sort? name) a) ,(? (sort? name) b) ...) ,(? (sort? name) c) ...)
                             (let ([,(? (sort? name) a) ,(? (sort? name) b)] ...) ,(? (sort? name) c) ...)))

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


(define src '(define (fn a) a))


(define (apply-ith i fn source)
  (lens-transform (list-ref-lens i) source fn))

(define atom? (compose not pair?))

(define-match-expander atom
  (syntax-rules ()
    [(atom <name>)
     (? (compose not pair?) <name>)]))

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

(define (hole-pat pat)
  (if (atom? pat)
      (if (member pat sort-names) 'hole pat)
      (map hole-pat pat)))

(define (sel◇ source) `(◇ ,source))

(define (get-pat source)
  (match source
    [`(if ,a ,b ,c)
     `(if expr expr expr)]
    [`(begin ,as ...)
     `(begin ,@(make-list (length as) `expr))]
    [`(define (,a ,bs ...) ,cs ...)
     `(define (name ,@(make-list (length bs) `expr)) ,@(make-list (length cs) `expr))]
    [`(let ([,as ,bs] ...) ,cs ...)
     `(let (,@(make-list (length as) `[name expr])) ,@(make-list (length cs) `expr))]))

(define/match (parse source [context 'hole])
  [(_ 'hole)
   (let* ([pat (hole-pat (get-pat source))]
          [contexts (map (λ (i) (apply-ith i sel◇ pat)) (range 0 (length source)))])
     `(,#hash((self . pat)) ,@(map parse source contexts)))]
  [((atom a) (atom s))
   (let ([pat (hole-pat (get-pat source))])
     #hash((self . pat)))]
  [(`(,s ...) `(,c ...))
   (let ([contexts (map (λ (i) [(◇ (,ls ...)) ⋱↦ ,(apply-ith i sel◇ c)]) (range 0 (length source)))])
     (map parse source contexts))]
  [(_ _) (println "error on") (println source) (println context)])

(parse src)

; ----------------------------------------------------------------------------

#; (define-syntax-rule (style-match source obj-src
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
                 [`(,(atom sort) ,name ,type)
                  (if (list? source) ; bit of a hack to escape-hatch things unaccounted-for in the form grammar
                      `(,(fruct sort type name text style mt) ,@(map recursor source obj-kids))
                      `(,(fruct sort type name text 0 mt)))]
                 [`((,sort ,name ,type) ,xs (... ...))
                  `(,(fruct sort type name text style mt) ,@(map recursor source obj-kids xs))])
               ))])
       (recursor source obj-src)))

#; (define (gui-pass:forms source obj-src)
     (style-match
      source obj-src
      [(atom a)
       `((id atom atom))] ; need to make literal case
      [`(,(♥ if) ,a ,b ,c)
       `((expr if wrapper) (comp if head) "none" "none" "none")]
      [`(,(♥ begin) ,expr ...)
       `((expr begin wrapper) (comp begin head) ,@(make-list (length expr) "none"))]
      [`(,(♥ define) ,(♥ (,id ,vars ...)) ,expr ...)
       `((expr define wrapper) (comp define head) ((comp define fn-wrapper) (new-id define name) ,@(make-list (length vars) "none")) ,@(make-list (length expr) "none"))]
      [`(,(♥ let) ,(♥ (,(♥ (,id ,expr-for-let)) ...)) ,expr ...)
       `((expr let wrapper) (comp let head) ((comp let inits-wrapper) ,@(make-list (length id) '((comp let pair-wrapper) (new-id let name) "none"))) ,@(make-list (length expr) "none"))]
      ))

