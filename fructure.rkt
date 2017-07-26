#lang racket

(require lens/common)
(require lens/data/list)
(require racket/gui/base)
(require fancy-app)
(require "fructure-style-ast.rkt")

; ----------------------------------------------------------------------------

(define atom? (compose not pair?))

(define proper-list? (conjoin list? (compose not empty?)))

(define (map-rec fn source) ; copy 
  (match (fn source)
    [(? list? ls) (map (curry map-rec fn) ls)]
    [(? atom? a) a]))

(require (rename-in racket (#%app call)))
(define-syntax #%app
  (syntax-rules (⋱↦ ↦ ↓ ≡)
    [[#%app pattern ≡]
     (match-lambda
       [pattern #true]
       [_ #false])]
    [[#%app pattern ↦ result]
     (match-lambda
       [`pattern `result]
       [x x])]
    [[#%app pattern ⋱↦ result]
     (#%app [pattern ⋱↦ result])]
    [(#%app [pattern ⋱↦ result] ...)
     (letrec ([transform (match-lambda
                           [`pattern `result] ...
                           [(? atom? a) a]
                           [(? list? ls) (map transform ls)])])
       transform)]
    [(#%app f-expr arg-expr ...) (call f-expr arg-expr ...)]))


(define-syntax-rule (↓ [pattern ⋱↦ result] ...)
  ([pattern ⋱↦ result] ...)) ; explicit fallthrough annotation


#; (define-syntax-rule (map-into source [<pattern> <payload> ...] ...)
     (letrec ([recursor (match-lambda
                          [<pattern> <payload> ...] ...
                          [(? list? x) (map recursor x)]
                          [(? atom? x) x])])
       (recursor source)))

#; (define-syntax-rule (map-into-fruct source [<pattern> <payload> ...] ...)
     (letrec ([recursor (match-lambda
                          [<pattern> <payload> ...] ...
                          [`(,x . ,xs) (map recursor xs)]
                          [(? atom? x) x])])
       (recursor source)))


; ----------------------------------------------------------------------------

(require (for-syntax racket/match racket/list racket/syntax racket/function))
(begin-for-syntax
 
  (define L1-form-names '(if begin define let lambda send new env kit meta))
  (define L1-terminal-names '(name name-new name-ref literal))
  (define L1-sort-names '(expr name free))
  (define L1-affo-names '(▹ selector ▹▹ search-selector c▹ command-selector c▹▹ command-sub-selector :))

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

  
  (define-values (\\
                  //
                  /@) (values ((curry list) 'quasiquote)
                              ((curry list) 'unquote)
                              ((curry list) 'unquote-splicing)))

  ; rewrites a form signature into a pattern-template pair for the parser
  (define (make-parse-pair pattern)
    (match pattern
      [(? (λ (x) (member x L1-form-names)))
       `(,pattern ,pattern)]
      [(? (λ (x) (member x L1-sort-names)))
       `(,(// (gensym)) ,pattern)]
      [`(ooo ,(app make-parse-pair `(,new-pat ,new-temp)))
       `((ooo ,new-pat) ,(/@ `(make-list (length ,(\\ (if (equal? 'unquote (first new-pat)) new-pat (first new-pat)))) ,(\\ new-temp))))]
      ; the above is sort of a hack. the test for first not equalling unquote detects when new-pat is actually a list of pats
      ; but maybe not robustly? to clarify, when the first is unquote we're assuming it's just a quoted, unquoted variable name
      [(? list? ls)
       (transpose (map make-parse-pair ls))]))

  
  ; maps in the ignore-affo pattern to stop in-line afforfances from interferring with parsing
  (define (add-ignores source)
    (match source
      ['... '...]
      [(list 'unquote x) source]
      [(? symbol?) (// `(ignore-affo ,source))]
      [(? list?) (// `(ignore-affo ,(map add-ignores source)))]))

  
  ; rewrites form signatures into pattern-template pair for the parser
  (define form-list->parse-pair
    (compose (match-lambda [`(,(app add-ignores pat) ,tem) `(,pat ,tem)])
             (curry map-rec redotdotdot)
             make-parse-pair
             (curry map-rec undotdotdot)))

  
  ; rewrites the forms of a language specification into parse-pairs 
  (define lang->parse-lang
    (match-lambda
      [`((,sort-name (|| ,form ...)) ...)
       (transpose `(,sort-name ,(map (curry map form-list->parse-pair) form)))])))



; changes a pattern into one that ignores over-wrapped affordances
(define-match-expander ignore-affo
  (syntax-rules ()
    [(ignore-affo <pat>)
     (app (λ (source) (match source
                        [`(,(? (λ (x) (member x L1-affo-names))) ,q ,a) (match a
                                                                          [`(,(? (λ (x) (member x L1-affo-names))) ,b) b]
                                                                          [_ a])]
                        [`(,(? (λ (x) (member x L1-affo-names))) ,a) (match a
                                                                       [`(,(? (λ (x) (member x L1-affo-names))) ,b) b]
                                                                       [_ a])]
                        ; the above is a hack. how do i recurse right in macros?
                        [_ source])) `<pat>)]))


; matches source to a form from the provided list 
(define-syntax (source+grammar->form stx)
  (syntax-case stx ()
    [(_ <sort> <source> <language>)  
     (let ([proc-lang (lang->parse-lang (eval (syntax->datum #'<language>)))])
       (with-syntax* ([((<sort-name> ((<new-pat> <new-tem>) ...)) ...) (datum->syntax #'<source> proc-lang)])
         #'(match <sort>
             ['<sort-name>
              (match <source>
                [`<new-pat> `<new-tem>] ...
                [(? atom? a) a])]
             ...)))]))




; parsing --------------------------------------------------------------------

; duplicates from before-syntax
(define L1-sort-names '(atom hole expr free))
(define L1-form-names '(if begin define let))
(define L1-terminal-names '(name name-ref literal))
(define L1-affo-names '(▹ selector ▹▹ search-selector c▹ command-selector c▹▹ command-sub-selector :))


(define-values (sort-name?
                terminal-name?
                form-name?
                affo-name?) (values (λ (x) (member x L1-sort-names))
                                    (λ (x) (member x L1-terminal-names))
                                    (λ (x) (member x L1-form-names))
                                    (λ (x) (member x L1-affo-names))))


(define (source->form sort source)
  (source+grammar->form
   sort
   source
   '((free (|| (free ...)))
     (def  (|| (define (name name ...) expr ...)
               (define name expr)))
     (expr (|| (if expr expr expr)
               (begin expr ...)
               (define (name name ...) expr ...)
               (define name expr)
               (let ([name expr] ...) expr ...)
               (lambda (name ...) expr ...)
               (send expr expr expr ...)
               (new expr [name expr] ...)
               (env expr ...)
               (kit expr ...)
               (meta expr ...)
               (expr expr ...))))))


(define (sel◇ source) `(◇ ,source))

(define/match (◇-project source)
  [(`(◇ ,a)) a]
  [((? atom? a)) #f]
  [(_) (let ([result (filter-map ◇-project source)])
         (if (empty? result) #f (first result)))])

(define (lens-ith-child i fn source)
  (lens-transform (list-ref-lens i) source fn))

(define (◇-ith-child i)
  [(◇ (,ls ...)) ⋱↦ ,(lens-ith-child i sel◇ ls)])

(define (->child-contexts parent-context src)
  (map (λ (i) ((◇-ith-child i) parent-context)) (range (length src))))


; map an s-expression to a fructure ast.
(define (sexp->fruct source [ctx `(top (◇ expr))])
  (match source
    [`(,(? affo-name? name) ,selectee) 
     `(,(hash 'symbol void
              'self `(,name hole)
              'sort 'affo
              'context ctx)
       ,(hash 'symbol name
              'self name
              'context `((◇ ,name) hole))
       ,(sexp->fruct selectee ctx))]
    ; the above is a hack. it skips (single) selections
    ; and just passes the context along to the selectee
    [`(c▹ ,thingy ,selectee)
     `(,(hash 'symbol void
              'self `(c▹ hole hole)
              'sort 'affo
              'context ctx) 
       ,(hash 'symbol 'c▹
              'self 'c▹
              'context `((◇ c▹) hole))
       ,(sexp->fruct thingy ctx)
       ,(sexp->fruct selectee ctx))]
    [_
     (match (◇-project ctx)
       [(? terminal-name?)
        (hash 'symbol source
              'self ctx ; hack for style
              'sort source 
              'context ctx)]
       [(? form-name?)
        (hash 'symbol source
              'self ctx
              'sort 'literal-symbol
              'context ctx)]
       [(? sort-name? sort)
        (let* ([form (source->form sort source)]
               [hash (hash 'symbol (if (list? form) void source)
                           'self `(◇ ,form) ;hack for style
                           'sort sort
                           'context ctx)])
          (if (list? form)
              `(,hash ,@(map sexp->fruct source (->child-contexts `(◇ ,form) source)))
              hash))]
       [(? list?)
        `(,(hash 'symbol void
                 'self ctx ; hack for style
                 'sort 'literal-list
                 'context ctx) ,@(map sexp->fruct source (->child-contexts ctx source)))])]))


#; (define/match (get-symbol fruct)
     [(`(,(hash-table ('self s)) ,xs ...)) s])

#; (define/match (project-symbol fruct)
     [(`(,x ,xs ...)) (map project-symbol xs)]
     [((hash-table ('self s))) s])


(define ((fruct->fruct+gui parent-ed) source)
  (let* ([ed (new fruct-ed%)]
         [sn (new fruct-sn% [editor ed] [parent-editor parent-ed])]
         [gui (gui sn ed parent-ed)])
    (match source
      [`(,hs ,xs ...) `(,(hash-set hs 'gui gui) ,@(map (fruct->fruct+gui ed) xs))]
      [(hash-table) (hash-set source 'gui gui)])))


(define/match ((fmap-fruct fn) source)
  [(_ (? atom?)) (fn source)]
  [(_ (? list?)) (map (curry fmap-fruct fn) source)])


#; '(fmap:fruct->fruct [(:in <in-pair> ...)
                        (:out <out-pair> ...)] ...)
#; '(curry fmap-fruct (match-lambda
                        [(hash-table <in-pair> ...)
                         (hash-set source <out-pair> ...)])) ; need to splice outpairs

(define (lookup-style-in styles property)
  (second (assoc property styles)))

(define (fill-in-parent-refs parent-styles)
  [(,property (parent ,parent-prop)) ⋱↦ (,property ,(lookup-style-in parent-styles property))])


(define default-styles '((format horizontal)
                         (background-color (color 150 255 150))
                         (text-color (color 128 128 128))
                         (border-style none)
                         (border-color (color 150 255 150))))

(define (cascade-styles [parent-styles default-styles])
  (match-lambda
    [(and hs (hash-table ('style styles)))
     (hash-set hs 'style ((fill-in-parent-refs parent-styles) styles))]
    [`(,(and hs (hash-table ('style (app (fill-in-parent-refs parent-styles) new-parent-styles)))) ,xs ...)
     `(,(hash-set hs 'style new-parent-styles) ,@(map (cascade-styles new-parent-styles) xs))]))

#; (lookup-style-in '((background-color (color 247 0 114))
                      (text-color (parent text-color))
                      (border-style square-brackets)
                      (border-color (color 255 255 255)))
                    'border-style)

#; ((fill-in-parent-refs
     '((background-color (color 247 0 18674))
       (text-color (color 999 999 999)) ))
    '((background-color (color 247 0 114))
      (text-color (parent text-color))
      (border-style square-brackets)
      (border-color (color 255 255 255))))

(define (make-gui parent-ed)
  (compose (fmap-fruct (match-lambda
                         [(and hs (hash-table ('symbol s) ('gui (gui _ ed _))))
                          (when (not (equal? s void))
                            (send ed insert (cond [(symbol? s) (symbol->string s)]
                                                  [else (~a s)]))) hs]))
           (fmap-fruct (match-lambda
                         [(and hs (hash-table ('style st) ('gui (gui sn ed _))))
                          (apply-style! st sn ed) hs]))
           (fmap-fruct (match-lambda
                         [(and hs (hash-table ('symbol s) ('gui (gui sn _ parent-ed))))
                          (send parent-ed insert sn) hs]))
           (cascade-styles)
           (fmap-fruct (match-lambda
                         [(and hs (hash-table ('self s)))
                          (hash-set hs 'style (lookup-style s))]))
           (fruct->fruct+gui parent-ed)
           sexp->fruct))






; gui objs & structs ------------------------------------

(struct gui (sn ed parent-ed))

(define fruct-board%
  (class pasteboard% 
    (define/override (on-default-char event)
      (char-input event))
    (super-new)))


(define fruct-ed%
  (class text% (super-new [line-spacing 0])
    
    (define/public (set-text-color color)
      ;must be after super so style field is intialized
      (define my-style-delta (make-object style-delta%))
      
      #; (send my-style-delta set-delta-background color) ; text bkg
      #; (send my-style-delta set-alignment-on 'top) ; ???
      #; (send my-style-delta set-transparent-text-backing-on #f) ; ineffective
      
      (match color
        [`(color ,r ,g ,b)
         (send my-style-delta set-delta-foreground (make-color r g b))])
      
      (send this change-style my-style-delta))
    
    (define/public (set-format format)
      (match format
        ['horizontal (format-horizontal)]
        ['vertical (format-vertical)]
        ['indent (format-indent-after 2)]))

    (define/public (set-string-form string)
      (remove-text-snips)
      (send this insert string))
         
    (define (remove-text-snips)
      (for ([pos (range 0 (send this last-position))])
        (when (is-a? (send this find-snip pos 'before) string-snip%)
          (send this release-snip (send this find-snip pos 'before))
          (remove-text-snips))))

    (define (format-horizontal)
      (remove-text-snips))
                    
    (define (format-vertical)
      (remove-text-snips)
      (let ([num-items (send this last-position)])
        (for ([pos (range 1 (- (* 2 num-items) 2) 2)])
          (send this insert "\n" pos))))

    
    ; todo: update this to take variable number of spaces
    (define (format-indent-after start-at)
      (remove-text-snips)
      (let ([num-items (send this last-position)])
        (for ([pos (range start-at (* 2 (sub1 num-items)) 2)])
          (send this insert "\n" pos))
        (for ([line-num (range 1 (sub1 num-items))])
          (send this insert "    " (send this line-start-position line-num)))))

    (define/override (on-default-char event)
      (char-input event))))


(define fruct-sn%
  (class editor-snip% (super-new [with-border? #f])
                    
    (init-field parent-editor)
    
    (field [background-color (make-color 28 28 28)]) ; current default
    (field [border-color (make-color 0 255 0)])
    (field [border-style 'none])
    
    (define/public (set-background-color color)
      (match color
        [`(color ,r ,g ,b)
         (set! background-color (make-color r g b))]))
    
    (define/public (set-border-color color)
      (match color
        [`(color ,r ,g ,b)
         (set! border-color (make-color r g b))]))
    
    (define/public (set-border-style style)
      (set! border-style style))

    (define/public (set-margins l t r b)
      #; (send this set-inset 0 0 0 0)
      #; (send this set-align-top-line #t) ; ???
      (send this set-margin l t r b))

    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
 
      #; (send dc set-text-mode 'transparent) ; ineffective
      #; (send dc set-background "blue") ; ineffective
      #; (send editor get-extent width height) ; try this instead?

      #; (define bottom-x (box 2))
      #; (define bottom-y (box 2))
      #; (send parent-editor get-snip-location this bottom-x bottom-y #t)
      #; (send dc draw-rectangle (+ x 0) (+ y 0) (+ (unbox bottom-x) 0) (+ (unbox bottom-y) 0))

      
      (define-values (a-w a-h a-descent a-space a-lspace a-rspace)
        (values (box 0) (box 0) (box 0) (box 0) (box 0) (box 0) ))
 
      (send this get-extent dc x y a-w a-h a-descent a-space a-lspace a-rspace)
      
      (define-values (left-x top-y right-x bot-y width height)
        (values x y (+ x (unbox a-w)) (+ y (unbox a-h)) (unbox a-w) (unbox a-h)))

      
      (define (draw-background color)
        (send this use-style-background #t) ; otherwise whiteness ensues
        (send dc set-brush color 'solid)
        (send dc set-pen color 1 'solid)
        (send dc draw-rectangle (+ x 0) (+ y 0) (+ width 0) (+ height 0)))

      (define (draw-left-square-bracket color)
        (send dc set-pen color 1 'solid)
        (send dc draw-line left-x top-y left-x (+ bot-y -1))
        (send dc draw-line left-x top-y (+ 2 left-x) top-y)
        (send dc draw-line left-x (+ bot-y -1) (+ 2 left-x) (+ bot-y -1)))

      (define (draw-right-square-bracket color)
        (set! color (make-color 120 145 222)) ; temp
        (send dc set-pen color 1 'solid)
        (send dc draw-line (+ -1 right-x) top-y (+ -1 right-x) (+ bot-y -1))
        (send dc draw-line right-x top-y (+ -2 right-x) top-y)
        (send dc draw-line right-x (+ bot-y -1) (+ -2 right-x) (+ bot-y -1)))

      (define (draw-square-brackets color)
        (draw-left-square-bracket color)
        #;(draw-right-square-bracket color))

      
      ; actual draw calls (order sensitive!) -------------------------
      
      (draw-background background-color)
      
      (case border-style
        ['none void]
        ['square-brackets (draw-square-brackets border-color)])

      (send dc set-pen (make-color 255 255 255) 1 'solid)
      (super draw dc x y left top right bottom dx dy draw-caret))))


(define (update-gui stage kit)
  (let* ([new-main-board (new fruct-board%)]
         #;[new-kit-board (new fruct-ed%)]
         [new-stage-board (new fruct-ed%)]
         [stage-board-snip (new fruct-sn% [editor new-stage-board] [parent-editor new-main-board])]
         #;[kit-snip (new fruct-sn% [editor new-kit-board] [parent-editor new-main-board])])

    (set! stage-gui ((make-gui new-stage-board) stage ))

    #;(pretty-print stage-gui)
    
    (send new-main-board insert stage-board-snip)
    
    #;(set! kit-gui ((make-gui new-kit-board) kit ))
    #;(send new-main-board insert kit-snip)
    
    #;(send new-main-board move-to stage-board-snip 200 0)
    
    (send my-canvas set-editor new-main-board)
    (send new-main-board set-caret-owner #f 'global)))







; transformation ---------------------------------------

(define simple-select
  [,a ⋱↦ (▹ ,a)])

(define simple-deselect
  [(▹ ,a) ⋱↦ ,a]) ; note: doesn't work if a is list. needs new rule with context

(define forms+ #hash(("define" . (define (▹ /name/) /expr/)) ; how about this notation?
                     ("λ" . (λ ((▹ variable) ⋯) expr))
                     ("define ()" . (define ((▹ name) variable ⋯) expr))
                     ("let" . (let ([(▹ name) expr] ⋯) expr))
                     ("letrec" . (letrec ([(▹ name) expr] ⋯) expr))
                     ("begin" . (begin (▹ expr) expr ⋯))
                     ("if" . (if (▹ expr) expr expr))
                     ("cond" . (cond [(▹ expr) expr] ⋯ [expr expr]))
                     ("match" . (match (▹ expr) [pattern expr] ⋯))
                     ("quote" . (quote (▹ expr)))
                     ("unquote" . (unquote (▹ expr)))
                     ("map" . (map (▹ fn) ls ⋯))))

(define (insert-form name)
  [(▹ ,a) ⋱↦ ,(hash-ref forms+ name)])


; simple nav 

(define first-child
  [(▹ (,a ,b ...)) ⋱↦ ((▹ ,a) ,@b)])

(define last-child
  [(▹ (,a ... ,b)) ⋱↦ (,@a (▹ ,b))])

(define parent
  [(,a ... (▹ ,b ...) ,c ...) ⋱↦ (▹ (,@a ,@b ,@c))])


; atomic nav

(define/match (first-contained-atom source)
  [(`(▹ ,(? atom? a))) `(▹ ,a)]
  [(`(▹ ,ls)) (first-contained-atom-inner ls)]
  [((? atom? a)) a] 
  [(ls) (map first-contained-atom ls)])

(define/match (first-contained-atom-inner source)
  [((? atom? a)) `(▹ ,a)]
  [(`(,a ,b ...)) `(,(first-contained-atom-inner a) ,@b)])

(define/match (selector-at-end? source)
  [((? atom? a)) #false]
  [(`(▹ ,a)) #true]
  [(`(,a ... ,b)) (selector-at-end? b)])

(define next-atom
  (↓ [(▹ ,a) ⋱↦ (▹ ,a)]
     [(,a ... ,(? selector-at-end? b) ,c ,d ...) ⋱↦ (,@a ,(simple-deselect b) ,(first-contained-atom `(▹ ,c)) ,@d)]))


(define/match (last-contained-atom source)
  [(`(▹ ,(? atom? a))) `(▹ ,a)]
  [(`(▹ ,ls)) (last-contained-atom-inner ls)]
  [((? atom? a)) a] 
  [(ls) (map last-contained-atom ls)])

(define/match (last-contained-atom-inner source)
  [((? atom? a)) `(▹ ,a)]
  [(`(,a ... ,b )) `(,@a ,(last-contained-atom-inner b))])

(define/match (selector-at-start? source)
  [((? atom? a)) #false]
  [(`(▹ ,a)) #true]
  [(`(,a ,b ...)) (selector-at-start? a)])

(define prev-atom
  (↓ [(▹ ,a) ⋱↦ (▹ ,a)]
     [(,a ... ,b ,(? selector-at-start? c) ,d ...) ⋱↦ (,@a ,(last-contained-atom `(▹ ,b)) ,(simple-deselect c) ,@d)]))




; predicate nav

(define (▹first-contained- p?)
  (match-lambda
    [`(▹ ,(? p? a)) `(▹ ,a)]
    [`(▹ ,ls) ((▹first-contained-inner- p?) ls)]
    [(? atom? a) a] 
    [(? list? ls) (map (▹first-contained- p?) ls)]))

(define (▹first-contained-inner- p?)
  (match-lambda
    [(? p? a) `(▹ ,a)]
    [`(,x ,xs ...) `(,((▹first-contained-inner- p?) x) ,@xs)]))

(define (▹next- p?)
  (↓ [(▹ ,a) ⋱↦ (▹ ,a)]
     [(,a ... ,(? selector-at-end? b) ,c ,d ...) ⋱↦ (,@a ,(simple-deselect b) ,((▹first-contained- p?) `(▹ ,c)) ,@d)]))

#; (define next-atom (▹next- atom?))

(define ▹first-search-result
  [(▹ ,a) ⋱↦ ,((▹first-contained- (curry equal? 3)#;[(▹▹ ,b) ≡]) a)])

#; ((▹first-contained- (curry equal? 3)#;[(▹▹ ,b) ≡]) '(▹ (2 3 (▹▹ 1))))
#; (▹first-search-result '(▹ (2 3 (▹▹ 1))))


; simple transforms

(define delete
  [(,a ... (▹ ,b ...) ,c ...) ⋱↦ (▹ (,@a ,@c ))])

(define insert-child-r
  [(▹ (,a ...)) ⋱↦ (,@a (▹ (☺)))])

(define insert-child-l
  [(▹ (,a ...)) ⋱↦ ((▹ (☺)) ,@a)])

(define new-sibling-r
  [(,a ... (▹ (,b ...)) ,c ...) ⋱↦ (,@a ,@b (▹ (☺)) ,@c)])

(define new-sibling-l
  [(,a ... (▹ (,b ...)) ,c ...) ⋱↦ (,@a (▹ (☺)) ,@b ,@c)])

(define wrap
  [(▹ (,a ...)) ⋱↦ (▹ ((,@a)))])




; helpers for main loop ------------------------------


; notes: how to do relativize-direction right:
; should ideally depend only on style, no looking into gui-data like it does now
; find most immediate (grand)parent contained within a parent with non-horizontal formatting type
; the prev/next siblings are candidates for moving (in)to when we press up/down
; complications:
; 1. if there is no prev/next sibling, then we try the parent, and so-on recursively
; 2. if the formatting type is indent, or some other format with mixed horizontal and vertical formatting
; hack for now: assume only other type is indent-after. compare position to 'after' to decide
; whether up/down should apply to parent, or if we have to recurse upwards 

(define/match (sel-to-pos fruct)
  [(`(▹ ,a)) '()]
  [((? atom?)) #f]
  [((? list?)) (let ([result (filter-map
                              (λ (sub num)
                                (let ([a (sel-to-pos sub)])
                                  (if a `(,num ,@a) #f)))
                              fruct
                              (range 0 (length fruct)))])
                 (if (empty? result) #f (first result)))])


(define/match ((?->lenses pred?) source)
  [(_ (? pred?)) `(,identity-lens)]
  [(_ (? atom?)) #f]
  [(_ (? list?)) (flatten (filter-map (λ (x i) (let ([res ((?->lenses pred?) x)])
                                                 (if res
                                                     (map (λ (y) (if y
                                                                     (lens-compose y (list-ref-lens i))
                                                                     #f))
                                                          res)
                                                     #f)))
                                      source
                                      (range (length source))))])

#;(define ((?->lenses pred?) source)
    (filter identity ((?->lens pred?) source)))

(define ▹->lenses
  (?->lenses [`(▹ ,a) ≡]))

(define ▹▹->lenses
  (?->lenses [`(▹▹ ,a) ≡]))

(define ▹-first-▹▹-in-▹
  [(▹ ,a) ⋱↦ ,(▹-first-▹▹-in a)])

(define (▹-first-▹▹-in source)
  (lens-transform (first (▹▹->lenses source)) source [(▹▹ ,a) ↦ (▹▹ (▹ ,a))]))

#; (▹-first-▹▹-in-▹ `(▹ (1 2 (8 9 (7 6 (▹▹ 3) 5)) (▹▹ 4))))

(define (▹-next-▹▹ source)
  (match (▹▹->lenses source)
    [`(,x ... ,(and a (app (curryr lens-view source) `(▹▹ (▹ ,w)))) ,b ,y ...)
     (lens-transform/list source
                          a [(▹▹ (▹ ,x)) ↦ (▹▹ ,x)]
                          b [(▹▹ ,x) ↦ (▹▹ (▹ ,x))])]
    [`(,b ,x ... ,(and a (app (curryr lens-view source) `(▹▹ (▹ ,w)))))
     (lens-transform/list source
                          a [(▹▹ (▹ ,x)) ↦ (▹▹ ,x)]
                          b [(▹▹ ,x) ↦ (▹▹ (▹ ,x))])]
    [_ source]))

#; (▹-next-▹▹ `(1 2 (8 9 (7 6 (▹▹ (▹ 3)) 5)) (▹▹ 4)))
#; (▹-next-▹▹ (▹-next-▹▹ `(1 2 (8 9 (7 6 (▹▹ (▹ 3)) 5)) (▹▹ 4))))
#; (▹-next-▹▹ (▹-next-▹▹ (▹-next-▹▹ `(1 2 (8 9 (7 6 (▹▹ (▹ 3)) 5)) (▹▹ 4)))))

#;(▹->lens `(1 2 (8 9 (7 6 (▹ 3) 5)) (▹ 4)))
#;(lens-view (second (▹->lenses `(1 2 (8 9 (7 6 (▹ 3) 5)) (▹ 4)))) `(1 2 (8 9 (7 6 (▹ 3) 5)) (▹ 4)))

#;((?->lens [`(▹ ,a) ≡]) `(1 2 (8 9 (7 6 (▹ 3) 5)) (▹ 4)))
#;(lens-view (second ((?->lenses [`(▹ ,a) ≡]) `(1 2 (8 9 (7 6 (▹ 3) 5)) (▹ 4)))) `(1 2 (8 9 (7 6 (▹ 3) 5)) (▹ 4)))

#;(lens-view (second (▹▹->lenses `(1 2 (8 9 (7 6 (▹▹ 3) 5)) (▹▹ 4)))) `(1 2 (8 9 (7 6 (▹▹ 3) 5)) (▹▹ 4)))
#;(lens-view (second (▹▹->lenses '((▹▹ (▹ define)) a ((▹▹ defne))))) '((▹▹ (▹ define)) a ((▹▹ defne))))
#; (▹-next-▹▹ '((▹▹ (▹ define)) (fn a) a ((▹▹ define) (g q r) (let ([a 5] [b 6]) (if 1 2 2)))))


(define/match (obj-at-pos fruct pos)
  [(_ `()) (first fruct)]
  [(_ `(,a . ,as)) (obj-at-pos (list-ref (rest fruct) a) as)])

(define (update! fn)
  (set! stage (fn stage))
  (set! stage-gui ((make-gui (new fruct-ed%)) stage))
  (update-gui stage kit-actual))

#; (define/match (get-symbol-as-string fruct)
     [((hash-table ('symbol (? symbol? s)))) (symbol->string s)]
     [(_) ""])

(define-namespace-anchor an)
(define ns (namespace-anchor->namespace an))

(define (eval-match-λ pat-tem)
  (match-let ([`(,pat ,tem) pat-tem])
    (eval `(match-lambda [,pat ,tem] [x x]) ns)))

(define (buf->pat+tem buf)
  `[(and x (? symbol? (app symbol->string (regexp (regexp ,(string-append "^" buf ".*"))))))
    `(▹▹ ,x)])

#; ((eval-match-λ '`(,a ,b) 'a) '(1 5))
#; (buf->pat+tem "def")

(define ((search-map-rec fn) source) 
  (match (fn source)
    [`(▹▹ ,x) `(▹▹ ,x)]
    [(? list? ls) (map (curry search-map-rec fn) ls)]
    [(? atom? a) a]))

; single largest source of trivial bugs for me this project: copying a recursive function without changing the name of the rec call

#; (search-map-rec (eval-match-λ (buf->pat+tem "a")) '(a b a))
#; (match "define"
     [(regexp (regexp "def.")) 777])
#; (match (hash 'symbol 'a)
     [(app get-symbol-as-string (regexp (regexp (string-append "a" ".*")))) "blop"])
#; ((curry map-rec (eval-match-λ (buf->pat "a") '111)) (hash 'symbol 'a))

(define ▹▹tag-hit (compose eval-match-λ buf->pat+tem))

(define (▹▹tag-hits str) (search-map-rec (▹▹tag-hit str)))

#; (buf->pat "def")
#; ((eval-match-λ (buf->pat "def") 1) "deffine")
#; ((eval-match-λ (buf->pat "a") '111) 'a)
#; (get-symbol-as-string (sexp->fruct '(begin a b c)))
#; (define (search-select fn source)
     (match source
       [(? atom?) (fn source)]
       ((? list?) (map (curry search-select fn) source))))







; main loop ---------------------------------------------

; todo:
; make command to re-root the tree on current selection, and to expand selection if root selected
; make quick !!! command to collapse a subtree (remember to remember state)


(define (char-input event)
  (match-let* ([key-code (send event get-key-code)])
    (when (not (equal? key-code 'release))
      (case mode
        ['select    (match key-code
                      ['escape (update! (compose simple-select simple-deselect))
                               (pretty-print stage-gui)]
                      ['right (update! next-atom)]
                      ['left  (update! prev-atom)]
                      ['up    (update! parent)]
                      ['down  (update! first-child)]
                      [(app string (regexp (regexp "[A-Za-z_]")))
                       (set! buffer (string-append buffer (string key-code)))
                       (update! [(▹ ,a) ⋱↦ (▹ ,((▹▹tag-hits buffer) a))])
                       (set! mode 'search)]
                      [#\return
                       (update! [(▹ ,a) ⋱↦ (c▹ (: "") ,a)]) 
                       (set! mode 'transform)])]
        ['search    (match key-code
                      ; precond: search results inside selector
                      ['right (update! ▹-next-▹▹)]
                      ['down  (update! ▹-first-▹▹-in-▹)]                       
                      ['escape     (set! buffer "")
                                   (update! [(▹▹ ,a) ⋱↦ ,a])
                                   (set! mode 'select)]
                      [#\backspace (set! buffer (substring buffer 0 (sub1 (string-length buffer))))
                                   (update! [(▹ ,a) ⋱↦ (▹ ,((▹▹tag-hits buffer) a))])]
                      [(app string (regexp #rx"[A-Za-z0-9_]"))
                       ; remember to deal with case of single (non-numeric!) character
                       (set! buffer (string-append buffer (string key-code)))
                       (update! [(▹ ,a) ⋱↦ (▹ ,((▹▹tag-hits buffer) a))])])]
        ['project   (match key-code)]
        ['transform (match key-code
                      ['escape     (update! [(c▹ (: ,w) ,a) ⋱↦ (▹ ,a)])
                                   (set! mode 'select)]
                      [#\return    (update! [(c▹ ,w ,a) ⋱↦ (▹ ,([(: ,x) ⋱↦ ,x] w))])
                                   (set! mode 'select)]                
                      [#\space     (update! [(,x ... (: ,str)) ⋱↦ (,@x ,str (: ""))])]
                      ['control    (update! [(: ,str) ⋱↦ (if (: ,str) 3 4)])]
                      [(app string (regexp #rx"[A-Za-z0-9_]"))
                       (update! [(: ,str) ⋱↦ (: ,(string-append str (string key-code)))])])]))))


; ◇-project src

; gui setup ---------------------------------------------

(define my-frame
  (new frame%
       [label "fructure"]
       [width 1300]
       [height 900]))

(define my-canvas
  (new editor-canvas%
       [parent my-frame]))


; testing ------------------------------------------------

; init stage and kit
(define stage
  (first-child '(▹ (define (fn a) a (define (g q r) (let ([a 5] [b 6]) (if 1 2 2))))))
  #; '(▹ (define (fn a) a (define (g q r) (let ([a 5] [b 6]) (if 1 2 2)))))
  #; '(▹ (if a b c))
  #; '(define (fn a) a (define (g q r) (let ([a 5] [b 6]) (if 1 2 2))))
  #; '(define (fn a) a (define (g q r) 2))
  #; '(define (selector (fn a)) 7))

(define kit-actual '(kit (env) (meta)))

; init gui refs
(define stage-gui '())
(define kit-gui '())

; init globals
(define mode 'select)
(define num-chars 0)
(define buffer "")
(define buf-pat "")

; init display
(send my-frame show #t)
(update-gui stage kit-actual)





#; ((make-gui (new fruct-ed%)) test-src )
#; (source+grammar->form  '((selector let) (▹ ([f a][f a][k a][g a])) 4 4 4) '((if expr expr expr)
                                                                               (begin expr ...)
                                                                               (define (name name ...) expr ...)
                                                                               (let ([name expr] ...) expr ...)))
#; (pretty-print (sexp->fruct test-src))
#; test-src
#; (project-symbol (sexp->fruct test-src))
#; (map-into-fruct (sexp->fruct test-src) [(hash-table ('self s)) s])
#; (map-into test-src ['a 'b])

