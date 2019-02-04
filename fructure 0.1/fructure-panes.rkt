#lang racket

(require racket/gui/base)
(require fancy-app)
(require "transform-engine.rkt")
(require "fructure-style.rkt")
(require "utility-fns.rkt")

(define-match-expander atom
  (syntax-rules ()
    [(atom <name>)
     (? (compose not pair?) <name>)]))

(provide
 (struct-out fruct))

; source structure data ---------------------------------

(define original-source '(define (build-gui-block code arg2 [parent-ed my-board] [position '()])
                           (let ([ed (new fruct-ed% [parent-editor parent-ed] [position position])]
                                 [sn (new fruct-sn% [editor ed] [parent-editor parent-ed] [position position])]
                                 [style (make-style position code)])
                             (send ed set-snip! sn)
                             (send parent-ed insert sn)
                             (define albatross (lazy dog (eating dirt)))
                             (if (list? code)
                                 (let ([builder (λ (sub pos) (build-gui-block sub ed (append position `(,pos))))]
                                       [kids (map builder code (range 0 (length code)))])
                                   (set-style! style sn ed)
                                   `(,(block-data position 'list style ed sn) ,@kids))
                                 (begin (set-style! style sn ed)
                                        (send ed insert (~v code))
                                        `(,(block-data position 'atom style ed sn)))))))

#;(define original-source '(define (update-gui)
                             (let ([new-main-board (new fruct-board%)]
                                   [new-kit-board (new fruct-ed%)]
                                   [new-stage-board (new fruct-ed%)]
                                   [stage-board-snip (new fruct-sn% [editor new-stage-board] [parent-editor new-main-board])]
                                   [kit-snip (new fruct-sn% [editor new-kit-board] [parent-editor new-main-board])])

                               (set! stage-gui (new-gui source new-stage-board))
                               (set! kit-gui (new-gui kit new-kit-board))
    
                               (send new-main-board insert stage-board-snip)
                               (send new-main-board insert kit-snip)
    
                               (send new-main-board move-to stage-board-snip 200 0)
                               (send my-canvas set-editor new-main-board)
                               (send new-main-board set-caret-owner #f 'global))))

#; (define original-source '(let ([a b] [c d]) e f))

#; (define original-source '("0" "1" ("20" "21" ("220")) "3"))


; macros & helpers ------------------------------------

; maps style templates into obj-src
(define-syntax-rule (style-match source obj-src
                                 [<pat> <style-pat>] ...)
  ;(define (gui-pass:forms source obj-src [form-context "none"])
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


; recursively applies payload via pattern
(define-syntax-rule (gui-pass [<pattern> <payload> ...] ...)
  (letrec
      ([recursor
        (match-lambda
          [<pattern> <payload> ...] ...
          [ls (map recursor ls)])])
    recursor))


; temp hack so selector fucks rendering less
(define-match-expander ♥
  (syntax-rules ()
    [(♥ <thing>)
     (app (λ (source) (match source
                        [`(,(== selector) ,a) a]
                        [_ source])) `<thing>)]))


; gui objs & structs ------------------------------------

(struct fruct (sort type name text style meta) #:transparent)
(struct meta (sn ed parent-ed))

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


; gui fns ----------------------------------------------

(define (atom->string source)
  (cond [(symbol? source) (symbol->string source)]
        [else (~a source)]))

(define (gui-pass:object source [parent-ed "no default"])
  (let* ([ed (new fruct-ed%)]
         [sn (new fruct-sn% [editor ed] [parent-editor parent-ed])]
         [mt (meta sn ed parent-ed)])
    (if (list? source)
        `(,(fruct 0 0 0 "?" 0 mt) ,@(map (λ (sub) (gui-pass:object sub ed)) source))
        `(,(fruct 0 0 0 (atom->string source) 0 mt)))))


(define (gui-pass:forms source obj-src)
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


#; `(define ,(atom id) ,expr ...)
#; `(wrapper (head) (name) (hole) ...)
; get symbol 'define from first expression then rewrite second
; 


(define (cascade-style style parent-style position)
  0)
; need to fill in references to default and parent properties


(define/match (gui-pass:cascade-style obj-src [parent-style 'default] [position '()])
  [((fruct sort type name text (app (λ (x) (cascade-style x parent-style position)) style) mt) _ _)
   (fruct sort type name text style mt)]
  [(`(,(fruct sort type name text (app (λ (x) (cascade-style x parent-style position)) style) mt) ,ls ...) _ _)
   `((fruct sort type name text style mt)
     ,@(map (λ (sub p-s pos) (gui-pass:cascade-style sub p-s (append position `(,pos))))
            (rest obj-src)
            (range 0 (length (rest obj-src)))
            (cascade-style style parent-style position)))])


(define (new-gui source parent-ed)
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




; core fns ---------------------------------------------

(define (update-kit kit kit-gui source stage-gui key-code)
  (when (equal? (get-selection source) '())
    (println "errrrrrr"))
  ;(pretty-print stage-gui)
  ((compose
    (update-section 'meta (meta-info (get-obj-selection stage-gui)))
    (update-section 'env (destruct-selection (get-selection source)))
    ) kit)
  
  )

(define (meta-info obj)
  (match-let ([(fruct a b c d e _) obj])
    `(,a ,b ,c ,d ,e)))



(define/match (destruct-selection source)
  [(`(define ,a ,b)) `((1 ,(if (list? a)(first a) a) ..) (2 ,(if (list? b)(first b) b) ..))]
  [(`(,head ,ls ...)) (println source)(map (λ (x y) `(,y ,(if (list? x)(first x) x) ..)) ls (range 1 (add1 (length ls))))]
  [((atom a)) `((1 ,a))]
  [(_) '("empty")])

(define/match (get-obj-selection obj-source)
  [(`(,(fruct _ _ 'selector _ _ _) ,whatever (,selected ,a ...))) selected]
  [(`(,(fruct _ _ _ _ _ _))) #f]
  [(_) (let ([result (filter identity (map get-obj-selection (rest obj-source)))])
         (if (empty? result) #f (first result)))])




; update-gui: create a new gui to replace the one currently in the canvas

(define (update-gui)
  (let* ([new-main-board (new fruct-board%)]
         [new-kit-board (new fruct-ed%)]
         [new-stage-board (new fruct-ed%)]
         [stage-board-snip (new fruct-sn% [editor new-stage-board] [parent-editor new-main-board])]
         [kit-snip (new fruct-sn% [editor new-kit-board] [parent-editor new-main-board])])

    (set! stage-gui (new-gui source new-stage-board))
    (set! kit-gui (new-gui kit new-kit-board))
    
    (send new-main-board insert stage-board-snip)
    (send new-main-board insert kit-snip)
    
    (send new-main-board move-to stage-board-snip 200 0)
    (send my-canvas set-editor new-main-board)
    (send new-main-board set-caret-owner #f 'global)))


; relativize-direction: change direction of nav keystrokes depending on visual layout

(define (relativize-direction key-code sn parent-ed)
  (define (before-linebreak?)
    (let* ([snip-pos (send parent-ed get-snip-position sn)]
           [next-snip (send parent-ed find-snip (add1 snip-pos) 'after-or-none)])
      (if (equal? next-snip #f)
          #f
          (equal? (send next-snip get-text 0 1) "\n"))))
  (define (after-linebreak?)
    (let* ([snip-pos (send parent-ed get-snip-position sn)]
           [prev-snip (send parent-ed find-snip snip-pos 'before-or-none)])
      (if (equal? prev-snip #f)
          #f
          (or (equal? (send prev-snip get-text 0 1) "\n")
              (equal? (send prev-snip get-text 0 1) " ")
              #|this hacky second case deals with indents|#))))
  (cond
    [(and (before-linebreak?) (equal? key-code #\s)) #\d]
    [(and (after-linebreak?) (equal? key-code #\w)) #\a]
    [else key-code]))

; notes on updating for atomic nav:
; first draft:
; when press up, find first grandparent with parent with vertical format
; select next sibling of that grandparent,
; unless last, then find next grandparent with vertical format, etc


; toggle-mode: toggles mode

(define (toggle-mode!)
  (if (equal? mode 'navigation) (set! mode 'text-entry) (set! mode 'navigation)))


; char-input: main interaction loop

(define (char-input event)
  (match-let* ([key-code (send event get-key-code)]
               [pos (sel-to-pos source)]
               [obj (obj-at-pos stage-gui pos)]
               [(fruct sort type name text style (meta sn ed parent-ed)) obj]
               #;[(block-data position parent-ed type style ed sn) obj])
    (when (not (equal? key-code 'release)) ; lets ignore key releases for now
      (case mode
        ['navigation (match key-code
                       [#\space (toggle-mode!)
                                (send parent-ed set-caret-owner sn 'global)
                                (send ed set-position 0)]                                     
                       [_ (set! key-code (relativize-direction key-code sn parent-ed))
                          (set! source (update source key-code))
                          (set! stage-gui (new-gui source (new fruct-ed%))) ; temp hack so stage-gui is current for next line
                          (set! kit (update-kit kit kit-gui source stage-gui key-code))
                          (update-gui)])]
        ['text-entry (match key-code
                       [#\space (toggle-mode!)      
                                (let ([input-chars (send ed get-text 0 num-chars)])
                                  (set! source ((insert-form input-chars) source))
                                  (set! num-chars 0))
                                (update-gui)]
                       [(app string (regexp #rx"[A-Za-z0-9_]")) (set! num-chars (add1 num-chars))
                                                                (send ed insert key-code)])]))))


; gui setup ---------------------------------------------

(define my-frame
  (new frame%
       [label "fructure"]
       [width 1300]
       [height 900]))

(define my-canvas
  (new editor-canvas%
       [parent my-frame]))

(send my-frame show #t)

; append selector
(define source (simple-select original-source))

; setup kit
(define kit '(kit (env) (meta))#;(simple-select '(env)))

; init guis
(define stage-gui '())
(define kit-gui '())

(update-gui)

; init globals
(define mode 'navigation)
(define num-chars 0)

; -------------------------------------------------------

#; (send my-frame set-alignment 'center 'center) ; ineffective?

#; (define (traverse gui)
     (if (list? gui)
         (map traverse gui)
         (block-data-sn gui)))


; notes -------------------------------------------------

; note on nav fixing

; things nav might depend on:
; style (direction, child-num, linebreaks)
; whether or not it's an atom
; style/atomicity of parent/children/siblings

; more advanced nav:
; 'sibling' cases for identically shaped siblings which can be moved around in like a grid
; must be both horizontal or both vertical
; identical shaped := something like: if sibling has same 'list shape' up to the level of depth where the selector is


; if horizontal, should right go: ($new (f ...) -> (new ($f)) ie skip to first child?
; makes it slower to get to second child (as well as selecting child instead of child.first obviously)
; but to alleviate bracketted case: when press space to edit an atom at beginning of list, could automatically select parent list




; notes on modes

; what if the key used to quit insert mode picked the action
; like instead of insert, exiting with SHIFT searched on entered string instead
; could use as main command entry mode
; would need to actually build an intermediary structure though, not just an intermeidary string
; and draw that structure as an overlay somehow (to start, let's try a different pane)


; notes on panes

; three to start
; stage, kit, and pattern



; pattern autoselect:
; when select node, automatch it's subtree to available patterns
; generate hashmap of pattern vars to selected's children
; for first child (interpreted as symbol literal), color grey
; for other childern, re-color according to color map (in correspondance with hashmap)
; populate kit.environment with hashmap (in list form?)



; affordances:
; write <...> into tree, select it to create new child of relevant type (when parent form takes list args)
; affordances are contextual patterns; they depend on (their position in) their parent form

#; (gui-pass:object '(atom atom) "noneparent-ed")
; transformers:
; as close to possible as exactly what meets the eye
