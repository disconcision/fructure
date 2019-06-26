#lang scribble/doc

@; adapted from dave herman's memoize docs

@(require (for-label containment-patterns))

@begin[(require scribble/manual)
       (require scribble/example)
       (require scribble/base)]

@(define the-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/match
                         racket/list
                         "main.rkt"))
     the-eval))

@title[#:tag "top"]{Containment Patterns: Capture contexts in s-expressions}

@author[@author+email["Andrew Blinn" "racket@andrewblinn.com"]]



Containment-patterns implements several match-expanders which can be used anywhere
@racket[racket/match] pattern-matching is available. ⋱ , ⋱+ , and ⋱1 descend
into s-expressions to capture arbitarily deep matches and their multi-holed contexts.

Insert a ⋱ in Dr. Racket by typing \ddo (diagonal dots) and then pressing alt+\

@table-of-contents[]

@defmodule[containment-patterns]{}

@section[#:tag "examples"]{An example}

@subsection[#:tag "nightmare"]{Tangerine Nightmare}

Seamlessly extract a 🍊 from a deeply-nested situation 🔥.

@examples[#:label #f
          #:eval the-eval
          (define situation
               `((🔥 🔥
                    (🔥 (4 🍆)))
                 (🔥 (🔥
                     (1 🍊)) 🔥 🔥)
                 (2 🍐) (🔥)))
           (match situation
               [(⋱ `(1 ,target)) target])]

@subsection[#:tag "examples"]{More examples}

@examples[#:label "1. Check if an item is contained in a nested list:"
          #:eval the-eval
          (match `(0 (0 1) 2 3)
            [(⋱ 1) #t])]

@examples[#:label "2. Extract data from a nested context:"
          #:eval the-eval
          (match `(0 (1 zap) 2 3)
            [(⋱ `(1 ,a)) a])]


@examples[#:label "3. Make an update in a nested context:"
          #:eval the-eval
          (match '(0 0 (0 (0 0 (▹ 1)) 0 0))
            [(⋱ context `(▹ ,a))
             (⋱ context `(▹ ,(add1 a)))])]

@examples[#:label "4. Make multiple substitutions.
                   Note how ⋱+ is optional in the template; a context is just a function:"
          #:eval the-eval
          (match '(0 1 (0 1 (1 0)) 0 1)
            [(⋱+ c 1)
             (c 3 4 5 6)])]

@examples[#:label "5. Move a cursor ▹ through a traversal in a nested list of 0s and 1s:"
          #:eval the-eval
          (match '(0 1 (0 1 (1 (▹ 0))) 0 1)
            [(⋱+ c (and a (or `(▹ ,_) (? number?))))
             (⋱+ c (match a [`(,x ... (▹ ,y) ,z ,w ...)
                             `(,@x ,y (▹ ,z) ,@w)]))])]




             

@section[#:tag "patterns"]{Pattern Forms}


@defform*[((⋱ <pattern>)
           (⋱ <context-name> <pattern>))]{
 Traverses a target s-expression left-to-right and depth-first
 until a match to @racket[<pattern>] is found, then hands off control to <pattern>.
 Optionally, the context surrounding the match is captured as a
 unary procedure called <context-name> satisfying
 @racket[(equal? (<context> the-match) orginal-sexpr)].
 In a template @racket[(⋱ <context-name> new-content)] is
 just a gloss for @racket[(<context-name> new-content)].}

@defform[#:id ⋱1 (⋱1 <context-name> <pattern>)]{
 Same as @racket[⋱] except it enforces that the match must be unique.}

@defform*[((⋱+ <context-name> <pattern>)
           (⋱+ <context-name> (until <stop-pattern>) <pattern>)
           (⋱+ <context-name> (capture-when <pattern>) <results-pattern>))]{
 Similar to @racket[⋱] except all matches are captured as an n)-element list, and
 @racket[<context-name>] is bound to an n-ary procedure representing
 an n-holed context.

 If the @racket[until] subform is included, the traversal neither matches nor descent into values
matching the @racket[<stop-pattern>].

@examples[#:label "Example: Toy scope-aware subtitution:"
          #:eval the-eval
          (match `(let ([w 1])
                    z
                    (let ([z 2]) z)
                    (let ([y 3]) z))
                  [(⋱+ c (until `(let ([z ,_]) ,_ ...))
                       (and x 'z))
                   (⋱+ c (make-list (length x) 'new-name))])]

If the @racket[capture-when] subform is included, results meeting @racket[<pattern>]
are captured as a list, which can then be matched against as a whole by
@racket[<results-pattern>]. Note that this is an experimental feature which may be
changed or removed in future versions.

@examples[#:label "Example: moving a cursor"
          #:eval the-eval
          (match '(0 1 (0 1 (1 (▹ 0))) 0 1)
                  [(⋱+ c (capture-when (or `(▹ ,_) (? number?)))
                       `(,x ... (▹ ,y) ,z ,w ...))
                   (⋱+ c 
                       `(,@x ,y (▹ ,z) ,@w))])]

Contrast this example to example 5 above; this variant is essentially
a gloss for nested matches.
}



@section[#:tag "why"]{Why}

I implemented containment patterns to write concise updates on
nested structures for syntax-rewriting purposes in structured-editing
and algebraic-stepper prototypes. See @hyperlink["https://github.com/disconcision/fructure" "fructure"]
for an actual use case, or for something simpler,
@hyperlink["https://github.com/disconcision/racketlab/blob/master/choice-stepper.rkt"
           "this toy stepper"].

@subsection[#:tag "how"]{How}

An 'n-holed context' is a captured composable continuation which
can be called in a pattern template as a normal n-ary procedure. These continuations
are captured as the pattern-matcher left-to-right preorder-traverses the target
looking for matches. 

@subsection[#:tag "gotchas"]{Gotchas}

Caveat: If you're using any matchers which have side-effects,
note that the inner pattern is evaluated twice for each successful match.


@section[#:tag "advanced"]{Advanced use: Traverse arbitrary structures}

@examples[#:label "Example: Toy scope-aware subtitution:"
          #:eval the-eval
          (match `(let ([w 1])
                    z
                    (let ([z 2]) z)
                    (let ([y 3]) z))
                  [(⋱+ c (until `(let ([z ,_]) ,_ ...))
                       (and x 'z))
                   (⋱+ c (make-list (length x) 'new-name))])]


@; @section[#:tag "forms"]{Forms}

@; @racketgrammar[formals id () (id . formals)]

