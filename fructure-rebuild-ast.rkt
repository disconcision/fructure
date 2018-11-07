#lang racket


`(datum d)
`(var v)
`(λ (id) expr)
`(app expr expr)
`(if expr expr expr)

; fns: cons


`(grammar:
  (expr (alt `hole
             `(datum ,num)
             `(var ,id)
             `(λ (,id) ,expr)
             `(app ,expr ,expr)
             `(if ,expr ,expr ,expr)))
  (id (alt (/id alpha) ; or empty id?
           (/id (seq id alphanum)))) ; note this order is left-rec
  (num (alt digit
            (seq digit num)))
  (alphanum (alt alpha digit))
  (alpha (? alpha?))
  (digit (? digit?)))


;transformations:
; constructors
#;([,expr -> `(datum ,num)]
   [,expr -> `(var ,id)]
   [,expr -> `(λ (,id) ,expr)]
   [,expr -> `(app ,expr ,expr)]
   [,expr -> `(if ,expr ,expr ,expr)]
   [,id -> `(/id ,alpha . ,+id)]
   [`(/id ,alphas ...) -> `(/id ,alphas ... ,alpha)])
; the semantics of unquote above is basically hole

; destructors
#;([`(datum ,num) -> ,expr]
   [`(var ,id) -> ,expr]
   [`(λ (,id) ,expr) -> ,expr]
   [`(app ,expr ,expr) -> ,expr]
   [`(if ,expr ,expr ,expr) -> ,expr]
   [`(/id ,alphas ...) -> ,id])

; snippets
#;([,expr ->`((λ (,id) ,expr:body) ,expr:init)] ; let as pattern
   )
; side note: named holes as above? as annotation?

; transforms
#;([`((λ (,x) ,body) ,init) -> `(let ([,x ,init] ,body))]
   )





; this is more graph than tree though...
`(meso-model-grammar:
  (closure code env)
  (env parent-env bindings)
  (bindings `((,id ,val) ...)))
; this is going to take some thought...





; interface:



; mode patterns:
; modes : select search transform
; better modes?: searchlection
; do mode as a prop:
; current game loop:
#;[_ (case mode
       ...
       ['transform (match key-code
                     ...
                     ['down (!do ([(c▹▹ ,a) ⋱↦ ((c▹▹ ,a))]))]
                     ...)]
       ...)]

; current transform:
#; [(c▹▹ ,a) ⋱↦ ((c▹▹ ,a))]
; mode version:
#; [([mode : transform] ⋱ (c▹▹ ,a)) -> ((c▹▹ ,a))]
; or more symmetric form?:
#; [([mode : transform] ⋱ (c▹▹ ,a)) -> ([] ⋱ ((c▹▹ ,a)))]
; to avoid too much duplication in interface we project these as such:

#; (gameloop
    ...
    ([mode : transform]
     ...
     'down [⋱(c▹▹ ,a) -> ((c▹▹ ,a))]
     (or 'escape 'return) (compose [⋱(s▹ ,buffer ,selection) -> (▹ ,selection)]
                                   ; bare ⋱ looses the context; your transform will be local
                                   ; putting ⋱ in sexp  and capturing props allows for changes on top level
                                   [⋱(▹▹ ,subselection) -> ,subselection]
                                   ; top level change: mode switch
                                   ; (empty [] captures existing props
                                   [([] ,a ...) -> ([mode : select] ,a ...)]                                   
                                   )   
     ...))




; interface elements
; top level panels
;   world
;   search pattern
;   paint (selection) pattern
;   transforms list
;     filter pattern
;     (sort)


; meta-interface elements
;   editable grammar (or integrate as grammar pebel)
;   editable stylesheet (or integrate this direct into regular editing via a style panel?)
;   (both of above can be selected-node specific; accessing on top yields general case)
;   editable catelog of metamodes/gameloops (each is a mode/transform machine triggered by keypresses)

; idea: make the grammar an editable element which is seperate for each world tree node
; nodes (by default?) inherit their parents grammar, but you can stack transformations on top of that
; these transformations, e.g. additions to the grammar, are recorded as props (scope/bindings)
; later: add rules to generate/instantiate these props from/as syntactic (binding) forms in the object grammar



; detailed selection mode:
; press a key to get an 'expanded view' of current node, i.e.
;   (a) expand important properies, including comments and annotations; english-expanded-form
;   (b) (?) ellipses-hole-patterns made visible
;   (c) (?) help search hits on subtree



; we need a standard (limited) form of searchlection/editing which is used
; to (a) select between current metamode



; an inventory 'stone' which magically creates e.g. the constrcutors/destructors of a grammar
; i.e. physicalize classes of transforms for which the instances can manifest when required
; so if you have this stone you can do the kind of free-editing it exposes





; HOW TO STYLEPARSE
; problem 1: affordances
#|
model: style language. small dsl.
lookup sort in sort style table, get style attributes
each style attribute can be either:
0. literal style data
1. (lookup location-name) ; location-name = {parent}
2. (from-prop property) ; attribute set equal to some syntax property
    e.g. (a property calculated as a mapping from) node depth

this pushes the algo work to the attribute engine.


basic styling sketch (algo version)
start at top with parent style = default style
send parent style when recursing on children
if child is syntax


seperated version:
1. create property called 'parent style'
   this is populated by a single parent style pass

2. create properties called 'syntactical parent style' & 'snaffo parent style'
   these can be trivial folded into a single pass
   here, all snaffos pass on the syntactical parent style unchanged,
   and all syntacticals pass on the snaffo parent style unchanged


but wait, styling isn't the issue, recognition is. we need to first calculate sort in the presence of snaffos
can we exploit similar mechanisms?
when parsing for sort, we need to match patterns, which may descent an arbitrary number of levels into the tree
as we descend (recurse) we might hit a snaffo. we want the pattern matcher to ignore the snaffo
if the snaffo aligns to a pattern variable, match it but bind nothing
if the snaffo aligns to a structural component (sublistform) of a pattern, proceed matching the subform in the snaffos primary slot
so snaffo model: snaffos (like forms) can have structural components(lists and symbols), and slots, one of which is labelled primary
(or just use the first slot by default?)


MAYBE ASIDE: idea for workaround to styleparse problem
lets say that initially (for some value of initially), there are no passthrough snaffos in the object tree
i.e. there are only bounding snaffos (top and holes)
sort attribute could just be calculated then, provided we passed them along correctly during world-transforms

seperate notes:
1. idea: have a seperate property called inherited-tint-factor for all nodes
     this can be set by e.g. pattern vars to tint the subtree under them
     mechanics: would the parser have to send them to the children? how would the children know to take them seriously?


game side notes:
1. introducing scripting as solving minor annoyances for fun and profit
   problem: mosquitos appears. initially they are mostly noise-annoying.
   but they do slow your movement speed a tiny bit. in some areas though
   mosquitos swarm, slowing your movement rate to near zero.
   solution: introduce a hand/flyswatter golem, capable of learning a single (nested) conditional
   'basic' mosquitoes would be easy. if mosquito-lands then swat-it. but more clever ones need more complicated strategies
   primitives would include a growing number of obervation variables and a (smaller) catalog of actions (initially wait & swat)
   you also need to find/make better golems to hold more complex decision structures.
   in general: all kinds of critters getting up in your business, causing various disadvantage
   either spend the time dealing with their issues, or build better passive defenses... by programming them!

inteface notes:
1. use scroll wheel (???) as continous undo/rollback? like be entering text for autocomplete,
   but forced autocomplete, so theres always a result or nothing there, and if theres a result, there
   is a highlighted potion which is precisely those letters which were manually inputed, and outlines of nodes which
   were explictly created by pressing space

2. general metaphor of the interface: flowing through configutation space

3. do we want a notion of 'local history' for undo? like bringing a subtree to a previous state
   (when possible) without affecting outer tree?

4. option: make undo tree a graph; current states are scanned against past ones to create the
   possibility of cycles. challenges: uniqueness of identity (same structure, different ids?). efficiency (hashing possible?)
   benefit: ? ability to 'equate' paths of transformations, in a limited way.
   benefit: natural setting to store/investigate usage data e.g. solutions to synatactic patterns as diverging then converging paths in config space.
   maybe bad cost-benefit on this one.

5. notion of inidividual structures as coordinates in configuarion space

6. notion of command-affordance as 'future-pointing'. the template slot is more 'futureward' than is the pattern slot.
   possibly represented visually via ortho-projection towards/away-from screen


7. blurring the object-interface distinction; world-transformations versus object-transformations

8. review of editor aspects/panels
     current state
       searchlection pattern
     attributes (of current selection; when not inlined. aside: actually, what CANT we inline?)
       attributes can include current active grammar (option to create an edited version as manual attribute)
       and also current style (editing just overrides locally; is there a clean way to make it global (ie in terms of grammar) if desired?)
       editing options for editing a form as inidivudal or as representative of the form template in the grammar?
       open questions of form metapysics: individual primacy or template primacy?
     toolkit
     timeline

|#

#|

time model:

right now we have 'current state'.
actually, we have two notions of state: state of the object, and state of the world (all user-configurable content)
should there be separate undo-systems for each?
object transformations as distinct from world transformations?
can this help abstract the 'undoing undo' issue? object-undo would be a world action, not an object action.
might just push the issues back one meta-level; analyse further?
so on the object level. all actions are transformation game objects.
so edges in the time tree/graph can be labelled with those transformations, plus a time stamp.
actually, just make them markupable with basically the same annotation system as syntax objects.
aside: again, the idea occurs: algorithmic autogenerated variations all based on a certain object 'atom' (in react sense)
so to implement:
set/list of vertices: 'object states'. distinguished 'original' root and 'current' vertices
set/list of directed edges: 'object changes'
fundamentally, all we need is the root vertex and the edges. this suffices to calculate any vertex
efficiency considerations dictate which other vertices are kept/cached
aside: style attributes calculated based on past states: e.g. 'average' of states 'cloud' as volatility measure?
visually: easiest: as long as its a tree, undo graph could be rendered just like a syntax tree.
but how to represent edge attributes?
operations we want to support:
  go back to previous state
  select and go forward to some particular subsequent state
what about object undos that effect world state? like if some subtree is bound in the palette? (slash pattern variables in the tree)
probably want to let object undos override/eliminate/ignore 'intermediary' interface states somewhow...
interface option:
  allow a navigation/selection mode in the time graph (while it's a tree) very similar to that in the syntax tree
  currently highlighted state would be made current
interface: logarthmic (actually more like inverse) display of previous vertices, edge lengths approaching zero to the left/past


aside: notion: sketch-building languages/structures by first building things you want into syntax as manual properties,
  then extending the object language so those desired properties can be derived from the syntax object.
  'property->syntax workflow'


|#

#;(menus grammars
         keybindings)

#;(windows world
           transforms
           history
           searchlection
           attributes)
; later: projection-pattern?


; main loop
#; (mainloop `(windows ,world
                       ,transforms
                       ,history
                       ,searchlection
                       ,attributes)
             ; get keyboard input
             ; say: F1-5 keys chose window (eaiser key to cycle them?)
             
             `(windows ,new-world
                       ,new-transforms
                       ,new-history
                       ,new-searchlection
                       ,new-attributes))


#|

lets start with moving the cursor around manually annotated syntax objects


for movement, plan in searchlection from the get-go
that is, immediately institute attributes having to do with selectability
to start: selectable :: bool
arrow key back-forward movement in world goes between locations matching the searchlection pattern
future: literal/symbol painting can be done directly on the pattern; this is automatically painted onto selected matches

|#

; movement clauses
#; ( `(,a ... (sel ,b) ,c  ,d ...)
     `([selectable]
       ,a ...  ,b (sel ,c)  ,d ...))
#; ( `(,a ... (sel ,b) ,c ...)
     `([selectable]
       sel ,a ... ,b ,c ...))
; wait that last one doesn't quite work; what if needs to go mutliple levels up
; need a deeper language of containment patterns
; something like:
#; ( `([selectable]
       ,p \\\ (sel ,b) )
     `(sel (,p \\\ ,a ... ,b ,c ...)))
; should the implicit rule be 'find match most local to sel? to internal pattern in general?
; btw, all these rules should be read as nested in an implicit `(,top [(mode: whatever)] \\\ ...)





