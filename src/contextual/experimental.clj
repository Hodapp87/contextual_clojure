;; walking rule tree:
;; If this is recursive, then it needs to return things we can compose,
;; and we need to decide what it produces or what it modifies (latter only
;; if it is to operate directly via side-effects)
;; Trampoline may help here.
;; Rule tree is produced by (rule ...).
;; Rule tree is then a sequence of transforms which include a child.
;; Child is either a rule tree, a primitive, or 'nil'. 'self' is a special case
;; which is taken to mean that the rule instantiates itself recursively.

;; Unsolved issue: What if I want the 'self' to refer to something higher up in
;; the tree, not the immediate branch?
;; I will need some sort of naming here. Maybe a field which receives a symbol
;; so that parts inside the tree may refer back up as needed? It's either this
;; or I must specify a level (e.g. 0 for the same rule, 1 for the parent rule)
;; but this seems more hackish.

; You don't need to answer whether or not it's a scene graph. You can just
; generate an AST that happens to be a scene graph and also happens to be the
; code that visualizes it, given an appropriate set of bindings.

; Questions to still handle:
; (1) How will I handle loops and tiling? How much of Lisp's parsing/evaluating
; do I retain access to?
; (2) Process of: (rule...) -> scene graph -> API calls/directives (P5, OpenGL,
; Canvas, SVG, PDF...)
; (3) What if the AST could include in parts of a scene graph?
; (4) What is the null shape, for those cases where recursion should terminate
; based on some probability value?
; (5) How do we handle bailout from recursion?
; (6) At some point, all my rules must reduce down to primitives. What are my
; primitives? Particularly, in a Clojure sense, how do I make them?
; (7) How do I represent randomness in the rules? i.e. like my rule-choose
; structure

