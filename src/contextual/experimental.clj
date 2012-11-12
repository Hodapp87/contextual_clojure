
; ------------------ 
; Experimental stuff
; ------------------

; Return the default transform map, letting 'xform' override what is there;
; e.g. (default-xform { :xscale 0.5 :yscale 0.5 })
(defn default-xform [{:as xform}]
  (merge {:xscale 1 :yscale 1 :scale 1 :xtrans 0 :ytrans 0 :rotate 0
          :xshear 0 :yshear 0 :hue 0 :sat 0 :brightness 0 :alpha 0
          :child nil}
         xform))
; todo: fix scale/xscale/yscale inconsistency

; Generate a rule tree given a list of transform properties, e.g.
; e.g. (rule { :xscale 0.5 :child shape1 }
;            { :yscale 0.5 :rotate 0.1 })
; If the :child property is nil or is not given, then this is treated as the
; rule instantiating itself recursively.
(defn rule [& arglist]
  (map default-xform arglist))

; Primitives (tentatively)
(def square (symbol "square"))
(def circle (symbol "circle"))
; (def triangle (symbol "triangle"))
; this will conflict as long as we have :use quil.core

; --- Code above, notes below ---

; Where I have left off:
;  - If I need rules to be mutually recursive - and honestly, for most of what
; I have done in CF, this is essential - then I cannot evaluate them right
; away.
;  - Look into, perhaps, the use of trampolines here, and structure the rules
; as functions which generate rules.

; Next piece: We need the code that walks the tree of rules and turns it into
; something else. Is that something else a scene graph, or is it direct
; commands into an API? Do we need a scene graph? We will for SVG, but for
; other representations we might do just as well without one.

; Process a rule tree
(defn walk_rules [rule-tree]
  nil)
; If this is recursive, then it needs to return things we can compose,
; and we need to decide what it produces or what it modifies (latter only
; if it is to operate directly via side-effects)

; You don't need to answer whether or not it's a scene graph. You can just
; generate an AST that happens to be a scene graph and also happens to be the
; code that visualizes it, given an appropriate set of bindings.

; A rule tree can be recursive and can be non-deterministic. A scene graph -
; I think - should have both of these traits eliminated.

(def othershape nil)
(def moreshapes nil)
(def someshape (rule othershape { :xscale 0.5 :yscale 0.5 :xtrans 1 :ytrans 1 }
                     moreshapes { :xscale 0.25 :yscale 0.25 }))
;(def shapefoo (rule-choose 0.1 shape1
;                           0.2 shape2))

; This needs to work and I'm not sure it does:
(def recursive_shape6 (rule recursive_shape6 { :xscale 0.25 :yscale 0.25 }))

; There is also no way to make a rule, just with (rule...), which refers to
; itself. This is, very likely, my entire issue.

; If this convention is followed:
;  - The maps contain transformations, and these maps can be handled as single
; values, e.g.
(def xform { :xscale 0.25 :yscale 0.25 :rotate 0.1 })
(def squares (rule blah xform))

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

