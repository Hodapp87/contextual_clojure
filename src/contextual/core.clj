; If you use this namespace, then name this file for_the_glory_of_art.clj
; and put it in the 'src' directory of the project (lein sets the classpath
; here)   
(ns contextual.core
  (:use quil.core))
  
(use 'clojure.repl)
  
(defn setup []
  (smooth)
  (frame-rate 60)
  (set-state! :mouse-position (atom [0 0]))
  (background 0))

(defn mouse-moved []
  (let [x (mouse-x)  y (mouse-y)]
    (reset! (state :mouse-position) [x y])))

; If we lack the ability to explicitly get at our transformation matrix, then
; we will have a hard time figuring out what the bounds of our canvas are.
; However, we don't necessarily need this.

; shapefn is a function which draws the shape in question. For the sake of
; sanity, it should always draw it centered at the origin; if this means
; you need to translate it, then do it. The reason for this is that scaling
; and rotation are always relative to the center.
(defn recurse [shapefn rot tx ty sx sy]
  (defn rec [global_sx global_sy]
    (if (or (> global_sx 0.005) (> global_sy 0.005))
      (do (shapefn)
          (translate tx ty)
          (rotate rot)
          (scale sx sy)
          ; sx2, sy2 keep track of global scale so that we know when to bail out.
          (rec (* global_sx sx)
               (* global_sy sy)))))
  ; We save our current matrix, and let 'rec' apply everything cumulatively.
  (push-matrix)
  (rec 1.0 1.0)
  ; Then return to where we started:
  (pop-matrix))

; At a higher level, what 'recurse' does is:
; (1) Receive a function.
; (2) Receive a set of transforms that influences what happens upon calling
; that function.
; (3) Recursively call that function and apply those transforms.
; (4) Bail out of that recursion when the transforms have hit some point, such
; as the scale being too small for the shape to be visible. To do this, we
; must know how to compose the transformations.
; (5) Optionally: Keep track of the boundaries of things. We don't do this,
; but it would be a nice thing to do, and Context Free does it.
;
; Even if we move into something with different semantics (say, stateless
; drawing of shapes, not stateful like P5 and to some extent OpenGL), we can
; probably still keep this all fundamentally the same just by generating some
; closures that are coupled to each other.

(defn square []
  (rect -0.5 -0.5 1 1))

(defn circle []
  (ellipse 0 0 0.5 0.5))

(defn start []
  (let [width 800
        height 800]
    ; One demo I rather like, albeit simple. Move the mouse around a bit.
    (defn draw_kaleid []
      (background 0)
      ;(stroke 0 0 0 255)
      (fill 0 255 0 32) 
      ;(stroke-weight 2)
      (no-stroke)
      (let [[mouseX mouseY] @(state :mouse-position)
            side 1
            ;shapefn #(rect (/ side -2) (/ side -2) side side)
            shapefn square
            rot (- (* (/ mouseX width) 6.28) 3.14)
            sx (/ mouseY (* 1.05 height))
            sy sx
            tx (* sx side)
            ty 0.0]
        (translate (/ side 2) (/ side 2))
        (scale width width)
        (recurse shapefn rot tx ty sx sy)))
    ; r,g,b here are the fill color.
    (defn draw_kaleid2 []
      (background 0)
      (no-stroke)
      (let [[mouseX mouseY] @(state :mouse-position)
            side 1
            ;shapefn #(rect (/ side -2) (/ side -2) side side)
            shapefn square
            rot (- (* (/ mouseX width) 6.28) 3.14)
            sx (/ mouseY (* 1.05 height))
            sy sx
            tx (* sx side)
            ty 0.0]
        (translate (/ side 2) (/ side 2))
        (translate (/ width 2) (/ width 2))
        (scale (/ width 2) (/ width 2))
        (fill 0 255 0 32) 
        (recurse shapefn rot tx ty sx sy)
        (fill 0 0 255 32)
        (recurse shapefn (* rot 2) (- tx) (- ty) sx sy)))
  (defsketch example
      :title "Recursion"
      :renderer :opengl
      :setup setup
      :draw draw_kaleid
      :mouse-moved mouse-moved
      :size [width height])))

(defn -main []
  (start))



; ------------------ 
; Experimental stuff
; ------------------

; Return the default transform map, letting 'xform' override what is there;
; e.g. (default-xform { :xscale 0.5 :yscale 0.5 })
(defn default-xform [{:as xform}]
  (merge {:xscale 1 :yscale 1 :scale 1 :xtrans 0 :ytrans 0 :rotate 0
          :xshear 0 :yshear 0 :hue 0 :sat 0 :brightness 0 :alpha 0}
         xform))
; todo: fix scale/xscale/yscale inconsistency

; Generate a rule tree, given any number of arguments alternating subrule
; and transform, e.g.
; e.g. (rule shape1 { :xscale 0.5 }
;            shape2 { :yscale 0.5 :rotate 0.1 })
(defn rule [& arglist]
  ; Split 'arglist' into subrule/xform pairs; use 'xform' to override defaults:
  (map (fn [pair] (let [[subrule xform] pair]
                    (list subrule (default-xform xform))))
       (partition 2 arglist)))

; Process a rule tree
(defn walk_rules [rule-tree]
  nil)
; If this is recursive, then it needs to return things we can compose.

; --- Code above, notes below ---

; Next piece: We need the code that walks the tree of rules and turns it into
; something else. Is that something else a scene graph, or is it direct
; commands into an API? Do we need a scene graph? We will for SVG, but for
; other representations we might do just as well without one.

; If we use something like this:
; (def someshape (othershape { :xscale 0.5 :yscale 0.5 :xtrans 1 :ytrans 1 }))
; Then what is 'othershape'? It can't be from a definition like this, for it
; is not a function, yet this is a function call. So that is not valid.

; Try something like this:
(def othershape nil)
(def moreshapes nil)
(def someshape (rule othershape { :xscale 0.5 :yscale 0.5 :xtrans 1 :ytrans 1 }
                     moreshapes { :xscale 0.25 :yscale 0.25 }))
(def shapefoo (rule-choose 0.1 shape1
                           0.2 shape2))
; Then this at least is consistent, albeit 'rule' still must be defined.
; All terms passed in as keyword args are evaluted. 'othershape' is okay to be
; evaluated, in fact, it should be evaluated.

; Then what is 'rule'? Need it return anything more than a validated version
; of its inputs? (That is, the dependent rules, and the whole map which is
; passed in - which now has defaults applied, for instance).

; This needs to work and I'm not sure it does:
(def recursive_shape2 (rule recursive_shape2 { :xscale 0.25 :yscale 0.25 }))

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
