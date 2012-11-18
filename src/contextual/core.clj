;; If you use this namespace, name this file for_the_glory_of_art.clj and put
;; it in the 'src' directory of the project (lein sets the classpath here)   
(ns contextual.core
  (:use quil.core))
;;(use 'clojure.repl)

;; This is Quil-specific.
(defn setup []
  (smooth)
  (frame-rate 60)
  (set-state! :mouse-position (atom [0 0]))
  (background 0))

(defn mouse-moved []
  (let [x (mouse-x)  y (mouse-y)]
    (reset! (state :mouse-position) [x y])))

;; This is not.

;; Return the default transform map, letting 'xform' override what is there;
;; e.g. (default-xform { :xscale 0.5 :yscale 0.5 })
(defn default-xform [{:as xform}]
  (merge {:xscale 1 :yscale 1 :xtrans 0 :ytrans 0 :rotate 0
          :xshear 0 :yshear 0 :hue 0 :sat 0 :brightness 0 :alpha 0
          :child nil}
         xform))
;; todo: fix scale/xscale/yscale inconsistency

;; Generate a rule tree given a list of transform properties, e.g.
;; e.g. (rule rulename
;;            { :xscale 0.5 :child shape1 }
;;            { :yscale 0.5 :rotate 0.1 })
;; 'ref' is an identifier - symbol, string, whatever you want. But if you want
;; to refer to this rule from a :child field anywhere inside of this, you must
;; use this same identifier there.
;; Return value is like (ref xform xform xform...), where xform is a map with
;; transform parameters.
(defn rule [ref & xformlist]
  (cons ref (map default-xform xformlist)))

;; By convention: If no xform list is given, then this rule is taken as a
;; reference that will be resolved later.
;; Why this is here: Suppose you're trying to define rule A.  Rule A invokes
;; rule B.  Rule B itself invokes rule A.  While this is a perfectly valid
;; recursive definition, we can't express this directly.

;; Primitives (tentatively)
(def square (symbol "square"))
(def circle (symbol "circle"))
;; (def triangle (symbol "triangle"))
;; this will conflict as long as we have :use quil.core

;; Compose two transforms, xf1 and xf2
(defn compose-transform [xf1 xf2]
  ;; All of the values on compose-fn are the functions which compose two
  ;; parameters in a transform (i.e. :xscale is *, which means that if you
  ;; compose two scale transforms, the amount multiplies)
  ;; TODO: The color transforms may need clamping.
  (let [compose-fn {:xscale * :yscale * :xtrans + :ytrans + :rotate + :xshear *
                    :yshear * :hue + :sat + :brightness + :alpha *}]
    ;; For every key in compose-fn, generate an (operation parameter) pair
    ;; by composing the transform operations in xf1 and xf2 with the given
    ;; functions; turn this list of (op param) into a map, like our inputs
    (into {}
          (map (fn [op] [op ((compose-fn op) (xf1 op) (xf2 op))])
               (keys compose-fn)))))

;; Observation: Anyplace where our tree splits into multiple branches, we must
;; maintain some sort of stack for backtracking, definitely if we're doing
;; depth-first, maybe even if we're doing breadth-first.

;; rule-tree: The tree which we're going to walk.
;; tree-map: A map from ref symbols to trees, should we need it.
;; local-xform: The current transform on our stack.
(defn rule-walk [rule-tree tree-map local-xform]
  (let [ref (first rule-tree)
        xforms (rest rule-tree)]
    (if (empty? xforms)
      ;; Resolve a reference first if needed.
      ;; TODO: Make this call fail more gracefully for an undefined reference
      ;; (i.e. 'ref' is not in tree-map)
      (rule-walk (tree-map ref) tree-map local-xform)
      ;; Otherwise, see if we're drawing so small by now that we can just stop.
      ;; TODO: Make this not a constant. Set it based on pixel size.
      (if (or (> (local-xform :xscale) 0.001)
              (> (local-xform :yscale) 0.001))
        (do
          ;; Add rule-tree to the map of references, in case some rule inside it
          ;; needed to reference it recursively.
          (let [next-map (assoc tree-map ref rule-tree)]
            (map (fn [branch]
                   (rule-walk (branch :child)
                              next-map
                              (compose-transform local-xform branch)))
                 xforms))
          (
           ;; side-effect stuff here... (this function doesn't draw anything
           ;; yet)
           ;; Other curious part: where do primitives actually go?  How do I
           ;; detect them?  Right now I have a lovely tree that expresses
           ;; transforms and recursive relationships, but no substance!
           )
          )))))

;; This is not yet organized, but is known to function mostly right:

;; shapefn is a function which draws the shape in question. For the sake of
;; sanity, it should always draw it centered at the origin; if this means
;; you need to translate it, then do it. The reason for this is that scaling
;; and rotation are always relative to the center.
(defn recurse [shapefn rot tx ty sx sy]
  (defn rec [global_sx global_sy]
    (if (or (> global_sx 0.005) (> global_sy 0.005))
      (do (shapefn)
          (translate tx ty)
          (rotate rot)
          (scale sx sy)
          ;; sx2, sy2 keep track of global scale so that we know when to bail out.
          (rec (* global_sx sx)
               (* global_sy sy)))))
  ;; We save our current matrix, and let 'rec' apply everything cumulatively.
  (push-matrix)
  (rec 1.0 1.0)
  ;; Then return to where we started:
  (pop-matrix))

;; At a higher level, what 'recurse' does is:
;; (1) Receive a function.
;; (2) Receive a set of transforms that influences what happens upon calling
;; that function.
;; (3) Recursively call that function and apply those transforms.
;; (4) Bail out of that recursion when the transforms have hit some point, such
;; as the scale being too small for the shape to be visible. To do this, we
;; must know how to compose the transformations.
;; (5) Optionally: Keep track of the boundaries of things. We don't do this,
;; but it would be a nice thing to do, and Context Free does it.
;;
;; Even if we move into something with different semantics (say, stateless
;; drawing of shapes, not stateful like P5 and to some extent OpenGL), we can
;; probably still keep this all fundamentally the same just by generating some
;; closures that are coupled to each other.

(defn square []
  (rect -0.5 -0.5 1 1))

(defn circle []
  (ellipse 0 0 0.5 0.5))

(defn start []
  (let [width 800
        height 800]
    ;; One demo I rather like, albeit simple. Move the mouse around a bit.
    (defn draw_kaleid []
      (background 0)
      ;;(stroke 0 0 0 255)
      (fill 0 255 0 32) 
      ;;(stroke-weight 2)
      (no-stroke)
      (let [[mouseX mouseY] @(state :mouse-position)
            side 1
            ;;shapefn #(rect (/ side -2) (/ side -2) side side)
            shapefn square
            rot (- (* (/ mouseX width) 6.28) 3.14)
            sx (/ mouseY (* 1.05 height))
            sy sx
            tx (* sx side)
            ty 0.0]
        (translate (/ side 2) (/ side 2))
        (scale width width)
        (recurse shapefn rot tx ty sx sy)))
    ;; r,g,b here are the fill color.
    (defn draw_kaleid2 []
      (background 0)
      (no-stroke)
      (let [[mouseX mouseY] @(state :mouse-position)
            side 1
            ;;shapefn #(rect (/ side -2) (/ side -2) side side)
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
      :draw draw_kaleid2
      :mouse-moved mouse-moved
      :size [width height])))

(defn -main []
  (start))