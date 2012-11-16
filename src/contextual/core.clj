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

;; Primitives (tentatively)
(def square (symbol "square"))
(def circle (symbol "circle"))
;; (def triangle (symbol "triangle"))
;; this will conflict as long as we have :use quil.core

;; Compose two transforms
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

(defn rule-trampoline [rule-tree tree-map]
  ;; What we need to keep track of: Current transform.
  ;; What we need to pass on: A tree; a map of any ref'ed symbols to resolve
  ;; (yes, technically, if this is trampolining then we need to pass a function
  ;; call utilizing these)
  ;; What we need to do: Inspect our tree and figure out how to recurse further.
  ;; Breadth-first or depth-first? (If and only if multiple rules are present)
  ;; We must compose the transform with the next. We must also bail out at some
  ;; point. Bailing out doesn't mean quitting the function - it could also mean
  ;; moving onto another rule because the chain we've been on is drawing too
  ;; small.
  ;; We also don't handle any sort of randomness, hmmm...
  (let [ref (first rule-tree)
        xforms (rest rule-tree)
        next-map (assoc tree-map ref rule-tree)]
    ;; Here is where you left off.
    nil)

;; This is not yet organized:

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
