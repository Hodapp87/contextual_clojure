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

;; instance: An invocation of another shape, combined with a transform.
;; 'type' can take 3 values:
;; :prim when 'child' is a primitive.
;; :ref when 'child' is an ID to resolve later (mainly for recursive cases)
;; nil when 'child' is a rule list
;; (Also a possibility for the last case: another type for randomness? For
;; repetition? Does that go here?)
(defn instance [child type xform]
  (assoc (default-xform xform) :child child :type type))

;; Return the default transform map, letting parameters in 'instance' override
;; what is there; e.g. (default-xform { :xscale 0.5 :yscale 0.5 })
(defn default-xform [{:as instance}]
  (merge {:xscale 1 :yscale 1 :xtrans 0 :ytrans 0 :rotate 0 :xshear 0 :yshear 0
          :hue 0 :sat 0 :brightness 0 :alpha 0} instance))
;; todo: fix scale/xscale/yscale inconsistency

;; or should randomness and repetition go in this somehow?
(defn rule [id & instances]
  {:id id :rules instances})

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

;; rule: The tree which we're going to walk.
;; ref-map: A map from ref symbols to rules, should we need it.
;; local-xform: The current transform on our stack.
(defn rule-walk [rule ref-map local-xform]
  (let [id (rule :id)]
    ;; See if we're drawing so small by now that we can just stop.
    ;; TODO: Make this not a constant. Set it based on pixel size.
    (if (or (> (local-xform :xscale) 0.001)
            (> (local-xform :yscale) 0.001))
      ;; Add rule-tree to the map of references, in case some rule inside it
      ;; needed to reference it recursively.
      (let [next-map (assoc ref-map id rule)]
        ;; Note that we call the result of invoke-rule.
        (map #((invoke-rule % next-map local-xform))
             (rule :rules))))))

(defn invoke-rule [inst ref-map local-xform]
  (let [instance (if (= (inst :type) :ref) ; resolve references if needed
                   (ref-map (inst :child))
                   inst)
        next-xform (compose-transform local-xform inst)]
    (do
      (print "Ref-map " (keys ref-map))
      ;;(print "\nInstance " instance)
      (print "\nID " (instance :id))
      (print "\nxform " next-xform)
      (print "\n")
      ;; N.B. We do not call the function here - we return a curried call,
      ;; partially to conserve stack space and partially to let rule-walk be
      ;; a little more flexible about what it chooses to do with the result.
      #(rule-walk instance ref-map next-xform))))

(def test-instance
  (rule :test
        (instance :test :ref { :xscale 0.25 :yscale 0.25 :xtrans 1 :ytrans 1 })))

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