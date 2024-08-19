; settings, globals, data structures {{{
(ns triangle.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [clojure.math]
            [clojure.core.matrix :as ccm] ; somehow needed
            [clojure.core.matrix.linear :as ccml]))
(defn setup
  []
  (q/frame-rate 144)
  (q/color-mode :hsb 360 100 100 100)
  (q/stroke-weight 0)
  (q/rect-mode :center))

(defn extract-values
  [m]
  (cond (map? m) (mapcat extract-values (vals m))
        (vector? m) (mapcat extract-values m)
        :else [m]))

(defn unpack [fun elems] (apply fun (extract-values elems)))

(defn Point [x y] (array-map :x x :y y))

(defn Triangle [p1 p2 p3] (array-map :p1 p1 :p2 p2 :p3 p3))

(def recursionCount 5)

(def seedTriangle (Triangle (Point -160 100) (Point 160 100) (Point 0 -170)))
(def edges
  [[(:p1 seedTriangle) (:p3 seedTriangle)]
   [(:p3 seedTriangle) (:p2 seedTriangle)]
   [(:p2 seedTriangle) (:p1 seedTriangle)]])
; }}}

(defn midpoint
  [p1 p2]
  (Point (/ (+ (:x p1) (:x p2)) 2) (/ (+ (:y p1) (:y p2)) 2)))

(defn normal
  [p1 p2]
  (let [ortho' (Point (- (:y p2) (:y p1)) (- (:x p1) (:x p2)))
        ortho-norm (ccml/norm [(:x ortho') (:y ortho')])]
    (Point (/ (:x ortho') ortho-norm) (/ (:y ortho') ortho-norm))))

(defn perpendicular-point
  [p1 p2 length]
  (let [midpoint' (midpoint p1 p2)
        normal' (normal p1 p2)
        scaled-normal (Point (* (:x normal') length) (* (:y normal') length))]
    (Point (+ (:x midpoint') (:x scaled-normal))
           (+ (:y midpoint') (:y scaled-normal)))))

(defn recursive-split
  [p1 p2 peak-length recursion-depth]
  (let [endpoint (perpendicular-point p1 p2 peak-length)
        basecase-triangle (Triangle p1 p2 endpoint)]
    (if (zero? recursion-depth)
      [basecase-triangle]
      (concat
        [basecase-triangle]
        (recursive-split p1 endpoint peak-length (dec recursion-depth))
        (recursive-split endpoint p2 peak-length (dec recursion-depth))))))

(defn draw-state
  []
  (q/background 0 0 16)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (let [mouse-len (/ (- (q/height) (q/mouse-y)) 8)
        triangles (mapcat (fn [[p1 p2]]
                            (recursive-split p1 p2 mouse-len recursionCount))
                    edges)
        hue-step (count (partition 64 triangles))]
    (doseq [[index triangle] (map-indexed vector triangles)]
      (let [hue (/ (* index hue-step) 4)]
        (q/fill hue 100 100 50)
        (unpack q/triangle triangle)))))

(defn ^:export run-sketch
  []
  (declare triangle)
  (q/defsketch triangle
               :host "triangle"
               :size [1000 1000]
               :setup setup
               :draw draw-state
               :middleware [m/fun-mode]))
