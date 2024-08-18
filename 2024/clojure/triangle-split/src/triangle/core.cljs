(ns triangle.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [clojure.math]
            [clojure.core.matrix :as ccm]
            [clojure.core.matrix.linear :as ccml]))

(defn setup
  []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 144)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  (q/random-seed 1)
  (q/background 255)
  (q/stroke 140)
  (q/stroke-weight 2)
  (q/rect-mode :center)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0, :angle 0})

(defn update-state
  [state]
  ; Update sketch state by changing circle color and position.
  {:color (mod (+ (:color state) 0.7) 255), :angle (+ (:angle state) 0.1)})


(defn extract-values
  [m]
  (cond (map? m) (mapcat extract-values (vals m))
        (vector? m) (mapcat extract-values m)
        :else [m]))

(defn unpack [fun elems] (apply fun (extract-values elems)))

(defn Point [x y] (array-map :x x :y y))

(defn Triangle [p1 p2 p3] (array-map :p1 p1 :p2 p2 :p3 p3))

(defn midpoint
  [p1 p2]
  (Point (/ (+ (:x p1) (:x p2)) 2) (/ (+ (:y p1) (:y p2)) 2)))

(defn ortho [p1 p2] (Point (- (:y p2) (:y p1)) (- (:x p1) (:x p2))))

(defn vec-vals [list] (vec (vals list)))

(defn split-at-vec [idx v]
    [(subvec v 0 idx) (subvec v idx)])

(defn normal
  [p1 p2]
  (let [ortho' (ortho p1 p2)
        ortho-norm (ccml/norm (vec-vals (ortho p1 p2)))]
    (Point (/ (:x ortho') ortho-norm) (/ (:y ortho') ortho-norm))))

(defn perpendicular-point
  [p1 p2 length]
  (let [midpoint' (midpoint p1 p2)
        normal' (normal p1 p2)
        scaled-normal (Point (* (:x normal') length) (* (:y normal' length)))]
    (Point (+ (:x midpoint') (:x scaled-normal))
           (+ (:y midpoint') (:y scaled-normal)))))

; Your goal is to recursively split a side into triangles with a specified
; depth
; a side is 2 :x :y points. There is a function split-side which takes
; p1, p2 and peak-length and outputs a vector of 2 new sides that need to be
; split

(defn split-side
  [x1 y1 x2 y2 length]
  (let [p1 (Point x1 y1)
        p2 (Point x2 y2)
        perp-point-scaled (perpendicular-point p1 p2 length)]
    (vec (extract-values [p1 perp-point-scaled p2 perp-point-scaled]))))

(defn split-sides
  [sides length]
  (mapcat (fn [[x1 y1 x2 y2]] (split-side x1 y1 x2 y2 length)) (partition 4 sides)))

(defn split-recursively
  [sides peak-length recursion-depth]
  (if (zero? recursion-depth)
    (split-sides sides peak-length)
    (split-recursively sides peak-length (dec recursion-depth))))

(defn draw-state
  [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 255)
  ; Set circle color.
  (q/fill (:color state) 255 255)
  ; Calculate x and y coordinates of the circle. Draw the edge.
  (let [p1 (Point 250 150)
        p2 (Point 250 350)
        mid (midpoint p1 p2)
        triangle (Triangle p1 p2 mid)
        ortho (ortho p1 p2)
        normal (normal p1 p2)
        perp-point (perpendicular-point p1 p2 200)]
    (unpack (partial q/text p1) p1)
    (unpack (partial q/text p2) p2)
    (unpack (partial q/text mid) mid)
    ; (unpack (partial q/text normal) normal)
    (unpack (partial q/text perp-point) perp-point)
    (unpack q/line [mid perp-point])
    ; (unpack q/line [mid ortho])
    ; (unpack q/line [mid normal])
    (unpack q/triangle [triangle])
    (mapcat (fn [[x1 y1 x2 y2]] (q/line x1 y1 x2 y2)) (partition 4 (split-side 250 150 250 350 100)))))

(comment
  (ccml/norm (vec-vals (ortho (Point 250 150) (Point 250 350))))
  (split-recursively [(Point 250 150) (Point 250 350)] 30 8)
  (mapcat (fn [[x1 y1 x2 y2]] (split-side x1 y1 x2 y2 30)) (partition 4 (split-side 250 150 250 350 30)))
  (split-sides (split-sides (split-side 250 150 250 350 30) 30)30)
  (map (fn [[p1 p2]] (ccml/norm (vec-vals p1))) (vec [(vec [(Point 250 150) (Point 250 350)]) (vec [(Point 250 150) (Point 250 350)])])))

; this function is called in index.html
(defn ^:export run-sketch
  []
  (declare triangle)
  (q/defsketch triangle
               :host "triangle"
               :size [500 500]
               :setup setup
               ; update-state is called on each iteration before
               ; draw-state.
               :update update-state
               :draw draw-state
               ; This sketch uses functional-mode middleware. Check quil
               ; wiki for more info about middlewares and particularly
               ; fun-mode.
               :middleware [m/fun-mode]))

