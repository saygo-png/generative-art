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
  (q/stroke 140)
  (q/stroke-weight 2)
  (q/rect-mode :center)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0, :angle 0})

(defn update-state
  [state]
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

(defn vec-vals [list] (vec (vals list)))

(defn normal
  [p1 p2]
  (let [ortho' (Point (- (:y p2) (:y p1)) (- (:x p1) (:x p2)))
        ortho-norm (ccml/norm (vec-vals ortho'))]
    (Point (/ (:x ortho') ortho-norm) (/ (:y ortho') ortho-norm))))

(defn perpendicular-point
  [p1 p2 length]
  (let [midpoint' (midpoint p1 p2)
        normal' (normal p1 p2)
        scaled-normal (Point (* (:x normal') length) (* (:y normal' length)))]
    (Point (+ (:x midpoint') (:x scaled-normal))
           (+ (:y midpoint') (:y scaled-normal)))))


(defn split-recursively
  [p1 p2 peak-length recursion-depth]
  (let [endpoint (perpendicular-point p1 p2 peak-length)
        basecase-triangle (Triangle p1 p2 endpoint)]
    (if (zero? recursion-depth)
      [basecase-triangle]
      (concat
        [basecase-triangle]
        (split-recursively p1 endpoint peak-length (dec recursion-depth))
        (split-recursively endpoint p2 peak-length (dec recursion-depth))))))


(defn draw-state [state] 
  (q/background 0 0 16)
  (q/fill (:color state) 255 255 100))
  

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
