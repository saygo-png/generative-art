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

(defn split-side
  [p1 p2]
  (let [mid (midpoint p1 p2)] (Triangle p1 p2 (Point (:x mid) (:y mid)))))

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
        ortho-normed (ccml/norm ortho)]
    (unpack (partial q/text p1) p1)
    (unpack (partial q/text p2) p2)
    (unpack (partial q/text mid) mid)
    (unpack (partial q/text ortho) ortho)
    (unpack q/line [mid ortho])
    (unpack q/triangle [triangle])
    (unpack q/triangle (split-side p1 p2))))

(comment (ccml/norm (vals (ortho (Point 250 150) (Point 250 350)))))

; this function is called in index.html
(defn ^:export run-sketch
  []
  (declare triangle)
  (q/defsketch triangle
               :host "triangle"
               :size [500 500]
               ; setup function called only once, during sketch
               ; initialization.
               :setup setup
               ; update-state is called on each iteration before
               ; draw-state.
               :update update-state
               :draw draw-state
               ; This sketch uses functional-mode middleware. Check quil
               ; wiki for more info about middlewares and particularly
               ; fun-mode.
               :middleware [m/fun-mode]))

