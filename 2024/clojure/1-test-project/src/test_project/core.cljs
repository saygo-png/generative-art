(ns test-project.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 144)
  (q/color-mode :hsb)
  (q/random-seed 1)
  (q/background 255)
  (q/no-stroke)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.05)})

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 0 0 255 5)
  ; Set rect color.
  (q/fill (:color state) 255 255)

  ; Draw the rectangle
  (q/rect (/ (q/width) 2) (/ (q/height) 2) 50 50))

; this function is called in index.html
(defn ^:export run-sketch []
  (declare triangle-splitting)
  (q/defsketch triangle-splitting
    :host "test-project"
    :size [500 500]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))

; uncomment this line to reset the sketch:
(run-sketch)
