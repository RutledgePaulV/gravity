(ns gravity.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.set :as sets])
  (:import (clojure.lang Atom)))

(def G 1)

(defn distance [m1 m2]
  (let [p1 @(:position m1)
        p2 @(:position m2)]
    (Math/sqrt
      (+ (Math/pow (- (:x p1) (:x p2)) 2)
         (Math/pow (- (:y p1) (:y p2)) 2)))))

(defn theta [m1 m2]
  (let [p1 @(:position m1)
        p2 @(:position m2)
        dy (- (:y p2) (:y p1))
        dx (- (:x p2) (:x p1))]
    (Math/atan2 dy dx)))

(defn calc-force [m1 m2]
  (let [r (distance m1 m2)]
    (/ (* G (:mass m1) (:mass m2)) (* r r))))

(defrecord Mass
  [position radius color mass velocity acceleration])

(defn rand-x [radius]
  (+ radius (rand-int (- (q/width) (* 2 radius)))))

(defn rand-y [radius]
  (+ radius (rand-int (- (q/height) (* 2 radius)))))

(defn rand-position [radius]
  (atom {:x (rand-x radius) :y (rand-y radius)}))

(defn rand-r []
  (+ 10 (rand-int 25)))

(defn rand-mass []
  (+ 25 (rand-int 25)))

(defn rand-large-mass []
  (+ 300 (rand-int 50)))

(defn black [] [0 0 0])

(defn rand-component []
  (+ 135 (rand-int 120)))

(defn rand-color []
  [(rand-component)
   (rand-component)
   (rand-component)])

(defn zero-vector []
  {:amplitude 0 :direction 0})

(defn init-blackhole []
  (let [radius 50]
    (->Mass
      (rand-position radius)
      radius
      (black)
      (rand-large-mass)
      (atom (zero-vector))
      (atom (zero-vector)))))

(defn init-asteroid []
  (let [radius (rand-r)]
    (->Mass
      (rand-position radius)
      radius
      (rand-color)
      (rand-mass)
      (atom (zero-vector))
      (atom (zero-vector)))))

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsb)
  {:blackholes (atom (set (repeatedly 5 init-blackhole)))
   :asteroids  (atom (set (repeatedly 500 init-asteroid)))})

(defn x-comp [v]
  (let [amp (:amplitude v) theta (:direction v)]
    (* amp (Math/cos theta))))

(defn y-comp [v]
  (let [amp (:amplitude v) theta (:direction v)]
    (* amp (Math/sin theta))))

(defn add-vectors [v1 v2]
  (let [dx (+ (x-comp v1) (x-comp v2))
        dy (+ (y-comp v1) (y-comp v2))
        amp (Math/sqrt (+ (* dx dx) (* dy dy)))
        direction (Math/atan2 dy dx)]
    {:amplitude amp :direction direction}))

(defn update-acceleration [blackhole asteroid]
  (let [force (calc-force blackhole asteroid)
        acceleration {:amplitude (/ force (:mass asteroid))
                      :direction (theta asteroid blackhole)}]
    (swap! (:acceleration asteroid)
           (partial add-vectors acceleration))))

(defn update-velocity [asteroid]
  (let [acceleration @(:acceleration asteroid)]
    (swap! (:velocity asteroid)
           (partial add-vectors acceleration))))

(defn update-position [asteroid]
  (let [velocity @(:velocity asteroid)
        +x (x-comp velocity)
        +y (y-comp velocity)]
    (swap! (:position asteroid)
           (fn [position]
             (-> position
                 (update :x (partial + +x))
                 (update :y (partial + +y)))))))

(defn within? [b a]
  (let [pb @(:position b)
        pa @(:position a)
        dx (- (:x pa) (:x pb))
        dy (- (:y pa) (:y pb))
        d (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (< d (:radius b))))

(defn update-state [state]
  (let [blackholes @(:blackholes state)
        asteroids @(:asteroids state)]
    (doseq [a asteroids]
      (reset! (get a :acceleration) (zero-vector))
      (doseq [b blackholes]
        (update-acceleration b a)
        (update-velocity a)
        (update-position a)
        (when (within? b a)
          (swap! (:asteroids state)
                 (fn [asteroids]
                   (sets/difference asteroids #{a}))))))
    state))

(defn draw-shape [thing]
  (let [{x :x y :y} @(:position thing)
        radius (get thing :radius)]
    (apply q/fill (:color thing))
    (q/ellipse x y radius radius)))

(defn draw-state [state]
  (q/background 240)
  (run! draw-shape @(:blackholes state))
  (run! draw-shape @(:asteroids state)))

(q/defsketch gravity
  :title "Asteroids in a blackhole field"
  :size [840 840]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
