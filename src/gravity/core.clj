(ns gravity.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:import (clojure.lang Atom)))

(def G 6.674E-11)

(defn distance [m1 m2]
  (let [p1 @(:position m1)
        p2 @(:position m2)]
    (Math/sqrt
      (+ (Math/pow (- (:x p1) (:x p2)) 2)
         (Math/pow (- (:y p1) (:y p2)) 2)))))

(defn calc-force [m1 m2]
  (let [r (distance m1 m2)]
    (/ (* G (:mass m1) (:mass m2)) (* r r))))

(defrecord Mass
  [position radius color mass velocity acceleration])

(defn rand-x []
  (rand-int (q/width)))

(defn rand-y []
  (rand-int (q/height)))

(defn rand-position []
  (atom {:x (rand-x) :y (rand-y)}))

(defn rand-r []
  (+ 25 (rand-int 25)))

(defn rand-mass []
  (+ 25 (rand-int 25)))

(defn black [] [0 0 0])

(defn rand-color []
  (->> (fn [] (rand-int 255))
       (repeatedly 3)
       (into [])))

(defn zero-vector []
  (atom {:amplitude 0 :direction 0}))

(defn init-blackhole []
  (->Mass
    (rand-position)
    (rand-r)
    (black)
    (rand-mass)
    (zero-vector)
    (zero-vector)))

(defn init-asteroid []
  (->Mass
    (rand-position)
    (rand-r)
    (rand-color)
    (rand-mass)
    (zero-vector)
    (zero-vector)))

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsb)
  {:blackholes (atom (set (repeatedly 1 init-blackhole)))
   :asteroids  (atom (set (repeatedly 1 init-asteroid)))})

(defn x-comp [v]
  10)

(defn y-comp [v]
  10)

(defn add-vectors [v1 v2]
  v1)

(defn update-acceleration [blackhole asteroid]
  (let [force (calc-force blackhole asteroid)
        acceleration (/ force (:mass asteroid))]
    (swap! (:acceleration asteroid) (partial add-vectors acceleration))))

(defn update-velocity [asteroid]
  (let [acceleration @(:acceleration asteroid)]
    (swap! (:velocity asteroid) (partial add-vectors acceleration))))

(defn update-position [asteroid]
  (let [velocity @(:velocity asteroid)
        +x (x-comp velocity)
        +y (y-comp velocity)]
    (swap! (:position asteroid)
           (fn [position]
             (-> position
                 (update :x (partial + +x))
                 (update :y (partial + +y)))))))

(defn update-state [state]
  (let [blackholes @(:blackholes state)
        asteroids @(:asteroids state)]
    (doseq [a asteroids]
      (reset! (get a :acceleration) (zero-vector))
      (doseq [b blackholes]
        (update-acceleration b a)
        (update-velocity a)
        (update-position a)))
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
