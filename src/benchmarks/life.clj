(ns benchmarks.life
  (:require [uncomplicate.fluokitten.core :refer [fmap! fmap foldmap fold]]
            [uncomplicate.neanderthal
             [core :refer :all :exclude [entry! entry]]
             [real :refer [entry! entry]]
             [native :refer :all]
             [math :refer :all]]
            [hiphip.double :as hiphip]
            [criterium.core :refer :all])
  )

(defn parallelMapCat[function input]
  (apply concat (map function input)))

(defn parallelMapCat[function input]
  (apply concat (pmap function input)))

(defn parallelMapCat[function input]
  (foldmap function input))

(apply concat (map reverse [[3 2 1 0] [6 5 4] [9 8 7]]))
(mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])
(parallelMapCat reverse [[3 2 1 0] [6 5 4] [9 8 7]])

(defn create-world
  "Creates rectangular world with the specified width and height.
  Optionally takes coordinates of living cells."
  [w h & living-cells]
  (vec (for [y (range w)]
         (vec (for [x (range w)]
                (if (contains? (first living-cells) [y x]) "X" " "))))))

(defn neighbours
  "Determines all the neighbours of a given coordinate"
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn stepper

  "Returns a step function for Life-like cell automata.
   neighbours takes a location and return a sequential collection
   of locations. survive? and birth? are predicates on the number
   of living neighbours."

  [neighbours birth? survive?]
  (fn [cells]
    (set (for [[loc n] (frequencies (parallelMapCat neighbours cells))
               :when (if (cells loc) (survive? n) (birth? n))]
           loc))))

; patterns
(def glider #{[2 0] [2 1] [2 2] [1 2] [0 1]})
(def light-spaceship #{[2 0] [4 0] [1 1] [1 2] [1 3] [4 3] [1 4] [2 4] [3 4]})

; steppers
(def conway-stepper (stepper neighbours #{3} #{2 3}))

(defn conway
  [[w h] pattern iterations]
  (->> (iterate conway-stepper pattern)
       (drop iterations)
       first
       (create-world w h)
       (map println)))

(defn -main []
  (println)
  (time (conway [100 100] glider 50))
)

(defn long-running-job [n]
    (Thread/sleep 3000) ; wait for 3 secondsb
    (+ n 10))

(time (doall (map long-running-job (range 4))))
(time (doall (pmap long-running-job (range 4))))
(time (doall (fmap long-running-job (range 4))))

(conway [10 10] glider 10)