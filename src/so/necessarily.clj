(ns so.necessarily
  (:require [overtone.live :refer :all :exclude [stop scale]]
            [leipzig.melody :refer :all]
            [leipzig.scale :refer [A C minor major scale high low from]]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]))

(defn choose-with [choice [[value probability] & weights]]
  (if (< choice probability)
    value
    (choose-with (- choice probability) weights)))

(defn select-from [weights]
  (let [total (->> weights vals (reduce +))]
    (choose-with (rand total) (seq weights))))

(def freqs ; major, p148
  {0   12000
   0.5 100
   1   10000
   1.5 100
   2   1400
   3   8000
   3.5 150
   4   15000
   4.5 100
   5   6000
   5.5 150
   6   4000})

(defn random [history]
  (rand-nth (keys freqs)))

(defn weighted [history]
  (select-from freqs))

(def tendencies ; major, p158
  {-1 {0 1.00000}
   0  {0 0.03416, 1 0.02806, 2 0.01974, 3 0.00210, 4 0.01321, 5 0.00839, 6 0.02321}
   1  {0 0.04190, 1 0.02632, 2 0.03282, 3 0.00678, 4 0.00825, 5 0.00201, 6 0.00586}
   2  {0 0.01555, 1 0.04865, 2 0.03142, 3 0.02644, 4 0.02365, 5 0.00281, 6 0.00029}
   3  {0 0.00054, 1 0.01260, 2 0.04127, 3 0.01506, 4 0.01712, 5 0.00441, 6 0.00125}
   4  {0 0.02557, 1 0.00530, 2 0.02854, 3 0.03653, 4 0.04835, 5 0.02076, 6 0.00369}
   5  {0 0.00238, 1 0.00168, 2 0.00065, 3 0.00342, 4 0.03642, 5 0.01261, 6 0.00854}
   6  {0 0.02025, 1 0.00510, 2 0.00035, 3 0.00029, 4 0.00323, 5 0.01327, 6 0.00448}})

(defn weighted-succession [[previous & history]]
  (select-from (tendencies previous)))

(defn generate [generator history]
  (let [pitch (generator history)
        updated-history (cons pitch history) ]
    (cons pitch (lazy-seq (generate generator updated-history)))))

(defn melody-with [generator]
  (->>
    (generate generator [-1])
    (take 16)
    (phrase (repeat 1/4))
    (where :pitch (comp A major))
    (times 2)))

(comment
  (live/play (melody-with random))
  (live/play (melody-with weighted))
  (live/play (melody-with weighted-succession)))

(definst overchauffeur [freq 110 dur 1.0 top 2500 vol 0.25]
  (-> (sin-osc freq)
      (+ (* 1/3 (sin-osc (* 2.01 freq))))
      (+ (* 1/2 (sin-osc (* 3.01 freq))))
      (+ (* 1/8 (sin-osc (* 5.01 freq))))
      (+ (* 2 (sin-osc (* 0.5 freq))))
      (clip2 0.7)
      (lpf top)
      (hpf 20)
      (* (env-gen (adsr 0.01 0.2 0.8 0.2) (line:kr 1 0 dur) :action FREE))
      (* vol)))

(defmethod live/play-note :default [{:keys [pitch duration]}]
  (overchauffeur :freq (midi->hz pitch) :dur duration))
