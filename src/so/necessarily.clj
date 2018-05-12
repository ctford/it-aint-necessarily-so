(ns so.necessarily
  (:require [overtone.live :refer :all :exclude [stop scale]]
            [leipzig.melody :refer :all]
            [leipzig.scale :refer [A C minor major scale high low from]]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]))

(def do 0)
  (def di 0.5)
(def re 1)
  (def ri 1.5)
(def mi 2)
(def fa 3)
  (def fi 3.5)
(def so 4)
  (def si 4.5)
(def la 5)
  (def li 5.5)
(def te 6)

(defn choose-with [choice [[value probability] & weights]]
  (if (< choice probability)
    value
    (choose-with (- choice probability) weights)))

(defn select-from [weights]
  (let [total (->> weights vals (reduce +))]
    (choose-with (rand total) (seq weights))))

(defn continuous [history]
  (rand 7))

(def freqs ; major, p148
  {do 12000
   di 100
   re 10000
   ri 100
   mi 1400
   fa 8000
   fi 150
   so 15000
   si 100
   la 6000
   li 150
   ti 4000})

(defn random [history]
  (rand-nth (keys freqs)))

(defn weighted [history]
  (select-from freqs))

(def tendencies ; major, p158
  {-1 {0 1.00000}
   do {do 0.03416, re 0.02806, mi 0.01974, fa 0.00210, so 0.01321, la 0.00839, ti 0.02321}
   re {do 0.04190, re 0.02632, mi 0.03282, fa 0.00678, so 0.00825, la 0.00201, ti 0.00586}
   mi {do 0.01555, re 0.04865, mi 0.03142, fa 0.02644, so 0.02365, la 0.00281, ti 0.00029}
   fa {do 0.00054, re 0.01260, mi 0.04127, fa 0.01506, so 0.01712, la 0.00441, ti 0.00125}
   so {do 0.02557, re 0.00530, mi 0.02854, fa 0.03653, so 0.04835, la 0.02076, ti 0.00369}
   la {do 0.00238, re 0.00168, mi 0.00065, fa 0.00342, so 0.03642, la 0.01261, ti 0.00854}
   ti {do 0.02025, re 0.00510, mi 0.00035, fa 0.00029, so 0.00323, la 0.01327, ti 0.00448}})

(defn weighted-succession [[previous & history]]
  (select-from (tendencies previous)))

(def steps
  {-1 {0  1.00000}
   do {            re 1.00000}
   re {do 0.50000, mi 0.50000}
   mi {re 0.50000, fa 0.50000}
   fa {mi 0.50000, so 0.50000}
   so {fa 0.50000, la 0.50000}
   la {so 0.50000, ti 0.50000}
   ti {la 1.00000}})

(defn stepwise [[previous & history]]
  (select-from (steps previous)))

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
  (live/play (melody-with continuous))
  (live/play (melody-with random))
  (live/play (melody-with weighted))
  (live/play (melody-with weighted-succession))
  (live/play (melody-with stepwise)))

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
