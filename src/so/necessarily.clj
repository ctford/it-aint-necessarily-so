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
(def ti -1)

(defn choose-with [choice [[value probability] & weights]]
  (if (< choice probability)
    value
    (choose-with (- choice probability) weights)))

(defn select-from [weights]
  (let [total (->> weights vals (reduce +))]
    (choose-with (rand total) (seq weights))))

(defn arbitrary-pitch [history]
  (rand 7))

(defn arbitrary-duration [history]
  (max 1/16 (rand 1)))

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

(defn random-pitch [history]
  (rand-nth (keys freqs)))

(defn weighted-pitch [history]
  (select-from freqs))

(def tendencies ; major, p158
  {do {do 0.03416, re 0.02806, mi 0.01974, fa 0.00210, so 0.01321, la 0.00839, ti 0.02321}
   re {do 0.04190, re 0.02632, mi 0.03282, fa 0.00678, so 0.00825, la 0.00201, ti 0.00586}
   mi {do 0.01555, re 0.04865, mi 0.03142, fa 0.02644, so 0.02365, la 0.00281, ti 0.00029}
   fa {do 0.00054, re 0.01260, mi 0.04127, fa 0.01506, so 0.01712, la 0.00441, ti 0.00125}
   so {do 0.02557, re 0.00530, mi 0.02854, fa 0.03653, so 0.04835, la 0.02076, ti 0.00369}
   la {do 0.00238, re 0.00168, mi 0.00065, fa 0.00342, so 0.03642, la 0.01261, ti 0.00854}
   ti {do 0.02025, re 0.00510, mi 0.00035, fa 0.00029, so 0.00323, la 0.01327, ti 0.00448}})

(defn pitch-succession [[previous & history]]
  (select-from (tendencies previous)))

(defn constant-duration [history]
  1/2)

(def metric-affinity
  {1/8  {1/8 0.875,  1/4 0.125}
   1/4  {1/8  0.125, 1/4 0.75,  1/2 0.125}
   1/2  {1/4  0.125, 1/2 0.75,  1   0.125}
   1    {1/4  0.125, 1/2 0.125, 1   0.75 }})

(defn metric-inertia [[previous & history]]
  (select-from (metric-affinity previous)))

(def metric-tendencies ; p243
  {0/4  {1/4 2, 2/4 600, 3/4 144, 4/4 2680,       6/4 1219,         8/4 1491,                             12/4 125,                                16/4 36  }
   1/4  {       2/4 2,   3/4 5                                                                                                                              }
   2/4  {                         4/4 589,        6/4 6,                                                                                           16/4 2   }
   3/4  {                         4/4 147,        6/4 2                                                                                                     }
   4/4  {                                  5/4 4, 6/4 760, 7/4 117, 8/4 2379,         10/4 48,            12/4 77,                                 16/4 4   }
   5/4  {                                         6/4 4                                                                                                     }
   6/4  {                                                  7/4 18,  8/4 1969,         10/4 3                                                                }
   7/4  {                                                           8/4 135                                                                                 }
   8/4  {                                                                     9/4 10, 10/4 880, 11/4 166, 12/4 3720,          14/4 367,            16/4 457 }
   9/4  {                                                                             10/4 7              12/4 3                                            }
   10/4 {                                                                                       11/4 13,  12/4 917,           14/4 7                        }
   11/4 {                                                                                                 12/4 179                                          }
   12/4 {                                                                                                            13/4 26, 14/4 1804, 15/4 227, 16/4 2924}
   13/4 {                                                                                                                     14/4 23,             16/4 3   }
   14/4 {                                                                                                                                15/4 50,  16/4 2147}
   15/4 {                                                                                                                                          16/4 277 }})

(defn metric-context [[previous & history]]
  (let [time (reduce + previous history)
        position (mod time 16/4)]
    (- (select-from (metric-tendencies position)) position)))

(defn generate [generator history]
  (let [pitch (generator history)
        updated-history (cons pitch history) ]
    (cons pitch (lazy-seq (generate generator updated-history)))))

(defn melody-with [pitch-generator duration-generator]
  (->>
    (generate pitch-generator [(rand-nth (keys tendencies))])
    (phrase (generate duration-generator [1/4 15/4]))
    (take-while #(-> % :time (< 8)))
    (times 2)
    (then (phrase [1] [do]))
    (where :pitch (comp C major))
    (tempo (bpm 90))))

(comment
  (live/play (melody-with arbitrary-pitch  arbitrary-duration))
  (live/play (melody-with weighted-pitch   constant-duration))
  (live/play (melody-with pitch-succession metric-context)))

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
