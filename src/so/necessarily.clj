(ns so.necessarily
  (:require [overtone.live :refer :all :exclude [stop scale]]
            [leipzig.melody :refer :all]
            [leipzig.scale :refer [A E C minor major scale high low from]]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]))

(def do 0)
  (def do# 0.5)
(def re 1)
  (def re# 1.5)
(def mi 2)
(def fa 3)
  (def fa# 3.5)
(def so 4)
  (def so# 4.5)
(def la 5)
  (def la# 5.5)
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
  {do  17.2
   do#  0.1
   re  14.4
   re#  0.1
   mi  20.1
   fa  11.5
   fa#  0.2
   so  21.6
   so#  0.1
   la   8.6
   la#  0.2
   ti   5.7})

(defn weighted-pitch [history]
  (select-from freqs))

(def pitch-tendencies ; major, p158-159.
  {do  {do 0.03416, do# 0.00008, re 0.02806, re# 0.00022, mi 0.01974, fa 0.00210, fa# 0.00013  so 0.01321,              la 0.00839,              ti 0.02321}
   do# {do 0.00002, do# 0.00004, re 0.00021,              mi 0.00003,                                                   la 0.00002,              ti 0.00002}
   re  {do 0.04190, do# 0.00033  re 0.02632, re# 0.00069, mi 0.03282, fa 0.00678,              so 0.00825,              la 0.00201,              ti 0.00586}
   re# {do 0.00015,              re 0.00054,              mi 0.00010, fa 0.00036,              so 0.00005                                                  }
   mi  {do 0.01555, do# 0.00004  re 0.04865, re# 0.00005  mi 0.03142, fa 0.02644, fa# 0.00088  so 0.02365, so# 0.00003, la 0.00281,              ti 0.00029}
   fa  {do 0.00054, do# 0.00002  re 0.01260, re# 0.00084  mi 0.04127, fa 0.01506, fa# 0.00004  so 0.01712, so# 0.00003  la 0.00441, la# 0.00006  ti 0.00125}
   fa# {do 0.00003               re 0.00016               mi 0.00037, fa 0.00010, fa# 0.00040, so 0.00257, so# 0.00002, la 0.00040,              ti 0.00003}
   so  {do 0.02557, do# 0.00001  re 0.00530, re# 0.00037  mi 0.02854, fa 0.03653, fa# 0.00207, so 0.04835, so# 0.00019, la 0.02076, la# 0.00054, ti 0.00369}
   so# {do 0.00001,                                       mi 0.00001  fa 0.00002  fa# 0.00001, so 0.00011, so# 0.00002, la 0.00014, la# 0.00003, ti 0.00002}
   la  {do 0.00238, do# 0.00004, re 0.00168,              mi 0.00065, fa 0.00342, fa# 0.00037, so 0.03642, so# 0.00017, la 0.01261, la# 0.00070, ti 0.00854}
   la# {do 0.00062,              re 0.00003, re# 0.00008, mi 0.00001, fa 0.00003,              so 0.00043,              la 0.00119, la# 0.00048,           }
   ti  {do 0.02025,              re 0.00510,              mi 0.00035, fa 0.00029, fa# 0.00010, so 0.00323, so# 0.00006, la 0.01327, la# 0.00001, ti 0.00448}})

(defn pitch-succession [[previous & history]]
  (select-from (pitch-tendencies previous)))

(defn constant-duration [history]
  1)

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

(defn stress [notes]
  (map
    (fn [{:keys [time] :as note}]
      (if (zero? (mod time 2)) (assoc note :stressed true) note))
    notes))

(defn melody-with [pitch-generator duration-generator]
  (->>
    (generate pitch-generator [(select-from freqs)])
    (phrase (generate duration-generator [1/4 15/4]))
    (take-while #(<= (+ (:time %) (:duration %)) 8))
    (times 2)
    (then (phrase [2] [do]))
    (where :pitch (comp high E major))
    stress
    (tempo (bpm 90))))

(comment
  (live/play (melody-with arbitrary-pitch  arbitrary-duration))
  (live/play (melody-with weighted-pitch   constant-duration))
  (live/play (melody-with pitch-succession metric-context)))

(definst bell [freq 440 dur 4 vol 0.25]
  (let [harmonic-series [1.0 2.0 3.0 4.2 5.3]
        proportions     [1.0 0.6 0.4 0.2 0.1]
        component
         (fn [harmonic proportion]
           (* 1/2 vol proportion
              (env-gen (perc 0.001 (* proportion dur)))
              (sin-osc (* harmonic freq))))
        whole (mix (map component harmonic-series proportions))]
    (detect-silence whole :action FREE)
    whole))

(defmethod live/play-note :default [{:keys [pitch duration stressed]}]
  (bell :freq (midi->hz pitch) :vol (if stressed 0.45 0.25)))
