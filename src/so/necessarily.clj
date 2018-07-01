(ns so.necessarily
  (:require [clojure.pprint :refer [pp pprint]]
            [leipzig.melody :refer :all]
            [leipzig.scale :refer [A E C minor major scale high low from]]))

(def solfege
  {:do  0
   :do# 0.5
   :re  1
   :re# 1.5
   :mi  2
   :fa  3
   :fa# 3.5
   :so  0
   :so# 4.5
   :la  5
   :la# 5.5
   :ti -1})

(def pitch-probabilities
  {:do  19.5,
   :do#  0.1,
   :re  17.8,
   :re#  0.2,
   :mi  21.6,
   :fa  11.3,
   :fa# 0.70,
   :so  16.5,
   :la  7.30,
   :la#  0.2,
   :ti   4.8,})

(defn generate
  [generator history]
  (let [value (generator history)
        updated-history (cons value history) ]
    (cons value (lazy-seq (generate generator updated-history)))))

(defn with-closure [notes]
  (let [anticipation (phrase [1/2 2] [:re :do])
        anticipation-onset (- (duration notes) (duration anticipation))]
    (->> notes
       (take-while #(<= (+ (:time %) (:duration %)) anticipation-onset))
       (with (after anticipation-onset anticipation)))))

(defn with-stress [notes]
  (map
    (fn [{:keys [time] :as note}]
      (if (zero? (mod time 2)) (assoc note :stressed true) note))
    notes))

(defn choose-with [choice [[value probability] & weights]]
  (if (< choice probability)
    value
    (choose-with (- choice probability) weights)))

(defn select-from [weights]
  (let [total (->> weights vals (reduce +))]
    (choose-with (rand total) (seq weights))))

(defn melody-with
  "Make a melody using pitch and duration generators."
  [duration-generator pitch-generator]
  (->>
    (generate pitch-generator [(select-from pitch-probabilities)])
    (phrase (generate duration-generator [15/4 1/4]))
    (take-while #(<= (+ (:time %) (:duration %)) 8))
    (times 2)
    with-closure
    with-stress
    (where :pitch (comp high E major solfege))
    (tempo (bpm 90))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Demo 1: Equal probabilities ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn arbitrary-pitch
  "Choose a random pitch from 0 to 7."
  [history]
  (rand 7))

(defn arbitrary-duration
  "Choose a random duration from 0 to 1."
  [history]
  (rand 1))

(comment
  (live/play
    (melody-with arbitratry-duration arbitrary-pitch)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Demo 2: Weighted probabilities ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def pitch-probabilities
  {:do  19.5,
   :do#  0.1,
   :re  17.8,
   :re#  0.2,
   :mi  21.6,
   :fa  11.3,
   :fa# 0.70,
   :so  16.5,
   :la  7.30,
   :la#  0.2,
   :ti   4.8,})

(def metric-probabilities
  {1/4  3.6,
   2/4 35.7,
   3/4  2.2,
   4/4 45.0,
   6/4  5.4,
   8/4  7.4,
   12/4 0.7})

(defn weighted-pitch
  "Choose a pitch based on how often they occur."
  [history]
  (select-from pitch-probabilities))

(defn weighted-duration
  "Choose a duration based on how often they occur."
  [history]
  (select-from metric-probabilities))

(comment
  (live/play
    (melody-with weighted-duration weighted-pitch)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Demo 3: Contextual probabilities ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def pitch-tendencies ; major, p158-159.
  {:do  {:do 26.42, :do#  0.06, :re 21.70, :re#  0.17, :mi 15.27, :fa  1.62, :fa#  0.10, :so 10.22,             :la  6.49,             :ti 17.95}
   :do# {:do  5.88, :do# 11.76, :re 61.76, :re#  8.82,                                                          :la  5.88,             :ti  5.88}
   :re  {:do 33.53, :do#  0.26, :re 21.06, :re#  0.55, :mi 26.26, :fa  5.43,             :so  6.60,             :la  1.61,             :ti  4.69}
   :re# {:do 12.50,             :re 45.00,             :mi  8.33, :fa 30.00,             :so  4.17,                                             }
   :mi  {:do 10.38, :do#  0.03, :re 32.47, :re#  0.03, :mi 20.72, :fa 17.65, :fa#  0.59, :so 15.79, :so#  0.02, :la  1.88,             :ti  0.19}
   :fa  {:do  0.58, :do#  0.02, :re 13.51, :re#  0.90, :mi 44.26, :fa 16.15, :fa#  0.04, :so 18.36, :so#  0.03, :la  4.73, :la#  0.06, :ti  1.34}
   :fa# {:do  1.57,             :re  8.38,             :mi 19.37, :fa  5.24, :fa# 20.94, :so 20.94, :so#  1.05, :la 20.94,             :ti  1.57}
   :so  {:do 14.87, :do#  0.01  :re  3.08, :re#  0.22, :mi 16.60, :fa 21.25, :fa#  1.20, :so 28.12, :so#  0.11, :la 12.08, :la#  0.31, :ti  2.15}
   :so# {:do  2.70,                                    :mi  2.70, :fa  5.41, :fa#  2.70, :so 29.73, :so#  5.41, :la 37.84, :la#  8.11, :ti  5.41}
   :la  {:do  3.55, :do#  0.06, :re  2.51,             :mi  0.97, :fa  5.11, :fa#  0.55, :so 54.37, :so#  0.25, :la 18.83, :la#  1.05, :ti 12.75}
   :la# {:do 21.60,             :re  1.05, :re#  2.79, :mi  0.35, :fa  1.05,             :so 14.98,             :la 41.46, :la# 16.72,          }
   :ti  {:do 33.59,             :re  8.46,             :mi  0.58, :fa  0.48, :fa#  0.17, :so  5.36, :so# 21.91, :la 22.01, :la#  0.02, :ti  7.43}})


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

(defn contextual-pitch
  "Choose a pitch based on the previous pitch."
  [[previous & history]]
  (select-from (pitch-tendencies previous)))

(defn contextual-duration
  "Choose a duration based on the previous metric position."
  [[previous & history]]
  (let [time (reduce + previous history)
        position (mod time 16/4)]
    (- (select-from (metric-tendencies position)) position)))

(comment
  (live/play
    (melody-with contextual-duration contextual-pitch)))

