(ns klangmeister.so.necessarily
  (:require [leipzig.melody :refer [tempo all where times duration with after then bpm]]
            [leipzig.scale :refer [A E C minor major scale high low from]]))

(defn rhythm
  "Translates a sequence of durations into a rhythm.
  e.g. (rhythm [1 1 2])"
  [durations]
  (let [times (reductions + 0 durations)]
    (map #(zipmap [:time :duration] [%1 %2]) times durations)))

(defn having
  "Zips an arbitrary quality onto a melody.
  e.g. (->> (rhythm [1 1/2]) (having :drum [:kick :snare]))"
  [k values notes]
  (map #(assoc %1 k %2) notes values))

(defn phrase
  "Translates a sequence of durations and pitches into a melody.
  nil pitches signify rests.
  e.g. (phrase [1/2 1/2 1/2 3/2 1/2 1/2 1/2] [0 1 2 nil 4 4/5 5])"
  [durations pitches]
  (->> (rhythm durations)
       (having :pitch pitches)
       (filter (comp not nil? :pitch))))

(defn solfege [k]
  (get {:ti -1
        :do  0
        :do# 0.5
        :re  1
        :re# 1.5
        :mi  2
        :fa  3
        :fa# 3.5
        :so  4
        :so# 4.5
        :la  5
        :la# 5.5
        :ti+ 6
        :do+ 7}
       k k))

(defn generate
  [generator history]
  (let [value (generator history)
        updated-history (cons value history) ]
    (cons value (lazy-seq (generate generator updated-history)))))

(defn with-closure [notes]
  (let [anticipation (->> (phrase [0.5 2] [:re :do])
                          (having :metric-entropy [1.5 1.5])
                          (having :pitch-entropy [1.5 1.5]))
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
  {0.25  3.6,
   0.50 35.7,
   0.75  2.2,
   1.00 45.0,
   1.50  5.4,
   2.00  7.4,
   3.00  0.6,
   4.00  0.1})

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
  {0.00 {0.25 0.03, 0.50  9.53, 0.75  2.29, 1.00 42.56,            1.50  19.36,            2.00  23.68,                                   3.00  1.99,                                     4.00  0.57}
   0.25 {           0.50 28.57, 0.75 71.43,                                                                                                                                                         }
   0.50 {                                   1.00 98.66,            1.50   1.01,                                                                                                           4.00  0.34}
   0.75 {                                   1.00 98.66,            1.50   1.34,                                                                                                                     }
   1.00 {                                               1.25 0.12, 1.50  22.43, 1.75 3.45, 2.00  70.20,            2.50  1.42,            3.00  2.27,                                     4.00  0.12}
   1.25 {                                                          1.50 100.00,                                                                                                                     }
   1.50 {                                                                       1.75 0.90, 2.00  98.94,            2.50  0.15,                                                                      }
   1.75 {                                                                                  2.00 100.00,                                                                                             }
   2.00 {                                                                                               2.25 0.18, 2.50 15.71, 2.75 2.96, 3.00  66.43,            3.50  6.55,             4.00  8.16}
   2.25 {                                                                                                          2.50 70.00,            3.00  30.00,                                              }
   2.50 {                                                                                                                      2.75 1.39, 3.00  97.87,            3.50  0.75,                       }
   2.75 {                                                                                                                                 3.00 100.00,                                              }
   3.00 {                                                                                                                                              3.25 0.52, 3.50 36.22, 3.75 4.56, 4.00  58.70}
   3.25 {                                                                                                                                                         3.50 88.46,            4.00  11.54}
   3.50 {                                                                                                                                                                     3.75 2.28, 4.00  97.72}
   3.75 {                                                                                                                                                                                4.00 100.00}})


(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn percentage->bits [x]
  (-> x (/ 100) log2 -))

(defn with-pitch-entropy [[a b & notes]]
  (let [a-entropy (or (:pitch-entropy a) (percentage->bits (pitch-probabilities (:pitch a))))
        a (assoc a :pitch-entropy a-entropy)]
    (if b
      (let [b-entropy (-> pitch-tendencies
                          (get (:pitch a))
                          (get (:pitch b))
                          percentage->bits)]
        (cons a (with-pitch-entropy (cons (assoc b :pitch-entropy b-entropy) notes))))
      [a])))

(defn position [x]
  (let [raw (mod x 4)]
    (if (zero? raw)
      4.0
      raw)))

(defn with-metric-entropy [[a b & notes]]
  (let [a-entropy (or (:metric-entropy a) (percentage->bits (metric-probabilities (:duration a))))
        a (assoc a :metric-entropy a-entropy)]
    (if b
      (let [b-entropy (-> metric-tendencies
                          (get (mod (:time a) 4))
                          (get (position (:time b)))
                          percentage->bits)]
        (cons a (with-metric-entropy (cons (assoc b :metric-entropy b-entropy) notes))))
      [a])))

(defn with-entropy [notes]
  (->> notes
       with-pitch-entropy
       with-metric-entropy))

(defn melody-with
  "Make a melody using pitch and duration generators."
  [duration-generator pitch-generator]
  (->>
    (generate pitch-generator [(select-from pitch-probabilities)])
    (phrase (generate duration-generator [3.75 0.25]))
    (take-while #(<= (+ (:time %) (:duration %)) 8))
    (times 2)
    with-entropy
    with-closure
    with-stress
    #_(tempo (bpm 90))))

(defn nil+ [x y]
  (when (and x y) (+ x y)))

(defn entropy [notes]
  {:pitch-entropy (->> notes
                       (map :pitch-entropy)
                       (reduce nil+))
   :metric-entropy (->> notes
                        (map :metric-entropy)
                        (reduce nil+))})
