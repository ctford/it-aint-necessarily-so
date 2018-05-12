(ns so.necessarily
  (:require [overtone.live :refer :all :exclude [stop scale]]
            [leipzig.melody :refer :all]
            [leipzig.scale :refer [A minor scale high low from]]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]))

(def melody (phrase (repeat 1) (range 0 8) ))

(def zeroth-order
  {0 1
   1 1
   2 1
   3 1
   4 1
   5 1
   6 1
   7 1})

(def first-order
  {0 0.15
   1 0.05
   2 0.25
   3 0.15
   4 0.25
   5 0.1
   6 0.0
   7 0.05})

(def second-order
  {-1 {0  1.0}
   0  {-1 0.5, 1 0.5}
   1  {0  0.5, 2 0.5}
   2  {1  0.5, 3 0.5}
   3  {2  0.5, 4 0.5}
   4  {3  1.0}})

(defn choose-with [choice [[value probability] & weights]]
  (if (< choice probability)
    value
    (choose-with (- choice probability) weights)))

(defn select-from [weights]
  (let [total (->> weights vals (reduce +))]
    (choose-with (rand total) (seq weights))))

(defn situate [pitches]
  (->>
    pitches
    (take 16)
    (phrase (repeat 1/4))
    (where :pitch (comp A minor))
    (times 2)))

(defn ngram [generate]
  (->>
    (repeatedly generate)
    situate))

(def unigram (ngram #(select-from first-order)))

(def nullogram (ngram #(select-from zeroth-order)))

(comment
  (live/play nullogram)
  (live/play unigram)
  (live/play bigram)
  )

(defn bigram* [prev]
  (let [next (select-from (second-order prev))]
    (->>
      (bigram* next)
      lazy-seq
      (cons next))))

(def bigram
  (situate (bigram* 0)))

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
