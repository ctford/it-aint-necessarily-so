(ns so.necessarily
  (:require [overtone.live :refer :all :exclude [stop scale]]
            [leipzig.melody :refer :all]
            [leipzig.scale :refer [A minor scale high low from]]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]))

(def melody (phrase (repeat 1) (range 0 8) ))

(def first-order
  {0 0.15
   1 0.05
   2 0.25
   3 0.15
   4 0.25
   5 0.1
   6 0.0
   7 0.05})

(defn choose-with [choice [[value probability] & weights]]
  (if (< choice probability)
    value
    (choose-with (- choice probability) weights)))

(defn select-from [weights]
  (let [total (->> weights vals (reduce +))]
    (choose-with (rand total) (seq weights))))

(defn ngram [generate]
  (->>
    (repeatedly generate)
    (take 16)
    (phrase (repeat 1/4))
    (where :pitch (comp A minor))
    (times 2)))

(def unigram (ngram #(select-from first-order)))

(comment
  (live/play unigram)
  )

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
