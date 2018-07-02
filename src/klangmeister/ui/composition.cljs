(ns klangmeister.ui.composition
  (:require [klangmeister.ui.editor :as editor]
            [klangmeister.actions :as action]))

(defn controls [k handle! state]
  (let [play [:button {:on-click #(handle! (action/->PlayOnce k))} "Play"]]
    [:div {:class "controls"} play]))

(def steps
  {:phrase
   ["A melody composed based on the frequencies that pitches and durations occur can sound nice sometimes, but usually lacks life."
    "(defn weighted-pitch
  \"Choose a pitch based on how often they occur.\"
  [history]
  (select-from pitch-probabilities))

(defn weighted-duration
  \"Choose a duration based on how often they occur.\"
  [history]
  (select-from metric-probabilities))

(->>
  (melody-with weighted-duration weighted-pitch)
  (where :pitch (comp equal C major solfege)))"]})

(defn render-one [k handle! state]
  (let [[text code] (steps k)]
    [:div
     [:p text]
     [editor/render k code handle! state]
     [controls k handle! state]]))

(defn render [handle! state]
  [:div
   [render-one :phrase handle! state]])
