(ns klangmeister.ui.synthesis-tutorial
  (:require [klangmeister.ui.editor :as editor]
            [klangmeister.actions :as action]))

(defn controls [k handle! state]
  (let [play [:button {:on-click #(handle! (action/->PlayOnce k))} "Play"]]
    [:div {:class "controls"} play]))

(def steps
  {:phrase
   ["When we take into account the last note, we can make melodies that sound much more realistic."
    "(defn contextual-pitch
  \"Choose a pitch based on the previous pitch.\"
  [[previous & history]]
  (select-from (pitch-tendencies previous)))

(defn contextual-duration
  \"Choose a duration based on the previous metric position.\"
  [[previous & history]]
  (let [time (reduce + previous history)
        position (mod time 16/4)]
    (- (select-from (metric-tendencies position)) position)))

(melody-with contextual-duration contextual-pitch)"]})

(defn render-one [k handle! state]
  (let [[text code] (steps k)]
    [:div
     [:p text]
     [editor/render k code handle! state]
     [controls k handle! state]]))

(defn render [handle! state]
  [:div
   [render-one :phrase handle! state]])
