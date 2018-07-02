(ns klangmeister.ui.arbitrary
  (:require [klangmeister.ui.editor :as editor]
            [klangmeister.actions :as action]))

(defn controls [k handle! state]
  (let [play [:button {:on-click #(handle! (action/->PlayOnce k))} "Play"]]
    [:div {:class "controls"} play]))

(def steps
  {:phrase
   ["A melody chosen randomly sounds a bit... weird."
    "(defn arbitrary-pitch
  \"Choose a random pitch from 0 to 2000hz.\"
  [history]
  (rand 2000))

(defn arbitrary-duration
  \"Choose a random duration from 0 to 1.\"
  [history]
  (rand 1))

(melody-with arbitrary-duration arbitrary-pitch)"]})

(defn render-one [k handle! state]
  (let [[text code] (steps k)]
    [:div
     [:p text]
     [editor/render k code handle! state]
     [controls k handle! state]]))

(defn render [handle! state]
  [:div
   [render-one :phrase handle! state]])
