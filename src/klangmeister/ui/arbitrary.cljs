(ns klangmeister.ui.arbitrary
  (:require [klangmeister.ui.editor :as editor]
            [klangmeister.actions :as action]))

(defn controls [k handle! state]
  (let [play [:button {:on-click #(handle! (action/->PlayOnce k))} "Play"]]
    [:div {:class "controls"} play]))

(def steps
  {:phrase
   ["We can compose a melody from pitches and durations chosen randomly."
    "(melody-with arbitrary-duration arbitrary-pitch)"]})

(defn render-one [k handle! state]
  (let [[text code] (steps k)]
    [:div
     [:p text]
     [editor/render k code handle! state]
     [controls k handle! state]]))

(defn render [handle! state]
  [:div
   [render-one :phrase handle! state]
   [:div
    [:p "Now that you know how to design synthesisers and compose melodies, try " [:a {:href "/klangmeister/performance"} "putting the two together"] "."]]])
