(ns klangmeister.ui.about
  (:require [klangmeister.ui.editor :as editor]
            [klangmeister.actions :as action]))

(defn controls [k handle! state]
  (let [play [:button {:on-click #(handle! (action/->PlayOnce k))} "Play"]]
    [:div {:class "controls"} play]))

(defn render [handle! state]
  (let [code "(->> (phrase (repeat 1/2) [:do :re :mi :fa :so :la :ti+ :do+])
     (where :pitch (comp equal A major solfege)))"]
    [:div
     [:p "This is a live coding environment that compiles Clojurescript and uses your browser's web audio API."]
     [editor/render :about code handle! state]
     [controls :about handle! state] ]))
