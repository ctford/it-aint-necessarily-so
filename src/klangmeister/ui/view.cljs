(ns klangmeister.ui.view
  (:require
    [klangmeister.ui.arbitrary :as arbitrary]
    [klangmeister.ui.composition :as composition]
    [klangmeister.ui.reference :as reference]
    [klangmeister.ui.about :as about]
    [klangmeister.ui.synthesis-tutorial :as synthesis]))

(defn link [current this href title]
  (if (= current this)
    title
    [:a {:href href} title]))

(defn tabs [current]
  [:div {:id "menu"}
   [:ul
    [:li (link current :arbitrary "/it-aint-necessarily-so/arbitrary" "Arbitrary composition")]
    [:li (link current :composition "/it-aint-necessarily-so/composition" "Weighted composition")]
    [:li (link current :synthesis "/it-aint-necessarily-so/synthesis" "Contextual composition")]
    [:li (link current :reference "/it-aint-necessarily-so/reference" "Reference")]
    [:li (link current :about "/it-aint-necessarily-so/about" "About")]]])

(defn frame [current content]
  [:div
   [:h1 [:a {:href "/it-aint-necessarilly-so/"} "It ain't necessarily so"]]
   [tabs current]
   content])

(defn synthesis [handle! state-atom]
  (frame :synthesis [synthesis/render handle! @state-atom]))

(defn about [handle! state-atom]
  (frame :about [about/render handle! @state-atom]))

(defn arbitrary [handle! state-atom]
  (frame :arbitrary [arbitrary/render handle! @state-atom]))

(defn composition [handle! state-atom]
  (frame :composition [composition/render handle! @state-atom]))

(defn reference [handle! state-atom]
  (frame :reference [reference/render handle! @state-atom]))
