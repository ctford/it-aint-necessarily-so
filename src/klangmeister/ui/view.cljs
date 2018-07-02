(ns klangmeister.ui.view
  (:require
    [klangmeister.ui.arbitrary :as arbitrary]
    [klangmeister.ui.composition :as composition]
    [klangmeister.ui.about :as about]
    [klangmeister.ui.synthesis-tutorial :as synthesis]))

(defn link [current this href title]
  (if (= current this)
    title
    [:a {:href href} title]))

(defn tabs [current]
  [:div {:id "menu"}
   [:ul
    [:li (link current :arbitrary "/it-aint-necessarily-so/arbitrary" "Arbitrary")]
    [:li (link current :composition "/it-aint-necessarily-so/composition" "Weighted")]
    [:li (link current :synthesis "/it-aint-necessarily-so/synthesis" "Contextual")]
    [:li (link current :reference "/it-aint-necessarily-so/slides.html" "Slides")]
    [:li (link current :about "/it-aint-necessarily-so/about" "About")]]])

(defn frame [current content]
  [:div
   [:h1 [:a {:href "/it-aint-necessarily-so/"} "It ain't necessarily so"]]
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
