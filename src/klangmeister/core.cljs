(ns klangmeister.core
  (:require
    [klangmeister.processing] ; Import action defs.
    [klangmeister.actions :as action]
    [klangmeister.ui.view :as view]
    [klangmeister.framework :as framework]
    [reagent.core :as reagent]
    [reagent.session :as session]
    [cljs-bach.synthesis :as synthesis]
    [accountant.core :as accountant]
    [secretary.core :as secretary :include-macros true]))

(defonce state-atom (reagent/atom {:audiocontext (synthesis/audio-context)}))

(secretary/defroute "/it-aint-necessarily-so/"            [query-params] (session/put! :gist (:gist query-params))
                                                               (session/put! :uri (:uri query-params))
                                                               (session/put! :current-page view/about))

(secretary/defroute "/it-aint-necessarily-so/index.html"  [query-params] (session/put! :gist (:gist query-params))
                                                               (session/put! :uri (:uri query-params))
                                                               (session/put! :current-page view/about))

(secretary/defroute "/it-aint-necessarily-so/arbitrary"    [] (session/put! :current-page view/arbitrary))
(secretary/defroute "/it-aint-necessarily-so/synthesis"    [] (session/put! :current-page view/synthesis))
(secretary/defroute "/it-aint-necessarily-so/composition" [] (session/put! :current-page view/composition))
(secretary/defroute "/it-aint-necessarily-so/reference"    [] (session/put! :current-page view/reference))
(secretary/defroute "/it-aint-necessarily-so/about"        [] (session/put! :current-page view/about))

(def handle!
  "An handler that components can use to raise events."
  (framework/handler-for state-atom))

(defn current-page
  "Extract the current page from the session and use it to build the page."
  []
  [(session/get :current-page) handle! state-atom])

(defn mount-root []
  (accountant/configure-navigation!)
  (accountant/dispatch-current!)
  (reagent/render [current-page] js/document.body))

(mount-root)
