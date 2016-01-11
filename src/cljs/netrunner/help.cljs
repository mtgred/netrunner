(ns netrunner.help
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]))

(def app-state (atom {}))

(defn help [cursor owner]
  (om/component
    (sab/html
      [:div.help.panel.blue-shade
       [:h3 "Help"]
       [:p "Help page test."]])))

(om/root help app-state {:target (. js/document (getElementById "help"))})
