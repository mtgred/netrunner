(ns nr.gameboard.settings
  (:require [clojure.string :as string]
            [nr.appstate :refer [app-state]]
            [nr.gameboard.actions :refer [stack-cards]]
            [nr.translations :refer [tr]]
            [nr.ws :as ws]
            [reagent.core :as r]))

(defn settings-pane []
  (fn []
    (let [s app-state]
      [:div.settings
       [:section
        [:h3 (tr [:settings.sounds "Play area"])]
        [:div
         [:label [:input {:type "checkbox"
                          :value true
                          :checked (get-in @app-state [:options :stacked-cards])
                          :on-change #(swap! app-state assoc-in [:options :stacked-cards] (.. % -target -checked))}]
          (tr [:game.stack-cards "Stack cards"])]]
        ]
       ])))
