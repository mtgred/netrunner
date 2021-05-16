(ns nr.angelarena.log
  (:require [clojure.string :as string]
            [nr.gameboard.state :refer [game-state]]
            [nr.translations :refer [tr]]
            [nr.ws :as ws]
            [reagent.core :as r]))

(defn inactivity-pane
  []
  (fn []
    (println (keys @game-state))
    (when (get-in @game-state [:angelarena-info :inactivity-warning])
      [:div "hello"])))

