(ns nr.angelarena.log
  (:require [clojure.string :as string]
            [nr.appstate :refer [app-state]]
            [nr.gameboard.state :refer [game-state not-spectator?]]
            [nr.translations :refer [tr]]
            [nr.utils :refer [time-span-string]]
            [nr.ws :as ws]
            [reagent.core :as r]))

(defn inactivity-pane
  []
  (fn []
    (when-let [{:keys [stage inactive-side inactive-user warning-time period-to-react]}
               (get-in @game-state [:angelarena-info :inactivity-warning])]
      (let [warning-time (js/Date. warning-time)]
        (case stage
          1 (let [time-inactive (/ (- (js/Date.now) warning-time) 1000)
                  time-remaining (- period-to-react time-inactive)]
              (if (= (:username inactive-user)
                     (get-in @app-state [:user :username]))
                [:div.angel-arena-time-warning
                 [:div.infobox
                  "You have been inactive for a while. "
                  "Please confirm, you are still there. "
                  "Otherwise your opponent will be able to claim victory or cancel the match in "
                  (time-span-string (Math.floor time-remaining)) "."]
                 [:div.button-bar.centered
                  [:button {:on-click #(println "need more time")} (tr [:angel-arena.still-here "Need more time"])]]]

                (when (not-spectator?)
                  [:div.angel-arena-time-warning
                   [:div.infobox
                    "Your opponent has been inactive for a while. "
                    "You will be able to claim victory or cancel the match in "
                    (time-span-string (Math.floor time-remaining))
                    "."]])))
          2 (let [time-inactive (/ (- (js/Date.now) warning-time) 1000)]
              (if (= (:username inactive-user)
                     (get-in @app-state [:user :username]))
                [:div.angel-arena-time-warning
                 [:div.infobox
                  "You have been inactive for "
                  (time-span-string (Math.floor time-inactive))
                  ". Your opponent can decide to either claim victory or cancel the match. "]
                 [:div.button-bar.centered
                  [:button {:on-click #(println "need more time")} (tr [:angel-arena.still-here "Need more time"])]]]

                (when (not-spectator?)
                  [:div.angel-arena-time-warning
                   [:div.infobox
                    "Your opponent has been inactive for "
                    (time-span-string (Math.floor time-inactive))
                    ". You can decide to either claim victory or cancel the match."]
                   [:div.button-bar.centered
                    [:button {:on-click #(println "claim")} (tr [:angel-arena.claim-victory "Claim victory"])]
                    [:button {:on-click #(println "cancel")} (tr [:angel-arena.cancel-match "Cancel match"])]]])))
          nil)))))
