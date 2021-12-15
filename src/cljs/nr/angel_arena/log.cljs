(ns nr.angel-arena.log
  (:require
   [nr.appstate :refer [app-state current-gameid]]
   [nr.gameboard.state :refer [game-state not-spectator?]]
   [nr.translations :refer [tr]]
   [nr.utils :refer [time-span-string]]
   [nr.ws :as ws]
   [reagent.core :as r]))

(defn more-time []
  (ws/ws-send! [:angel-arena/more-time {:gameid (current-gameid app-state)}]))

(defn claim-victory []
  (ws/ws-send! [:angel-arena/claim-victory {:gameid (current-gameid app-state)}]))

(defn cancel-match []
  (ws/ws-send! [:angel-arena/cancel-match {:gameid (current-gameid app-state)}]))

(defn inactivity-pane []
  (r/with-let [username (r/cursor app-state [:user :username])
               inactivity-warning (r/cursor game-state [:angel-arena-info :inactivity-warning])]
    (let [{:keys [stage inactive-side inactive-user warning-time period-to-react]} @inactivity-warning
          inactive-side (keyword inactive-side)
          inactivities-remaining (get-in @game-state [:angel-arena-info :inactivity-counter inactive-side] 1)]

      (case stage
        "inactive-left"
        [:div.angel-arena-time-warning
         (when (not-spectator?)
           [:div.infobox
            "Your opponent has left the game. You can wait for them to return, you may claim this game as a victory, or cancel the match."
            [:div.button-bar.centered
             [:button {:on-click claim-victory} (tr [:angel-arena.claim-victory "Claim victory"])]
             [:button {:on-click cancel-match} (tr [:angel-arena.cancel-match "Cancel match"])]]])]

        "inactive-pre-start"
        [:div.angel-arena-time-warning
         (when (not-spectator?)
           [:div.infobox
            "There was no activity in this game yet. You may cancel the match, if your opponent does not respond."
            [:div.button-bar.centered
             [:button {:on-click cancel-match} (tr [:angel-arena.cancel-match "Cancel match"])]]])]

        "inactive-warning"
        (let [time-inactive (/ (- (js/Date.now) (js/Date. warning-time)) 1000)
              time-remaining (- period-to-react time-inactive)]
          (if (= (:username inactive-user)
                 (get-in @app-state [:user :username]))
            [:div.angel-arena-time-warning
             [:div.infobox
              "You have been inactive for a while. "
              (if (pos? inactivities-remaining)
                "Please confirm, you are still there. Otherwise, y"
                "Y") "our opponent will be able to claim victory or cancel the match in "
              (time-span-string (js/Math.floor time-remaining)) "."]
             (when (pos? inactivities-remaining)
               [:div.button-bar.centered
                [:button {:on-click more-time} (tr [:angel-arena.still-here "Need more time"])]])]
            (if (not-spectator?)
              [:div.angel-arena-time-warning
               [:div.infobox
                "Your opponent has been inactive for a while. "
                "You will be able to claim victory or cancel the match in "
                (time-span-string (js/Math.floor time-remaining))
                "."]]
              [:div.angel-arena-time-warning
               [:div.infobox
                (:username inactive-user) " has been inactive for a while. "
                "Their opponent will be able to claim victory or cancel the match in "
                (time-span-string (js/Math.floor time-remaining))
                "."]])))

        "inactive-countdown"
        (let [time-inactive (/ (- (js/Date.now) (js/Date. warning-time)) 1000)]
          (if (= @username (:username inactive-user))
            [:div.angel-arena-time-warning
             [:div.infobox
              (str "You have been inactive for "
                   (time-span-string (js/Math.floor time-inactive))
                   ". Your opponent can decide to either claim victory or cancel the match.")]
             (when (pos? inactivities-remaining)
               [:div.button-bar.centered
                [:button {:on-click more-time} (tr [:angel-arena.still-here "Need more time"])]])]
            (if (not-spectator?)
              [:div.angel-arena-time-warning
               [:div.infobox
                (str "Your opponent has been inactive for "
                     (time-span-string (js/Math.floor time-inactive))
                     ". You can decide to either claim victory or cancel the match.")]
               [:div.button-bar.centered
                [:button {:on-click claim-victory} (tr [:angel-arena.claim-victory "Claim victory"])]
                [:button {:on-click cancel-match} (tr [:angel-arena.cancel-match "Cancel match"])]]]
              [:div.angel-arena-time-warning
               [:div.infobox
                (str (:username inactive-user) " has been inactive for "
                     (time-span-string (js/Math.floor time-inactive))
                     ". Their opponent can decide to either claim victory or cancel the match.")]])))

        ; else
        nil))))
