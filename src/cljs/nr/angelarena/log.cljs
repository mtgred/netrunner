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
  (r/with-let [inactivity-warning (r/cursor game-state [:angelarena-info :inactivity-warning])
               interval (r/atom nil)
               update-me (r/atom 0)] ; For some reason, r/force-update did not work, but this does...
    (r/create-class
      {:display-name "inactivity-pane"

       :component-will-unmount
       (fn []
         (when @interval
           (js/clearInterval @interval)
           (reset! interval nil)))

       :reagent-render
       (fn []
         (if-let [{:keys [stage inactive-side inactive-user warning-time period-to-react]}
                  @inactivity-warning]
           (let [inactive-side (keyword inactive-side)
                 warning-time (js/Date. warning-time)
                 more-time #(ws/ws-send! [:angelarena/more-time {:gameid (:gameid @game-state)}])
                 claim-victory #(ws/ws-send! [:angelarena/claim-victory {:gameid (:gameid @game-state)}])
                 cancel-match #(ws/ws-send! [:angelarena/cancel-match {:gameid (:gameid @game-state)}])
                 inactivities-remaining (get-in @game-state [:angelarena-info :inactivity-counter inactive-side] 1)]
             (when-not @interval
               (reset! interval (js/setInterval #(swap! update-me inc) 1000)))
             @update-me
             (case stage
               1 (let [time-inactive (/ (- (js/Date.now) warning-time) 1000)
                       time-remaining (- period-to-react time-inactive)]
                   (if (= (:username inactive-user)
                          (get-in @app-state [:user :username]))
                     [:div.angel-arena-time-warning
                      [:div.infobox
                       "You have been inactive for a while. "
                       (if (pos? inactivities-remaining)
                         (str "Please confirm, you are still there. "
                              "Otherwise, y")
                         "Y") "our opponent will be able to claim victory or cancel the match in "
                       (time-span-string (Math.floor time-remaining)) "."]
                      (when (pos? inactivities-remaining)
                        [:div.button-bar.centered
                         [:button {:on-click more-time} (tr [:angel-arena.still-here "Need more time"])]])]

                     (if (not-spectator?)
                       [:div.angel-arena-time-warning
                        [:div.infobox
                         "Your opponent has been inactive for a while. "
                         "You will be able to claim victory or cancel the match in "
                         (time-span-string (Math.floor time-remaining))
                         "."]]
                       [:div.angel-arena-time-warning
                        [:div.infobox
                         (:username inactive-user) " has been inactive for a while. "
                         "Their opponent will be able to claim victory or cancel the match in "
                         (time-span-string (Math.floor time-remaining))
                         "."]])))
               2 (let [time-inactive (/ (- (js/Date.now) warning-time) 1000)]
                   (if (= (:username inactive-user)
                          (get-in @app-state [:user :username]))
                     [:div.angel-arena-time-warning
                      [:div.infobox
                       "You have been inactive for "
                       (time-span-string (Math.floor time-inactive))
                       ". Your opponent can decide to either claim victory or cancel the match."]
                      (when (pos? inactivities-remaining)
                        [:div.button-bar.centered
                         [:button {:on-click more-time} (tr [:angel-arena.still-here "Need more time"])]])]

                     (if (not-spectator?)
                       [:div.angel-arena-time-warning
                        [:div.infobox
                         "Your opponent has been inactive for "
                         (time-span-string (Math.floor time-inactive))
                         ". You can decide to either claim victory or cancel the match."]
                        [:div.button-bar.centered
                         [:button {:on-click claim-victory} (tr [:angel-arena.claim-victory "Claim victory"])]
                         [:button {:on-click cancel-match} (tr [:angel-arena.cancel-match "Cancel match"])]]]
                       [:div.angel-arena-time-warning
                        [:div.infobox
                         (:username inactive-user) " has been inactive for "
                         (time-span-string (Math.floor time-inactive))
                         ". Their opponent can decide to either claim victory or cancel the match."]])))
               nil))
           (when @interval
             (js/clearInterval @interval)
             (reset! interval nil))))})))
