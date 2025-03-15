(ns nr.player-view
  (:require [reagent.core :as r]
            [nr.avatar :refer [avatar]]
            [nr.translations :refer [tr tr-side]]
            [nr.utils :refer [faction-icon notnum->zero num->percent]]))

(defn user-status-span
  "Returns a [:span] showing players game completion rate"
  [player]
  (r/with-let [started (get-in player [:user :stats :games-started])
               completed (get-in player [:user :stats :games-completed])
               completion-rate (str (notnum->zero (num->percent completed started)) "%")
               completion-rate (if (< started 10) [tr [:lobby_too-little-data "Too little data"]] completion-rate)]
    [:span.user-status (get-in player [:user :username])
     [:div.status-tooltip.blue-shade
      [:div [tr [:lobby_completion-rate "Game Completion Rate"]] ": " completion-rate]]]))


(defn player-view
  ([player] (player-view player nil))
  ([player game]
   [:span.player
    [avatar (:user player) {:opts {:size 22}}]
    [user-status-span player]
    (when (not (:password game))
      (let [side (:side player)
            faction (:faction (:identity (:deck player)))
            identity (:title (:identity (:deck player)))
            specs (:allow-spectator game)]
        (cond
          (and (some? faction)
               (not= "Neutral" faction)
               specs)
          (faction-icon faction identity)
          side
          (str " (" (tr-side side) ")"))))]))
