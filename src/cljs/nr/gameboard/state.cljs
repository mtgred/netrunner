(ns nr.gameboard.state
  (:require
   [nr.appstate :refer [app-state]]
   [nr.ws :as ws]
   [reagent.core :as r]))

(defonce game-state (r/atom {}))
(defonce last-state (atom {}))

(defmethod ws/event-msg-handler :game/typing [{typing :?data}]
  (swap! game-state assoc :typing typing))

(defonce replay-side (r/atom :spectator))

(defn parse-state [state]
  (js->clj (.parse js/JSON state) :keywordize-keys true))

(defn get-side [state]
  (let [user-id (get-in @app-state [:user :_id])]
    (cond
      (:replay state) @replay-side
      (= user-id (get-in state [:runner :user :_id])) :runner
      (= user-id (get-in state [:corp :user :_id])) :corp
      :else :spectator)))

(defn not-spectator? []
  (not= :spectator (get-side @game-state)))

(defn check-lock?
  "Check if we can clear client lock based on action-id"
  []
  (let [aid [(:side @game-state) :aid]]
    (when (not= (get-in @game-state aid)
                (get-in @last-state aid))
      (reset! ws/lock false))))
