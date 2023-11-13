(ns nr.status-bar
  (:require
   [nr.appstate :refer [app-state]]
   [nr.gameboard.actions :refer [concede mute-spectators]]
   [nr.gameboard.replay :refer [set-replay-side]]
   [nr.lobby :refer [filter-games leave-game]]
   [nr.player-view :refer [player-view]]
   [nr.translations :refer [tr]]
   [nr.ws :as ws]
   [reagent.core :as r]))

(defn current-game-count [user games connected?]
  (r/with-let [c (r/track (fn [] (count (filter-games @user @games (:visible-formats @app-state)))))]
    [:div.float-right
     (tr [:nav/game-count] @c)
     (when (not @connected?)
       [:a.reconnect-button {:on-click #(ws/chsk-reconnect!)} "Attempt reconnect"])]))

(defn in-game-buttons [user current-game gameid]
  (when (and (:started @current-game)
             (not= "local-replay" @gameid))
    (let [user-id (-> @user :_id)
          is-player (some #(= user-id (-> % :user :_id)) (:players @current-game))]
      [:div.float-right
       (when is-player
         [:a.concede-button {:on-click #(concede)}
          (tr [:game.concede "Concede"])])
       [:a.leave-button {:on-click #(leave-game)}
        (if (:replay @current-game)
          (tr [:game.leave-replay "Leave replay"])
          (tr [:game.leave "Leave game"]))]
       (when is-player
         [:a.mute-button {:on-click #(mute-spectators)}
          (if (:mute-spectators @current-game)
            (tr [:game.unmute "Unmute spectators"])
            (tr [:game.mute "Mute spectators"]))])])))

(defn replay-and-spectator-buttons [gameid]
  (when (not (nil? @gameid))
    [:div.float-right
     [:a {:on-click #(leave-game)}
      (if (= "local-replay" @gameid)
        (tr [:game.leave-replay "Leave replay"])
        (tr [:game.leave "Leave game"]))]
     (when (= "local-replay" @gameid)
       [:a.replay-button {:on-click #(set-replay-side :corp)}
        (tr [:game.corp-view "Corp View"])])
     (when (= "local-replay" @gameid)
       [:a.replay-button {:on-click #(set-replay-side :runner)}
        (tr [:game.runner-view "Runner View"])])
     (when (= "local-replay" @gameid)
       [:a.replay-button {:on-click #(set-replay-side :spectator)}
        (tr [:game.spec-view "Spectator View"])])]))

(defn spectator-list [current-game]
  (when-let [game @current-game]
    (when (:started game)
      (let [c (count (:spectators game))]
        (when (pos? c)
          [:div.spectators-count.float-right (str c " Spectator" (when (< 1 c) "s"))
           [:div.blue-shade.spectators
            (for [p (:spectators game)]
              ^{:key (get-in p [:user :_id])}
              [player-view p])]])))))

(defn status []
  (r/with-let [user (r/cursor app-state [:user])
               games (r/cursor app-state [:games])
               gameid (r/cursor app-state [:gameid])
               current-game (r/cursor app-state [:current-game])
               connected? (r/cursor app-state [:connected])]
    [:div
     [current-game-count user games connected?]
     [in-game-buttons user current-game gameid]
     [replay-and-spectator-buttons gameid]
     [spectator-list current-game]]))
