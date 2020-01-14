(ns nr.main
  (:require [goog.events :as events]
            [goog.history.EventType :as EventType]
            [jinteki.nav :as nav]
            [nr.appstate :refer [app-state]]
            [nr.about :refer [about]]
            [nr.auth :refer [auth-forms auth-menu]]
            [nr.account :refer [account]]
            [nr.cardbrowser :refer [card-browser]]
            [nr.chat :refer [chat]]
            [nr.deckbuilder :refer [deck-builder]]
            [nr.gameboard :refer [concede gameboard game-state mute-spectators stack-servers flip-runner-board]]
            [nr.gamelobby :refer [filter-blocked-games game-lobby leave-game player-view]]
            [nr.help :refer [help]]
            [nr.news :refer [news news-state]]
            [nr.stats :refer [stats]]
            [reagent.core :as r])
  (:import goog.history.Html5History))

(def tokens #js ["/" "/cards" "/deckbuilder" "/play" "/help" "/account" "/stats" "/about"])

(def history (Html5History.))

(defn navigate [token]
  (let [page-number (.indexOf tokens token)]
    (.carousel (js/$ ".carousel") page-number))
  (try (js/ga "send" "pageview" token) (catch js/Error e))
  (.setToken history token)
  (swap! app-state assoc :active-page [token]))

(events/listen history EventType/NAVIGATE #(navigate (.-token %)))
(.setUseFragment history false)
(.setPathPrefix history "")
(.setEnabled history true)

(defn navbar []
  (r/with-let [active (r/cursor app-state [:active-page])]
    [:ul.carousel-indicator {}
     (doall (for [[name route ndx show-fn?] nav/navbar-links]
              (when (or (not show-fn?) (show-fn? @app-state))
                [:li {:class (if (= (first @active) route) "active" "")
                      :key name
                      :on-click #(.setToken history route)
                      :data-target "#main" :data-slide-to ndx}
                 [:a {:href route} name]])))]))

(defn status []
  (r/with-let [user (r/cursor app-state [:user])
               games (r/cursor app-state [:games])
               gameid (r/cursor app-state [:gameid])]
    [:div
     [:div.float-right
      (let [c (count (filter-blocked-games @user @games))]
        (str c " Game" (when (not= c 1) "s")))]
     (if-let [game (some #(when (= @gameid (:gameid %)) %) @games)]
       (let [user-id (-> @user :_id)
             is-player (some #(= user-id (-> % :user :_id)) (:players game))]
         (when (:started game)
           [:div.float-right
            (when is-player
              [:a.concede-button {:on-click #(concede)} "Concede"])
            [:a.leave-button {:on-click #(leave-game)} "Leave game"]
            (when is-player
              [:a.mute-button {:on-click #(mute-spectators (not (:mute-spectators game)))}
               (if (:mute-spectators game) "Unmute spectators" "Mute spectators")])
            [:a.stack-servers-button {:on-click #(stack-servers)}
             (if (get-in @app-state [:options :stacked-servers]) "Unstack servers" "Stack servers")]
            (when (not= :runner (:side @game-state))
              [:a.stack-servers-button {:on-click #(flip-runner-board)}
               (if (= "irl" (get-in @app-state [:options :runner-board-order]))
                 "Rig layout: IRL" "Rig layout: jnet")])]))
       (when (not (nil? @gameid))
         [:div.float-right [:a {:on-click #(leave-game)} "Leave game"]]))
     (when-let [game (some #(when (= @gameid (:gameid %)) %) @games)]
       (when (:started game)
         (let [c (count (:spectators game))]
           (when (pos? c)
             [:div.spectators-count.float-right (str c " Spectator" (when (> c 1) "s"))
              [:div.blue-shade.spectators
               (for [p (:spectators game)]
                 ^{:key (get-in p [:user :_id])}
                 [player-view p game])]]))))]))

(defn mount-root []
  ; navbar stuff
  (r/render [navbar] (.getElementById js/document "left-menu"))
  (r/render [status] (.getElementById js/document "status"))
  (r/render [auth-menu] (.getElementById js/document "right-menu"))
  (r/render [auth-forms] (.getElementById js/document "auth-forms"))
  ; main screens
  (r/render [about] (.getElementById js/document "about"))
  (r/render [account (r/cursor app-state [:user])] (.getElementById js/document "account"))
  (r/render [card-browser] (.getElementById js/document "cardbrowser"))
  (r/render [chat] (.getElementById js/document "chat"))
  (r/render [deck-builder] (.getElementById js/document "deckbuilder"))
  (r/render [gameboard] (.getElementById js/document "gameboard"))
  (r/render [game-lobby] (.getElementById js/document "gamelobby"))
  (r/render [help] (.getElementById js/document "help"))
  (r/render [news] (.getElementById js/document "news"))
  (r/render [stats] (.getElementById js/document "stats")))

(defn init! []
  (mount-root))
