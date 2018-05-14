(ns nr.main
  (:require [goog.events :as events]
            [goog.history.EventType :as EventType]
            [jinteki.nav :as nav]
            [nr.appstate :refer [app-state]]
            [nr.gameboard :as gameboard]
            [nr.gamelobby :as gamelobby]
            [reagent.core :as r])
  (:import goog.history.Html5History))

(enable-console-print!)

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

; Check if we can put a cursor on show-fn? to avoid re-renders (or if needed)
(defn navbar []
  (let [active (r/cursor app-state [:active-page])]
    (fn []
      [:ul.carousel-indicator {}
       (doall (for [[name route ndx show-fn?] nav/navbar-links]
                (when (or (not show-fn?) (show-fn? @app-state))
                  [:li {:class (if (= (first @active) route) "active" "")
                        :key name
                        :on-click #(.setToken history route)
                        :data-target "#main" :data-slide-to ndx}
                   [:a {:href route} name]])))])))

; need gamelobby and gameboard for this to work
(defn status
  (let [user (r/cursor app-state [:user])
        games (r/cursor app-state [:games])
        gameid (r/cursor app-state [:gameid])]
    (fn []
      [:div
       [:div.float-right
        (let [c (count (gamelobby/filter-blocked-games @user @games))]
          (str c " Game" (when (not= c 1) "s")))]
       (if-let [game (some #(when (= @gameid (:gameid %)) %) @games)]
         (let [user-id (-> @user :_id)
               is-player (some #(= user-id (-> % :user :_id)) (:players game))]
           (when (:started game)
             [:div.float-right
              (when is-player
                [:a.concede-button {:on-click gameboard/concede} "Concede"])
              [:a.leave-button {:on-click gamelobby/leave-game} "Leave game"]
              (when is-player
                [:a.mute-button {:on-click #(gameboard/mute-spectators (not (:mute-spectators game)))}
                 (if (:mute-spectators game) "Unmute spectators" "Mute spectators")])]))
         (when (not (nil? @gameid))
           [:div.float-right [:a {:on-click gamelobby/leave-game} "Leave game"]]))
       (when-let [game (some #(when (= @gameid (:gameid %)) %) @games)]
         (when (:started game)
           (let [c (count (:spectators game))]
             (when (pos? c)
               [:div.spectators-count.float-right (str c " Spectator" (when (> c 1) "s"))
                [:div.blue-shade.spectators (gamelobby/player-view
                                              (map (fn [%] {:player % :game game})
                                                   (:spectators game)))]]))))])))

(defn mount-root []
  (r/render [navbar] (.getElementById js/document "left-menu")))

(defn init! []
  (mount-root))
