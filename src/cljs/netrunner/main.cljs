(ns netrunner.main
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [netrunner.appstate :refer [app-state]]
            [netrunner.gameboard :as gameboard]
            [netrunner.gamelobby :as gamelobby])
  (:import goog.history.Html5History))

(def tokens #js ["/" "/cards" "/deckbuilder" "/play" "/help" "/account" "/about"])

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

(defn navbar [cursor owner]
  (om/component
   (sab/html
    [:ul.carousel-indicator {}
     (for [page [["Chat" "/" 0]
                 ["Cards" "/cards" 1]
                 ["Deck Builder" "/deckbuilder" 2]
                 ["Play" "/play" 3]
                 ["Help" "/help" 4]
                 ["Settings" "/account" 5]
                 ["About" "/about" 6]]]
       (let [route (second page)]
         [:li {:class (if (= (first (:active-page cursor)) route) "active" "")
               :on-click #(.setToken history route)
               :data-target "#main" :data-slide-to (last page)}
          [:a {:href route} (first page)]]))])))

(defn status [cursor owner]
  (om/component
   (sab/html
    [:div
     [:div.float-right
      (let [c (count (:games cursor))]
        (str c " Game" (when (not= c 1) "s")))]
     (if-let [game (some #(when (= (:gameid cursor) (:gameid %)) %) (:games cursor))]
       (when (:started game)
         [:div.float-right
          (when (not= (:side @app-state) :spectator)
            [:a.concede-button {:on-click #(gameboard/send-command "concede" {:user (:user @app-state)})} "Concede"])
          [:a {:on-click #(gamelobby/leave-game)} "Leave game"]])
       (when (= (:side @app-state) :spectator)
         [:div.float-right [:a {:on-click #(gamelobby/leave-game)} "Leave game"]]))
     (when-let [game (some #(when (= (:gameid cursor) (:gameid %)) %) (:games cursor))]
       (when (:started game)
         (let [c (count (:spectators game))]
            (when (pos? c)
              [:div.spectators-count.float-right (str c " Spectator" (when (> c 1) "s"))
               [:div.blue-shade.spectators (om/build-all gamelobby/player-view
                                                         (map (fn [%] {:player % :game game})
                                                              (:spectators game)))]]))))])))

(om/root navbar app-state {:target (. js/document (getElementById "left-menu"))})
(om/root status app-state {:target (. js/document (getElementById "status"))})