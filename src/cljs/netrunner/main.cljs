(ns netrunner.main
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [goog.events :as events]
            [netrunner.ajax :refer [GET]]
            [netrunner.toast :refer [toast] :as toast])
  (:import goog.history.Html5History
           goog.history.EventType))

(def app-state
  (atom {:active-page "/"
         :user (js->clj js/user :keywordize-keys true)
         :cards [] :sets [] :mwl []
         :decks [] :decks-loaded false
         :games [] :gameid nil :messages []}))

(def tokens #js ["/" "/cards" "/deckbuilder" "/play" "/help" "/about"])

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

(go (while true
      (let [msg (<! socket-channel)]
        (reset! lock false)
        (case (:type msg)
          ("do" "notification" "quit") (do (swap! game-state (if (:diff msg) #(differ/patch @last-state (:diff msg))
                                                                             #(assoc (:state msg) :side (:side @game-state))))
                                           (swap! last-state #(identity @game-state)))
          nil))))

(defn navbar [cursor owner]
  (om/component
   (sab/html
    [:ul.carousel-indicator {}
     (for [page [["Jinteki" "/" 0]
                 ["Cards" "/cards" 1]
                 ["Deck Builder" "/deckbuilder" 2]
                 ["Play" "/play" 3]
                 ["Help" "/help" 4]
                 ["About" "/about" 5]]]
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
            [:a.concede-button {:on-click #(netrunner.gameboard/send-command "concede" {:user (:user @app-state)})} "Concede"])
          [:a {:on-click #(netrunner.gamelobby/leave-game)} "Leave game"]])
       (when (= (:side @app-state) :spectator)
         [:div.float-right [:a {:on-click #(netrunner.gamelobby/leave-game)} "Leave game"]]))
     (when-let [game (some #(when (= (:gameid cursor) (:gameid %)) %) (:games cursor))]
       (when (:started game)
         (let [c (count (:spectators game))]
            (when (pos? c)
              [:div.spectators-count.float-right (str c " Spectator" (when (> c 1) "s"))
               [:div.blue-shade.spectators (om/build-all netrunner.gamelobby/player-view
                                                         (map (fn [%] {:player % :game game})
                                                              (:spectators game)))]]))))])))

(defn version [cursor owner]
  (reify
    om/IWillMount
    (will-mount [this]
      (let [edit-channel (om/get-state owner :edit-channel)]
        (go (while true
            (let [card (<! zoom-channel)]
              (om/set-state! owner :zoom card))))

    om/IDidUpdate
      (did-update [this prev-props prev-state]
        (let [version-val (.html (js/$ "#version-val"))]
          (cond
              (not= version-val (:version @app-state)) (toast (str (str "You have an older version: " (:version @app-state)) version-val) "warning"))))

    om/IRenderState
      (render-state [this state]
        (sab/html
          [:div
            [:div.float-right "Version: "
              [:span (:version cursor)]]]))))

(om/root navbar app-state {:target (. js/document (getElementById "left-menu"))})
(om/root status app-state {:target (. js/document (getElementById "status"))})
(om/root version app-state {:target (. js/document (getElementById "version"))})
