(ns netrunner.gamelobby
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :refer [join]]
            [netrunner.auth :refer [authenticated avatar] :as auth]))

(def app-state (atom {:games [] :messages []}))
(def socket-channel (chan))
(def join-channel (chan))
(def socket (.connect js/io (str js/iourl "/lobby")))
(.on socket "netrunner" #(put! socket-channel (js->clj % :keywordize-keys true)))

(go (while true
      (let [msg (<! socket-channel)]
        (.log js/console (clj->js msg))
        (case (:type msg)
          "game" (put! join-channel (:gameid msg))
          "games" (swap! app-state assoc :games (sort-by :date > (:games msg)))
          "say" (swap! app-state update-in [:messages] #(conj % {:user (:user msg) :text (:text msg)}))))))

(defn send [msg]
  (.emit socket "netrunner" (clj->js msg)))

(defn new-game [cursor owner]
  (authenticated
   (fn [user]
     (om/set-state! owner :title (str (:username user) "'s game"))
     (om/set-state! owner :editing true)
     (-> ".game-title" js/$ .select))))

(defn create-game [cursor owner]
  (authenticated
   (fn [user]
     (if (empty? (om/get-state owner :title))
       (om/set-state! owner :flash-message "Please fill a game title.")
       (do
         (om/set-state! owner :editing false)
         (send {:action "create" :title (om/get-state owner :title)}))))))

(defn join-game [gameid cursor owner]
  (authenticated
   (fn [user]
     (send {:action "join" :gameid gameid}))))

(defn start-game [owner])

(defn leave-game [owner]
  (send {:action "leave" :gameid (om/get-state owner :gameid)})
  (om/set-state! owner :in-game false)
  (om/set-state! owner :gameid nil)
  (swap! app-state assoc :messages []))

(defn send-msg [event owner]
  (.preventDefault event)
  (let [input (om/get-node owner "msg-input")
        text (.-value input)]
    (when-not (empty? text)
      (send {:action "say" :gameid (om/get-state owner :gameid) :text text})
      (aset input "value" "")
      (.focus input))))

(defn player-view [cursor]
  (om/component
   (sab/html
    [:span.player
     (om/build avatar cursor {:opts {:size 22}})
     (:username cursor)])))

(defn chat-view [messages owner]
  (reify
    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (let [div (om/get-node owner "msg-list")]
        (aset div "scrollTop" (.-scrollHeight div))))

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div
        [:h3 "Chat"]
        [:div.message-list {:ref "msg-list"}
         (for [msg messages]
           (if (= (:user msg) "__system__")
             [:div.system (:text msg)]
             [:div.message
              (om/build avatar (:user msg) {:opts {:size 38}})
              [:div.content
               [:div.username (get-in msg [:user :username])]
               [:div (:text msg)]]]))]
        [:form.msg-box {:on-submit #(send-msg % owner)}
         [:input {:ref "msg-input" :placeholder "Say something"}]
         [:button "Send"]]]))))

(defn game-lobby [{:keys [games] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:side "Runner"})

    om/IWillMount
    (will-mount [this]
      (go (while true
            (let [gameid (<! join-channel)]
              (om/set-state! owner :gameid gameid)))))

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.lobby.panel.blue-shade
        [:div.games
         [:div.button-bar
          [:button {:class (if (:in-game state) "disabled" "")
                    :on-click #(new-game cursor owner)} "New game"]]
         [:div.game-list
          (if (empty? games)
            [:h4 "No game"]
            (for [game games]
              [:div.gameline {:class (when (= (:gameid state) (:id game)) "active")}
               (when-not (or (:gameid state) (:editing state) (= (count (:players game)) 2))
                 (let [id (:id game)]
                   [:button {:on-click #(join-game id cursor owner)} "Join"]))
               [:h4 (:title game)]
               [:div
                (om/build-all player-view (:players game))]]))]]

        [:div.game-panel
         (if (:editing state)
           (do
             [:div
              [:div.button-bar
               [:button {:type "button" :on-click #(create-game cursor owner)} "Create"]
               [:button {:type "button" :on-click #(om/set-state! owner :editing false)} "Cancel"]]
              [:h4 "Title"]
              [:input.game-title {:on-change #(om/set-state! owner :title (.. % -target -value))
                                  :value (:title state) :placeholder "Title"}]
              [:p.flash-message (:flash-message state)]])
           (when-let [game (some #(when (= (:gameid state) (:id %)) %) games)]
             (let [username (get-in @auth/app-state [:user :username])]
               [:div
                [:div.button-bar
                 (when (= (get-in (first (:players game)) [:user :username]) username)
                   [:button {:on-click #(start-game owner)} "Start"])
                 [:button {:on-click #(leave-game owner)} "Leave"]]
                [:h2 (:title game)]
                [:h3 "Players"]
                [:div.players
                 (for [player (:players game)]
                   [:div (om/build player-view player)])]
                (om/build chat-view (:messages cursor) {:state state})])))]]))))

(om/root game-lobby app-state {:target (. js/document (getElementById "gamelobby"))})
