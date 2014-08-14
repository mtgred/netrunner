(ns netrunner.gameboard
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.main :refer [app-state]]
            [netrunner.auth :refer [avatar] :as auth]
            [netrunner.cardbrowser :refer [image-url] :as cb]))

(def game-state
  (atom {:gameid 0
         :log []
         :side :corp
         :corp {:user {:username "" :emailhash ""}
                :identity {}
                :deck []
                :hand []
                :discard []
                :rfg []
                :remote-servers []
                :click 3
                :credit 5
                :bad-publicity 0
                :agenda-point 0
                :max-hand-size 5}
         :runner {:user {:username "" :emailhash ""}
                  :identity {}
                  :deck []
                  :hand []
                  :discard []
                  :rfg []
                  :rig []
                  :click 4
                  :credit 5
                  :memory 4
                  :link 0
                  :tag 0
                  :agenda-point 0
                  :max-hand-size 5
                  :brain-damage 0}}{}))

(defn init-game [game side]
  (swap! game-state merge game)
  (swap! game-state assoc :side side))

(def zoom-channel (chan))
(def socket (.connect js/io (str js/iourl "/lobby")))
(def socket-channel (chan))
(.on socket "netrunner" #(put! socket-channel (js->clj % :keywordize-keys true)))

(go (while true
      (let [msg (<! socket-channel)]
        (case (:type msg)
          "say" (swap! game-state update-in [:log] #(conj % {:user (:user msg) :text (:text msg)}))
          "state" (swap! game-state merge (:state msg))
          nil))))

(defn send [msg]
  (.emit socket "netrunner" (clj->js msg)))

(defn send-msg [event owner]
  (.preventDefault event)
  (let [input (om/get-node owner "msg-input")
        text (.-value input)]
    (when-not (empty? text)
      (send {:action "say" :gameid (:gameid @game-state) :text text})
      (aset input "value" "")
      (.focus input))))

(defn log-pane [messages owner]
  (reify
    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (let [div (om/get-node owner "msg-list")]
        (aset div "scrollTop" (.-scrollHeight div))))

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.log
        [:div.panel.blue-shade {:ref "msg-list"}
         (for [msg messages]
           (if (= (:user msg) "__system__")
             [:div.system (:text msg)]
             [:div.message
              (om/build avatar (:user msg) {:opts {:size 38}})
              [:div.content
               [:div.username (get-in msg [:user :username])]
               [:div (:text msg)]]]))]
        [:form {:on-submit #(send-msg % owner)}
         [:input {:ref "msg-input" :placeholder "Say something"}]]]))))

(defn card-view [cursor]
  (om/component
   (sab/html
    [:div.panel.blue-shade.card {:on-mouse-enter #(put! zoom-channel cursor)
                                 :on-mouse-leave #(put! zoom-channel false)}
     [:img.card.bg {:src (image-url cursor) :onError #(-> % .-target js/$ .hide)}]])))

(defn hand-view [{:keys [identity hand max-hand-size user] :as cursor}]
  (om/component
   (sab/html
    (let [side (:side identity)]
      [:div.panel.blue-shade.hand
       [:div.header
        (str (if (= side "Corp") "HQ" "Grip") " (" (count hand) ")")
        [:span.float-right (str "Max hand size: " max-hand-size)]]
       (if (= user (:user @app-state))
         (om/build-all card-view hand)
         (repeat (count hand) [:img.card {:src (str "/img/" (.toLowerCase side) ".png")}]))]))))

(defn handle-deck-click [side]
  (when (= side (:side @game-state))
    (send {:action "do" :gameid (:gameid @game-state) :side side :command "draw"})))

(defmulti deck-view #(get-in % [:identity :side]))

(defmethod deck-view "Runner" [{:keys [deck] :as cursor}]
  (om/component
   (sab/html
    [:div.panel.blue-shade.deck {}
     [:div.header (str "Stack (" (count deck) ")")]
     (when (> (count deck) 0)
       [:img.card.bg {:src "/img/runner.png" :on-double-click #(handle-deck-click :runner)}])])))

(defmethod deck-view "Corp" [{:keys [deck] :as cursor}]
  (om/component
   (sab/html
    [:div.panel.blue-shade.deck {}
     [:div.header (str "R&D (" (count deck) ")")]
     (when (> (count deck) 0)
       [:img.card.bg {:src "/img/corp.png" :on-double-click #(handle-deck-click :corp)}])])))

(defmulti discard-view #(get-in % [:identity :side]))

(defmethod discard-view "Runner" [{:keys [discard] :as cursor}]
  (om/component
   (sab/html
    [:div.panel.blue-shade.discard
     [:div.header (str "Heap (" (count discard) ")")]])))

(defmethod discard-view "Corp" [{:keys [discard] :as cursor}]
  (om/component
   (sab/html
    [:div.panel.blue-shade.discard
     [:div.header (str "Archive (" (count discard) ")")]])))

(defn rfg-view [cursor]
  (om/component
   (sab/html [:div.panel.blue-shade.rfg {} "Removed"])))

(defn scored-view [cursor]
  (om/component
   (sab/html [:div.panel.blue-shade.scored {} "Scored"])))

(defmulti stats-view #(get-in % [:identity :side]))

(defmethod stats-view "Runner" [{:keys [user click credit memory link tag brain-damage]} owner]
  (om/component
   (sab/html
    [:div.panel.blue-shade {}
     [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
     [:div (str click " Click" (if (> click 1) "s" ""))]
     [:div (str credit " Credit" (if (> credit 1) "s" ""))]
     [:div (str memory " Memory Unit" (if (> memory 1) "s" ""))]
     [:div (str link " Link" (if (> link 1) "s" ""))]
     [:div (str tag " Tag" (if (> tag 1) "s" ""))]
     [:div (str brain-damage " Brain Damage" (if (> brain-damage 1) "s" ""))]])))

(defmethod stats-view "Corp" [{:keys [user click credit bad-publicity]} owner]
  (om/component
   (sab/html
    [:div.panel.blue-shade {}
     [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
     [:div (str click " Click" (if (> click 1) "s" ""))]
     [:div (str credit " Credit" (if (> credit 1) "s" ""))]
     [:div (str bad-publicity " Bad Publicit" (if (> bad-publicity 1) "ies" "y"))]])))

(defmulti board #(get-in % [:identity :side]))

(defmethod board "Corp" [cursor]
  (om/component
   (sab/html
    [:div.board {}])))

(defmethod board "Runner" [cursor]
  (om/component
   (sab/html
    [:div.board {}])))

(defn zones [cursor]
  (om/component
   (sab/html
    [:div.dashboard
     (om/build hand-view cursor)
     (om/build card-view (:identity cursor))
     (om/build deck-view cursor)
     (om/build discard-view cursor)
     (when (> (count (:rfg cursor)) 0)
       (om/build rfg-view (:rfg cursor)))])))

(defn gameboard [{:keys [side gameid] :as cursor} owner]
  (reify
    om/IWillMount
    (will-mount [this]
      (go (while true
            (let [card (<! zoom-channel)]
              (om/set-state! owner :zoom card)))))

    om/IRenderState
    (render-state [this state]
      (sab/html
       (let [me (if (= side :corp) (:corp cursor) (:runner cursor))
             opponent (if (= side :corp) (:runner cursor) (:corp cursor))]
         (when (> gameid 0)
           [:div.gameboard
            [:div.leftpane
             [:div
              (om/build stats-view opponent)
              (om/build scored-view opponent)]
             [:div
              (om/build scored-view me)
              (om/build stats-view me)]]

            [:div.centralpane
             (om/build zones opponent)
             (om/build board opponent)
             (om/build board me)
             (om/build zones me)]

            [:div.rightpane {}
             [:div.card-zoom
              (when-let [card (om/get-state owner :zoom)]
                [:img.card.bg {:src (image-url card)}])]
             (om/build log-pane (:log cursor))]]))))))

(om/root gameboard game-state {:target (. js/document (getElementById "gameboard"))})
