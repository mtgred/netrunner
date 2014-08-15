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

(defn send-command [command]
  (send {:action "do" :gameid (:gameid @game-state) :side (:side @game-state) :command command}))

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

(defn label [cursor owner opts]
  (om/component
   (sab/html
    (let [fn (or (:fn opts) count)]
      [:div.header {:class (when (> (count cursor) 0) "darkbg")}
       (str (:name opts) " (" (fn cursor) ")")]))))

(defn hand-view [{:keys [identity hand max-hand-size user] :as cursor}]
  (om/component
   (sab/html
    (let [side (:side identity)]
      [:div.panel.blue-shade.hand
        (om/build label hand {:opts {:name (if (= side "Corp") "HQ" "Grip")}})
        (if (= user (:user @app-state))
          (om/build-all card-view hand)
          (repeat (count hand) [:img.card {:src (str "/img/" (.toLowerCase side) ".png")}]))]))))

(defmulti deck-view #(get-in % [:identity :side]))

(defmethod deck-view "Runner" [{:keys [deck] :as cursor}]
  (om/component
   (sab/html
    [:div.panel.blue-shade.deck {}
     (om/build label deck {:opts {:name "Stack"}})
     (when (> (count deck) 0)
       [:img.card.bg {:src "/img/runner.png" :on-click #(send-command "draw")}])])))

(defmethod deck-view "Corp" [{:keys [deck] :as cursor}]
  (om/component
   (sab/html
    [:div.panel.blue-shade.deck {}
     (om/build label deck {:opts {:name "R&D"}})
     (when (> (count deck) 0)
       [:img.card.bg {:src "/img/corp.png" :on-click #(send-command "draw")}])])))

(defmulti discard-view #(get-in % [:identity :side]))

(defmethod discard-view "Runner" [{:keys [discard] :as cursor}]
  (om/component
   (sab/html
    [:div.panel.blue-shade.discard
     (om/build label discard {:opts {:name "Heap"}})])))

(defmethod discard-view "Corp" [{:keys [discard] :as cursor}]
  (om/component
   (sab/html
    [:div.panel.blue-shade.discard
     (om/build label discard {:opts {:name "Archive"}})])))

(defn rfg-view [{:keys [rfg] :as cursor}]
  (om/component
   (sab/html
    (when (> (count (:rfg cursor)) 0)
      [:div.panel.blue-shade.rfg
       (om/build label rfg {:opts {:name "Removed"}})]))))

(defn scored-view [{:keys [rfg] :as cursor}]
  (om/component
   (sab/html
    [:div.panel.blue-shade.scored
     (om/build label rfg {:opts {:name "Scored"}})])))

(defmulti stats-view #(get-in % [:identity :side]))

(defmethod stats-view "Runner" [{:keys [user click credit memory link tag brain-damage max-hand-size]} owner]
  (om/component
   (sab/html
    [:div.panel.blue-shade {}
     [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
     [:div (str click " Click" (if (> click 1) "s" ""))]
     [:div (str credit " Credit" (if (> credit 1) "s" ""))]
     [:div (str memory " Memory Unit" (if (> memory 1) "s" ""))]
     [:div (str link " Link" (if (> link 1) "s" ""))]
     (when (> tag 0)
       [:div (str tag " Tag" (if (> tag 1) "s" ""))])
     (when (> brain-damage 0)
       [:div (str brain-damage " Brain Damage" (if (> brain-damage 1) "s" ""))])
     (when-not (= max-hand-size 5)
       [:div (str max-hand-size " Max hand size")])])))

(defmethod stats-view "Corp" [{:keys [user click credit bad-publicity max-hand-size]} owner]
  (om/component
   (sab/html
    [:div.panel.blue-shade {}
     [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
     [:div (str click " Click" (if (> click 1) "s" ""))]
     [:div (str credit " Credit" (if (> credit 1) "s" ""))]
     (when (> bad-publicity 0)
       [:div (str bad-publicity " Bad Publicit" (if (> bad-publicity 1) "ies" "y"))])
     (when-not (= max-hand-size 5)
       [:div (str max-hand-size " Max hand size")])])))

(defmulti board #(get-in % [:identity :side]))

(defmethod board "Corp" [cursor]
  (om/component
   (sab/html
    [:div {}])))

(defmethod board "Runner" [cursor]
  (om/component
   (sab/html
    [:div {}])))

(defn zones [cursor]
  (om/component
   (sab/html
    [:div.dashboard
     (om/build hand-view cursor)
     (om/build discard-view cursor)
     (om/build deck-view cursor)
     (om/build card-view (:identity cursor))])))

(defn cond-button [text cond command]
  (sab/html
   (if cond
     [:button.disabled text]
     [:button {:on-click #(send-command command)} text])))

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
            [:div.mainpane
             (om/build zones opponent)

             [:div.centralpane
              [:div.button-pane.panel.blue-shade
               (when (= side :runner)
                 (cond-button "Remove Tag" (or (< (:click me) 1) (< (:credit me) 2) (< (:tag me) 1))
                              "remove-tag"))
               (when (= side :corp)
                 (cond-button "Purge" (< (:click me) 3) "purge"))
               (cond-button "Draw" (< (:click me) 1) "draw")
               (cond-button "Take Credit" (< (:click me) 1) "credit")]

              [:div.leftpane
               [:div
                (om/build stats-view opponent)
                (om/build scored-view opponent)
                (om/build rfg-view opponent)]
               [:div
                (om/build rfg-view opponent)
                (om/build scored-view me)
                (om/build stats-view me)]]

              [:div.board
               (om/build board me)]]
             (om/build zones me)]
            [:div.rightpane {}
             [:div.card-zoom
              (when-let [card (om/get-state owner :zoom)]
                [:img.card.bg {:src (image-url card)}])]
             (om/build log-pane (:log cursor))]]))))))

(om/root gameboard game-state {:target (. js/document (getElementById "gameboard"))})
