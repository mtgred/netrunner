(ns netrunner.game
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.auth :refer [avatar] :as auth]))

(def socket (.connect js/io (str js/iourl "/lobby")))
(def socket-channel (chan))
(.on socket "netrunner" #(put! socket-channel (js->clj % :keywordize-keys true)))

(def app-state
  (atom {:gameid 0
         :log []
         :side :corp
         :corp {:user {:username "" :emailhash ""}
                :r&d []
                :hq []
                :archive []
                :remote-servers []
                :click 3
                :credit 5
                :bad-publicity 0
                :agenda-point 0
                :max-hand-size 5}
         :runner {:user {:username "" :emailhash ""}
                  :stack []
                  :grip []
                  :heap []
                  :rig []
                  :click 4
                  :credit 5
                  :memory 4
                  :link 0
                  :tag 0
                  :agenda-point 0
                  :max-hand-size 5
                  :brain-damage 0}}{}))

(defn init-game [gameid side corp runner]
  (swap! app-state assoc :gameid gameid :side (keyword (.toLowerCase side)))
  (swap! app-state assoc-in [:runner :user] (:user runner))
  (swap! app-state assoc-in [:runner :deck] (:deck runner))
  (swap! app-state assoc-in [:corp :user] (:user corp))
  (swap! app-state assoc-in [:corp :deck] (:deck corp)))

(go (while true
      (let [msg (<! socket-channel)]
        (.log js/console "game" (clj->js msg))
        (case (:type msg)
          "say" (swap! app-state update-in [:log] #(conj % {:user (:user msg) :text (:text msg)}))
          nil))))

(defn send [msg]
  (.emit socket "netrunner" (clj->js msg)))

(defn send-msg [event owner]
  (.preventDefault event)
  (let [input (om/get-node owner "msg-input")
        text (.-value input)]
    (when-not (empty? text)
      (send {:action "say" :gameid (:gameid @app-state) :text text})
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

(defn hand-view [cursor owner]
  (om/component
   (sab/html [:div.panel.blue-shade.hand {} "Hand"])))

(defn deck-view [cursor owner]
  (om/component
   (sab/html [:div.panel.blue-shade.deck {} "Deck"])))

(defn discard-view [cursor owner]
  (om/component
   (sab/html [:div.panel.blue-shade.discard {} "Discard"])))

(defn scored-view [cursor owner]
  (om/component
   (sab/html [:div.panel.blue-shade.scored {} (str "Agenda Points")])))

(defn runner-stats-view [{:keys [user click credit memory link tag brain-damage max-hand-size]} owner]
  (om/component
   (sab/html
    [:div.panel.blue-shade {}
     [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
     [:div (str click " Click" (if (> click 1) "s" ""))]
     [:div (str credit " Credit" (if (> credit 1) "s" ""))]
     [:div (str memory " Memory Unit" (if (> memory 1) "s" ""))]
     [:div (str link " Link" (if (> link 1) "s" ""))]
     [:div (str tag " Tag" (if (> tag 1) "s" ""))]
     [:div (str brain-damage " Brain Damage" (if (> brain-damage 1) "s" ""))]
     [:div (str max-hand-size " Max Hand Size")]])))

(defn corp-stats-view [{:keys [user click credit bad-publicity max-hand-size]} owner]
  (om/component
   (sab/html
    [:div.panel.blue-shade {}
     [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
     [:div (str click " Click" (if (> click 1) "s" ""))]
     [:div (str credit " Credit" (if (> credit 1) "s" ""))]
     [:div (str bad-publicity " Bad Publicit" (if (> bad-publicity 1) "ies" "y"))]
     [:div (str max-hand-size " Max Hand Size")]])))

(defn corp-board [cursor owner]
  (om/component
   (sab/html
    [:div.board {}])))

(defn runner-board [cursor owner]
  (om/component
   (sab/html
    [:div.board {}])))

(defn deck []
  (if (= (:side app-state) :runner) :stack :r&d))

(defn hand []
  (if (= (:side app-state) :runner) :grip :hq))

(defn discard []
  (if (= (:side app-state) :runner) :heap :archive))

(defn gameboard [{:keys [side] :as cursor} owner]
  (om/component
   (sab/html
    [:div.gameboard
     [:div.leftpane
      [:div
       (if (= side :corp)
         (om/build runner-stats-view (:runner cursor))
         (om/build corp-stats-view (:corp cursor)))
       (om/build scored-view ((if (= side :corp) :runner :corp) cursor))]
      [:div
       (om/build scored-view (side cursor))
       (if (= side :corp)
         (om/build corp-stats-view (:corp cursor))
         (om/build runner-stats-view (:runner cursor)))]]
     [:div.centralpane
      (om/build corp-board (:corp cursor))
      (om/build runner-board (:runner cursor))
      [:div.dashboard
       (om/build hand-view ((hand) cursor))
       (om/build deck-view ((deck) cursor))
       (om/build discard-view ((discard) cursor))]]
     [:div.rightpane {}
      [:div.card-zoom]
      (om/build log-pane (:log cursor))]])))

(om/root gameboard app-state {:target (. js/document (getElementById "gameboard"))})
