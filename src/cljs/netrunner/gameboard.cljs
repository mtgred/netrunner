(ns netrunner.gameboard
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.main :refer [app-state]]
            [netrunner.auth :refer [avatar] :as auth]
            [netrunner.cardbrowser :refer [image-url] :as cb]))

(def game-state (atom {}))

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
          "state" (swap! game-state merge (:state msg))
          nil))))

(defn send [msg]
  (.emit socket "netrunner" (clj->js msg)))

(defn send-command
  ([command] (send-command command nil))
  ([command args]
     (send {:action "do" :gameid (:gameid @game-state) :side (:side @game-state)
            :command command :args args})))

(defn send-msg [event owner]
  (.preventDefault event)
  (let [input (om/get-node owner "msg-input")
        text (.-value input)]
    (when-not (empty? text)
      (send-command "say" {:text text})
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
    [:div.blue-shade.card {:on-mouse-enter #(put! zoom-channel cursor)
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
    (let [side (:side identity)
          size (count hand)]
      [:div.panel.blue-shade.hand {:class (when (> size 6) "squeeze")}
       (om/build label hand {:opts {:name (if (= side "Corp") "HQ" "Grip")}})
       (map-indexed (fn [i card]
                      (sab/html
                       [:div.card-wrapper {:style {:left (* (/ 320 (dec size)) i)}}
                        (if (= user (:user @app-state))
                          [:div {:on-click #(send-command "play" {:card @card})}
                           (om/build card-view card)]
                          [:img.card {:src (str "/img/" (.toLowerCase side) ".png")}])]))
                    hand)]))))

(defmulti deck-view #(get-in % [:identity :side]))

(defmethod deck-view "Runner" [{:keys [deck] :as cursor}]
  (om/component
   (sab/html
    [:div.panel.blue-shade.deck {}
     (om/build label deck {:opts {:name "Stack"}})
     (when (> (count deck) 0)
       [:img.card.bg {:src "/img/runner.png"}])])))

(defmethod deck-view "Corp" [{:keys [deck] :as cursor}]
  (om/component
   (sab/html
    [:div.panel.blue-shade.deck {}
     (om/build label deck {:opts {:name "R&D"}})
     (when (> (count deck) 0)
       [:img.card.bg {:src "/img/corp.png"}])])))

(defmulti discard-view #(get-in % [:identity :side]))

(defmethod discard-view "Runner" [{:keys [discard] :as cursor}]
  (om/component
   (sab/html
    [:div.panel.blue-shade.discard
     (om/build label discard {:opts {:name "Heap"}})
     (when-not (empty? discard)
       (om/build card-view (last discard)))])))

(defmethod discard-view "Corp" [{:keys [discard] :as cursor}]
  (om/component
   (sab/html
    [:div.panel.blue-shade.discard
     (om/build label discard {:opts {:name "Archive"}})
     (when-not (empty? discard)
       (om/build card-view (last discard)))])))

(defn rfg-view [{:keys [rfg] :as cursor}]
  (om/component
   (sab/html
    (let [size (count rfg)]
      (when (> size 0)
        [:div.panel.blue-shade.rfg {:class (when (> size 2) "squeeze")}
         (om/build label rfg {:opts {:name "Removed"}})
         (map-indexed (fn [i card]
                        (sab/html
                         [:div.card-wrapper {:style {:left (* (/ 100 (dec size)) i)}}
                          [:div (om/build card-view card)]]))
                      rfg)])))))

(defn scored-view [{:keys [scored] :as cursor}]
  (om/component
   (sab/html
    (let [size (count scored)]
      [:div.panel.blue-shade.scored {:class (when (> size 2) "squeeze")}
       (om/build label scored {:opts {:name "Scored"}})
       (map-indexed (fn [i card]
                      (sab/html
                       [:div.card-wrapper {:style {:left (* (/ 100 (dec size)) i)}}
                        [:div (om/build card-view card)]]))
                    scored)]))))

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
     [:div.panel.blue-shade.identity
      (om/build card-view (:identity cursor))]])))

(defn cond-button [text command cond]
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
               (if-not (:keep me)
                 [:div
                  [:h4 "Keep hand?"]
                  [:button {:on-click #(send-command "keep")} "Keep hand"]
                  [:button {:on-click #(send-command "mulligan")} "Mulligan"]]
                 (when-not (:keep opponent)
                   [:h4 "Waiting for opponent's mulligan choice."]))

               (when (and (:keep me) (:keep opponent))
                 [:div
                  (when (= side :runner)
                    (cond-button "Remove Tag" "remove-tag"
                                 (or (< (:click me) 1) (< (:credit me) 2) (< (:tag me) 1))))
                  (when (= side :corp)
                    (cond-button "Purge" "purge" (< (:click me) 3)))
                  (cond-button "Draw" "draw" (< (:click me) 1))
                  (cond-button "Take Credit" "credit" (< (:click me) 1))])]

              [:div.leftpane
               [:div
                (om/build stats-view opponent)
                (om/build scored-view opponent)
                (om/build rfg-view opponent)]
               [:div
                (om/build rfg-view me)
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
