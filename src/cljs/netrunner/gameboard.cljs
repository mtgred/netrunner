(ns netrunner.gameboard
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.main :refer [app-state]]
            [netrunner.auth :refer [avatar] :as auth]
            [netrunner.cardbrowser :refer [image-url add-symbols] :as cb]))

(defonce game-state (atom {}))

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

(defn handle-card-click [{:keys [type zone abilities] :as card} owner]
  (cond
   (= zone ["hand"]) (case type
                       ("Upgrade" "ICE") (-> (om/get-node owner "servers") js/$ .toggle)
                       ("Agenda" "Asset") (if (empty? (get-in @game-state [:corp :servers :remote]))
                                            (send-command "play" {:card card :server "New remote"})
                                            (-> (om/get-node owner "servers") js/$ .toggle))
                       (send-command "play" {:card card}))
   (or (= (first zone) "rig")
       (= (:side @game-state) :runner)
       (and (= (first zone) "servers")
            (:rezzed card))) (let [count (count abilities)]
                               (cond (> count 1) (-> (om/get-node owner "abilities") js/$ .toggle)
                                     (= count 1) (send-command "ability" {:card card :ability 0})))
   (and (= (first zone) "servers")
        (= (:side @game-state) :corp)
        (not (:rezzed card))) (send-command "rez" {:card card})))

(defn in-play? [card]
  (let [dest (when (= (:side card) "Runner")
               (get-in @game-state [:runner :rig (keyword (.toLowerCase (:type card)))]))]
    (some #(= (:title %) (:title card)) dest)))

(defn playable? [{:keys [title side zone cost type uniqueness abilities] :as card}]
  (let [my-side (:side @game-state)
        me (my-side @game-state)]
    (and (= (keyword (.toLowerCase side)) my-side)
         (and (= zone ["hand"])
              (or (not uniqueness) (not (in-play? card)))
              (or (#{"Agenda" "Asset" "Upgrade" "ICE"} type) (>= (:credit me) cost))
              (> (:click me) 0)))))

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
        [:div.messages.panel.blue-shade {:ref "msg-list"}
         (for [msg messages]
           (if (= (:user msg) "__system__")
             [:div.system {:dangerouslySetInnerHTML #js {:__html (add-symbols (:text msg))}}]
             [:div.message
              (om/build avatar (:user msg) {:opts {:size 38}})
              [:div.content
               [:div.username (get-in msg [:user :username])]
               [:div (:text msg)]]]))]
        [:form {:on-submit #(send-msg % owner)}
         [:input {:ref "msg-input" :placeholder "Say something"}]]]))))

(defn remote-list []
  (map #(str "Server " %) (-> (get-in @game-state [:corp :servers :remote]) count range reverse)))

(defn card-view [{:keys [code type] :as cursor} owner {:keys [flipped] :as opts}]
  (om/component
   (when code
     (sab/html
      [:div.blue-shade.card {:on-mouse-enter #(when (or (not flipped) (= (:side @game-state) :corp))
                                                (put! zoom-channel cursor))
                             :on-mouse-leave #(put! zoom-channel false)
                             :on-click #(handle-card-click @cursor owner)}
       (when-let [url (image-url cursor)]
         (if flipped
           [:img.card.bg {:src "/img/corp.png"}]
           [:img.card.bg {:src url :onError #(-> % .-target js/$ .hide)}]))
       (when-let [counter (:counter cursor)]
         (when (> counter 0)
           [:div.darkbg.counter counter]))
       (when-let [abilities (:abilities cursor)]
         (when (> (count abilities 1))
           [:div.blue-shade.panel.abilities {:ref "abilities"}
            (map-indexed
             (fn [i label]
               [:div {:on-click #(do (send-command "ability" {:card @cursor :ability i})
                                     (-> (om/get-node owner "abilities") js/$ .fadeOut))
                      :dangerouslySetInnerHTML #js {:__html (add-symbols label)}}])
             abilities)]))
       (when (#{"Agenda" "Asset" "ICE" "Upgrade"} type)
         (let [centrals ["HQ" "R&D" "Archives"]
               remotes (conj (remote-list) "New remote")
               servers (case type
                         ("Upgrade" "ICE") (concat remotes centrals)
                         ("Agenda" "Asset") remotes)]
           [:div.blue-shade.panel.servers {:ref "servers"}
            (map (fn [label]
                   [:div {:on-click #(do (send-command "play" {:card @cursor :server label})
                                         (-> (om/get-node owner "servers") js/$ .fadeOut))}
                    label])
                 servers)]))]))))

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
       (for [card hand]
         [:div.card-wrapper {:class (if (playable? card) "playable" "")}
          (if (= user (:user @app-state))
            (om/build card-view card)
            [:img.card {:src (str "/img/" (.toLowerCase side) ".png")}])])
      (om/build label hand {:opts {:name (if (= side "Corp") "HQ" "Grip")}})]))))

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

(defmethod discard-view "Runner" [{:keys [discard] :as cursor} owner]
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
     (om/build label discard {:opts {:name "Archives"}})
     (when-not (empty? discard)
       (om/build card-view (last discard)))])))

(defn rfg-view [{:keys [rfg] :as cursor}]
  (om/component
   (sab/html
    (let [size (count rfg)]
      (when (> size 0)
        [:div.panel.blue-shade.rfg {:class (when (> size 3) "squeeze")}
         (for [card rfg]
           [:div.card-wrapper (om/build card-view card)])
         (om/build label rfg {:opts {:name "Removed"}})])))))

(defn scored-view [{:keys [scored] :as cursor}]
  (om/component
   (sab/html
    (let [size (count scored)]
      [:div.panel.blue-shade.scored {:class (when (> size 3) "squeeze")}
       (for [card scored]
         [:div.card-wrapper (om/build card-view card)])
       (om/build label scored {:opts {:name "Scored Area"}})]))))

(defn controls [key]
  (sab/html
   [:div.controls
    [:button.small {:on-click #(send-command "change" {:key key :delta 1}) :type "button"} "+"]
    [:button.small {:on-click #(send-command "change" {:key key :delta -1}) :type "button"} "-"]]))

(defmulti stats-view #(get-in % [:identity :side]))

(defmethod stats-view "Runner" [{:keys [user click credit memory link tag brain-damage max-hand-size]} owner]
  (om/component
   (sab/html
    (let [me? (= (:side @game-state) :runner)]
      [:div.stats.panel.blue-shade {}
       [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
       [:div (str click " Click" (if (> click 1) "s" "")) (when me? (controls :click))]
       [:div (str credit " Credit" (if (> credit 1) "s" "")) (when me? (controls :credit))]
       [:div (str memory " Memory Unit" (if (> memory 1) "s" "")) (when me? (controls :memory))]
       [:div (str link " Link" (if (> link 1) "s" "")) (when me? (controls :link))]
       [:div (str tag " Tag" (if (> tag 1) "s" "")) (when me? (controls :tag))]
       [:div (str brain-damage " Brain Damage" (if (> brain-damage 1) "s" ""))
        (when me? (controls :brain-damage))]
       [:div (str max-hand-size " Max hand size") (when me? (controls :max-hand-size))]]))))

(defmethod stats-view "Corp" [{:keys [user click credit bad-publicity max-hand-size]} owner]
  (om/component
   (sab/html
    (let [me? (= (:side @game-state) :corp)]
      [:div.stats.panel.blue-shade {}
       [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
       [:div (str click " Click" (if (> click 1) "s" "")) (when me? (controls :click))]
       [:div (str credit " Credit" (if (> credit 1) "s" "")) (when me? (controls :credit))]
       [:div (str bad-publicity " Bad Publicit" (if (> bad-publicity 1) "ies" "y"))
        (when me? (controls :bad-publicity))]
       [:div (str max-hand-size " Max hand size") (when me? (controls :max-hand-size))]]))))

(defn server-view [{:keys [ices content] :as cursor} owner opts]
  (om/component
   (sab/html
    [:div.server {:class (when (= (:side @game-state) :runner) "opponent")}
     [:div.ices (for [ice ices] (om/build card-view ice {:opts {:flipped (not (:rezzed ice))}}))]
     [:div.content {:class (when (= (count content) 1) "center")}
      (for [card (reverse content)] (om/build card-view card {:opts {:flipped (not (:rezzed card))}}))
      (when opts (om/build label content {:opts opts}))]])))

(defmulti board-view #(get-in % [:identity :side]))

(defmethod board-view "Corp" [{:keys [servers]}]
  (om/component
   (sab/html
    [:div.corp-board
     (om/build server-view (:archives servers))
     (om/build server-view (:rd servers))
     (om/build server-view (:hq servers))
     (map-indexed (fn [i server] (om/build server-view server {:opts {:name (str "Server " i)}}))
                  (:remote servers))])))

(defmethod board-view "Runner" [{:keys [rig]}]
  (om/component
   (sab/html
    [:div.runner-board
     (for [zone [:program :resource :hardware]]
       [:div (for [c (zone rig)]
               [:div.card-wrapper {:class (when (playable? c) "playable")}
                (om/build card-view c)])])])))

(defn zones [cursor]
  (om/component
   (sab/html
    [:div.dashboard
     (om/build hand-view cursor)
     (om/build discard-view cursor)
     (om/build deck-view cursor)
     [:div.panel.blue-shade.identity
      (om/build card-view (:identity cursor))]])))

(defn cond-button [text cond f]
  (sab/html
   (if cond
     [:button.disabled text]
     [:button {:on-click f} text])))

(defn gameboard [{:keys [side gameid active-player end-turn] :as cursor} owner]
  (reify
    om/IWillMount
    (will-mount [this]
      (go (while true
            (let [card (<! zoom-channel)]
              (om/set-state! owner :zoom card)))))

    om/IRenderState
    (render-state [this state]
      (sab/html
       (when (> gameid 0)
         (let [me (side cursor)
               opponent ((if (= side :corp) :runner :corp) cursor)]
           [:div.gameboard
            [:div.mainpane
             (om/build zones opponent)

             [:div.centralpane
              [:div.leftpane
               [:div
                (om/build stats-view opponent)
                (om/build scored-view opponent)
                (om/build rfg-view opponent)]
               [:div
                (om/build rfg-view me)
                (om/build scored-view me)
                (om/build stats-view me)]]

              [:div.button-pane
               (if-not (:keep me)
                 [:div.panel.blue-shade
                  [:h4 "Keep hand?"]
                  [:button {:on-click #(send-command "keep")} "Keep"]
                  [:button {:on-click #(send-command "mulligan")} "Mulligan"]])

               (when (:keep me)
                 [:div.panel.blue-shade
                  (if (= (keyword active-player) side)
                    (when (and (zero? (:click me)) (not end-turn))
                      [:button {:on-click #(send-command "end-turn")} "End Turn"])
                    (when end-turn
                      [:button {:on-click #(send-command "start-turn")} "Start Turn"]))
                  (when (= side :runner)
                    (cond-button "Remove Tag"
                                 (or (< (:click me) 1) (< (:credit me) 2) (< (:tag me) 1))
                                 #(send-command "remove-tag")))
                  (when (= side :runner)
                    [:div.run-button
                     (cond-button "Run" (< (:click me) 1)
                                  #(-> (om/get-node owner "servers") js/$ .toggle))
                     (let [servers (concat (remote-list) ["HQ" "R&D" "Archives"])]
                       [:div.blue-shade.panel.servers {:ref "servers"}
                        (map (fn [label]
                               [:div {:on-click #(do (send-command "run" {:server label})
                                                     (-> (om/get-node owner "servers") js/$ .fadeOut))}
                                label])
                             servers)])])
                  (when (= side :corp)
                    (cond-button "Purge" (< (:click me) 3) #(send-command "purge")))
                  (cond-button "Draw" (< (:click me) 1) #(send-command "draw"))
                  (cond-button "Gain Credit" (< (:click me) 1) #(send-command "credit"))])]

              [:div.board
               (om/build board-view opponent)
               (om/build board-view me)]]
             (om/build zones me)]
            [:div.rightpane {}
             [:div.card-zoom
              (when-let [card (om/get-state owner :zoom)]
                [:img.card.bg {:src (image-url card)}])]
             (om/build log-pane (:log cursor))]]))))))

(om/root gameboard game-state {:target (. js/document (getElementById "gameboard"))})
