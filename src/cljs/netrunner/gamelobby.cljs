(ns netrunner.gamelobby
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :refer [join]]
            [netrunner.main :refer [app-state]]
            [netrunner.auth :refer [authenticated avatar] :as auth]
            [netrunner.gameboard :refer [init-game game-state]]
            [netrunner.cardbrowser :refer [image-url] :as cb]
            [netrunner.deckbuilder :refer [deck-status-span deck-status-label]]))

(def socket-channel (chan))
(def socket (.connect js/io (str js/iourl "/lobby")))
(.on socket "netrunner" #(put! socket-channel (js->clj % :keywordize-keys true)))

(defn launch-game [game]
  (let [user (:user @app-state)
        side (if (= (get-in game [:runner :user]) user)
               :runner
               (if (= (get-in game [:corp :user]) user)
                 :corp
                 :spectator))]
    (swap! app-state assoc :side side)
    (init-game game side))
  (set! (.-onbeforeunload js/window) #(clj->js "Leaving this page will disconnect you from the game."))
  (-> "#gamelobby" js/$ .fadeOut)
  (-> "#gameboard" js/$ .fadeIn))

(defn sort-games-list [games]
  (sort-by #(vec (map (assoc % :started (not (:started %)))
                      [:started :date]))
           > games))

(go (while true
      (let [msg (<! socket-channel)]
        (case (:type msg)
          "game" (do (swap! app-state assoc :gameid (:gameid msg))
                     (when (:started msg) (launch-game nil)))
          "games" (do (when (:gamesdiff msg)
                        (swap! app-state update-in [:games]
                               (fn [games]
                                 (let [gamemap (into {} (map #(assoc {} (keyword (:gameid %)) %) games))
                                       create (merge gamemap (get-in msg [:gamesdiff :create]))
                                       update (merge create (get-in msg [:gamesdiff :update]))
                                       delete (apply dissoc update (map keyword (keys (get-in msg [:gamesdiff :delete]))))]
                                   (sort-games-list (vals delete))))))
                      (when (:games msg)
                        (swap! app-state assoc :games (sort-games-list (vals (:games msg)))))
                      (when-let [sound (:notification msg)]
                        (when-not (:gameid @app-state)
                          (.play (.getElementById js/document sound)))))
          "say" (do (swap! app-state update-in [:messages]
                           #(conj % {:user (:user msg) :text (:text msg)}))
                    (when-let [sound (:notification msg)]
                      (.play (.getElementById js/document sound ))))
          "start" (launch-game (:state msg))
          "Invalid password" (js/console.log "pwd" (:gameid msg))
          nil))))

(defn send
  ([msg] (send msg nil))
  ([msg fn]
   (try (js/ga "send" "event" "lobby" msg) (catch js/Error e))
   (.emit socket "netrunner" (clj->js msg) fn)))

(defn new-game [cursor owner]
  (authenticated
   (fn [user]
     (om/set-state! owner :title (str (:username user) "'s game"))
     (om/set-state! owner :side "Corp")
     (om/set-state! owner :editing true)
     (om/set-state! owner :flash-message "")
     (om/set-state! owner :protected false)
     (om/set-state! owner :password "")
     (om/set-state! owner :allowspectator true)
     (om/set-state! owner :spectatorhands false)
     (-> ".game-title" js/$ .select))))

(defn create-game [cursor owner]
  (authenticated
   (fn [user]
     (if (empty? (om/get-state owner :title))
       (om/set-state! owner :flash-message "Please fill a game title.")
       (if (and (om/get-state owner :protected)
                (empty? (om/get-state owner :password)))
        (om/set-state! owner :flash-message "Please fill a password")
        (do (om/set-state! owner :editing false)
            (swap! app-state assoc :messages [])
            (send {:action "create"
                   :title (om/get-state owner :title)
                   :password (om/get-state owner :password)
                   :allowspectator (om/get-state owner :allowspectator)
                   :spectatorhands (om/get-state owner :spectatorhands)
                   :side (om/get-state owner :side)
                   :room (om/get-state owner :current-room)})))))))

(defn join-game [gameid owner action password]
  (authenticated
   (fn [user]
     (om/set-state! owner :editing false)
     (swap! app-state assoc :messages [])
     (send {:action action :gameid gameid :password password} #(if (= % "invalid password")
                                                                 (om/set-state! owner :error-msg "Invalid password")
                                                                 (om/set-state! owner :prompt false))))))

(defn leave-lobby [cursor owner]
  (send {:action "leave-lobby" :gameid (:gameid @app-state)})
  (om/update! cursor :gameid nil)
  (om/update! cursor :message []))
  (swap! app-state dissoc :password-gameid)

(defn leave-game []
  (send {:action "leave-game" :gameid (:gameid @app-state)
         :user (:user @app-state) :side (:side @game-state)})
  (reset! game-state nil)
  (swap! app-state dissoc :gameid :side :password-gameid)
  (.removeItem js/localStorage "gameid")
  (set! (.-onbeforeunload js/window) nil)
  (-> "#gameboard" js/$ .fadeOut)
  (-> "#gamelobby" js/$ .fadeIn))

(defn concede []
  (send {:action "concede" :gameid (:gameid @app-state)
         :user (:user @app-state) :side (:side @game-state)}))

(defn send-msg [event owner]
  (.preventDefault event)
  (let [input (om/get-node owner "msg-input")
        text (.-value input)
        $div (js/$ ".lobby .message-list")]
    (when-not (empty? text)
      (send {:action "say" :gameid (:gameid @app-state) :text text})
      (.scrollTop $div (+ (.prop $div "scrollHeight") 500))
      (aset input "value" "")
      (.focus input))))

(defn deckselect-modal [{:keys [gameid games decks sets user]} owner opts]
  (om/component
   (sab/html
    [:div.modal.fade#deck-select
     [:div.modal-dialog
      [:h3 "Select your deck"]
      [:div.deck-collection
       (let [players (:players (some #(when (= (:gameid %) gameid) %) games))
             side (:side (some #(when (= (:user %) user) %) players))]
         [:div {:data-dismiss "modal"}
          (for [deck (sort-by :date > (filter #(= (get-in % [:identity :side]) side) decks))]
            [:div.deckline {:on-click #(send {:action "deck" :gameid (:gameid @app-state) :deck deck})}
             [:img {:src (image-url (:identity deck))}]
             [:div.float-right (deck-status-span sets deck)]
             [:h4 (:name deck)]
             [:div.float-right (-> (:date deck) js/Date. js/moment (.format "MMM Do YYYY"))]
             [:p (get-in deck [:identity :title])]])])]]])))

(defn faction-icon
  [faction identity]
  (let [icon-span (fn [css-faction] [:span.faction-icon {:class css-faction :title identity}])]
    (case faction
      "Adam" (icon-span "adam")
      "Anarch" (icon-span "anarch")
      "Apex" (icon-span "apex")
      "Criminal" (icon-span "criminal")
      "Haas-Bioroid" (icon-span "hb")
      "Jinteki" (icon-span "jinteki")
      "NBN" (icon-span "nbn")
      "Shaper" (icon-span "shaper")
      "Sunny Lebeau" (icon-span "sunny")
      "Weyland Consortium" (icon-span "weyland")
      [:span.side "(Unknown)"])))

(defn player-view [{:keys [player game] :as args}]
  (om/component
   (sab/html
    [:span.player
     (om/build avatar (:user player) {:opts {:size 22}})
     (get-in player [:user :username])
     (let [side (:side player)
           faction (:faction player)
           identity (:identity player)
           specs (:allowspectator game)]
       (cond
         (and (some? faction) (not= "Neutral" faction) specs) (faction-icon faction identity)
         side [:span.side (str "(" side ")")]))])))

(defn chat-view [messages owner]
  (reify
    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (let [div (om/get-node owner "msg-list")
            height (.-scrollHeight div)]
        (when (< (- height (.-scrollTop div) (.height (js/$ ".lobby .chat-box"))) 500)
          (aset div "scrollTop" (.-scrollHeight div)))))

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.chat-box
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
        [:div
         [:form.msg-box {:on-submit #(send-msg % owner)}
          [:input {:ref "msg-input" :placeholder "Say something" :accessKey "l"}]
          [:button "Send"]]]]))))

(defn game-view [{:keys [title password started players gameid current-game password-game] :as game} owner]
  (reify
    om/IRenderState
    (render-state [this state]
     (letfn [(join [action]
                (let [password (:password password-game password)]
                 (if (empty? password)
                  (join-game (if password-game (:gameid password-game) gameid) owner action nil)
                  (if-let [input-password (om/get-state owner :password)]
                    (join-game (if password-game (:gameid password-game) gameid) owner action input-password)
                    (do (swap! app-state assoc :password-gameid gameid) (om/set-state! owner :prompt action))))))]
       (sab/html
        [:div.gameline {:class (when (= current-game gameid) "active")}
         (when (and (:allowspectator game) (not current-game))
           [:button {:on-click #(join "watch")} "Watch"])
         (when-not (or current-game (= (count players) 2) started)
           [:button {:on-click #(join "join")} "Join"])
         (let [c (count (:spectators game))]
           [:h4 (str (when-not (empty? (:password game))
                       "[PRIVATE] ")
                     (:title game)
                     (when (pos? c)
                       (str  " (" c " spectator" (when (> c 1) "s") ")")))])
         [:div (om/build-all player-view (map (fn [%] {:player % :game game}) (:players game)))]
         (when-let [prompt (om/get-state owner :prompt)]
           [:div.password-prompt
            [:h3 (str "Password for " (if password-game (:title password-game) title))]
            [:p
             [:input.game-title {:on-change #(om/set-state! owner :password (.. % -target -value))
                                 :type "password"
                                 :value (:password state) :placeholder "Password" :maxLength "30"}]]
            [:p
             [:button {:type "button" :on-click #(join prompt)}
              prompt]
             [:span.fake-link {:on-click #(do (swap! app-state dissoc :password-gameid) (om/set-state! owner :prompt false))}
              "Cancel"]]
            (when-let [error-msg (om/get-state owner :error-msg)]
              [:p.flash-message error-msg])])])))))

(defn game-list [{:keys [games gameid password-game] :as cursor} owner]
  (let [roomgames (filter #(= (:room %) (om/get-state owner :current-room)) games)]
    [:div.game-list
     (if (empty? roomgames)
       [:h4 "No games"]
       (for [game roomgames]
        (om/build game-view (assoc game :current-game gameid :password-game password-game))))]))

(defn game-lobby [{:keys [games gameid messages sets user password-gameid] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:current-room "casual"})

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.lobby.panel.blue-shade
        [:div.games
         [:div.button-bar
          (if gameid
            [:button.float-left {:class "disabled"} "New game"]
            [:button.float-left {:on-click #(new-game cursor owner)} "New game"])
          (let [count-games (fn [room] (count (filter #(= room (:room %)) games)))
                room-tab (fn [room roomname]
                           [:span.roomtab
                            (if (= room (om/get-state owner :current-room))
                              {:class "current"}
                              {:on-click #(om/set-state! owner :current-room room)})
                            roomname " (" (count-games room) ")"])]
            [:div.rooms
             (room-tab "competitive" "Competitive")
             (room-tab "casual" "Casual")])]
        (let [password-game (some #(when (= password-gameid (:gameid %)) %) games)]
         (game-list (assoc cursor :password-game password-game) owner))]

        [:div.game-panel
         (if (:editing state)
           [:div
            [:div.button-bar
             [:button {:type "button" :on-click #(create-game cursor owner)} "Create"]
             [:button {:type "button" :on-click #(om/set-state! owner :editing false)} "Cancel"]]

            (when-let [flash-message (:flash-message state)]
               [:p.flash-message flash-message])

            [:section
             [:h3 "Title"]
             [:input.game-title {:on-change #(om/set-state! owner :title (.. % -target -value))
                                 :value (:title state) :placeholder "Title" :maxLength "100"}]]

            [:section
             [:h3 "Side"]
             (for [option ["Corp" "Runner"]]
               [:p
                [:label [:input {:type "radio"
                                 :name "side"
                                 :value option
                                 :on-change #(om/set-state! owner :side (.. % -target -value))
                                 :checked (= (om/get-state owner :side) option)}] option]])]

            [:section
             [:h3 "Options"]
             [:p
              [:label
               [:input {:type "checkbox" :checked (om/get-state owner :allowspectator)
                        :on-change #(om/set-state! owner :allowspectator (.. % -target -checked))}]
               "Allow spectators"]]
             [:p
              [:label
               [:input {:type "checkbox" :checked (om/get-state owner :spectatorhands)
                        :on-change #(om/set-state! owner :spectatorhands (.. % -target -checked))
                        :disabled (not (om/get-state owner :allowspectator))}]
               "Make players' hands visible to spectators"]]
             [:p {:style {:display (if (om/get-state owner :spectatorhands) "block" "none")}}
              "This will reveal both players' hands to ALL spectators of your game. We recommend "
              "using a password to prevent strangers from spoiling the game."]
             [:p
              [:label
               [:input {:type "checkbox" :checked (om/get-state owner :private)
                        :on-change #(om/set-state! owner :protected (.. % -target -checked))}]
               "Password protected"]]
             (when (:protected state)
               [:p
                [:input.game-title {:on-change #(om/set-state! owner :password (.. % -target -value))
                                    :type "password"
                                    :value (:password state) :placeholder "Password" :maxLength "30"}]])]]

           (when-let [game (some #(when (= gameid (:gameid %)) %) games)]
             (let [players (:players game)]
               [:div
                [:div.button-bar
                 (when (= (-> players first :user) user)
                   (if (every? :deck players)
                     [:button {:on-click #(send {:action "start" :gameid (:gameid @app-state)})} "Start"]
                     [:button {:class "disabled"} "Start"]))
                 [:button {:on-click #(leave-lobby cursor owner)} "Leave"]
                 (when (= (-> players first :user) user)
                   [:button {:on-click #(send {:action "swap" :gameid gameid})} "Swap sides"])]
                [:div.content
                 [:h2 (:title game)]
                 (when-not (every? :deck players)
                   [:div.flash-message "Waiting players deck selection"])
                 [:h3 "Players"]
                 [:div.players
                  (for [player (:players game)]
                    [:div
                     (om/build player-view {:player player})
                     (when-let [deck (:deck player)]
                       [:span {:class (deck-status-label sets deck)}
                        [:span.label
                         (if (= (:user player) user)
                           (:name deck)
                           "Deck selected")]])
                     (when-let [deck (:deck player)]
                       [:div.float-right (deck-status-span sets deck true)])
                     (when (= (:user player) user)
                       [:span.fake-link.deck-load
                        {:data-target "#deck-select" :data-toggle "modal"} "Select deck"])])]
                 (when (:allowspectator game)
                   [:div.spectators
                    (let [c (count (:spectators game))]
                      [:h3 (str c " Spectator" (when (> c 1) "s"))])
                    (for [spectator (:spectators game)]
                      (om/build player-view {:player spectator}))])]
                (om/build chat-view messages {:state state})])))]
        (om/build deckselect-modal cursor)]))))

(om/root game-lobby app-state {:target (. js/document (getElementById "gamelobby"))})
