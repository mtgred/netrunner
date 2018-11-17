(ns nr.gamelobby
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :refer [join]]
            [jinteki.utils :refer [str->int]]
            [nr.ajax :refer [GET]]
            [nr.appstate :refer [app-state]]
            [nr.auth :refer [authenticated avatar] :as auth]
            [nr.cardbrowser :refer [image-url non-game-toast] :as cb]
            [nr.deckbuilder :refer [deck-status-span format-deck-status-span num->percent slug->format]]
            [nr.gameboard :refer [game-state launch-game parse-state toast]]
            [nr.stats :refer [notnum->zero]]
            [nr.ws :as ws]
            [reagent.core :as r]
            [reagent-modals.modals :as reagent-modals]
            [taoensso.sente :as sente]))

(def socket-channel (chan))

(def lobby-dom (atom {}))

(defn- play-sound
  [element-id]
  (when (get-in @app-state [:options :lobby-sounds])
    (when-let [element (.getElementById js/document element-id)]
      (.play element))))

(defn resume-sound
  "Chrome doesn't allow audio until audio context is resumed (or created) after a user interaction."
  []
  (when-let [audio-context (aget js/Howler "ctx")]
    (.resume audio-context)))

(defn sort-games-list [games]
  (sort-by #(vec (map (assoc % :started (not (:started %))
                               :mygame (if-let [og (:originalPlayers %)]
                                         (some (fn [p] (= p (get-in @app-state [:user :_id])))
                                               (map (fn [g] (get-in g [:user :_id])) og))
                                         false))
                      [:started :date :mygame]))
           > games))

(ws/register-ws-handler!
  :games/list
  #(swap! app-state assoc :games (sort-games-list %)))

(ws/register-ws-handler!
  :games/diff
  (fn [{:keys [diff notification] :as msg}]
    (swap! app-state update-in [:games]
           (fn [games]
             (let [gamemap (into {} (map #(assoc {} (:gameid %) %) games))
                   create (merge gamemap (:create diff))
                   update (merge create (:update diff))
                   delete (apply dissoc update (keys (:delete diff)))]
               (sort-games-list (vals delete)))))
    (when (and notification (not (:gameid @app-state)))
      (play-sound notification))))

(ws/register-ws-handler!
  :lobby/select
  #(do (swap! app-state assoc :gameid (:gameid %))
       (when (:started %) (launch-game (parse-state (:state %))))))

(ws/register-ws-handler!
  :lobby/message
  (fn [{:keys [text notification] :as msg}]
    (swap! app-state update :messages conj msg)
    (when notification
      (play-sound notification))))

(ws/register-ws-handler!
  :lobby/timeout
  (fn [{:keys [gameid] :as msg}]
    (when (= gameid (:gameid @app-state))
      (non-game-toast "Game lobby closed due to inactivity" "error" {:time-out 0 :close-button true})
      (swap! app-state assoc :gameid nil))))

;TODO this can be deleted? legacy
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
                          (play-sound sound))))
          "say" (do
                  (swap! app-state update-in [:messages]
                           #(conj % {:user (:user msg) :text (:text msg)}))
                    (when-let [sound (:notification msg)]
                      (play-sound sound)))
          "start" (launch-game (:state msg))
          "Invalid password" (js/console.log "pwd" (:gameid msg))
          "lobby-notification" (toast (:text msg) (:severity msg) nil)
          nil))))

(defn send
  ([msg] (send msg nil))
  ([msg fn]
   (try (js/ga "send" "event" "lobby" msg) (catch js/Error e))))

(defn new-game [s]
  (authenticated
    (fn [user]
      (swap! s assoc :title (str (:username user) "'s game"))
      (swap! s assoc :side "Corp")
      (swap! s assoc :format "standard")
      (swap! s assoc :editing true)
      (swap! s assoc :flash-message "")
      (swap! s assoc :protected false)
      (swap! s assoc :password "")
      (swap! s assoc :allowspectator true)
      (swap! s assoc :spectatorhands false)
      (-> ".game-title" js/$ .select))))

(defn create-game [s]
  (authenticated
    (fn [user]
      (if (empty? (:title @s))
        (swap! s assoc :flash-message "Please fill a game title.")
        (if (and (:protected @s)
                 (empty? (:password @s)))
          (swap! s assoc :flash-message "Please fill a password")
          (do (swap! s assoc :editing false)
              (swap! app-state assoc :messages [])
              (ws/ws-send! [:lobby/create
                            {:title          (:title @s)
                             :password       (:password @s)
                             :allowspectator (:allowspectator @s)
                             :spectatorhands (:spectatorhands @s)
                             :side           (:side @s)
                             :format         (:format @s)
                             :room           (:current-room @s)
                             :options        (:options @app-state)}])))))))

(defn join-game [gameid s action password]
  (authenticated
    (fn [user]
      (swap! s assoc :editing false)
      (swap! app-state assoc :messages [])
      (ws/ws-send! [(case action
                      "join" :lobby/join
                      "watch" :lobby/watch
                      "rejoin" :netrunner/rejoin)
                    {:gameid gameid :password password :options (:options @app-state)}]
                   8000
                   #(if (sente/cb-success? %)
                      (case %
                        403 (swap! s assoc :error-msg "Invalid password")
                        404 (swap! s assoc :error-msg "Not allowed")
                        200 (swap! s assoc :prompt false))
                      (swap! s assoc :error-msg "Connection aborted"))))))

(defn leave-lobby [s]
  (ws/ws-send! [:lobby/leave])
  (swap! app-state assoc :gameid nil)
  (swap! app-state assoc :message [])
  (swap! s assoc :prompt false)
  (swap! app-state dissoc :password-gameid))

(defn leave-game []
  (ws/ws-send! [:netrunner/leave {:gameid-str (:gameid @game-state)}])
  (reset! game-state nil)
  (swap! app-state dissoc :gameid :side :password-gameid :win-shown :start-shown)
  (.removeItem js/localStorage "gameid")
  (set! (.-onbeforeunload js/window) nil)
  (-> "#gameboard" js/$ .fadeOut)
  (-> "#gamelobby" js/$ .fadeIn))

(defn deckselect-modal [user {:keys [gameid games decks format]}]
  [:div
    [:h3 "Select your deck"]
    [:div.deck-collection
     (let [players (:players (some #(when (= (:gameid %) @gameid) %) @games))
           side (:side (some #(when (= (-> % :user :_id) (:_id @user)) %) players))]
       [:div
        (doall
          (for [deck (sort-by :date > (filter #(= (get-in % [:identity :side]) side) @decks))]
            ^{:key (:_id deck)}
            [:div.deckline {:on-click #(do (ws/ws-send! [:lobby/deck (:_id deck)])
                                           (reagent-modals/close-modal!))}
             [:img {:src (image-url (:identity deck))
                    :alt (get-in deck [:identity :title] "")}]
             [:div.float-right [deck-status-span deck]]
             [:h4 (:name deck)]
             [:div.float-right (-> (:date deck) js/Date. js/moment (.format "MMM Do YYYY"))]
             [:p (get-in deck [:identity :title])]]))])]])

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

(defn user-status-span
  "Returns a [:span] showing players game completion rate"
  [player]
  (r/with-let [started (get-in player [:user :stats :games-started])
               completed (get-in player [:user :stats :games-completed])
               completion-rate (str (notnum->zero (num->percent completed started)) "%")
               completion-rate (if (< started 10) "Too little data" completion-rate)]
    [:span.user-status (get-in player [:user :username])
     [:div.status-tooltip.blue-shade
      [:div "Game Completion Rate: " completion-rate]]]))

(defn player-view
  ([player] (player-view player nil))
  ([player game]
   [:span.player
    [avatar (:user player) {:opts {:size 22}}]
    [user-status-span player]
    (let [side (:side player)
          faction (:faction (:identity (:deck player)))
          identity (:title (:identity (:deck player)))
          specs (:allowspectator game)]
      (cond
        (and (some? faction) (not= "Neutral" faction) specs) (faction-icon faction identity)
        side [:span.side (str "(" side ")")]))]))

(defn send-msg [s]
  (let [input (:msg-input @lobby-dom)
        text (:msg @s)]
    (when-not (empty? text)
      (ws/ws-send! [:lobby/say {:gameid (:gameid @app-state) :msg text}])
      (let [msg-list (:message-list @lobby-dom)]
        (set! (.-scrollTop msg-list) (+ (.-scrollHeight msg-list) 500)))
      (swap! s assoc :msg "")
      (.focus input))))

(defn chat-view []
  (let [s (r/atom {})
        messages (r/cursor app-state [:messages])]
    (r/create-class
      {:display-name "chat-view"

       :component-did-update
       (fn []
         (let [msg-list (:message-list @lobby-dom)
               height (.-scrollHeight msg-list)]
           (when (< (- height (.-scrollTop msg-list) (.height (js/$ ".lobby .chat-box"))) 500)
             (set! (.-scrollTop msg-list) (.-scrollHeight msg-list)))))

       :reagent-render
       (fn []
         [:div.chat-box
          [:h3 "Chat"]
          [:div.message-list {:ref #(swap! lobby-dom assoc :message-list %)}
           (map-indexed (fn [i msg]
                          (if (= (:user msg) "__system__")
                            ^{:key i}
                            [:div.system (:text msg)]
                            ^{:key i}
                            [:div.message
                             [avatar (:user msg) {:opts {:size 38}}]
                             [:div.content
                              [:div.username (get-in msg [:user :username])]
                              [:div (:text msg)]]]) )
                          @messages)]
          [:div
           [:form.msg-box {:on-submit #(do (.preventDefault %)
                                           (send-msg s))}
            [:input {:ref #(swap! lobby-dom assoc :msg-input %)
                     :placeholder "Say something" :accessKey "l" :value (:msg @s)
                     :on-change #(swap! s assoc :msg (-> % .-target .-value))}]
            [:button "Send"]]]])})))

(defn game-view [{:keys [title format password started players gameid current-game password-game original-players editing] :as game}]
  (r/with-let [s (r/atom {})
                join (fn [action]
                       (let [password (:password password-game password)]
                         (if (empty? password)
                           (join-game (if password-game (:gameid password-game) gameid) s action nil)
                           (if-let [input-password (:password @s)]
                             (join-game (if password-game (:gameid password-game) gameid) s action input-password)
                             (do (swap! app-state assoc :password-gameid gameid) (swap! s assoc :prompt action))))))]
      [:div.gameline {:class (when (= current-game gameid) "active")}
       (when (and (:allowspectator game) (not (or password-game current-game editing)))
         [:button {:on-click #(do (join "watch") (resume-sound))} "Watch" editing])
       (when-not (or current-game editing (= (count players) 2) started password-game)
         [:button {:on-click #(do (join "join") (resume-sound))} "Join"])
       (when (and (not current-game) (not editing) started (not password-game)
                  (some #(= % (get-in @app-state [:user :_id]))
                        (map #(get-in % [:user :_id]) original-players)))
         [:button {:on-click #(do (join "rejoin") (resume-sound))} "Rejoin"])
       (let [c (count (:spectators game))]
         [:h4 (str (when-not (empty? (:password game))
                     "[PRIVATE] ")
                   (:title game)
                   (when (pos? c)
                     (str  " (" c " spectator" (when (> c 1) "s") ")")))])

       [:div {:class "game-format"}
        [:span.format-label "Format:  "]
        [:span.format-type (slug->format format "Unknown")]]

       [:div (doall
               (for [player (:players game)]
                 ^{:key (-> player :user :_id)}
                 [player-view player game]))]

       (when-let [prompt (:prompt @s)]
         [:div.password-prompt
          [:h3 (str "Password for " (if password-game (:title password-game) title))]
          [:p
           [:input.game-title {:on-change #(swap! s assoc :password (.. % -target -value))
                               :type "password"
                               :value (:password @s) :placeholder "Password" :maxLength "30"}]]
          [:p
           [:button {:type "button" :on-click #(join prompt)}
            prompt]
           [:span.fake-link {:on-click #(do
                                          (swap! app-state dissoc :password-gameid)
                                          (swap! s assoc :prompt false)
                                          (swap! s assoc :error-msg nil)
                                          (swap! s assoc :password nil))}
            "Cancel"]]
          (when-let [error-msg (:error-msg @s)]
            [:p.flash-message error-msg])])]))

(defn- blocked-from-game
  "Remove games for which the user is blocked by one of the players"
  [username game]
  (let [players (get game :players [])
        blocked-users (flatten (map #(get-in % [:user :options :blocked-users] []) players))]
    (= -1 (.indexOf blocked-users username))))

(defn- blocking-from-game
  "Remove games with players we are blocking"
  [blocked-users game]
  (let [players (get game :players [])
        player-names (map #(get-in % [:user :username] "") players)
        intersect (clojure.set/intersection (set blocked-users) (set player-names))]
    (empty? intersect)))

(defn filter-blocked-games
  [user games]
  (let [blocked-games (filter #(blocked-from-game (:username user) %) games)
        blocked-users (get-in user [:options :blocked-users] [])]
    (filter #(blocking-from-game blocked-users %) blocked-games)))

(defn game-list [user {:keys [current-room games gameid password-game editing]}]
   (let [roomgames (r/track (fn [] (filter #(= (:room %) current-room) @games)))
         filtered-games (r/track #(filter-blocked-games @user @roomgames))]
        [:div.game-list
         (if (empty? @filtered-games)
           [:h4 "No games"]
           (doall
             (for [game @filtered-games]
               ^{:key (:gameid game)}
               [game-view (assoc game :current-game @gameid :password-game password-game :editing editing)])))]))

(def open-games-symbol "○")
(def closed-games-symbol "●")

(defn- room-tab
  "Creates the room tab for the specified room"
  [user s games room room-name]
  (r/with-let [room-games (r/track (fn [] (filter #(= room (:room %)) @games)))
               filtered-games (r/track (fn [] (filter-blocked-games @user @room-games)))
               closed-games (r/track (fn [] (count (filter #(:started %) @filtered-games))))
               open-games (r/track (fn [] (- (count @filtered-games) @closed-games)))]
    [:span.roomtab
     (if (= room (:current-room @s))
       {:class "current"}
       {:on-click #(swap! s assoc :current-room room)})
     room-name " (" @open-games open-games-symbol " "
     @closed-games closed-games-symbol ")"]))

(defn- first-user?
  "Is this user the first user in the game?"
  [players user]
  (= (-> players first :user :_id) (:_id user)))

(defn game-lobby []
  (r/with-let [s (r/atom {:current-room "casual"})
               decks (r/cursor app-state [:decks])
               games (r/cursor app-state [:games])
               gameid (r/cursor app-state [:gameid])
               password-gameid (r/cursor app-state [:password-gameid])
               sets (r/cursor app-state [:sets])
               user (r/cursor app-state [:user])]
    [:div
     [:div.lobby-bg]
     [:div.container
      [:div.lobby.panel.blue-shade
       [:div.games
        [:div.button-bar
         (if (or @gameid (:editing @s))
           [:button.float-left {:class "disabled"} "New game"]
           [:button.float-left {:on-click #(do (new-game s)
                                               (resume-sound))} "New game"])
         [:div.rooms
          [room-tab user s games "competitive" "Competitive"]
          [room-tab user s games "casual" "Casual"]]]
        (let [password-game (some #(when (= @password-gameid (:gameid %)) %) @games)]
          [game-list user {:password-game password-game :editing (:editing @s)
                           :games games :gameid gameid :current-room (:current-room @s)}])]

       [:div.game-panel
        ; this is the right hand panel in the lobby
        (if (:editing @s)
          [:div
           [:div.button-bar
            [:button {:type "button" :on-click #(create-game s)} "Create"]
            [:button {:type "button" :on-click #(swap! s assoc :editing false)} "Cancel"]]

           (when-let [flash-message (:flash-message @s)]
             [:p.flash-message flash-message])

           [:section
            [:h3 "Title"]
            [:input.game-title {:on-change #(swap! s assoc :title (.. % -target -value))
                                :value (:title @s) :placeholder "Title" :maxLength "100"}]]

           [:section
            [:h3 "Side"]
            (doall
              (for [option ["Corp" "Runner"]]
                ^{:key option}
                [:p
                 [:label [:input {:type "radio"
                                  :name "side"
                                  :value option
                                  :on-change #(swap! s assoc :side (.. % -target -value))
                                  :checked (= (:side @s) option)}] option]]))]

           [:section
            [:h3 "Format"]
            [:select.format {:value (:format @s "standard")
                             :on-change #(swap! s assoc :format (.. % -target -value))}
             (for [[k v] slug->format]
               ^{:key k}
               [:option {:value k} v])]]

           [:section
            [:h3 "Options"]
            [:p
             [:label
              [:input {:type "checkbox" :checked (:allowspectator @s)
                       :on-change #(swap! s assoc :allowspectator (.. % -target -checked))}]
              "Allow spectators"]]
            [:p
             [:label
              [:input {:type "checkbox" :checked (:spectatorhands @s)
                       :on-change #(swap! s assoc :spectatorhands (.. % -target -checked))
                       :disabled (not (:allowspectator @s))}]
              "Make players' hidden information visible to spectators"]]
            [:div {:style {:display (if (:spectatorhands @s) "block" "none")}}
             [:p "This will reveal both players' hidden information to ALL spectators of your game, "
              "including hand and face-down cards."]
             [:p "We recommend using a password to prevent strangers from spoiling the game."]]
            [:p
             [:label
              [:input {:type "checkbox" :checked (:private @s)
                       :on-change #(let [checked (.. % -target -checked)]
                                     (swap! s assoc :protected checked)
                                     (when (not checked) (swap! s assoc :password "")))}]
              "Password protected"]]
            (when (:protected @s)
              [:p
               [:input.game-title {:on-change #(swap! s assoc :password (.. % -target -value))
                                   :type "password"
                                   :value (:password @s) :placeholder "Password" :maxLength "30"}]])]]
          (when-let [game (some #(when (= @gameid (:gameid %)) %) @games)]
            (let [players (:players game)]
              [:div
               [:div.button-bar
                (when (first-user? players @user)
                  (if (every? :deck players)
                    [:button {:on-click #(ws/ws-send! [:netrunner/start @gameid])} "Start"]
                    [:button {:class "disabled"} "Start"]))
                [:button {:on-click #(leave-lobby s)} "Leave"]
                (when (first-user? players @user)
                  [:button {:on-click #(ws/ws-send! [:lobby/swap @gameid])} "Swap sides"])]
               [:div.content
                [:h2 (:title game)]
                (when-not (every? :deck players)
                  [:div.flash-message "Waiting players deck selection"])
                [:h3 "Players"]
                [:div.players
                 (doall
                   (for [player (:players game)
                         :let [player-id (get-in player [:user :_id])
                               this-player (= player-id (:_id @user))]]
                     ^{:key player-id}
                     [:div
                      [player-view player game]
                      (when-let [{:keys [name status]} (:deck player)]
                        [:span {:class (:status status)}
                         [:span.label
                          (if this-player
                            name
                            "Deck selected")]])
                      (when-let [deck (:deck player)]
                        [:div.float-right [format-deck-status-span (:status deck) true false]])
                      (when this-player
                        [:span.fake-link.deck-load
                         {:on-click #(reagent-modals/modal!
                                       [deckselect-modal user {:games games :gameid gameid
                                                               :sets sets :decks decks
                                                               :format (:format @s "standard")}])}
                         "Select Deck"])
                      ]))]
                (when (:allowspectator game)
                  [:div.spectators
                   (let [c (count (:spectators game))]
                     [:h3 (str c " Spectator" (when (not= c 1) "s"))])
                   (for [spectator (:spectators game)
                         :let [_id (get-in spectator [:user :_id])]]
                     ^{:key _id}
                     [player-view spectator])])]
               [chat-view]])))]
       [reagent-modals/modal-window]]]]))
