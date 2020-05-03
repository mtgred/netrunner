(ns nr.gamelobby
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :refer [join]]
            [jinteki.validator :refer [trusted-deck-status]]
            [jinteki.utils :refer [str->int]]
            [nr.appstate :refer [app-state]]
            [nr.auth :refer [authenticated] :as auth]
            [nr.avatar :refer [avatar]]
            [nr.cardbrowser :refer [image-url non-game-toast] :as cb]
            [nr.deckbuilder :refer [num->percent]]
            [nr.deck-status :refer [deck-format-status-span]]
            [nr.gameboard :refer [game-state launch-game parse-state toast]]
            [nr.game-row :refer [game-row]]
            [nr.player-view :refer [player-view]]
            [nr.sounds :refer [play-sound resume-sound]]
            [nr.utils :refer [slug->format cond-button]]
            [nr.ws :as ws]
            [reagent.core :as r]
            [reagent-modals.modals :as reagent-modals]
            [taoensso.sente :as sente]))

(def lobby-dom (atom {}))

(defn sort-games-list [games]
  (sort-by (fn [game]
             [(when-let [players (:players game)]
                (not (some (fn [p]
                             (= (get-in p [:user :_id])
                                (get-in @app-state [:user :_id])))
                           players)))
              (:started game)
              (:date game)])
           games))

(ws/register-ws-handler!
  :games/list
  #(swap! app-state assoc :games (sort-games-list %)))

(ws/register-ws-handler!
  :games/diff
  (fn [{:keys [diff notification] :as msg}]
    (swap! app-state update-in [:games]
           (fn [games]
             (let [gamemap (into {} (map #(assoc {} (:gameid %) %) games))
                   create-diff (merge gamemap (:create diff))
                   update-diff (merge create-diff (:update diff))
                   delete-diff (apply dissoc update-diff (keys (:delete diff)))]
               (sort-games-list (vals delete-diff)))))
    (when (and notification (not (:gameid @app-state)))
      (play-sound notification))))

(ws/register-ws-handler!
  :lobby/select
  (fn [{:keys [gameid started state]}]
    (swap! app-state assoc :gameid gameid)
    (when started
      (launch-game (parse-state state)))))

(ws/register-ws-handler!
  :lobby/notification
  (fn [notification]
    (play-sound notification)))

(ws/register-ws-handler!
  :lobby/timeout
  (fn [{:keys [gameid] :as msg}]
    (when (= gameid (:gameid @app-state))
      (non-game-toast "Game lobby closed due to inactivity" "error" {:time-out 0 :close-button true})
      (swap! app-state assoc :gameid nil))))

(defn send
  ([msg] (send msg nil))
  ([msg fn]
   (try (js/ga "send" "event" "lobby" msg) (catch js/Error e))))

(defn new-game [s]
  (authenticated
    (fn [user]
      (swap! s assoc
             :title (str (:username user) "'s game")
             :side "Corp"
             :format "standard"
             :editing true
             :flash-message ""
             :protected false
             :password ""
             :allow-spectator true
             :spectatorhands false)
      (-> ".game-title" js/$ .select))))

(defn create-game [s]
  (authenticated
    (fn [user]
      (cond
        (empty? (:title @s))
        (swap! s assoc :flash-message "Please fill a game title.")

        (and (:protected @s)
             (empty? (:password @s)))
        (swap! s assoc :flash-message "Please fill a password")

        :else
        (do (swap! s assoc :editing false)
            (ws/ws-send! [:lobby/create
                          (assoc
                          (select-keys @s [:title :password :allow-spectator
                                           :spectatorhands :side :format :room])
                          :options (:options @app-state))]))))))

(defn leave-lobby [s]
  (ws/ws-send! [:lobby/leave])
  (swap! app-state assoc :gameid nil :message [] :password-gameid nil)
  (swap! s assoc :prompt false))

(defn leave-game []
  (ws/ws-send! [:netrunner/leave {:gameid-str (:gameid @game-state)}])
  (reset! game-state nil)
  (swap! app-state dissoc :gameid :side :password-gameid :win-shown :start-shown)
  (set! (.-cursor (.-style (.-body js/document))) "default")
  (.removeItem js/localStorage "gameid")
  (set! (.-onbeforeunload js/window) nil)
  (-> "#gameboard" js/$ .fadeOut)
  (-> "#gamelobby" js/$ .fadeIn))

(defn deckselect-modal [user {:keys [gameid games decks format]}]
  [:div
    [:h3 "Select your deck"]
    [:div.deck-collection
     (let [players (:players (some #(when (= (:gameid %) @gameid) %) @games))
           side (:side (some #(when (= (-> % :user :_id) (:_id @user)) %) players))
           same-side? (fn [deck] (= side (get-in deck [:identity :side])))
           legal? (fn [deck] (get-in deck
                                     [:status (keyword format) :legal]
                                     (get-in (trusted-deck-status (assoc deck :format format))
                                         [(keyword format) :legal]
                                         false)))]
       [:div
        (doall
          (for [deck (->> @decks
                          (filter same-side?)
                          (sort-by (juxt legal? :date) >))]
            ^{:key (:_id deck)}
            [:div.deckline {:on-click #(do (ws/ws-send! [:lobby/deck (:_id deck)])
                                           (reagent-modals/close-modal!))}
             [:img {:src (image-url (:identity deck))
                    :alt (get-in deck [:identity :title] "")}]
             [:div.float-right [deck-format-status-span deck format true]]
             [:h4 (:name deck)]
             [:div.float-right (-> (:date deck) js/Date. js/moment (.format "MMM Do YYYY"))]
             [:p (get-in deck [:identity :title])]]))])]])

(defn send-msg [s]
  (let [text (:msg @s)]
    (when-not (empty? text)
      (ws/ws-send! [:lobby/say {:gameid (:gameid @app-state)
                                :msg text}])
      (let [msg-list (:message-list @lobby-dom)]
        (set! (.-scrollTop msg-list) (+ (.-scrollHeight msg-list) 500)))
      (swap! s assoc :msg ""))))

(defn chat-view []
  (let [s (r/atom {})]
    (r/create-class
      {:display-name "chat-view"

       :component-did-update
       (fn []
         (let [msg-list (:message-list @lobby-dom)
               height (.-scrollHeight msg-list)]
           (when (< (- height (.-scrollTop msg-list) (.height (js/$ ".lobby .chat-box"))) 500)
             (set! (.-scrollTop msg-list) (.-scrollHeight msg-list)))))

       :reagent-render
       (fn [game]
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
                          (:messages game))]
          [:div
           [:form.msg-box {:on-submit #(do (.preventDefault %)
                                           (send-msg s))}
            [:input {:placeholder "Say something"
                     :type "text"
                     :value (:msg @s)
                     :on-change #(swap! s assoc :msg (-> % .-target .-value))}]
            [:button "Send"]]]])})))

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
     (if (= room (:room @s))
       {:class "current"}
       {:on-click #(swap! s assoc :room room)})
     room-name " (" @open-games open-games-symbol " "
     @closed-games closed-games-symbol ")"]))

(defn- first-user?
  "Is this user the first user in the game?"
  [players user]
  (= (-> players first :user :_id) (:_id user)))

(defn game-list [user {:keys [room games gameid password-game editing]}]
  (let [roomgames (r/track (fn [] (filter #(= (:room %) room) @games)))
        filtered-games (r/track #(filter-blocked-games @user @roomgames))]
    [:div.game-list
     (if (empty? @filtered-games)
       [:h4 "No games"]
       (doall
         (for [game @filtered-games]
           ^{:key (:gameid game)}
           [game-row (assoc game :current-game @gameid :password-game password-game :editing editing)])))]))

(defn games-list-panel [s games gameid password-gameid user]
  [:div.games
   [:div.button-bar
    [cond-button "New game"
     (and (not (or @gameid (:editing @s)))
          (->> @games
               (mapcat :players)
               (filter #(= (-> % :user :_id) (:_id @user)))
               empty?))
     #(do (new-game s)
          (resume-sound))]
    [:div.rooms
     [room-tab user s games "competitive" "Competitive"]
     [room-tab user s games "casual" "Casual"]]]
   (let [password-game (some #(when (= @password-gameid (:gameid %)) %) @games)]
     [game-list user {:password-game password-game
                      :editing (:editing @s)
                      :games games
                      :gameid gameid
                      :room (:room @s)}])])

(defn create-new-game
  [s]
  (when (:editing @s)
    [:div
     [:div.button-bar
      [:button {:type "button"
                :on-click #(create-game s)} "Create"]
      [:button {:type "button"
                :on-click #(swap! s assoc :editing false)} "Cancel"]]
     (when-let [flash-message (:flash-message @s)]
       [:p.flash-message flash-message])
     [:section
      [:h3 "Title"]
      [:input.game-title {:on-change #(swap! s assoc :title (.. % -target -value))
                          :value (:title @s)
                          :placeholder "Title"
                          :maxLength "100"}]]
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
                            :checked (= (:side @s) option)}]
            option]]))]

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
        [:input {:type "checkbox" :checked (:allow-spectator @s)
                 :on-change #(swap! s assoc :allow-spectator (.. % -target -checked))}]
        "Allow spectators"]]
      [:p
       [:label
        [:input {:type "checkbox" :checked (:spectatorhands @s)
                 :on-change #(swap! s assoc :spectatorhands (.. % -target -checked))
                 :disabled (not (:allow-spectator @s))}]
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
                             :value (:password @s)
                             :placeholder "Password"
                             :maxLength "30"}]])]]))

(defn pending-game
  [s decks games gameid password-gameid sets user]
  (let [game (some #(when (= @gameid (:gameid %)) %) @games)
        players (:players game)]
    (when game
      [:div
       [:div.button-bar
        (when (first-user? players @user)
          [cond-button
           "Start"
           (every? :deck players)
           #(ws/ws-send! [:netrunner/start @gameid])])
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
                [:div.float-right [deck-format-status-span deck (:format game "standard") true]])
              (when this-player
                [:span.fake-link.deck-load
                 {:on-click #(reagent-modals/modal!
                               [deckselect-modal user {:games games :gameid gameid
                                                       :sets sets :decks decks
                                                       :format (:format game "standard")}])}
                 "Select Deck"])]))]
        (when (:allow-spectator game)
          [:div.spectators
           (let [c (count (:spectators game))]
             [:h3 (str c " Spectator" (when (not= c 1) "s"))])
           (for [spectator (:spectators game)
                 :let [_id (get-in spectator [:user :_id])]]
             ^{:key _id}
             [player-view spectator])])]
       [chat-view game]])))

(defn right-panel
  [decks s games gameid password-gameid sets user]
  [:div.game-panel
   [create-new-game s]
   [pending-game s decks games gameid password-gameid sets user]])

(defn game-lobby []
  (r/with-let [s (r/atom {:room "casual"})
               decks (r/cursor app-state [:decks])
               games (r/cursor app-state [:games])
               gameid (r/cursor app-state [:gameid])
               password-gameid (r/cursor app-state [:password-gameid])
               sets (r/cursor app-state [:sets])
               user (r/cursor app-state [:user])
               active (r/cursor app-state [:active-page])]
    (when (= "/play" (first @active))
      [:div.container
       [:div.lobby-bg]
       [:div.lobby.panel.blue-shade
        [games-list-panel s games gameid password-gameid user]
        [right-panel decks s games gameid password-gameid sets user]
        [reagent-modals/modal-window]]])))
