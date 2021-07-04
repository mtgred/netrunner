(ns nr.gamelobby
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :refer [join]]
            [jinteki.validator :refer [trusted-deck-status legal-deck?]]
            [jinteki.utils :refer [str->int superuser?]]
            [nr.appstate :refer [app-state]]
            [nr.ajax :refer [GET]]
            [nr.auth :refer [authenticated] :as auth]
            [nr.avatar :refer [avatar]]
            [nr.cardbrowser :refer [image-url] :as cb]
            [nr.deck-status :refer [deck-format-status-span]]
            [nr.deckbuilder :refer [deck-name]]
            [nr.gameboard.actions :refer [launch-game toast]]
            [nr.gameboard.state :refer [game-state parse-state]]
            [nr.game-row :refer [game-row]]
            [nr.history :refer [history]]
            [nr.player-view :refer [player-view]]
            [nr.sounds :refer [play-sound resume-sound]]
            [nr.translations :refer [tr tr-side tr-format]]
            [nr.utils :refer [slug->format cond-button non-game-toast num->percent]]
            [nr.ws :as ws]
            [reagent.core :as r]
            [differ.core :as differ]
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

(defn process-games-update
  [{:keys [diff notification]}]
  (swap! app-state update :games
          (fn [games]
            (let [gamemap (into {} (map #(assoc {} (:gameid %) %) games))
                  update-diff (reduce-kv
                                (fn [m k v]
                                  ; spectators is nil on the client but not the API, confusing differ which expects an empty set
                                  (assoc m k (merge {:spectators '()} (get m k {}) v)))
                                gamemap
                                (:update diff))
                  delete-diff (apply dissoc update-diff (:delete diff))]
              (sort-games-list (vals delete-diff)))))
  (when (and notification (not (:gameid @app-state)))
    (play-sound notification)))

(defmethod ws/-msg-handler :games/list [{data :?data}]
  (let [gamemap (into {} (map #(assoc {} (:gameid %) %) data))
        missing-gameids (->> (:games @app-state)
                             (remove #(get gamemap (:gameid %)))
                             (map :gameid))]
    (process-games-update {:diff {:update gamemap
                                  :delete missing-gameids}})))

(defmethod ws/-msg-handler :games/diff [{data :?data}]
  (process-games-update data))

(defmethod ws/-msg-handler :games/differ
  [{{:keys [diff]} :?data}]
  (swap! app-state update-in [:games]
         (fn [games]
           (let [gamemap (into {} (map #(assoc {} (:gameid %) %) games))
                 update-diff (reduce-kv
                               (fn [m k v]
                                 (assoc m k (reduce #(differ/patch %1 %2) (get m k {}) v)))
                               gamemap
                               (:update diff))]
             (sort-games-list (vals update-diff))))))

(defmethod ws/-msg-handler :lobby/select
  [{{:keys [gameid started state]} :?data}]
  (swap! app-state assoc :gameid gameid)
  (when started
    (launch-game (parse-state state))))

(defmethod ws/-msg-handler :lobby/notification [{data :?data}] (play-sound data))

(defmethod ws/-msg-handler :lobby/timeout
  [{{:keys [gameid]} :?data}]
  (when (= gameid (:gameid @app-state))
    (non-game-toast (tr [:lobby.closed-msg "Game lobby closed due to inactivity"]) "error" {:time-out 0 :close-button true})
    (swap! app-state assoc :gameid nil)))

(defn send
  ([msg] (send msg nil))
  ([msg fn]
   (try (js/ga "send" "event" "lobby" msg) (catch js/Error e))))

(defn new-game [s]
  (authenticated
    (fn [user]
      (let [fmt (:format (:create-game-deck @app-state) "standard")
            side (:side (:identity (:create-game-deck @app-state)) "Any Side")]
        (swap! s assoc
               :title (str (:username user) "'s game")
               :side side
               :format fmt
               :editing true
               :replay false
               :save-replay (if (= "casual" (:room @s)) false true)
               :api-access false
               :flash-message ""
               :protected false
               :password ""
               :timed false
               :timer nil
               :allow-spectator true
               :spectatorhands false
               :create-game-deck (:create-game-deck @app-state))
        (swap! app-state assoc :editing-game true)
        (swap! app-state dissoc :create-game-deck)
        (-> ".game-title" js/$ .select)))))

(defn replay-game [s]
  (authenticated
    (fn [user]
      (swap! s assoc
             :gameid "local-replay"
             :title (str (:username user) "'s game")
             :side "Corp"
             :format "standard"
             :editing true
             :replay true
             :flash-message ""
             :protected false
             :password ""
             :allow-spectator true
             :spectatorhands true))))

(defn start-shared-replay
  ([s gameid]
   (start-shared-replay s gameid nil))
  ([s gameid jump-to]
   (authenticated
     (fn [user]
       (swap! s assoc
              :title (str (:username user) "'s game")
              :side "Corp"
              :format "standard"
              :editing false
              :replay true
              :flash-message ""
              :protected false
              :password ""
              :allow-spectator true
              :spectatorhands true)
       (go (let [{:keys [status json]} (<! (GET (str "/profile/history/full/" gameid)))]
             (case status
               200
               (let [replay (js->clj json :keywordize-keys true)
                     history (:history replay)
                     init-state (first history)
                     init-state (assoc init-state :gameid gameid)
                     init-state (assoc-in init-state [:options :spectatorhands] true)
                     diffs (rest history)
                     init-state (assoc init-state :replay-diffs diffs)]
                 (ws/event-msg-handler
                   {:id :netrunner/start
                    :?data (.stringify js/JSON (clj->js
                                                 (if jump-to
                                                   (assoc init-state :replay-jump-to jump-to)
                                                   init-state)))}))
               404
               (non-game-toast (tr [:lobby.replay-link-error "Replay link invalid."])
                               "error" {:time-out 0 :close-button true}))))))))

(defn start-replay [s]
  (let [reader (js/FileReader.)
        file (:replay-file s)
        onload (fn [onload-ev] (let [replay (-> onload-ev .-target .-result)
                                     replay (js->clj (.parse js/JSON replay) :keywordize-keys true)
                                     history (:history replay)
                                     init-state (first history)
                                     init-state (assoc-in init-state [:options :spectatorhands] true)
                                     diffs (rest history)
                                     init-state (assoc init-state :replay-diffs diffs :gameid "local-replay")]
                                 (ws/event-msg-handler
                                   {:id :netrunner/start
                                    :?data (.stringify js/JSON (clj->js init-state))})))]
    (aset reader "onload" onload)
    (.readAsText reader file)))

(defn create-game [s]
  (authenticated
    (fn [user]
      (if (:replay @s)
        (cond
          (not (:replay-file @s))
          (swap! s assoc :flash-message (tr [:lobby.replay-invalid-file "Select a valid replay file."]))

          :else
          (do (swap! s assoc :editing false)
              (start-replay @s)))
        (cond
          (empty? (:title @s))
          (swap! s assoc :flash-message (tr [:lobby.title-error "Please fill a game title."]))

          (and (:protected @s)
               (empty? (:password @s)))
          (swap! s assoc :flash-message (tr [:lobby.password-error "Please fill a password."]))

          :else
          (do (swap! s assoc :editing false)
              (swap! app-state dissoc :editing-game)
              (ws/ws-send! [:lobby/create
                            (select-keys @s [:title :password :allow-spectator :save-replay
                                             :spectatorhands :side :format :room :timer :api-access])])))))))

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
    [:h3 (tr [:lobby.select-title "Select your deck"])]
    [:div.deck-collection.lobby-deck-selector
     (let [players (:players (some #(when (= (:gameid %) @gameid) %) @games))
           side (:side (some #(when (= (-> % :user :_id) (:_id @user)) %) players))
           same-side? (fn [deck] (= side (get-in deck [:identity :side])))]
       [:div
        (doall
          (for [deck (->> @decks
                          (filter #(and (same-side? %) (or (legal-deck? %) (= format "casual"))))
                          (sort-by (juxt legal-deck? :date) >))]
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
          [:h3 (tr [:lobby.chat "Chat"])]
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
                              [:div (:text msg)]]]))
                        (:messages game))]
          [:div
           [:form.msg-box {:on-submit #(do (.preventDefault %)
                                           (send-msg s))}
            [:input {:placeholder (tr [:chat.placeholder "Say something"])
                     :type "text"
                     :value (:msg @s)
                     :on-change #(swap! s assoc :msg (-> % .-target .-value))}]
            [:button (tr [:chat.send "Send"])]]]])})))

(defn- blocked-from-game
  "Remove games for which the user is blocked by one of the players"
  [user game]
  (or (superuser? user)
      (let [players (get game :players [])
            blocked-users (flatten (map #(get-in % [:user :options :blocked-users] []) players))]
        (= -1 (.indexOf blocked-users (:username user))))))

(defn- blocking-from-game
  "Remove games with players we are blocking"
  [blocked-users game]
  (let [players (get game :players [])
        player-names (map #(get-in % [:user :username] "") players)
        intersect (clojure.set/intersection (set blocked-users) (set player-names))]
    (empty? intersect)))

(defn filter-blocked-games
  [user games]
  (if (= "tournament" (:room (first games)))
    games
    (let [blocked-games (filter #(blocked-from-game user %) games)
          blocked-users (get-in user [:options :blocked-users] [])]
      (filter #(blocking-from-game blocked-users %) blocked-games))))

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
       [:h4 (tr [:lobby.no-games "No games"])]
       (doall
         (for [game @filtered-games]
           ^{:key (:gameid game)}
           [game-row (assoc game :current-game @gameid :password-game password-game :editing editing)])))]))

(defn games-list-panel [s games gameid password-gameid user]
  [:div.games
   (when-let [params (:replay-id @app-state)]
     (swap! app-state dissoc :replay-id)
     (let [bug-report? (re-find #"bug-report" params)
           id-match (re-find #"([0-9a-f\-]+)" params)
           n-match (re-find #"n=(\d+)" params)
           d-match (re-find #"d=(\d+)" params)
           b-match (re-find #"b=(\d+)" params)
           replay-id (nth id-match 1)
           n (when n-match (js/parseInt (nth n-match 1)))
           d (when d-match (js/parseInt (nth d-match 1)))
           b (when b-match (js/parseInt (nth b-match 1)))]
       (when replay-id
         (.replaceState (.-history js/window) {} "" "/play") ; remove query parameters from url
         (if bug-report?
           (start-shared-replay s replay-id {:bug (or b 0)})
           (if (and n d)
             (start-shared-replay s replay-id {:n n :d d})
             (start-shared-replay s replay-id nil)))
         (resume-sound)
         nil)))
   [:div.button-bar
    [:div.rooms
     [room-tab user s games "tournament" (tr [:lobby.tournament "Tournament"])]
     [room-tab user s games "competitive" (tr [:lobby.competitive "Competitive"])]
     [room-tab user s games "casual" (tr [:lobby.casual "Casual"])]]
    [:div.lobby-buttons
     [cond-button (tr [:lobby.new-game "New game"])
      (and (not (or @gameid
                    (:editing @s)
                    (= "tournament" (:room @s))))
           (->> @games
                (mapcat :players)
                (filter #(= (-> % :user :_id) (:_id @user)))
                empty?))
      #(do (new-game s)
           (resume-sound))]
     [:button.reload-button {:type "button"
                             :on-click #(ws/ws-send! [:lobby/list])} (tr [:lobby.reload "Reload list"])]
     [cond-button (tr [:lobby.load-replay "Load replay"])
      (and (not (or @gameid
                    (:editing @s)
                    (= "tournament" (:room @s))))
           (->> @games
                (mapcat :players)
                (filter #(= (-> % :user :_id) (:_id @user)))
                empty?))
      #(do (replay-game s)
           (resume-sound))]]]
   (let [password-game (some #(when (= @password-gameid (:gameid %)) %) @games)]
     [game-list user {:password-game password-game
                      :editing (:editing @s)
                      :games games
                      :gameid gameid
                      :room (:room @s)}])])

(defn create-new-game
  [s user]
  (when (:editing @s)
    (if (:replay @s)
      [:div
       [:div.button-bar
        [:button {:type "button"
                  :on-click #(create-game s)} (tr [:lobby.start-replay "Start replay"])]
        [:button {:type "button"
                  :on-click #(do
                               (swap! s assoc :editing false)
                               (swap! app-state dissoc :editing-game))}
         (tr [:lobby.cancel "Cancel"])]]
       (when-let [flash-message (:flash-message @s)]
         [:p.flash-message flash-message])
       [:div [:input {:field :file
                      :type :file
                      :on-change #(swap! s assoc :replay-file (aget (.. % -target -files) 0))}]]]
      [:div
       [:div.button-bar
        [:button {:type "button"
                  :on-click #(create-game s)} (tr [:lobby.create "Create"])]
        [:button {:type "button"
                  :on-click #(swap! s assoc :editing false)} (tr [:lobby.cancel "Cancel"])]]
       (when-let [flash-message (:flash-message @s)]
         [:p.flash-message flash-message])
       [:div.content
        [:section
         [:h3 (tr [:lobby.title "Title"])]
         [:input.game-title {:on-change #(swap! s assoc :title (.. % -target -value))
                             :value (:title @s)
                             :placeholder (tr [:lobby.title "Title"])
                             :maxLength "100"}]]
        [:section
         [:h3 (tr [:lobby.side "Side"])]
         (doall
           (for [option ["Any Side" "Corp" "Runner"]]
             ^{:key option}
             [:p
              [:label [:input {:type "radio"
                               :name "side"
                               :value option
                               :on-change #(swap! s assoc :side (.. % -target -value))
                               :checked (= (:side @s) option)}]
               (tr-side option)]]))]

        [:section
         [:h3 (tr [:lobby.format "Format"])]
         [:select.format {:value (:format @s "standard")
                          :on-change #(swap! s assoc :format (.. % -target -value))}
          (doall (for [[k v] slug->format]
                   ^{:key k}
                   [:option {:value k} (tr-format v)]))]]

        [:section
         [:h3 (tr [:lobby.options "Options"])]
         [:p
          [:label
           [:input {:type "checkbox" :checked (:allow-spectator @s)
                    :on-change #(swap! s assoc :allow-spectator (.. % -target -checked))}]
           (tr [:lobby.spectators "Allow spectators"])]]
         [:p
          [:label
           [:input {:type "checkbox" :checked (:spectatorhands @s)
                    :on-change #(swap! s assoc :spectatorhands (.. % -target -checked))
                    :disabled (not (:allow-spectator @s))}]
           (tr [:lobby.hidden "Make players' hidden information visible to spectators"])]]
         [:div.infobox.blue-shade {:style {:display (if (:spectatorhands @s) "block" "none")}}
          [:p "This will reveal both players' hidden information to ALL spectators of your game, "
           "including hand and face-down cards."]
          [:p "We recommend using a password to prevent strangers from spoiling the game."]]
         [:p
          [:label
           [:input {:type "checkbox" :checked (:private @s)
                    :on-change #(let [checked (.. % -target -checked)]
                                  (swap! s assoc :protected checked)
                                  (when (not checked) (swap! s assoc :password "")))}]
           (tr [:lobby.password-protected "Password protected"])]]
         (when (:protected @s)
           [:p
            [:input.game-title {:on-change #(swap! s assoc :password (.. % -target -value))
                                :type "password"
                                :value (:password @s)
                                :placeholder (tr [:lobby.password "Password"])
                                :maxLength "30"}]])

         (when-not (= "casual" (:room @s))
           [:p
            [:label
             [:input {:type "checkbox" :checked (:timed @s)
                      :on-change #(let [checked (.. % -target -checked)]
                                    (swap! s assoc :timed checked)
                                    (swap! s assoc :timer (if checked 35 nil)))}]
             (tr [:lobby.timed-game "Start with timer"])]])
         (when (:timed @s)
           [:p
            [:input.game-title {:on-change #(swap! s assoc :timer (-> % (.. -target -value) str->int))
                                :type "number"
                                :value (:timer @s)
                                :placeholder (tr [:lobby.timer-length "Timer length (minutes)"])}]])
         [:div.infobox.blue-shade {:style {:display (if (:timed @s) "block" "none")}}
          [:p "Timer is only for convenience: the game will not stop when timer runs out."]]

         [:p
          [:label
           [:input {:type "checkbox" :checked (:save-replay @s)
                    :on-change #(swap! s assoc :save-replay (.. % -target -checked))}]
           (str "🟢 " (tr [:lobby.save-replay "Save replay"]))]]
         [:div.infobox.blue-shade {:style {:display (if (:save-replay @s) "block" "none")}}
          [:p "This will save a replay file of this match with open information (e.g. open cards in hand)."
           " The file is available only after the game is finished."]
          [:p "Only your latest 15 unshared games will be kept, so make sure to either download or share the match afterwards."]
          [:p [:b "BETA Functionality:"] " Be aware that we might need to reset the saved replays, so " [:b "make sure to download games you want to keep."]
           " Also, please keep in mind that we might need to do future changes to the site that might make replays incompatible."]]

         (let [has-keys (:has-api-keys @user false)]
           [:p
            [:label
             [:input {:disabled (not has-keys)
                      :type "checkbox" :checked (:api-access @s)
                      :on-change #(swap! s assoc :api-access (.. % -target -checked))}]
             (tr [:lobby.api-access "Allow API access to game information"])
             (when (not has-keys)
               (str " " (tr [:lobby.api-requires-key "(Requires an API Key in Settings)"])))]])
           [:div.infobox.blue-shade {:style {:display (if (:api-access @s) "block" "none")}}
            [:p "This allows access to information about your game to 3rd party extensions. Requires an API Key to be created in Settings"]]]]])))

(defn pending-game
  [s decks games gameid password-gameid sets user]
  (let [game (some #(when (= @gameid (:gameid %)) %) @games)
        players (:players game)]
    (when game
      (when-let [create-deck (:create-game-deck @s)]
        (ws/ws-send! [:lobby/deck (:_id create-deck)])
        (swap! app-state dissoc :create-game-deck)
        (swap! s dissoc :create-game-deck))
      [:div
       [:div.button-bar
        (when (first-user? players @user)
          [cond-button
           (tr [:lobby.start "Start"])
           (every? :deck players)
           #(ws/ws-send! [:netrunner/start @gameid])])
        [:button {:on-click #(leave-lobby s)} (tr [:lobby.leave "Leave"])]
        (when (first-user? players @user)
          (if (> (count players) 1)
            [:button {:on-click #(ws/ws-send! [:lobby/swap {:gameid @gameid}])}
             (tr [:lobby.swap "Swap sides"])]
            [:div.dropdown
             [:button.dropdown-toggle {:data-toggle "dropdown"}
              (tr [:lobby.swap "Swap sides"])
              [:b.caret]]
             [:ul.dropdown-menu.blue-shade
              [:a.block-link {:on-click #(ws/ws-send! [:lobby/swap {:gameid @gameid
                                                                    :side "Any Side"} ])}
               (tr-side "Any Side")]
              [:a.block-link {:on-click #(ws/ws-send! [:lobby/swap {:gameid @gameid
                                                                    :side "Corp"}])}
               (tr-side "Corp")]
              [:a.block-link {:on-click #(ws/ws-send! [:lobby/swap {:gameid @gameid
                                                                    :side "Runner"}])}
               (tr-side "Runner")]]]))]
       [:div.content
        [:h2 (:title game)]
        (when-not (every? :deck players)
          [:div.flash-message (tr [:lobby.waiting "Waiting players deck selection"])])
        [:h3 (tr [:lobby.players "Players"])]
        [:div.players
         (doall
           (map-indexed
             (fn [idx player]
               (let [player-id (get-in player [:user :_id])
                     this-player (= player-id (:_id @user))]
                 ^{:key (or player-id idx)}
                 [:div
                  [player-view player game]
                  (when-let [{:keys [name status]} (:deck player)]
                    [:span {:class (:status status)}
                     [:span.label
                      (if this-player
                        (deck-name (:deck player) 25)
                        (tr [:lobby.deck-selected "Deck selected"]))]])
                  (when-let [deck (:deck player)]
                    [:div.float-right [deck-format-status-span deck (:format game "standard") true]])
                  (when (and this-player (not (= (:side player) (tr-side "Any Side"))))
                    [:span.fake-link.deck-load
                     {:on-click #(reagent-modals/modal!
                                   [deckselect-modal user {:games games :gameid gameid
                                                           :sets sets :decks decks
                                                           :format (:format game "standard")}])}
                     (tr [:lobby.select-deck "Select Deck"])])]))
             players))]
        [:h3 (tr [:lobby.options "Options"])]
        [:ul.options
         (when (:allow-spectator game)
           [:li (tr [:lobby.spectators "Allow spectators"])])
         (when (:timer game)
           [:li "Game timer set for " (:timer game) " minutes"])
         (when (:spectatorhands game)
           [:li (tr [:lobby.hidden "Make players' hidden information visible to spectators"])])
         (when (:password game)
           [:li (tr [:lobby.password-protected "Password protected"])])
         (when (:save-replay game)
           [:li (str "🟢 " (tr [:lobby.save-replay "Save replay"]))])
         (when (:save-replay game)
           [:div.infobox.blue-shade {:style {:display (if (:save-replay @s) "block" "none")}}
            [:p "This will save a replay file of this match with open information (e.g. open cards in hand)."
             " The file is available only after the game is finished."]
            [:p "Only your latest 15 unshared games will be kept, so make sure to either download or share the match afterwards."]
            [:p [:b "BETA Functionality:"] " Be aware that we might need to reset the saved replays, so " [:b "make sure to download games you want to keep."]
             " Also, please keep in mind that we might need to do future changes to the site that might make replays incompatible."]])
         (when (:api-access game)
           [:li (tr [:lobby.api-access "Allow API access to game information"])])]

        (when (:allow-spectator game)
          [:div.spectators
           (let [c (:spectator-count game)]
             [:h3 (tr [:lobby.spectator-count "Spectators"] c)])
           (for [spectator (:spectators game)
                 :let [_id (get-in spectator [:user :_id])]]
             ^{:key _id}
             [player-view spectator])])
        [chat-view game]]])))

(defn right-panel
  [decks s games gameid password-gameid sets user]
  [:div.game-panel
   [create-new-game s user]
   [pending-game s decks games gameid password-gameid sets user]])

(defn game-lobby []
  (r/with-let [s (r/atom {:room "casual"})
               decks (r/cursor app-state [:decks])
               games (r/cursor app-state [:games])
               gameid (r/cursor app-state [:gameid])
               password-gameid (r/cursor app-state [:password-gameid])
               sets (r/cursor app-state [:sets])
               user (r/cursor app-state [:user])
               cards-loaded (r/cursor app-state [:cards-loaded])
               active (r/cursor app-state [:active-page])]
    (when (and (= "/play" (first @active)) @cards-loaded)
      (authenticated (fn [_] nil))
      (when (and (not (or @gameid (:editing @s)))
                 (some? (:create-game-deck @app-state)))
        (new-game s))
      [:div.container
        [:div.lobby-bg]
        [:div.lobby.panel.blue-shade
          [games-list-panel s games gameid password-gameid user]
          [right-panel decks s games gameid password-gameid sets user]]])))
