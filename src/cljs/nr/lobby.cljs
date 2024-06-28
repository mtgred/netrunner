(ns nr.lobby
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<!] :as async]
   [clojure.set :refer [difference union]]
   [nr.ajax :refer [GET]]
   [nr.angel-arena.lobby :as angel-arena]
   [nr.appstate :refer [app-state current-gameid]]
   [nr.auth :refer [authenticated] :as auth]
   [nr.game-row :refer [game-row]]
   [nr.gameboard.actions :refer [leave-game!]]
   [nr.new-game :refer [create-new-game]]
   [nr.password-game :refer [password-game]]
   [nr.pending-game :refer [pending-game]]
   [nr.replay-game :refer [start-replay-div]]
   [nr.sounds :refer [play-sound resume-sound]]
   [nr.translations :refer [tr tr-format]]
   [nr.utils :refer [cond-button non-game-toast slug->format]]
   [nr.ws :as ws]
   [reagent.core :as r]
   [taoensso.sente :as sente]))

(defmethod ws/event-msg-handler :lobby/list [{data :?data}]
  (swap! app-state assoc :games data))

(defmethod ws/event-msg-handler :lobby/state [{data :?data}]
  (when-not (= "local-replay" (:gameid @app-state))
    (swap! app-state assoc :current-game data)
    (when (:started data)
      (ws/ws-send! [:game/resync {:gameid (:gameid data)}]))))

(defmethod ws/event-msg-handler :lobby/notification [{data :?data}]
  (play-sound data))

(defmethod ws/event-msg-handler :lobby/toast [{{:keys [message type]} :?data}]
  (non-game-toast message type {:time-out 30000 :close-button true}))

(defmethod ws/event-msg-handler :lobby/timeout
  [{{:keys [gameid]} :?data}]
  (when (= gameid (:gameid @app-state))
    (non-game-toast (tr [:lobby.closed-msg "Game lobby closed due to inactivity"])
                    "error"
                    {:time-out 0 :close-button true})
    (swap! app-state assoc :gameid nil)))

(defn replay-game [s]
  (authenticated
    (fn [_]
      (swap! s assoc :replay true))))

(defn start-shared-replay
  ([s gameid] (start-shared-replay s gameid nil))
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
                 (ws/event-msg-handler-wrapper
                   {:id :game/start
                    :?data (.stringify js/JSON (clj->js
                                                 (if jump-to
                                                   (assoc init-state :replay-jump-to jump-to)
                                                   init-state)))}))
               404
               (non-game-toast (tr [:lobby.replay-link-error "Replay link invalid."])
                               "error" {:time-out 0 :close-button true}))))))))

(defn leave-game []
  (if (= "local-replay" (:gameid @app-state))
    (do
      (swap! app-state assoc :gameid nil)
      (leave-game!))
    (ws/ws-send! [:game/leave {:gameid (current-gameid app-state)}]
                 8000
                 #(when (sente/cb-success? %)
                    (leave-game!)))))

(defn- hidden-formats
  "Remove games which the user has opted to hide"
  [visible-formats game]
  (contains? visible-formats (get game :format)))

(defn filter-games
  [user games visible-formats]
  (if (= "tournament" (:room (first games)))
    games
    (let [is-visible #(or (contains? (get % :players) (:username user))
                          (hidden-formats visible-formats %))]
      (filter is-visible games))))

(def open-games-symbol "○")
(def closed-games-symbol "●")

(defn room-count-str [open-count closed-count]
  (str " (" @open-count open-games-symbol " " @closed-count closed-games-symbol ")"))

(defn- room-tab
  "Creates the room tab for the specified room"
  [user s games room room-name]
  (r/with-let [room-games (r/track (fn [] (filter #(= room (:room %)) @games)))
               filtered-games (r/track (fn [] (filter-games @user @room-games (:visible-formats @app-state))))
               closed-count (r/track (fn [] (count (filter :started @filtered-games))))
               open-count (r/track (fn [] (- (count @room-games) @closed-count)))]
    [:div.roomtab
     (if (= room (:room @s))
       {:class "current"}
       {:on-click #(do (swap! s assoc :room room)
                       (swap! s dissoc :editing))})
     room-name (room-count-str open-count closed-count)]))

(defn game-list [state user games current-game]
  (r/with-let [editing (r/cursor state [:editing])
               room (r/cursor state [:room])
               visible-formats (r/cursor app-state [:visible-formats])]
    (let [room-games (filter #(= (:room %) @room) @games)
          filtered-games (filter-games @user room-games @visible-formats)]
    [:<>
     [:div.game-count
      [:h4 (str (tr [:lobby.game-count] (count filtered-games))
                (when (not= (count slug->format) (count @visible-formats))
                  (str "  " (tr [:lobby.filtered "(filtered)"]))))]]
     [:div.game-list
      (if (empty? filtered-games)
        [:h4 (tr [:lobby.no-games "No games"])]
        (doall
          (for [game filtered-games]
            ^{:key (:gameid game)}
            [game-row state game @current-game @editing])))]])))

(defn format-visible? [slug] (contains? (:visible-formats @app-state) slug))

(defn- on-change-format-visibility
  "Handle change event for format-toggle input"
  [slug evt]
  (.stopPropagation evt)
  (if (format-visible? slug)
    (swap! app-state update-in [:visible-formats] difference #{slug})
    (swap! app-state update-in [:visible-formats] union #{slug}))
  (.setItem js/localStorage "visible-formats" (.stringify js/JSON (clj->js (:visible-formats @app-state)))))

(defn format-toggle [slug]
  (r/with-let [id (str "filter-" slug)]
    [:div
     [:input.visible-formats {:id id
                              :type "checkbox"
                              :on-change #(on-change-format-visibility slug %)
                              :checked (format-visible? slug)}]
     [:label {:for id
              :on-click #(.stopPropagation %)}
      (-> slug slug->format tr-format)]]))

(defn new-game-button [s games gameid user]
  [cond-button (tr [:lobby.new-game "New game"])
   ;; TODO: rewrite this check
   (and (not (or @gameid
                 (:editing @s)
                 (= "tournament" (:room @s))))
        (->> @games
             (mapcat :players)
             (filter #(= (-> % :user :_id) (:_id @user)))
             empty?))
   #(authenticated
      (fn [_]
        (swap! s assoc :editing true)
        (-> ".game-title" js/$ .select)
        (resume-sound)))])

(defn reload-lobby-button []
  [:button.reload-button {:type "button"
                          :on-click #(ws/ws-send! [:lobby/list])}
   (tr [:lobby.reload "Reload list"])])

(defn load-replay-button [s games gameid user]
  [cond-button (tr [:lobby.load-replay "Load replay"])
   ;; TODO: rewrite this check
   (and (not (or @gameid
                 (:editing @s)
                 (= "tournament" (:room @s))))
        (->> @games
             (mapcat :players)
             (filter #(= (-> % :user :_id) (:_id @user)))
             empty?))
   #(do (replay-game s)
        (resume-sound))])

(defn button-bar [s games current-game user visible-formats]
  [:div.button-bar
    [:div.rooms
     [:div#filter.dropdown
      [:a.dropdown-toggle {:href "" :data-toggle "dropdown"}
       "Filter"
       [:b.caret]]
       [:div.dropdown-menu.blue-shade
        (doall (for [[k] slug->format]
                 ^{:key k}
                 [format-toggle k (contains? visible-formats k)]))]]
     [room-tab user s games "casual" (tr [:lobby.casual "Casual"])]
     [room-tab user s games "angel-arena" (tr [:lobby.angel-arena "Angel Arena"])]
     [room-tab user s games "competitive" (tr [:lobby.tournament "Tournament"])]]
    (when-not (= "angel-arena" (:room @s))
      [:div.lobby-buttons
       [new-game-button s games current-game user]
       [reload-lobby-button]
       [load-replay-button s games current-game user]])])

(defn games-list-panel [state games current-game user visible-formats]
  (r/create-class
    {:display-name "games-list"
     :component-did-mount
     (fn []
       (ws/lobby-updates-continue!))
     :component-will-unmount
     (fn []
       (ws/lobby-updates-pause!))

     :reagent-render
     (fn []
       [:div.games
        [button-bar state games current-game user visible-formats]
        (if @ws/lobby-updates-state
          (if (= "angel-arena" (:room @state))
            [angel-arena/game-list state {:games games
                                          :current-game current-game}]
            [game-list state user games current-game])
          [:div
           "Lobby updates halted." ; this should never be visible
           [:button {:on-click #(ws/lobby-updates-continue!)} "Reenable lobby updates"]])])}))

(defn right-panel
  [state decks current-game user]
  (if (= "angel-arena" (:room @state))
    [angel-arena/game-panel decks]
    [:div.game-panel
     (cond
       (:replay @state)
       [start-replay-div state]
       (:editing @state)
       [create-new-game state user]
       (:password-game @state)
       [password-game state]
       (and @current-game (not (:started @current-game)))
       [pending-game current-game user])]))

(defn load-replay-from-params [s params]
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
      ; remove query parameters from url
      (.replaceState (.-history js/window) {} "" "/play")
      (if bug-report?
        (start-shared-replay s replay-id {:bug (or b 0)})
        (if (and n d)
          (start-shared-replay s replay-id {:n n :d d})
          (start-shared-replay s replay-id nil)))
      (resume-sound)
      nil)))

(defn game-lobby []
  (r/with-let [state (r/atom {:room "casual"})
               decks (r/cursor app-state [:decks])
               games (r/cursor app-state [:games])
               current-game (r/cursor app-state [:current-game])
               user (r/cursor app-state [:user])
               visible-formats (r/cursor app-state [:visible-formats])
               replay-id (r/cursor app-state [:replay-id])]
    [:div.container
     [:div.lobby-bg]
     (do (authenticated (fn [_] nil)) nil)
     ; TODO: make starting a game from deckbuilder work again
     ; (when (and (not (or @gameid (:editing @s)))
     ;            (some? (:create-game-deck @app-state)))
     ;   (new-game s))
     (if-let [params @replay-id]
       (load-replay-from-params state params)
       [:div.lobby.panel.blue-shade
        [games-list-panel state games current-game user visible-formats]
        [right-panel state decks current-game user]])]))
