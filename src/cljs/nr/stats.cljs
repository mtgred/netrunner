(ns nr.stats
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<!] :as async]
   [clojure.string :refer [capitalize]]
   [jinteki.cards :refer [all-cards]]
   [nr.ajax :refer [DELETE GET]]
   [nr.appstate :refer [app-state]]
   [nr.auth :refer [authenticated] :as auth]
   [nr.avatar :refer [avatar]]
   [nr.end-of-game-stats :refer [build-game-stats]]
   [nr.translations :refer [tr tr-format tr-lobby tr-side]]
   [nr.utils :refer [day-word-with-time-formatter faction-icon format-date-time
                     notnum->zero num->percent player-highlight-option-class
                     render-message render-player-highlight set-scroll-top store-scroll-top]]
   [nr.ws :as ws]
   [reagent.core :as r]))

(def state (r/atom {:games nil}))

(defn- fetch-game-history []
  (go (let [{:keys [status json]} (<! (GET "/profile/history"))]
        (when (= 200 status) (swap! state assoc :games json)))))

(defn update-deck-stats
  "Update the local app-state with a new version of deck stats"
  [deck-id stats]
  (let [deck (first (filter #(= (:_id %) deck-id) (:decks @app-state)))
        deck (assoc deck :stats stats)
        others (remove #(= (:_id %) deck-id) (:decks @app-state))]
    (swap! app-state assoc :decks (conj others deck))))

(defmethod ws/event-msg-handler :stats/update [{{:keys [userstats deck-id deckstats]} :?data}]
  (swap! app-state assoc :stats userstats)
  (update-deck-stats deck-id deckstats)
  (fetch-game-history))

(defn share-replay [state gameid]
  (go (let [{:keys [status]} (<! (GET (str "/profile/history/share/" gameid)))]
        (when (= 200 status)
          (swap! state assoc :view-game
                 (assoc (:view-game @state) :replay-shared true))))))

(defn- replay-link [game]
  (str (.-origin (.-location js/window)) "/replay/" (:gameid game)))

(defn launch-replay [game] (set! (.-location js/window) (replay-link game)))

(defn game-details [state]
  (let [game (:view-game @state)]
    [:div.games.panel
     [:p.return-button [:button {:on-click #(swap! state dissoc :view-game)} (tr [:stats.view-games "Return to stats screen"])]]
     [:h4 (:title game) (when (:has-replay game) (if (:replay-shared game) " ⭐" " 🟢"))]
     [:div
      [:div.game-details-table
       [:div (str (tr [:stats.lobby "Lobby"]) ": " (capitalize (tr-lobby (:room game))))]
       [:div (str (tr [:stats.format "Format"]) ": " (capitalize (tr-format (:format game))))]
       [:div (str (tr [:stats.winner "Winner"]) ": " (capitalize (tr-side (:winner game))))]
       [:div (str (tr [:stats.win-method "Win method"]) ": " (:reason game))]
       [:div (str (tr [:stats.started "Started"]) ": " (format-date-time day-word-with-time-formatter (:start-date game)))]
       [:div (str (tr [:stats.ended "Ended"]) ": " (format-date-time day-word-with-time-formatter (:end-date game)))]]
      (when (:stats game)
        [build-game-stats (get-in game [:stats :corp]) (get-in game [:stats :runner])])
      [:p
       (when (and (:has-replay game)
                  (not (:replay-shared game)))
         [:button {:on-click #(share-replay state (:gameid game))} (tr [:stats.share "Share replay"])])
       (if (:has-replay game)
         [:span
          [:button {:on-click #(launch-replay game)} (tr [:stats.launch "Launch Replay"])]
          [:a.button {:href (str "/profile/history/full/" (:gameid game)) :download (str (:title game) ".json")} (tr [:stats.download "Download replay"])]]
         (tr [:stats.unavailable "Replay unavailable"]))]
      (when (:replay-shared game)
        [:p [:input.share-link {:type "text" :read-only true :value (replay-link game)}]])]]))

(defn clear-user-stats []
  (authenticated
    (fn [_]
      (go (let [result (<! (DELETE "/profile/stats/user"))]
            (swap! app-state assoc :stats result))))))

(defn stat-view [{:keys [start-key complete-key win-key lose-key stats]}]
  (r/with-let [started (notnum->zero (start-key stats))
               completed (notnum->zero (complete-key stats))
               pc (notnum->zero (num->percent completed started))
               win (notnum->zero (win-key stats))
               lose (notnum->zero (lose-key stats))
               pw (notnum->zero (num->percent win (+ win lose)))
               pl (notnum->zero (num->percent lose (+ win lose)))
               incomplete (notnum->zero (- started completed))
               pi (notnum->zero (num->percent incomplete started))]
    [:section
     [:div (tr [:stats.started "Started"]) ": " started]
     [:div (tr [:stats.completed "Completed"]) ": " completed " (" pc "%)"]
     [:div (tr [:stats.not-completed "Not completed"]) ": " incomplete  " (" pi "%)"]
     (when-not (= "none" (get-in @app-state [:options :gamestats]))
       [:div [:div (tr [:stats.won "Won"]) ": " win  " (" pw "%)"]
        [:div (tr [:stats.lost "Lost"]) ": " lose  " (" pl "%)"]])]))

(defn stats-panel [stats]
  [:div.games.panel
   [:div.games
    [:div
     [:h3 (tr [:stats.game-stats "Game Stats"])]
     [stat-view {:stats @stats
                 :start-key :games-started :complete-key :games-completed
                 :win-key :wins :lose-key :loses}]]
    [:div
     [:h3 (tr [:stats.corp-stats "Corp Stats"])]
     [stat-view {:stats @stats
                 :start-key :games-started-corp :complete-key :games-completed-corp
                 :win-key :wins-corp :lose-key :loses-corp}]]
    [:div
     [:h3 (tr [:stats.runner-stats "Runner Stats"])]
     [stat-view {:stats @stats
                 :start-key :games-started-runner :complete-key :games-completed-runner
                 :win-key :wins-runner :lose-key :loses-runner}]]]
   [:p [:button {:on-click #(clear-user-stats)} (tr [:stats.clear-stats "Clear Stats"])]]] )

(defn left-panel [state stats]
  (if (:view-game @state)
    [game-details state]
    [stats-panel stats]))

(defn game-log [_state log-scroll-top]
  (r/create-class
    {:display-name "stats-game-log"
     :component-did-mount #(set-scroll-top % @log-scroll-top)
     :component-will-unmount #(store-scroll-top % log-scroll-top)
     :reagent-render
     (fn [state _log-scroll-top]
       (let [game (:view-game @state)
             corp (get-in game [:corp :player :username])
             runner (get-in game [:runner :player :username])]
         [:div {:style {:overflow "auto"}}
          [:div.panel.messages {:class (player-highlight-option-class)}
           (if (seq (:log game))
             (doall (map-indexed
                      (fn [i msg]
                        (when-not (and (= (:user msg) "__system__") (= (:text msg) "typing"))
                          (if (= (:user msg) "__system__")
                            [:div.system {:key i} (render-message (render-player-highlight (:text msg) corp runner))]
                            [:div.message {:key i}
                             [avatar (:user msg) {:opts {:size 38}}]
                             [:div.content
                              [:div.username (get-in msg [:user :username])]
                              [:div (render-message (:text msg))]]])))
                      (:log game)))
             [:h4 (tr [:stats.no-log "No log available"])])]]))}))

(def faction-icon-memo (memoize faction-icon))

(defn fetch-log [state game]
  (go (let [{:keys [status json]} (<! (GET (str "/profile/history/" (:gameid game))))]
        (when (= 200 status)
          (swap! state assoc :view-game (assoc game :log json))))))

(defn game-row
  [state {:keys [title corp runner turn winner replay-shared has-replay start-date] :as game} log-scroll-top]
  (let [corp-id (get @all-cards (:identity corp))
        runner-id (get @all-cards (:identity runner))
        turn-count (if turn turn 0)
        user (:user @app-state)
        user-win (if (= (str winner) "corp") 
                (if (= (:username user) (get-in corp [:player :username])) " (You)" "")
                (if (= (:username user) (get-in runner [:player :username])) " (You)" ""))]
    [:div.gameline {:style {:min-height "auto"
                            :border-color (when winner (if (= user-win " (You)") "#6AB56A" "#Ea7d7f"))}}
     [:button.float-right
      {:on-click #(do
                    (fetch-log state game)
                    (reset! log-scroll-top 0))}
      (tr [:stats.view-log "View log"])]
     [:h4.log-title
      {:title (when replay-shared "Replay shared")}
      title " (" (tr [:stats.turn-count] turn-count) ")" (when has-replay (if replay-shared " ⭐" " 🟢"))]

     [:div.log-date (format-date-time day-word-with-time-formatter start-date)]

     [:div
      [:span.player
       [avatar (:player corp) {:opts {:size 24}}]
       (get-in corp [:player :username]) " - "
       (faction-icon-memo (:faction corp-id) (:title corp-id)) " " (:title corp-id)]]

     [:div
      [:span.player
       [avatar (:player runner) {:opts {:size 24}}]
       (get-in runner [:player :username]) " - "
       (faction-icon-memo (:faction runner-id) (:title runner-id)) " " (:title runner-id)]]

     (when winner
       [:h4 (tr [:stats.winner "Winner"]) ": " (tr-side winner) (str user-win)])]))

(defn history [_state list-scroll-top _log-scroll-top]
  (r/create-class
    {:display-name "stats-history"
     :component-did-mount #(set-scroll-top % @list-scroll-top)
     :component-will-unmount #(store-scroll-top % list-scroll-top)
     :reagent-render
     (fn [state _list-scroll-top log-scroll-top]
       (let [all-games (:games @state)
             games (if (:filter-replays @state) (filter #(:replay-shared %) all-games) all-games)
             cnt (count games)]
         [:div.game-list
           [:div.controls
            [:button {:on-click #(swap! state update :filter-replays not)}
             (if (:filter-replays @state)
               (tr [:stats.all-games "Show all games"])
               (tr [:stats.shared-games "Only show shared"]))]
            [:span.log-count (str (tr [:stats.log-count] cnt) (when (:filter-replays @state)
                                                                (str " " (tr [:stats.filtered "(filtered)"]))))]]
           (if (empty? games)
             [:h4 (tr [:stats.no-games "No games"])]
             (doall
               (for [game games]
                 ^{:key (:gameid game)}
                 [game-row state game log-scroll-top])))]))}))

(defn right-panel [state list-scroll-top log-scroll-top]
  (if (:view-game @state)
    [game-log state log-scroll-top]
    [:div.game-panel
     [history state list-scroll-top log-scroll-top]]))

(defn stats []
  (let [stats (r/cursor app-state [:stats])
        list-scroll-top (atom 0)
        log-scroll-top (atom 0)]
    (fetch-game-history)
    (fn []
      [:div.page-container
       [:div.stats-bg]
       [:div.lobby.panel.blue-shade
        [left-panel state stats]
        [right-panel state list-scroll-top log-scroll-top]]])))
