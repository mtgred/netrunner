(ns nr.stats
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]
            [jinteki.cards :refer [all-cards]]
            [nr.ajax :refer [GET DELETE]]
            [nr.appstate :refer [app-state]]
            [nr.auth :refer [authenticated] :as auth]
            [nr.avatar :refer [avatar]]
            [nr.deckbuilder :refer [num->percent]]
            [nr.utils :refer [faction-icon render-message]]
            [nr.ws :as ws]
            [reagent.core :as r]))

(defn update-deck-stats
  "Update the local app-state with a new version of deck stats"
  [deck-id stats]
  (let [deck (first (filter #(= (:_id %) deck-id) (:decks @app-state)))
        deck (assoc deck :stats stats)
        others (remove #(= (:_id %) deck-id) (:decks @app-state))]
    (swap! app-state assoc :decks (conj others deck))))

(ws/register-ws-handler!
  :stats/update
  #(do (swap! app-state assoc :stats (-> % :userstats))
       (update-deck-stats (-> % :deck-id) (-> % :deckstats))))

(defn notnum->zero
  "Converts a non-positive-number value to zero.  Returns the value if already a number"
  [input]
  (if (pos? (int input)) input 0))

(defn clear-user-stats []
  (authenticated
    (fn [user]
      (let [id (get-in @app-state [:user :_id])]
        (try (js/ga "send" "event" "user" "clearuserstats") (catch js/Error e))
        (go (let [result (<! (DELETE "/profile/stats/user"))]
              (swap! app-state assoc :stats result)))))))

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
     [:div "Started: " started]
     [:div "Completed: " completed " (" pc "%)"]
     [:div "Not completed: " incomplete  " (" pi "%)"]
     (when-not (= "none" (get-in @app-state [:options :gamestats]))
       [:div [:div "Won: " win  " (" pw "%)"]
        [:div "Lost: " lose  " (" pl "%)"]])]))

(defn left-panel [stats]
  [:div.games.panel
   [:div.games
    [:div
     [:h3 "Game Stats"]
     [stat-view {:stats @stats
                 :start-key :games-started :complete-key :games-completed
                 :win-key :wins :lose-key :loses}]]
    [:div
     [:h3 "Corp Stats"]
     [stat-view {:stats @stats
                 :start-key :games-started-corp :complete-key :games-completed-corp
                 :win-key :wins-corp :lose-key :loses-corp}]]
    [:div
     [:h3 "Runner Stats"]
     [stat-view {:stats @stats
                 :start-key :games-started-runner :complete-key :games-completed-runner
                 :win-key :wins-runner :lose-key :loses-runner}]]]
   [:p [:button {:on-click #(clear-user-stats)} "Clear Stats"]]])

(defn fetch-log [state game]
  (go (let [{:keys [status json]} (<! (GET (str "/profile/history/" (:gameid game))))]
        (when (= 200 status)
          (swap! state assoc :view-game (assoc game :log json))))))

(defn game-row
  [state {:keys [title corp runner corp-id runner-id turn winner reason] :as game}]
  (let [corp-id (first (filter #(= (:title %) corp-id) @all-cards))
        runner-id (first (filter #(= (:title %) runner-id) @all-cards))]
    [:div.gameline {:style {:min-height "auto"}}
     [:button.float-right
      {:on-click #(fetch-log state game)}
      "View log"]
     [:h4 title " (" (or turn 0) " turn" (if (not= 1 turn) "s") ")"]

     [:div
      [:span.player
       [:span.user-status (str "Corp: " corp " ")]
       (faction-icon (:faction corp-id) (:title corp-id)) " " (:title corp-id)]]

     [:div
      [:span.player
       [:span.user-status (str "Runner: " runner " ")]
       (faction-icon (:faction runner-id) (:title runner-id)) " " (:title runner-id)]]

     (when winner
       [:h4 "Winner: " winner])]))

(defn history [state]
  (let [games (reverse (:games @state))]
    [:div.game-panel
     [:div.game-list
      (if (empty? games)
        [:h4 "No games"]
        (doall
          (for [game games]
            ^{:key (:gameid game)}
            [game-row state game])))]]))

(defn game-log [state]
  (let [game (:view-game @state)]
    [:div {:style {:overflow "auto"}}
     [:button.float-right {:on-click #(swap! state dissoc :view-game)} "View games"]
     [:div.panel.messages
      (doall (map-indexed
               (fn [i msg]
                 (when-not (and (= (:user msg) "__system__") (= (:text msg) "typing"))
                   (if (= (:user msg) "__system__")
                     [:div.system {:key i} (render-message (:text msg))]
                     [:div.message {:key i}
                      [avatar (:user msg) {:opts {:size 38}}]
                      [:div.content
                       [:div.username (get-in msg [:user :username])]
                       [:div (render-message (:text msg))]]])))
               (:log game)))]]))

(defn right-panel [state]
  (if (:view-game @state)
    [game-log state]
    [history state]))

(defn stats []
  (r/with-let [stats (r/cursor app-state [:stats])
               active (r/cursor app-state [:active-page])
               state (r/atom {:games nil})]

    (go (let [{:keys [status json]} (<! (GET "/profile/history"))
              games (map #(assoc % :start-date (js/Date. (:start-date %))
                                 :end-date (js/Date. (:end-date %))) json)]
          (when (= 200 status)
            (swap! state assoc :games games))))

    (when (= "/stats" (first @active))
      [:div.container
       [:div.lobby.panel.blue-shade
        [left-panel stats]
        [right-panel state]]])))
