(ns nr.tournament
  (:require [nr.appstate :refer [app-state]]
            [nr.ws :as ws]
            [nr.avatar :refer [avatar]]
            [nr.stats :refer [faction-icon-memo]]
            [jinteki.utils :refer [slugify str->int]]
            [reagent.core :as r]))

(defn change-cobra-link
  [state value]
  (swap! state dissoc :cobra-link)
  (swap! state assoc :url value))

(defn cobra-link
  [state]
  [:div
   [:h3 "Public Cobr.ai tournament"]
   [:input {:placeholder "cobr.ai tournament link"
            :type "text"
            :value (:url @state)
            :on-change #(change-cobra-link state (-> % .-target .-value))}]])

(defn parse-id
  [{value :url}]
  (let [link (first (next (re-find #"(?:tournaments/)(\d+)" value)))
        number (re-find #"\d+" value)]
    (or link number)))

(defn process-link
  [state]
  (when (not-empty (:url @state))
    (swap! state assoc :cobra-link (parse-id @state))
    (ws/ws-send! [:tournament/fetch {:cobra-link (:cobra-link @state)}])))

(defn load-tournament-button
  [state]
  [:button {:on-click #(process-link state)} "Load tournament"])

(defn missing-players
  [state]
  (when (:cobra-link @state)
    (let [players (:missing-players @state)]
      [:div
       [:h3 "Players in Cobra with no registered jnet accounts"]
       [:ul.missing-players
        (doall
          (map-indexed
            (fn [idx player]
              ^{:key idx} [:li player])
            players))]])))

(defn delete-all-tables
  [state]
  (ws/ws-send! [:tournament/delete {:cobra-link (:cobra-link @state)}]))

(defn delete-tournament-button
  [state]
  (when (:cobra-link @state)
    [:div
     [:h4 "Delete all tables for this tournament?"]
     [:button {:on-click #(delete-all-tables state)} "Delete tables"]]))

(defn round-selector
  [state]
  (when (:cobra-link @state)
    [:div
     [:h3 "Select round"]
     [:select {:value (or (:selected-round @state) (count (:rounds @state)))
               :on-change #(swap! state assoc :selected-round (.. % -target -value))}
      (doall
        (for [round (range 1 (inc (count (:rounds @state))))]
          ^{:key round}
          [:option {:value (dec round)}
           (str "Round " round)]))]]))

(defn create-tables
  [state]
  (ws/ws-send! [:tournament/create (select-keys @state
                                     [:cobra-link :selected-round :save-replays? :timer :single-sided?])]))

(defn select-round-button
  [state]
  (when (:cobra-link @state)
    [:button {:on-click #(create-tables state)}
     "Create tables for this round"]))

(defn success
  [state]
  (let [results (:results @state)
        result-type (:result-type @state)]
    (when results
      [:<>
       [:h2 "Success!"]
       [:p (str results " tables have been " result-type)]])))

(defn save-replay-option
  [state]
  (when (:cobra-link @state)
    [:p
     [:label
      [:input {:type "checkbox" :checked (:save-replays? @state)
               :on-change #(swap! state assoc :save-replays? (.. % -target -checked))}]
      (str "🟢 Save replays")]]))

(defn timed-option
  [s]
  (when (:cobra-link @s)
    [:<> [:p
          [:label
           [:input {:type "checkbox" :checked (:timed @s)
                    :on-change #(let [checked (.. % -target -checked)]
                                  (swap! s assoc :timed checked)
                                  (swap! s assoc :timer (if checked 35 nil)))}]
           "Round timer"]]
     (when (:timed @s)
       [:p
        [:input.game-title {:on-change #(swap! s assoc :timer (-> % (.. -target -value) str->int))
                            :type "number"
                            :value (:timer @s)
                            :placeholder "Timer length (minutes)"}]])
     [:div.infobox.blue-shade {:style {:display (if (:timed @s) "block" "none")}}
      [:p "Timer is only for convenience: the game will not stop when timer runs out."]]]))

(defn single-sided-option
  [state]
  (when (:cobra-link @state)
    [:<>
     [:p
      [:label
       [:input {:type "checkbox" :checked (:single-sided? @state)
                :on-change #(swap! state assoc :single-sided? (.. % -target -checked))}]
       (str "Single-sided round?")]]
     [:div.infobox.blue-shade {:style {:display (if (:single-sided? @state) "block" "none")}}
      [:p "A second game will not be created when the first is completed."]]]))

(defn tournament-container
  [state]
  [:div.panel.tournament-settings-container
   [:h1 "Tournament stuff"]
   (when (:cobra-link @state)
     [:h2 (:tournament-name @state)])
   [cobra-link state]
   [load-tournament-button state]
   [missing-players state]
   [delete-tournament-button state]
   [round-selector state]
   [timed-option state]
   [save-replay-option state]
   [single-sided-option state]
   [select-round-button state]
   [success state]])

(defn load-players
  [state json]
  (swap! state merge (:data json)))

(defn store-results
  [state result-type json]
  (let [json-key (keyword (str result-type "-rounds"))]
    (swap! state assoc
           :results (get-in json [:data json-key])
           :result-type result-type)))

(defn deck-info
  [deck]
  [:div
   [:div.id-info
    [:span.id-label "ID: "]
    [:span.id-title.influence {:class (slugify (get-in deck [:identity :faction]))}
     (get-in deck [:identity :title])]]
   [:div.hash-info
    [:span.hash-label "Hash: "]
    [:span.hash-value (:hash deck)]]])

(defn player-info
  [player]
  [:div
   [:span.player
    [avatar player {:opts {:size 24}}]
    (get-in player [:player :username]) " - "
    (if-let [id (get-in player [:deck :identity])]
      (str (faction-icon-memo (:faction id) (:title id)) " " (:title id)))]]
  [:div.player-info
   [:div.user-info
    [:span.username-label "Username: "]
    [:span.username-value (get-in player [:user :username])]]
   [:div.side-info
    [:span.side-label "Side: "]
    [:span.side-value (:side player)]]
   (if (:deck player)
     [deck-info (:deck player)])])

(defn game-info
  [game]
  [:div.gameline
   [:div [:span.game-title (str (:title game))]]
   [:div.game-status
    (if (:started game)
        [:span.started (str "Started " (.toLocaleTimeString (js/Date. (:start-date game))))]
        [:span.not-started (str "Not started")])]
   [:div.players
    [player-info (first (:players game))]
    [player-info (second (:players game))]]])

(defonce state (r/atom {:selected-round "0"}))

(defmethod ws/-msg-handler :tournament/loaded [{data :?data}]
  (load-players state data))

(defmethod ws/-msg-handler :tournament/created [{data :?data}]
  (store-results state "created" data))

(defmethod ws/-msg-handler :tournament/deleted [{data :?data}]
  (store-results state "deleted" data))

(defn tournament []
  (r/with-let [user (r/cursor app-state [:user])
               active (r/cursor app-state [:active-page])
               cobra-link (r/cursor state [:cobra-link])
               games (r/cursor app-state [:games])]
    (when (and (= "/tournament" (first @active))
               (:tournament-organizer @user))
      [:div.container
       [:div.lobby.panel.blue-shade
        [tournament-container state]
        [:ul.game-list
         (let [filtered-games (filter #(and @cobra-link (= @cobra-link (:cobra-link %))) @games)]
           (doall (for [game filtered-games]
                    ^{:key (:gameid game)}
                    [game-info game])))]]])))
