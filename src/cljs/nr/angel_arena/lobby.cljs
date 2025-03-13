(ns nr.angel-arena.lobby
  (:require
   [clojure.string :refer [capitalize lower-case]]
   [jinteki.cards :refer [all-cards]]
   [jinteki.utils :refer [superuser?]]
   [nr.appstate :refer [app-state]]
   [nr.avatar :refer [avatar]]
   [nr.cardbrowser :refer [image-url]]
   [nr.deck-status :refer [deck-format-status-span]]
   [nr.deckbuilder :refer [deck-name]]
   [nr.game-row :refer [join-game]]
   [nr.player-view :refer [user-status-span]]
   [nr.sounds :refer [resume-sound]]
   [nr.translations :refer [tr tr-format tr-pronouns]]
   [nr.utils :refer [cond-button faction-icon format-zoned-date-time
                     mdy-formatter slug->format time-span-string tristate-button]]
   [nr.ws :as ws]
   [reagent-modals.modals :as reagent-modals]
   [reagent.core :as r]
   [taoensso.sente :as sente]))

(defonce arena-supported-formats (atom [:standard :startup :eternal]))

(defonce runs (r/atom nil))
(defonce latest-runs (r/atom nil))
(defonce chosen-format (r/atom (first @arena-supported-formats)))
(defonce queueing (r/atom nil))
(defonce queue-times (r/atom nil))

(defn- fetch-runs []
  (ws/ws-send! [:angel-arena/fetch-runs]
               8000
               #(when (sente/cb-success? %)
                  (reset! runs %))))

(defn- fetch-queue-times []
  (ws/ws-send! [:angel-arena/fetch-queue-times]
               8000
               #(when (sente/cb-success? %)
                  (reset! queue-times %))))

(defn- fetch-history []
  (ws/ws-send! [:angel-arena/fetch-history]
               8000
               #(when (sente/cb-success? %)
                  (reset! latest-runs %))))

; XXX: Copied from utils.clj.. maybe make cljc
(defn- get-wins
  [{:keys [games side]}]
  (count (filter #(= (name side) (:winner %)) games)))

(defn- get-losses
  [{:keys [games]} wins]
  (- (count (filter :winner games)) wins))

(defn time-left [run-info wins losses]
  (let [time-since-start (- (js/Date.now) (js/Date.parse (:run-started @run-info)))
        allowed-days (+ 3 wins losses)]
    [:div.time (str "Time left: " (time-span-string (/ (- (* 1000 60 60 24 allowed-days)
                                                          time-since-start)
                                                       1000)))]))

(defn- deck-view [side deck]
  (r/with-let [run-info (r/track #(get-in @runs [@chosen-format side]))]
    (let [wins (get-wins @run-info)
          losses (get-losses @run-info wins)]
      [:div.deck
       [:img {:src (image-url (:identity deck))
              :alt (get-in deck [:identity :title] "")}]
       [:h4 (deck-name deck)]
       [:div.result.float-right (str wins " wins")]
       [:div (get-in deck [:identity :title])]
       [:div.result.float-right (str losses " losses")]
       [time-left run-info wins losses]])))

(defn- deck-buttons [side deck]
  (r/with-let [abandon (r/atom false)]
    [:div.buttons
     [:div.button-row
      [tristate-button
       [tr [:angel-arena_queueing "Queueing..."]]
       [tr [:angel-arena_queue-for-match "Queue for match"]]
       (= (:_id deck) @queueing)
       (and @queueing
            (not= (:_id deck) @queueing))
       #(if @queueing
          (do (ws/ws-send! [:angel-arena/dequeue {:deck-id (:_id deck)}])
              (reset! queueing nil))
          (do (ws/ws-send! [:angel-arena/queue {:deck-id (:_id deck)}])
              (reset! queueing (:_id deck))))]
      "Average waiting time: " (time-span-string (get-in @queue-times [@chosen-format side]))]
     [:div.button-row
      (if @abandon
        [:span [tr [:angel-arena_are-you-sure "Are you sure?"]] " "
         [:button.small {:on-click #(do (ws/ws-send! [:angel-arena/abandon-run {:deck-id (:_id deck)}])
                                        (fetch-runs))} [tr [:angel-arena_are-you-sure-yes "yes"]]]
         [:button.small {:on-click #(reset! abandon false)} [tr [:angel-arena_are-you-sure-no "no"]]]]
        [cond-button [tr [:angel-arena_abandon-run "Abandon run"]]
         (not @queueing)
         #(reset! abandon true)])]]))

(defn- deck-games [side]
  (let [run-info (get-in @runs [@chosen-format side])]
    [:div.games
     (doall
       (for [{:keys [game-id opponent winner]} (:games run-info)]
         (let [result (cond
                        (nil? winner) "aborted"
                        (= winner (name side)) "won"
                        :else "lost")]
           [:div.match {:key game-id :class [result]}
            [:img.identity {:class [result]
                            :src (image-url (get @all-cards (:identity opponent)))
                            :alt (:identity opponent)
                            :title (str (:identity opponent) "\nOpponent: " (:username opponent))}]])))]))

(defn- deckselect-modal [{:keys [side decks]}]
  [:div
   [:h3 [tr [:angel-arena_select-deck "Select your deck"]]]
   [:div.deck-collection.lobby-deck-selector
    (let [same-side? (fn [deck] (= (capitalize (name side))
                                   (get-in deck [:identity :side])))
          correct-format? (fn [deck] (let [form (get-in deck [:status :format])]
                                       (= (keyword form) @chosen-format)))
          legal? (fn [deck] (let [form (get-in deck [:status :format])]
                              (get-in deck [:status (keyword form) :legal])))
          eligible-decks (->> @decks
                              (filter same-side?)
                              (filter correct-format?)
                              (filter legal?)
                              (sort-by :date >))]
      (if (empty? eligible-decks)
        [:div.infobox.one-line.blue-shade
         [:p [tr [:angel-arena_no-eligible-decks "No legal decks found for this side and format."]]]]
        (doall
          (for [deck eligible-decks]
            ^{:key (:_id deck)}
            [:div.deckline {:on-click #(do (ws/ws-send! [:angel-arena/start-run
                                                         {:deck-id (:_id deck)}])
                                           (reagent-modals/close-modal!)
                                           (fetch-runs))}
             [:img {:src (image-url (:identity deck))
                    :alt (get-in deck [:identity :title] "")}]
             [:div.float-right [deck-format-status-span deck (get-in deck [:status :format]) true]]
             [:h4 (:name deck)]
             [:div.float-right (format-zoned-date-time mdy-formatter (:date deck))]
             [:p (get-in deck [:identity :title])]]))))]])

(defn- new-run-button-bar [side decks]
  [:div.button-bar
   [cond-button [tr [:angel-arena_start-new-run "Start new run"]]
    (not @queueing)
    #(reagent-modals/modal!
       [deckselect-modal {:side side :decks decks}])]])

(defmethod ws/event-msg-handler :angel-arena/run-update
  [{{:keys [finished-run] :as data} :?data}]
  ;; TODO: Implement this
  (when finished-run
    (println "Run finished :" data "\nWould display dialog box now..."))
  (fetch-runs)
  (fetch-history))

(defn latest-run-view
  [{:keys [_id identity deck-name side games run-finished] :as run}]
  (let [wins (get-wins run)
        losses (get-losses run wins)
        opened (r/atom false)]
    (fn []
      [:div.run {:key _id}
       [:div.unfold-button {:on-click #(swap! opened not)
                            :class [(when @opened "open")]}]
       [:div.deck
        [:img {:src (image-url (get @all-cards identity))
               :alt identity}]
        [:h4 deck-name]
        [:div.result.float-right (str wins " wins")]
        [:div identity]
        [:div.result.float-right (str losses " losses")]
        [:div "Run started: " (.toLocaleString (js/Date. run-finished))]]
       [:div.unfold {:class [(when @opened "open")]
                     :style {:max-height (when @opened (* 100 (count (remove #(nil? (:winner %)) games))))}}
        [:div.games
         (doall
           (for [{:keys [game-id reason opponent winner]} games]
             (let [result (cond
                            (nil? winner) "aborted"
                            (= winner (name side)) "won"
                            :else "lost")]
               ^{:key game-id}
               [:div.match {:class [result]}
                [:img.identity {:class [result]
                                :src (image-url (get @all-cards (:identity opponent)))
                                :alt (:identity opponent)
                                :title (str (:identity opponent))}]

                [:div.name-area [avatar (:username opponent) {:opts {:size 32}}]
                 [:div.name-box
                  [:div.username (:username opponent)]
                  (when-let [pronouns (:pronouns opponent)]
                    (let [pro-str (if (= "blank" pronouns) "" (tr-pronouns pronouns))]
                      [:div.pronouns (lower-case pro-str)]))]]
                [:div.info (case result
                             "aborted" [:p "Aborted"]
                             "won" (when reason [:p (str "Won by " reason)])
                             "lost" (when reason [:p (str "Lost by " reason)]))
                 [:p [:a {:href (str "/replay/" game-id)} "Replay"]]]])))]]])))

(defn latest-runs-view []
  (into [:div.latest-runs {:key @chosen-format}]
        (for [run (filter #(= (name @chosen-format) (:format %)) @latest-runs)]
          ^{:key (:run-started run)}
          [latest-run-view run])))

(defn game-panel [decks]
  (r/create-class
    {:display-name "game-panel"

     :component-did-mount
     (fn []
       (fetch-runs)
       (fetch-queue-times)
       (fetch-history))

     :reagent-render
     (fn []
       (if-not @runs
         [:div.game-panel.angel-arena
          [:h3 [tr [:angel-arena_requesting-run-data "Requesting run data..."]]]]
         [:div.game-panel.angel-arena
          [:h3 [tr [:angel-arena_format "Format"]]]
          [:div.format-bar
           (doall
             (for [form @arena-supported-formats]
               ^{:key form}
               [:span.tab {:on-click #(reset! chosen-format form)
                           :class [(when (= @chosen-format form) "current")]}
                (get slug->format (name form))]))]
          [:h3 [tr [:angel-arena_active-corp-run "Active Corp run"]]]
          (if (get-in @runs [@chosen-format :corp])
            (let [deck (first (filter #(= (str (:_id %))
                                          (get-in @runs [@chosen-format :corp :deck-id]))
                                      @decks))]
              [:div.run
               [deck-view :corp deck]
               [deck-games :corp]
               [deck-buttons :corp deck]])
            [:div.run [new-run-button-bar :corp decks]])

          [:h3 [tr [:angel-arena_active-runner-run "Active Runner run"]]]
          (if (get-in @runs [@chosen-format :runner])
            (let [deck (first (filter #(= (str (:_id %))
                                          (get-in @runs [@chosen-format :runner :deck-id]))
                                      @decks))]
              [:div.run
               [deck-view :runner deck]
               [deck-games :runner]
               [deck-buttons :runner deck]])
            [:div.run [new-run-button-bar :runner decks]])

          [:h3 [tr [:angel-arena_latest-runs "Latest runs"]]]
          [latest-runs-view]]))}))

(defn- player-view
  ([player] (player-view player nil))
  ([player game]
   [:span.player
    [avatar (:user player) {:opts {:size 22}}]
    [user-status-span player]
    (let [side (:side player)
          faction (:faction (:identity (:deck player)))
          identity (:title (:identity (:deck player)))
          specs (:allow-spectator game)]
      (cond
        (and (some? faction)
             (not= "Neutral" faction)
             specs)
        (faction-icon faction identity)

        side
        (str " (" (tr [:side_name] {:side side}) ")")))
    (when-let [{:keys [wins losses]} (:run-info player)]
      [:span.standings wins "-" losses])]))

(defn- game-row
  [lobby-state {:keys [format started players gameid original-players] :as game} current-game]
  (r/with-let [user (:user @app-state)
               join (fn [action] (join-game lobby-state game action "Any Side"))]
    [:div.gameline {:class (when (= current-game gameid) "active")}
     (when (or (superuser? user)
               (and (:allow-spectator game)
                    (not current-game)))
       [:button {:on-click #(do (join "watch")
                                (resume-sound))}
        [tr [:lobby_watch "Watch"]]])
     (when (and (not current-game)
                started
                (= 1 (count players))
                (some #(= (get-in % [:user :username]) (:username user)) original-players))
       [:button {:on-click #(do (join "rejoin")
                                (resume-sound))}
        [tr [:lobby_rejoin "Rejoin"]]])
     (let [c (:spectator-count game)]
       [:h4
        (str (when (:save-replay game) "🟢")
             (:title game)
             (when (pos? c) (str " (" (tr [:lobby_spectator-count] {:cnt c}) ")")))])

     [:div {:class "game-format"}
      [:span.format-label [tr [:lobby_default-game-format "Default game format"]] ":  "]
      [:span.format-type (tr-format (slug->format format "Unknown"))]]

     (into [:div]
           (map
             (fn [player]
               ^{:key (-> player :user :username)}
               [player-view player game])
             original-players))]))

(defn get-player-wins [game]
  (map #(get-wins (:run-info %)) (:players game)))

(defn game-list [lobby-state {:keys [games current-game]}]
  (r/with-let [room-games (r/track (fn [] (filter #(= "angel-arena" (:room %)) @games)))]
    [:div.game-list
     (if (empty? @room-games)
       [:h4 [tr [:angel-arena_no-games "No games"]]]
       (let [groups (->> @room-games
                         (group-by #(apply max (get-player-wins %)))
                         (sort-by first >))]
         (doall
           (for [[wins games] groups]
             [:div.win-group {:key wins}
              [:div.win-divider {:key (str wins "-divider")} wins " " [tr [:angel-arena_wins "wins"]]]
              (doall
                (for [game games]
                  ^{:key (:gameid game)}
                  [game-row lobby-state game @current-game]))]))))]))
