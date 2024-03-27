(ns nr.pending-game
  (:require
   [jinteki.validator :refer [singleton-deck? trusted-deck-status]]
   [nr.appstate :refer [app-state current-gameid]]
   [nr.cardbrowser :refer [image-url] :as cb]
   [nr.deck-status :refer [deck-format-status-span]]
   [nr.deckbuilder :refer [deck-name]]
   [nr.lobby-chat :refer [lobby-chat]]
   [nr.player-view :refer [player-view]]
   [nr.translations :refer [tr tr-side]]
   [nr.utils :refer [cond-button format-date-time mdy-formatter
                     non-game-toast]]
   [nr.ws :as ws]
   [reagent-modals.modals :as reagent-modals]
   [reagent.core :as r]
   [taoensso.sente :as sente]))

(defn select-deck [deck]
  (ws/ws-send! [:lobby/deck {:gameid (current-gameid app-state)
                             :deck-id (:_id deck)}]
               1500
               #(when (sente/cb-error? %)
                  (non-game-toast "Cannot select that deck" "error")))
  (reagent-modals/close-modal!))

(defn select-deck-modal [user current-game]
  (r/with-let [decks (r/cursor app-state [:decks])]
    [:div
     [:h3 (tr [:lobby.select-title "Select your deck"])]
     [:div.deck-collection.lobby-deck-selector
      (let [fmt (:format @current-game)
            players (:players @current-game)
            singleton? (:singleton @current-game)
            singleton-fn? (fn [deck] (or (not singleton?) (singleton-deck? deck)))
            ;;(or (not singleton?) (singleton-id? (get-in deck [:identity])))) -- this one restricts to the ids only
            side (:side (some #(when (= (-> % :user :_id) (:_id @user)) %) players))
            same-side? (fn [deck] (= side (get-in deck [:identity :side])))
            legal? (fn [deck fmt] (or (= "casual" fmt)
                                      (get-in deck [:status (keyword fmt) :legal]
                                              (get-in (trusted-deck-status (assoc deck :format fmt))
                                                      [(keyword fmt) :legal]
                                                      false))))]
        (doall
         (for [deck (->> @decks
                         (filter same-side?)
                         (filter singleton-fn?)
                         (filter #(legal? % fmt))
                         (sort-by :date)
                         (reverse))]
           [:div.deckline {:key (:_id deck)
                           :on-click #(select-deck deck)}
            [:img {:src (image-url (:identity deck))
                   :alt (get-in deck [:identity :title] "")}]
            [:div.float-right [deck-format-status-span deck fmt true]]
            [:h4 (:name deck)]
            [:div.float-right
             (format-date-time mdy-formatter (:date deck))]
            [:p (get-in deck [:identity :title])]])))]]))

(defn- first-user?
  "Is this user the first user in the game?"
  [players user]
  (= (-> players first :user :_id) (:_id user)))

(defn start-button [user gameid players]
  (when (first-user? @players @user)
    [cond-button (tr [:lobby.start "Start"])
     (every? :deck @players)
     #(ws/ws-send! [:game/start {:gameid @gameid}])]))

(defn leave-button [gameid]
  [:button
   {:on-click
    (fn [e]
      (.preventDefault e)
      (ws/ws-send! [:lobby/leave {:gameid @gameid}]
                   8000
                   #(when (sente/cb-success? %)
                      (swap! app-state assoc :editing false :current-game nil))))}
   (tr [:lobby.leave "Leave"])])

(defn singleton-info-box [current-game]
  (when (:singleton @current-game)
    [:div.infobox.blue-shade
     [:p "This lobby is running in singleton mode. This means decklists will be restricted to only those which do not contain any duplicate cards."]]))

(defn first-five-info-box [current-game]
  (when (:first-five @current-game)
    [:div.infobox.blue-shade
     [:p "This lobby is running in first-five mode. This means that instead of draws and mulligans, players will decide their opening hands."]]))

(defn swap-sides-button [user gameid players]
  (when (first-user? @players @user)
    (if (< 1 (count @players))
      [:button {:on-click #(ws/ws-send! [:lobby/swap {:gameid @gameid}])}
       (tr [:lobby.swap "Swap sides"])]
      [:div.dropdown
       [:button.dropdown-toggle {:data-toggle "dropdown"}
        (tr [:lobby.swap "Swap sides"])
        [:b.caret]]
       (into
        [:ul.dropdown-menu.blue-shade]
        (for [side ["Any Side" "Corp" "Runner"]]
          (let [is-player-side (= side (-> @players first :side))]
            [:a.block-link
             (if is-player-side
               {:style {:color "grey" :cursor "default"} :disabled true}
               {:on-click #(ws/ws-send! [:lobby/swap {:gameid @gameid
                                                      :side side}])})
             [:li (tr-side side)]])))])))

(defn button-bar [user gameid players]
  [:div.button-bar
   [start-button user gameid players]
   [leave-button gameid]
   [swap-sides-button user gameid players]])

(defn player-item [user current-game player]
  (let [player-id (get-in player [:user :_id])
        this-player (= player-id (:_id @user))]
    [:div {:key player-id}
     [player-view player (dissoc @current-game :password)]
     (when-let [{:keys [status]} (:deck player)]
       [:span {:class (:status status)}
        [:span.label
         (if this-player
           (deck-name (:deck player) 25)
           (tr [:lobby.deck-selected "Deck selected"]))]])
     (when-let [deck (:deck player)]
       [:div.float-right [deck-format-status-span deck (:format @current-game "standard") true]])
     (when (and this-player (not (= (:side player) (tr-side "Any Side"))))
       [:span.fake-link.deck-load
        {:on-click #(reagent-modals/modal! [select-deck-modal user current-game])}
        (tr [:lobby.select-deck "Select Deck"])])]))

(defn player-list [user current-game players]
  [:<>
   [:h3 (tr [:lobby.players "Players"])]
   (into
    [:div.players]
    (map (fn [player] [player-item user current-game player])
         @players))])

(defn options-list [current-game]
  (let [{:keys [allow-spectator api-access password
                save-replay spectatorhands timer]} @current-game]
    [:<>
     [:h3 (tr [:lobby.options "Options"])]
     [:ul.options
      (when allow-spectator
        [:li (tr [:lobby.spectators "Allow spectators"])])
      (when timer
        [:li "Game timer set for " timer " minutes"])
      (when spectatorhands
        [:li (tr [:lobby.hidden "Make players' hidden information visible to spectators"])])
      (when password
        [:li (tr [:lobby.password-protected "Password protected"])])
      (when save-replay
        [:<>
         [:li (str "🟢 " (tr [:lobby.save-replay "Save replay"]))]
         [:div.infobox.blue-shade {:style {:display (if save-replay "block" "none")}}
          [:p "This will save a replay file of this match with open information (e.g. open cards in hand)."
           " The file is available only after the game is finished."]
          [:p "Only your latest 15 unshared games will be kept, so make sure to either download or share the match afterwards."]
          [:p [:b "BETA Functionality:"]
           " Be aware that we might need to reset the saved replays, so "
           [:b "make sure to download games you want to keep."]
           " Also, please keep in mind that we might need to do future changes to the site that might make replays incompatible."]]])
      (when api-access
        [:li (tr [:lobby.api-access "Allow API access to game information"])])]]))

(defn spectator-list [current-game]
  (let [{:keys [allow-spectator spectators]} @current-game]
    (when allow-spectator
      [:div.spectators
       [:h3 (tr [:lobby.spectator-count "Spectators"] (count spectators))]
       (for [spectator spectators
             :let [_id (get-in spectator [:user :_id])]]
         ^{:key _id}
         [player-view spectator])])))

(defn pending-game [current-game user]
  (r/with-let [gameid (r/cursor current-game [:gameid])
               players (r/cursor current-game [:players])
               messages (r/cursor current-game [:messages])
               create-game-deck (r/cursor app-state [:create-game-deck])]
    (when-let [cd @create-game-deck]
      (ws/ws-send! [:lobby/deck {:gameid (current-gameid app-state)
                                 :deck-id (:_id cd)}]
                   8000
                   #(when (sente/cb-error? %)
                      (non-game-toast "Cannot select that deck" "error")))
      (swap! app-state dissoc :create-game-deck))
    [:div
     [button-bar user gameid players]
     [:div.content
      [:h2 (:title @current-game)]
      [singleton-info-box current-game]
      [first-five-info-box current-game]
      (when-not (every? :deck @players)
        [:div.flash-message
         (tr [:lobby.waiting "Waiting players deck selection"])])
      [player-list user current-game players]
      [options-list current-game]
      [spectator-list current-game]
      [lobby-chat current-game messages]]]))
