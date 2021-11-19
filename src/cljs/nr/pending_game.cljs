(ns nr.pending-game
  (:require
   [jinteki.validator :refer [trusted-deck-status]]
   [nr.appstate :refer [app-state]]
   [nr.cardbrowser :refer [image-url] :as cb]
   [nr.deck-status :refer [deck-format-status-span]]
   [nr.deckbuilder :refer [deck-name]]
   [nr.lobby-chat :refer [lobby-chat]]
   [nr.player-view :refer [player-view]]
   [nr.translations :refer [tr tr-side]]
   [nr.utils :refer [cond-button non-game-toast]]
   [nr.ws :as ws]
   [reagent-modals.modals :as reagent-modals]
   [taoensso.sente :as sente]
   [reagent.core :as r]))

(defn leave-lobby []
  (ws/ws-send! [:lobby/leave] 8000
               #(when (sente/cb-success? %)
                  (swap! app-state assoc :current-game nil))))

(defn select-deck [deck]
  (fn []
    (ws/ws-send! [:lobby/deck (:_id deck)] 8000
                 #(when (sente/cb-error? %)
                    (non-game-toast "Cannot select that deck" "error")))
    (reagent-modals/close-modal!)))

(defn select-deck-modal [user current-game]
  (r/with-let [decks (r/cursor app-state [:decks])]
    [:div
     [:h3 (tr [:lobby.select-title "Select your deck"])]
     [:div.deck-collection.lobby-deck-selector
      (let [fmt (:format @current-game)
            players (:players @current-game)
            side (:side (some #(when (= (-> % :user :_id) (:_id @user)) %) players))
            same-side? (fn [deck] (= side (get-in deck [:identity :side])))
            legal? (fn [deck] (get-in deck [:status (keyword fmt) :legal]
                                      (get-in (trusted-deck-status (assoc deck :format fmt))
                                              [(keyword fmt) :legal]
                                              false)))]
        (doall
          (for [deck (->> @decks
                          (filter same-side?)
                          (sort-by (complement legal?))
                          (sort-by :date))]
            [:div.deckline {:key (:_id deck)
                            :on-click (select-deck deck)}
             [:img {:src (image-url (:identity deck))
                    :alt (get-in deck [:identity :title] "")}]
             [:div.float-right [deck-format-status-span deck fmt true]]
             [:h4 (:name deck)]
             [:div.float-right (-> (:date deck) js/Date. js/moment (.format "MMM Do YYYY"))]
             [:p (get-in deck [:identity :title])]])))]]))

(defn- first-user?
  "Is this user the first user in the game?"
  [players user]
  (= (-> players first :user :_id) (:_id user)))

(defn button-bar [user gameid players]
  [:div.button-bar
   (when (first-user? @players @user)
     [cond-button (tr [:lobby.start "Start"])
      (every? :deck @players)
      #(ws/ws-send! [:netrunner/start @gameid])])
   [:button {:on-click leave-lobby} (tr [:lobby.leave "Leave"])]
   (when (first-user? @players @user)
     (if (> (count @players) 1)
       [:button {:on-click #(ws/ws-send! [:lobby/swap {:gameid @gameid}])}
        (tr [:lobby.swap "Swap sides"])]
       [:div.dropdown
        [:button.dropdown-toggle {:data-toggle "dropdown"}
         (tr [:lobby.swap "Swap sides"])
         [:b.caret]]
        (into
          [:ul.dropdown-menu.blue-shade]
          (for [side ["Any Side" "Corp" "Runner"]]
            [:a.block-link {:on-click #(ws/ws-send! [:lobby/swap {:gameid @gameid
                                                                  :side side}])}
             (tr-side side)]))]))])

(defn player-list [user current-game players]
  [:<>
   [:h3 (tr [:lobby.players "Players"])]
   [:div.players
    (doall
      (map-indexed
        (fn [idx player]
          (let [player-id (get-in player [:user :_id])
                this-player (= player-id (:_id @user))]
            ^{:key (or player-id idx)}
            [:div
             [player-view player @current-game]
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
        @players))]])

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

(defn pending-game []
  (r/with-let [user (r/cursor app-state [:user])
               current-game (r/cursor app-state [:current-game])
               gameid (r/cursor app-state [:current-game :gameid])
               players (r/cursor app-state [:current-game :players])
               messages (r/cursor app-state [:current-game :messages])
               create-game-deck (r/cursor app-state [:create-game-deck])]
    (fn []
      (when-let [cd @create-game-deck]
        (ws/ws-send! [:lobby/deck (:_id cd)])
        (swap! app-state dissoc :create-game-deck))
      [:div
       [button-bar user gameid players]
       [:div.content
        [:h2 (:title @current-game)]
        (when-not (every? :deck @players)
          [:div.flash-message (tr [:lobby.waiting "Waiting players deck selection"])])
        [player-list user current-game players]
        [options-list current-game]
        [spectator-list current-game]
        [lobby-chat messages]]])))
