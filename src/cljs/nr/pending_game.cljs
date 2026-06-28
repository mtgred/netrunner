(ns nr.pending-game
  (:require
    [jinteki.validator :refer [singleton-deck? trusted-deck-status]]
    [jinteki.preconstructed :refer [matchup-by-key]]
    [jinteki.utils :refer [constructed-game?]]
    [nr.appstate :refer [app-state current-gameid]]
    [nr.cardbrowser :refer [image-url] :as cb]
    [nr.deck-status :refer [deck-format-status-span]]
    [nr.deckbuilder :refer [deck-name default-deck?]]
    [nr.lobby-chat :refer [lobby-chat]]
    [nr.player-view :refer [player-view]]
    [nr.translations :refer [tr tr-element tr-element-with-embedded-content tr-span tr-side]]
    [nr.utils :refer [cond-button format-date-time mdy-formatter
                      tr-non-game-toast non-game-toast]]
    [nr.ws :as ws]
    [reagent-modals.modals :as reagent-modals]
    [reagent.core :as r]
    [taoensso.sente :as sente]))

(defn is-constructed?
  "Games using the starter decks are not constructed"
  [current-game]
  (constructed-game? @current-game))

(defn is-preconstructed?
  [current-game]
  (not (is-constructed? current-game)))

(defn select-deck [deck]
  (ws/ws-send! [:lobby/deck {:gameid (current-gameid app-state)
                             :deck-id (:_id deck)}]
               1500
               #(when (sente/cb-error? %)
                  (tr-non-game-toast [:lobby_select-error "Cannot select that deck"] "error")))
  (reagent-modals/close-modal!))

(defn deck-valid-for-lobby?
  [deck {:keys [format singleton]} side]
  (and (= side (get-in deck [:identity :side]))
       (or (not singleton) (singleton-deck? deck))
       (or (= "casual" format)
           (get-in deck [:status (keyword format) :legal]
                   (get-in (trusted-deck-status (assoc deck :format format))
                           [(keyword format) :legal]
                           false)))))

(defn select-deck-modal [user current-game]
  (r/with-let [decks (r/cursor app-state [:decks])]
    (let [game @current-game
          fmt (:format game)
          players (:players game)
          side (:side (some #(when (= (-> % :user :_id) (:_id @user)) %) players))
          matches-format? (fn [deck] (= (:format deck) fmt))
          by-date-desc (fn [decks] (reverse (sort-by :date decks)))
          legal-decks (filter #(deck-valid-for-lobby? % game side) @decks)
          appropriate-decks (concat (by-date-desc (filter matches-format? legal-decks))
                                     (by-date-desc (remove matches-format? legal-decks)))]
      (if (seq appropriate-decks)
        [:div
         [tr-element :h3 [:lobby_select-title "Select your deck"]]
         [:div.deck-collection.lobby-deck-selector
          (doall
            (for [deck appropriate-decks]
              [:div.deckline {:key (:_id deck)
                              :on-click #(select-deck deck)}
               [:img {:src (image-url (:identity deck))
                      :alt (get-in deck [:identity :title] "")}]
               [:div.float-right [deck-format-status-span deck fmt true]]
               [:h4 (when (default-deck? deck)
                      [:span.deck-default-star {:title (tr [:deck-builder_default "Default deck"])} "★ "])
                (:name deck)]
               [:div.float-right
                (format-date-time mdy-formatter (:date deck))]
               [:p (get-in deck [:identity :title])]]))]]
        [:div
         [tr-element :h3 [:lobby_no-valid-decks "You do not have any decks that are valid for this format"]]
         [tr-element :h3 [:lobby_no-valid-decks-format (str "This lobby is for the " fmt " format")] {:format fmt}]
         [tr-element :h4 [:lobby_no-valid-decks-help "Please check the validity of your decklists and ensure you are queueing for a game of the appropriate format. If you are a new player and wish to play the learner decks, you need to create or join a game of the System Gateway format." {:format fmt}]]]))))

(defn- first-user?
  "Is this user the first user in the game?"
  [players user]
  (= (-> players first :user :_id) (:_id user)))

(defn start-button [current-game user gameid players]
  (when (first-user? @players @user)
    [cond-button [tr-span [:lobby_start "Start"]]
     (or (every? :deck @players)
         (is-preconstructed? current-game)
         (= "chimera" (:format current-game)))
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
   [tr-span [:lobby_leave "Leave"]]])

(defn precon-info-box [current-game]
  (when-let [precon (:precon @current-game)]
    [:div.infobox.blue-shade
     [tr-element :p (:tr-desc (matchup-by-key precon))]]))

(defn chimera-info-box [current-game]
  (when (= "chimera" (:format @current-game))
    [:div.infobox.blue-shade
     (let [link [:a {:href "https://www.playchimera.net" :target "_blank"} "playchimera.net"]]
       [tr-element-with-embedded-content :p [:lobby_chimera-info [:span "Chimera is a format in which each player plays with randomly generated decklists. Visit " link " for more info on the rules and decisions behind the format."]] {:link link} nil])]))

(defn singleton-info-box [current-game]
  (when (:singleton @current-game)
    [:div.infobox.blue-shade
     [tr-element :p [:lobby_singleton-restriction "This lobby is running in singleton mode. This means decklists will be restricted to only those which do not contain any duplicate cards."]]]))

(defn swap-sides-button [user gameid players]
  (when (first-user? @players @user)
    (if (< 1 (count @players))
      [:button {:on-click #(ws/ws-send! [:lobby/swap {:gameid @gameid}])}
       [tr-span [:lobby_swap "Swap sides"]]]
      [:div.dropdown
       [:button.dropdown-toggle {:data-toggle "dropdown"}
        [tr-span [:lobby_swap "Swap sides"]]
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

(defn button-bar [current-game user gameid players]
  [:div.button-bar
   [start-button current-game user gameid players]
   [leave-button gameid]
   [swap-sides-button user gameid players]])

(defn default-deck-for-lobby
  [current-game side]
  (let [deck-id (get-in @app-state [:options :default-decks (keyword side) (keyword (:format @current-game))])
        deck (some #(when (= (:_id %) deck-id) %) (:decks @app-state))]
    (when (and deck (deck-valid-for-lobby? deck @current-game side))
      deck)))

(defn player-item [user current-game player]
  (let [player-id (get-in player [:user :_id])
        this-player (= player-id (:_id @user))
        side (:side player)
        can-select? (and (is-constructed? current-game)
                         this-player
                         (not (= side (tr-side "Any Side"))))]
    [:div {:key player-id}
     [player-view player (dissoc @current-game :password)]
     (when-let [{:keys [status]} (:deck player)]
       [:span {:class (:status status)}
        [:span.label
         (if this-player
           (deck-name (:deck player) 25)
           [tr-span [:lobby_deck-selected "Deck selected"]])]])
     (when-let [deck (:deck player)]
       [:div.float-right [deck-format-status-span deck (:format @current-game "standard") true]])
     (when can-select?
       [:span.fake-link.deck-load
        {:on-click #(reagent-modals/modal! [select-deck-modal user current-game])}
        [tr-span [:lobby_select-deck "Select Deck"]]])
     (when (and can-select?
                (not (:deck player))
                (= "casual" (:room @current-game)))
       (when-let [default (default-deck-for-lobby current-game side)]
         [:span.fake-link.deck-load
          {:on-click #(select-deck default)}
          [tr-span [:lobby_select-default-deck "Select Default Deck"]]]))]))

(defn player-list [user current-game players]
  [:<>
   [tr-element :h3 [:lobby_players "Players"]]
   (into
     [:div.players]
     (map (fn [player] [player-item user current-game player])
          @players))])

(defn options-list [current-game]
  (let [{:keys [allow-spectator api-access password
                save-replay spectatorhands timer replay-id replay-timestamp]} @current-game]
    [:<>
     [tr-element :h3 [:lobby_options "Options"]]
     [:ul.options
      (when allow-spectator
        [tr-element :li [:lobby_spectators "Allow spectators"]])
      (when timer
        [tr-element :li [:lobby_timer-set-for (str "Game timer set for " timer " minutes")] {:minutes timer}])
      (when spectatorhands
        [tr-element :li [:lobby_hidden "Make players' hidden information visible to spectators"]])
      (when password
        [tr-element :li [:lobby_password-protected "Password protected"]])
      (when save-replay
        [:<>
         [:li "🟢 "  [tr-span [:lobby_save-replay "Save replay"]]]
         [:div.infobox.blue-shade {:style {:display (if save-replay "block" "none")}}
          [tr-element :p [:lobby_save-replay-details "This will save a replay file of this match with open information (e.g. open cards in hand). The file is available only after the game is finished."]]
          [tr-element :p [:lobby_save-replay-unshared "Only your latest 15 unshared games will be kept, so make sure to either download or share the match afterwards."]]
          [tr-element :p [:lobby_save-replay-beta "BETA Functionality: Be aware that we might need to reset the saved replays, so make sure to download games you want to keep. Also, please keep in mind that we might need to do future changes to the site that might make replays incompatible."]]]])
      (when (and replay-id replay-timestamp)
        [:<>
         [tr-element :li [:lobby_replay-restoration "Replay Restoration"]]
         [:div.infobox.blue-shade
          {:style {:display (if (empty? replay-id) "none" "block")}}
          [tr-element :p [:lobby_replay_restoration_beta "BETA Functionality: This lobby will attempt to start the game at the selected game state in the replay."]]
          [tr-element :p [:lobby_replay_restoration_explanation "This feature is not able to fully restore game states. Expect certain functionality to be broken and needing manual fixing. Especially certain trigger contexts (e.g. whether the Runner has made a run last turn, whether a card has been trashed, ...) will be missing."]]
          [tr-element :p [:lobby_replay_restoration_deck_selection "You will need to select the exact decks that have been used in the replay."]]]])
      (when api-access
        [tr-element :li [:lobby_api-access "Allow API access to game information"]])]]))

(defn spectator-list [current-game]
  (let [{:keys [allow-spectator spectators]} @current-game]
    (when allow-spectator
      [:div.spectators
       [tr-element :h3 [:lobby_spectator-count "Spectators"] {:cnt (count spectators)}]
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
                      (tr-non-game-toast [:lobby_cannot-select-deck "Cannot select that deck"] "error")))
      (swap! app-state dissoc :create-game-deck))
    [:div
     [button-bar current-game user gameid players]
     [:div.content
      [:h2 (:title @current-game)]
      [precon-info-box current-game]
      [singleton-info-box current-game]
      [chimera-info-box current-game]
      (when-not (or (every? :deck @players)
                    (not (is-constructed? current-game)))
        [tr-element :div.flash-message [:lobby_waiting "Waiting players deck selection"]])
      [player-list user current-game players]
      [options-list current-game]
      [spectator-list current-game]
      [lobby-chat current-game messages]]]))
