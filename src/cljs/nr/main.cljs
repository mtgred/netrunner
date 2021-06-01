(ns nr.main
  (:require [nr.appstate :refer [app-state]]
            [nr.about :refer [about]]
            [nr.auth :refer [auth-forms auth-menu]]
            [nr.account :refer [account]]
            [nr.cardbrowser :refer [card-browser]]
            [nr.chat :refer [chat-page]]
            [nr.deckbuilder :refer [deck-builder]]
            [nr.gameboard.board :refer [gameboard]]
            [nr.gameboard.actions :refer [concede mute-spectators stack-cards flip-runner-board]]
            [nr.gameboard.replay :refer [set-replay-side]]
            [nr.gameboard.state :refer [game-state]]
            [nr.gamelobby :refer [filter-blocked-games game-lobby leave-game]]
            [nr.help :refer [help]]
            [nr.history :refer [navigate-to-current navigate]]
            [nr.navbar :refer [navbar]]
            [nr.player-view :refer [player-view]]
            [nr.stats :refer [stats]]
            [nr.tournament :refer [tournament]]
            [nr.translations :refer [tr]]
            [nr.admin :refer [admin]]
            [nr.users :refer [users]]
            [nr.features :refer [features]]
            [reagent-modals.modals :as reagent-modals]
            [reagent.core :as r]))

(defn- status []
  (r/with-let [user (r/cursor app-state [:user])
               games (r/cursor app-state [:games])
               gameid (r/cursor app-state [:gameid])]
    [:div
     [:div.float-right
      (let [c (count (filter-blocked-games @user @games))]
        (tr [:nav/game-count] c))]
     (if-let [game (some #(when (= @gameid (:gameid %)) %) @games)]
       (let [user-id (-> @user :_id)
             is-player (some #(= user-id (-> % :user :_id)) (:players game))]
         (when (:started game)
           [:div.float-right
            (when is-player
              [:a.concede-button {:on-click #(concede)} (tr [:game.concede "Concede"])])
            [:a.leave-button {:on-click #(leave-game)} (if (:replay game) (tr [:game.leave-replay "Leave replay"]) (tr [:game.leave "Leave game"]))]
            (when is-player
              [:a.mute-button {:on-click #(mute-spectators (not (:mute-spectators game)))}
               (if (:mute-spectators game) (tr [:game.unmute "Unmute spectators"]) (tr [:game.mute "Mute spectators"]))])]))
       (when (not (nil? @gameid))
         [:div.float-right
          [:a {:on-click #(leave-game)} (if (= "local-replay" @gameid) (tr [:game.leave-replay "Leave replay"]) (tr [:game.leave "Leave game"]))]
          (when (= "local-replay" @gameid)
            [:a.replay-button {:on-click #(set-replay-side :corp)} (tr [:game.corp-view "Corp View"])])
          (when (= "local-replay" @gameid)
            [:a.replay-button {:on-click #(set-replay-side :runner)} (tr [:game.runner-view "Runner View"])])
          (when (= "local-replay" @gameid)
            [:a.replay-button {:on-click #(set-replay-side :spectator)} (tr [:game.spec-view "Spectator View"])])]))
     (when-let [game (some #(when (= @gameid (:gameid %)) %) @games)]
       (when (:started game)
         (let [c (:spectator-count game)]
           (when (pos? c)
             [:div.spectators-count.float-right (str c " Spectator" (when (> c 1) "s"))
              [:div.blue-shade.spectators
               (for [p (:spectators game)]
                 ^{:key (get-in p [:user :_id])}
                 [player-view p])]]))))]))

(defn- get-server-data
  [tag]
  (-> (.getElementById js/document "server-originated-data")
      (.getAttribute (str "data-" tag))))

(defn pages []
  (r/create-class
    {:display-name "main-pages"

     :component-did-mount
     (fn []
      (let [ver (get-server-data "version")
            rid (get-server-data "replay-id")]
        (swap! app-state assoc :app-version ver)
        (swap! app-state assoc :replay-id rid)
        (if rid
          (navigate "/play")
          (navigate-to-current))))

     :reagent-render
     (fn []
       [:div#main.carousel.slide {:data-interval "false"}
        [:div.carousel-inner
         [:div.item.active
          [:div.home-bg]
          [chat-page]]
         [:div.item
          [:div.cardbrowser-bg]
          [card-browser]]
         [:div.item
          [:div.deckbuilder-bg]
          [deck-builder]]
         [:div.item
          [:div#gamelobby [game-lobby]]
          [:div#gameboard [gameboard]]]
         [:div.item
          [:div.help-bg]
          [help]]
         [:div.item
          [:div.account-bg]
          [account]]
         [:div.item
          [:div.stats-bg]
          [stats]]
         [:div.item
          [:div.about-bg]
          [about]]
         [:div.item
          [:div.about-bg]
          [tournament]]
         [:div.item
          [:div.help-bg]
          [admin]]
         [:div.item
          [:div.account-bg]
          [users]]
         [:div.item
          [:div.help-bg]
          [features]]]])}))

(defn main-window []
  [:<>
   [:nav.topnav.blue-shade
    [:div#left-menu [navbar]]
    [:div#right-menu [auth-menu]]
    [:div#status [status]]]
   [:div#auth-forms [auth-forms]]
   [reagent-modals/modal-window]
   [pages]])

(defn init! []
  (r/render [main-window] (.getElementById js/document "main-content")))
