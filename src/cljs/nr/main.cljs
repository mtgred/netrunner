(ns nr.main
  (:require [nr.appstate :refer [app-state]]
            [nr.about :refer [about]]
            [nr.auth :refer [auth-forms auth-menu]]
            [nr.account :refer [account]]
            [nr.cardbrowser :refer [card-browser]]
            [nr.chat :refer [chat-page]]
            [nr.deckbuilder :refer [deck-builder]]
            [nr.gameboard :refer [concede gameboard game-state mute-spectators stack-servers flip-runner-board]]
            [nr.gamelobby :refer [filter-blocked-games game-lobby leave-game]]
            [nr.help :refer [help]]
            [nr.history :refer [navigate-to-current]]
            [nr.navbar :refer [navbar]]
            [nr.player-view :refer [player-view]]
            [nr.stats :refer [stats]]
            [nr.tournament :refer [tournament]]
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
        (str c " Game" (when (not= c 1) "s")))]
     (if-let [game (some #(when (= @gameid (:gameid %)) %) @games)]
       (let [user-id (-> @user :_id)
             is-player (some #(= user-id (-> % :user :_id)) (:players game))]
         (when (:started game)
           [:div.float-right
            (when is-player
              [:a.concede-button {:on-click #(concede)} "Concede"])
            [:a.leave-button {:on-click #(leave-game)} "Leave game"]
            (when is-player
              [:a.mute-button {:on-click #(mute-spectators (not (:mute-spectators game)))}
               (if (:mute-spectators game) "Unmute spectators" "Mute spectators")])
            [:a.stack-servers-button {:on-click #(stack-servers)}
             (if (get-in @app-state [:options :stacked-servers]) "Unstack servers" "Stack servers")]
            (when (not= :runner (:side @game-state))
              [:a.stack-servers-button {:on-click #(flip-runner-board)}
               (if (= "irl" (get-in @app-state [:options :runner-board-order]))
                 "Rig layout: IRL" "Rig layout: jnet")])]))
       (when (not (nil? @gameid))
         [:div.float-right [:a {:on-click #(leave-game)} "Leave game"]]))
     (when-let [game (some #(when (= @gameid (:gameid %)) %) @games)]
       (when (:started game)
         (let [c (:spectator-count game)]
           (when (pos? c)
             [:div.spectators-count.float-right (str c " Spectator" (when (> c 1) "s"))
              [:div.blue-shade.spectators
               (for [p (:spectators game)]
                 ^{:key (get-in p [:user :_id])}
                 [player-view p game])]]))))]))

(defn pages []
  (r/create-class
    {:display-name "main-pages"

     :component-did-mount
     (fn [] (navigate-to-current))

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
