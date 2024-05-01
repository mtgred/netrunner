(ns nr.routes
  (:require
   [nr.about :refer [about]]
   [nr.account :refer [account]]
   [nr.admin :refer [admin]]
   [nr.appstate :refer [app-state]]
   [nr.cardbrowser :refer [card-browser]]
   [nr.chat :refer [chat-page]]
   [nr.deckbuilder :refer [deck-builder]]
   [nr.features :refer [features]]
   [nr.gameboard.board :refer [gameboard]]
   [nr.gameboard.board-alternate :refer [gameboard-alternate]]
   [nr.help :refer [help]]
   [nr.lobby :refer [game-lobby]]
   [nr.stats :refer [stats]]
   [nr.tournament :refer [tournament]]
   [nr.users :refer [users]]
   [reagent.core :as r]
   [reitit.frontend :as rf]
   [reitit.frontend.easy :as rfe]))

(defonce current-view (r/atom {:data {:view chat-page}}))

(defn update-current-view [match _history]
  (reset! current-view match))

(defn lobby-or-game []
  (let [game-started? (r/cursor app-state [:current-game :started])
        new-theme (r/cursor app-state [:options :new-theme])]
    (fn []
      (if @game-started?
        (if @new-theme [gameboard-alternate] [gameboard])
        [game-lobby]))))

(def routes
  (rf/router
    [["/" {:name :nav/chat
           :view chat-page}]
     ["/cards" {:name :nav/cards
                :view card-browser}]
     ["/deckbuilder" {:name :nav/deckbuilder
                      :view deck-builder}]
     ["/play" {:name :nav/lobby
               :view lobby-or-game}]
     ["/replay/:rid" {:name :nav/replay-lobby
               :view lobby-or-game}]
     ["/bug-report/:rid" {:name :nav/bug-report
                      :view lobby-or-game}]
     ["/help" {:name :nav/help
               :view help}]
     ["/account" {:name :nav/account
                  :view account}]
     ["/stats" {:name :nav/stats
                :view stats}]
     ["/about" {:name :nav/about
                :view about}]
     ["/tournament" {:name :nav/tournament
                     :view tournament}]
     ["/admin" {:name :nav/admin
                :view admin}]
     ["/users" {:name :nav/users
                :view users}]
     ["/features" {:name :nav/features
                   :view features}]]))

(defn init-routes!
  "Start the routing"
  []
  (rfe/start! routes update-current-view {:use-fragment false}))
