(ns nr.main
  (:require
   [nr.about :refer [about]]
   [nr.account :refer [account]]
   [nr.admin :refer [admin]]
   [nr.appstate :refer [app-state]]
   [nr.auth :refer [auth-forms auth-menu]]
   [nr.cardbrowser :refer [card-browser]]
   [nr.chat :refer [chat-page]]
   [nr.deckbuilder :refer [deck-builder]]
   [nr.features :refer [features]]
   [nr.gameboard.board :refer [gameboard]]
   [nr.gamelobby :refer [game-lobby]]
   [nr.help :refer [help]]
   [nr.navbar :refer [navbar navigate navigate-to-current]]
   [nr.stats :refer [stats]]
   [nr.status-bar :refer [status]]
   [nr.tournament :refer [tournament]]
   [nr.users :refer [users]]
   [reagent-modals.modals :as reagent-modals]
   [reagent.core :as r]
   [time-literals.data-readers]
   [time-literals.read-write]))

(time-literals.read-write/print-time-literals-cljs!)

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
