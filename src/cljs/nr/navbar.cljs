(ns nr.navbar
  (:require [nr.appstate :refer [app-state]]
            [nr.history :refer [history]]
            [nr.translations :refer [tr]]
            [reagent.core :as r]))

(def navbar-links
  [[(tr [:nav/chat "Chat"]) "/" 0 nil]
   [(tr [:nav/cards "Cards"]) "/cards" 1 nil]
   [(tr [:nav/deck-builder "Deck Builder"]) "/deckbuilder" 2 nil]
   [(tr [:nav/play "Play"]) "/play" 3 nil]
   [(tr [:nav/help "Help"]) "/help" 4 nil]
   [(tr [:nav/settings "Settings"]) "/account" 5 #(:user %)]
   [(tr [:nav/stats "Stats"]) "/stats" 6 #(:user %)]
   [(tr [:nav/about "About"]) "/about" 7 nil]
   [(tr [:nav/tournaments "Tournaments"]) "/tournament" 8 #(:tournament-organizer (:user %))]
   [(tr [:nav/admin "Admin"]) "/admin" 9 #(:isadmin (:user %))]
   [(tr [:nav/users "Users"]) "/users" 10 #(:isadmin (:user %))]
   [(tr [:nav/features "Features"]) "/features" 11 #(:isadmin (:user %))]])

(defn navbar []
  (r/with-let [active (r/cursor app-state [:active-page])]
    [:ul.carousel-indicator
     (doall
       (for [[name route ndx show-fn?] navbar-links]
         (when (or (not show-fn?)
                   (show-fn? @app-state))
           [:li {:class (if (= (first @active) route) "active" "")
                 :id (str (clojure.string/lower-case name) "-nav")
                 :key name
                 :on-click #(.setToken history route)
                 :data-target "#main"
                 :data-slide-to ndx}
            [:a {:href route} name]])))]))
