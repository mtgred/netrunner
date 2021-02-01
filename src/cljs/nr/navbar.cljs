(ns nr.navbar
  (:require [nr.appstate :refer [app-state]]
            [nr.history :refer [history]]
            [nr.translations :refer [tr]]
            [reagent.core :as r]))

(def navbar-links
  [[(tr [:nav/chat "Chat"]) "chat" "/" 0 nil]
   [(tr [:nav/cards "Cards"]) "card" "/cards" 1 nil]
   [(tr [:nav/deck-builder "Deck Builder"]) "deckbuilder" "/deckbuilder" 2 nil]
   [(tr [:nav/play "Play"]) "play" "/play" 3 nil]
   [(tr [:nav/help "Help"]) "help" "/help" 4 nil]
   [(tr [:nav/settings "Settings"]) "settings" "/account" 5 #(:user %)]
   [(tr [:nav/stats "Stats"]) "stats" "/stats" 6 #(:user %)]
   [(tr [:nav/about "About"]) "about" "/about" 7 nil]
   [(tr [:nav/tournaments "Tournaments"]) "tournaments" "/tournament" 8 #(:tournament-organizer (:user %))]
   [(tr [:nav/admin "Admin"]) "admin" "/admin" 9 #(:isadmin (:user %))]
   [(tr [:nav/users "Users"]) "users" "/users" 10 #(:isadmin (:user %))]
   [(tr [:nav/features "Features"]) "features" "/features" 11 #(:isadmin (:user %))]])

(defn navbar []
  (r/with-let [active (r/cursor app-state [:active-page])]
    [:ul.carousel-indicator
     (doall
       (for [[name cls route ndx show-fn?] navbar-links]
         (when (or (not show-fn?)
                   (show-fn? @app-state))
           [:li {:class (if (= (first @active) route) "active" "")
                 :id (str cls "-nav")
                 :key name
                 :on-click #(.setToken history route)
                 :data-target "#main"
                 :data-slide-to ndx}
            [:a {:href route} name]])))]))
