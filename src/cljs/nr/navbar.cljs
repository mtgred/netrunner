(ns nr.navbar
  (:require [nr.appstate :refer [app-state]]
            [nr.history :refer [history]]
            [reagent.core :as r]))

(def navbar-links
  [["Chat" "/" 0 nil]
   ["Cards" "/cards" 1 nil]
   ["Deck Builder" "/deckbuilder" 2 nil]
   ["Play" "/play" 3 nil]
   ["Help" "/help" 4 nil]
   ["Settings" "/account" 5 #(:user %)]
   ["Stats" "/stats" 6 #(:user %)]
   ["About" "/about" 7 nil]
   ["Tournaments" "/tournament" 8 #(:tournament-organizer (:user %))]])

(defn navbar []
  (r/with-let [active (r/cursor app-state [:active-page])]
    [:ul.carousel-indicator
     (doall
       (for [[name route ndx show-fn?] navbar-links]
         (when (or (not show-fn?)
                   (show-fn? @app-state))
           [:li {:class (if (= (first @active) route) "active" "")
                 :key name
                 :on-click #(.setToken history route)
                 :data-target "#main"
                 :data-slide-to ndx}
            [:a {:href route} name]])))]))
