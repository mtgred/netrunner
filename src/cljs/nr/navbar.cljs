(ns nr.navbar
  (:require
   [goog.events :as events]
   [goog.history.EventType :as EventType]
   [nr.appstate :refer [app-state]]
   [nr.routes :as routes]
   [nr.translations :refer [tr]])
  (:import
   goog.history.Html5History))

(def navbar-links
  [{:title [:nav_welcome "Welcome"]
    :cls "landing"
    :route "/"}
    {:title [:nav_chat "Chat"]
    :cls "chat"
    :route "/chat"}
   {:title [:nav_cards "Cards"]
    :cls "card"
    :route "/cards"}
   {:title [:nav_deck-builder "Deck Builder"]
    :cls "deckbuilder"
    :route "/deckbuilder"}
   {:title [:nav_play "Play"]
    :cls "play"
    :route "/play"}
   {:title [:nav_help "Help"]
    :cls "help"
    :route "/help"}
   {:title [:nav_settings "Settings"]
    :cls "settings"
    :route "/account"
    :show? :user}
   {:title [:nav_stats "Stats"]
    :cls "stats"
    :route "/stats"
    :show? :user}
   {:title [:nav_about "About"]
    :cls "about"
    :route "/about"}
   {:title [:nav_tournaments "Tournaments"]
    :cls "tournaments"
    :route "/tournament"
    :show? #(:tournament-organizer (:user %))}
   {:title [:nav_admin "Admin"]
    :cls "admin"
    :route "/admin"
    :show? #(:isadmin (:user %))}
   {:title [:nav_users "Users"]
    :cls "users"
    :route "/users"
    :show? #(or (:isadmin (:user %))
                (:ismoderator (:user %)))}
   {:title [:nav_users "Prizes"]
    :cls "prizes"
    :route "/prizes"
    :show? #(or (:isadmin (:user %))
                (:ismoderator (:user %)))}
   {:title [:nav_features "Features"]
    :cls "features"
    :route "/features"
    :show? #(:isadmin (:user %))}])

(def history (Html5History.))

(defn navigate [token]
  (.setToken history token))

(events/listen history EventType/NAVIGATE #(navigate (.-token ^js %)))
(.setUseFragment history false)
(.setPathPrefix history "")
(.setEnabled history true)


(defn navbar []
  [:ul
   (doall
     (for [[idx {:keys [title cls route show?]}] (map-indexed vector navbar-links)]
       (when (or (not show?)
                 (show? @app-state))
         [:li {:class (if (= (:path @routes/current-view) route) "active" "")
               :id (str cls "-nav")
               :key (tr title)
               ; :on-click #(.setToken history route)
               :data-target "#main"
               :data-slide-to idx}
          [:a {:href route} (tr title)]])))])
