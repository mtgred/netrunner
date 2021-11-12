(ns nr.navbar
  (:require
   [goog.events :as events]
   [goog.history.EventType :as EventType]
   [nr.appstate :refer [app-state]]
   [nr.translations :refer [tr]]
   [nr.ws :as ws]
   [reagent.core :as r])
  (:import
   goog.history.Html5History))

(def navbar-links
  [{:title (tr [:nav/chat "Chat"])
    :cls "chat"
    :route "/"}
   {:title (tr [:nav/cards "Cards"])
    :cljs "card"
    :route "/cards"}
   {:title (tr [:nav/deck-builder "Deck Builder"])
    :clj "deckbuilder"
    :route "/deckbuilder"}
   {:title (tr [:nav/play "Play"])
    :clj "play"
    :route "/play"}
   {:title (tr [:nav/help "Help"])
    :clj "help"
    :route "/help"}
   {:title (tr [:nav/settings "Settings"])
    :clj "settings"
    :route "/account"
    :show? :user}
   {:title (tr [:nav/stats "Stats"])
    :cls "stats"
    :route "/stats"
    :show? :user}
   {:title (tr [:nav/about "About"])
    :cls "about"
    :route "/about"}
   {:title (tr [:nav/tournaments "Tournaments"])
    :cljs "tournaments"
    :route "/tournament"
    :show? #(:tournament-organizer (:user %))}
   {:title (tr [:nav/admin "Admin"])
    :cls "admin"
    :route "/admin"
    :show? #(:isadmin (:user %))}
   {:title (tr [:nav/users "Users"])
    :cls "users"
    :route "/users"
    :show? #(or (:isadmin (:user %))
                (:ismoderator (:user %)))
    :on-click #(ws/ws-send! [:admin/fetch-users])}
   {:title (tr [:nav/features "Features"])
    :cls "features"
    :route "/features"
    :show? #(:isadmin (:user %))}])

(def tokens
    (->> navbar-links
         (map :route)
         (into [])
         (clj->js)))

(def history (Html5History.))

(defn navigate-to-current []
  (let [token (first (:active-page @app-state ["/"]))
        page-number (.indexOf tokens token)]
    (.carousel (js/$ ".carousel") page-number)))

(defn navigate [token]
  (let [page-number (.indexOf tokens token)]
    (.carousel (js/$ ".carousel") page-number))
  (.setToken history token)
  (swap! app-state assoc :active-page [token]))

(events/listen history EventType/NAVIGATE #(navigate (.-token %)))
(.setUseFragment history false)
(.setPathPrefix history "")
(.setEnabled history true)


(defn navbar []
  (r/with-let [active (r/cursor app-state [:active-page])]
    [:ul.carousel-indicator
     (doall
       (for [[idx {:keys [title cls route show? on-click]}] (map-indexed vector navbar-links)]
         (when (or (not show?)
                   (show? @app-state))
           [:li {:class (if (= (first @active) route) "active" "")
                 :id (str cls "-nav")
                 :key title
                 :on-click #(do (.setToken history route)
                                (when on-click
                                  (on-click)))
                 :data-target "#main"
                 :data-slide-to idx}
            [:a {:href route} title]])))]))
