(ns netrunner.main
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [goog.events :as events])
  (:import goog.history.Html5History
           goog.history.EventType))

(def app-state
  (atom {:active-page ["/"]}))

(defn navigate [token]
  (let [page-number (case token "/" 0 "/cards" 1 "/play" 2 "/deckbuilder" 3 "/news" 4)]
    (.carousel (js/$ ".carousel") page-number))
  (try (js/ga "send" "pageview") (catch js/Error e))
  (swap! app-state assoc :active-page [token]))

(def history (Html5History.))

(events/listen history EventType/NAVIGATE #(navigate (.-token %)))
(.setUseFragment history false)
(.setPathPrefix history "")
(.setEnabled history true)

(defn navbar [app owner]
  (om/component
   (let [page (:active-page app)]
     (sab/html
      [:ul.carousel-indicator {}
       [:li {:class (if (= (first page) "/") "active" "")
             :on-click #(.setToken history "/")
             :data-target "#main" :data-slide-to 0}
        [:a {:href "/"} "Manabase"]]
       [:li {:class (if (= (first page) "/cards") "active" "")
             :on-click #(.setToken history "/cards")
             :data-target "#main" :data-slide-to 1}
        [:a {:href "/cards"} "Cards"]]
       [:li {:class (if (= (first page) "/play") "active" "")
             :on-click #(.setToken history "/play")
             :data-target "#main" :data-slide-to 2}
        [:a {:href "/play"} "Play"]]
       [:li {:class (if (= (first page) "/deckbuilder") "active" "")
             :on-click #(.setToken history "/deckbuilder")
             :data-target "#main" :data-slide-to 3}
        [:a {:href "/deckbuilder"} "Decks"]]
       [:li {:class (if (= (first page) "/news") "active" "")
             :on-click #(.setToken history "/news")
             :data-target "#main" :data-slide-to 4}
        [:a {:href "/news"} "News"]]]))))

(om/root navbar app-state {:target (. js/document (getElementById "topnav"))})
