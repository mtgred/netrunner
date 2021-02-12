(ns nr.history
  (:require [goog.events :as events]
            [goog.history.EventType :as EventType]
            [nr.appstate :refer [app-state]])
  (:import goog.history.Html5History))

(def tokens #js ["/" "/chat" "/cards" "/deckbuilder" "/play" "/help" "/account" "/stats" "/about" "/tournament" "/admin" "/users" "/features"])

(def history (Html5History.))

(defn navigate-to-current []
  (let [token (first (:active-page @app-state ["/"]))
        page-number (.indexOf tokens token)]
    (.carousel (js/$ ".carousel") page-number)))

(defn navigate [token]
  (let [page-number (.indexOf tokens token)]
    (.carousel (js/$ ".carousel") page-number))
  (try (js/ga "send" "pageview" token) (catch js/Error e))
  (.setToken history token)
  (swap! app-state assoc :active-page [token]))

(events/listen history EventType/NAVIGATE #(navigate (.-token %)))
(.setUseFragment history false)
(.setPathPrefix history "")
(.setEnabled history true)
