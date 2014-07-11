(ns netrunner.main
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [goog.events :as events])
  (:import goog.history.Html5History
           goog.history.EventType))

(def app-state (atom {:active-page "/"}))

(def tokens #js ["/" "/cards" "/play" "/deckbuilder" "/news"])

(def history (Html5History.))

(defn navigate [token]
  (let [page-number (.indexOf tokens token)]
    (.carousel (js/$ ".carousel") page-number))
  (try (js/ga "send" "pageview") (catch js/Error e))
  (.setToken history token)
  (swap! app-state assoc :active-page [token]))

(events/listen history EventType/NAVIGATE #(navigate (.-token %)))
(.setUseFragment history false)
(.setPathPrefix history "")
(.setEnabled history true)

(defn navbar [cursor owner]
  (om/component
   (sab/html
    [:ul.carousel-indicator {}
     (for [page [["Manabase" "/" 0]
                 ["Cards" "/cards" 1]
                 ["Play" "/play" 2]
                 ["Decks" "/deckbuilder" 3]
                 ["News" "/news" 4]]]
       (let [route (second page)]
         [:li {:class (if (= (first (:active-page cursor)) route) "active" "")
               :on-click #(.setToken history route)
               :data-target "#main" :data-slide-to (last page)}
          [:a {:href route} (first page)]]))])))

(om/root navbar app-state {:target (. js/document (getElementById "left-menu"))})

(defn handle-swipe [direction]
  (let [page (.indexOf tokens (first (:active-page @app-state)))]
    (if (= direction :left)
      (when (< (inc page) (count tokens))
        (navigate (nth tokens (inc page))))
      (when (> page 0)
        (navigate (nth tokens (dec page)))))))

(when (js* "\"ontouchstart\" in window")
  (-> ".carousel" js/$ (.swipeleft #(handle-swipe :left)))
  (-> ".carousel" js/$ (.swiperight #(handle-swipe :right))))
