(ns netrunner.news
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [netrunner.ajax :refer [GET]]))

(def app-state (atom {}))

(go (swap! app-state assoc :news (:json (<! (GET "/data/news")))))

(defn news [cursor owner]
  (om/component
   (sab/html
    [:div.news-app
      [:div.news-box.panel.blue-shade
       [:h4 "News"]

       [:ul.list
        (for [d (:news cursor)]
          [:li.news-item 
            [:div.date
              (-> (:date d) js/Date. js/moment (.format "dddd MMM Do - HH:mm"))]
            [:div.title
              (:title d)]])]
       ]])))

(om/root news app-state {:target (. js/document (getElementById "news"))})