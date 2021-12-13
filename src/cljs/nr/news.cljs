(ns nr.news
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<!]]
   [nr.ajax :refer [GET]]
   [nr.utils :refer [day-word-with-time-formatter format-zoned-date-time
                     render-icons]]
   [reagent.core :as r]))

(def news-state (r/atom {}))

(go (swap! news-state assoc :news (:json (<! (GET "/data/news")))))

(defn news []
  (r/with-let [news (r/cursor news-state [:news])]
    [:div#news.news-box.panel.blue-shade
     (into
       [:ul.list]
       (for [d @news]
         [:li.news-item
          {:key (:date d)}
          [:span.date (format-zoned-date-time day-word-with-time-formatter (str (:date d) "Z"))]
          [:span.title (render-icons (:item d ""))]]))]))
