(ns nr.news
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<!]]
   [nr.ajax :refer [GET]]
   [nr.appstate :refer [app-state]]
   [nr.utils :refer [render-icons]]
   [reagent.core :as r]))

(def news-state (r/atom {}))

(go (swap! news-state assoc :news (:json (<! (GET "/data/news")))))

(defn news []
  (r/with-let [news (r/cursor news-state [:news])
               active (r/cursor app-state [:active-page])]
    [:div#news.news-box.panel.blue-shade
     (when (or (= "/news" (first @active))
               (= "/" (first @active)))
       [:ul.list
        (doall
          (for [d @news]
            [:li.news-item
             {:key (:date d)}
             [:span.date (-> (:date d) js/Date. js/moment (.format "dddd MMM Do - HH:mm"))]
             [:span.title (render-icons (:item d ""))]]))])]))
