(ns nr.news
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [nr.ajax :refer [GET]]
            [nr.utils :refer [render-icons]]
            [nr.ws :refer [ws-send!]]
            [nr.appstate :refer [app-state]]
            [reagent.core :as r]))

(def news-state (r/atom {}))

(go (swap! news-state assoc :news (:json (<! (GET "/data/news")))))

(defn news []
  (r/with-let [news (r/cursor news-state [:news])
               active (r/cursor app-state [:active-page])]
    (when (or (= "/news" (first @active))
              (= "/" (first @active)))
      [:div.news-box.panel.blue-shade
       [:ul.list
        (doall
          (for [d @news]
            [:li.news-item
             {:key (:date d)}
             [:span.date (-> (:date d) js/Date. js/moment (.format "dddd MMM Do - HH:mm"))]
             [:span.title (render-icons (js/marked (:title d)))]]))]])))
