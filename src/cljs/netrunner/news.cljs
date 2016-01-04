(ns netrunner.news
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [netrunner.cardbrowser :refer [add-symbols] :as cb]
            [netrunner.ajax :refer [GET]]))

(def app-state (atom {}))

(go (swap! app-state assoc :news (:json (<! (GET "/data/news")))))

(defn news [cursor owner]
  (om/component
   (sab/html
    [:div.news-box.panel.blue-shade
     [:ul.list
      (for [d (:news cursor)]
        [:li.news-item
         [:span.date (-> (:date d) js/Date. js/moment (.format "dddd MMM Do - HH:mm"))]
         [:span.title {:dangerouslySetInnerHTML #js {:__html (cb/add-symbols (js/marked (:title d)))}}]])]])))

(om/root news app-state {:target (. js/document (getElementById "news"))})
