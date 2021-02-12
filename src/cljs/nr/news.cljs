(ns nr.news
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [nr.ajax :refer [GET]]
            [nr.utils :refer [render-icons]]
            [nr.ws :refer [ws-send!]]
            [nr.appstate :refer [app-state]]
            [nr.translations :refer [tr]]
            [reagent.core :as r]))

(def news-state (r/atom {}))

(go (swap! news-state assoc :news (:json (<! (GET "/data/news")))))

(defn news []
  (r/with-let [news (r/cursor news-state [:news])
               active (r/cursor app-state [:active-page])]
    (when (or (= "/news" (first @active))
              (= "/" (first @active)))
      [:div#news.news-box.panel.blue-shade
       [:ul.list
        (doall
          (for [d @news]
            [:li.news-item
             {:key (:date d)}
             [:span.date (-> (:date d) js/Date. js/moment (.format "dddd MMM Do - HH:mm"))]
             [:span.title (render-icons (:item d ""))]]))]])))

(defn changelog []
  (fn []
    (let [stuff [{:level :big
                  :type :feature
                  :author "lostgeek"
                  :prlink "https://github.com/mtgred/netrunner/pull/4326"
                  :prnumber 4326
                  :title "Paying multiple costs is now bundled together"
                  :description "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est."}]]
      [:div.changelog
       (doall
         (for [[level group] (group-by :level stuff)]
           [:div.changelog-level
            [:h3 (case level
                   :big "Big changes"
                   :notable "Notable changes"
                   :small "Small changes")]
            (for [entry group]
              [:div.changelog-entry
               [:h4 (:title entry)]
               [:ul.metainfo
                [:li.pr "GitHub " [:a {:href (:prlink entry)} "#" (:prnumber entry)]]
                [:li.type "Type: " (case (:type entry)
                                     :feature "Feature")]
                [:li.author "Author: " (:author entry)]
                ]
               [:p.description (:description entry)]])]))])))

(defn news-page []
  (let [active (r/cursor app-state [:active-page])]
    (fn []
      (when (= "/" (first @active))
        [:div.page-container
         [:h1 (tr [:news.title "Play Android: Netrunner in your browser"])]
         [news]
         [:div.panel.content-page.blue-shade
          [:h2 "Recent changes"]
          [changelog]]
         [:div#version [:span (str "Version " (:app-version @app-state "Unknown"))]]]))))
