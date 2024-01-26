(ns nr.main
  (:require
   [nr.appstate :refer [app-state]]
   [nr.auth :refer [auth-forms auth-menu]]
   [nr.navbar :refer [navbar navigate]]
   [nr.routes :as routes]
   [nr.status-bar :refer [status]]
   [nr.ws :refer [start-router! lobby-updates-pause! lobby-updates-continue!]]
   [reagent-modals.modals :as reagent-modals]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [time-literals.data-readers]
   [time-literals.read-write]))

(time-literals.read-write/print-time-literals-cljs!)

(defn- get-server-data
  [tag]
  (-> (.getElementById js/document "server-originated-data")
      (.getAttribute (str "data-" tag))))

(defn pages []
  (r/with-let
    [visibilitychange-fn (fn []
                           (if (identical? (.-visibilityState js/document) "visible")
                             (lobby-updates-continue!)
                             (lobby-updates-pause!)))]
    (r/create-class
      {:display-name "main-pages"
       :component-did-mount
       (fn []
         (let [ver (get-server-data "version")
               rid (get-server-data "replay-id")]
           (swap! app-state assoc :app-version ver)
           (swap! app-state assoc :replay-id rid)
           (when rid
             (navigate "/play"))
           (-> js/document (.addEventListener
                             "visibilitychange"
                             visibilitychange-fn))))
       :component-will-unmount
       (fn []
         (-> js/document (.removeEventListener
                           "visibilitychange"
                           visibilitychange-fn)))
       :reagent-render
       (fn []
         [:div#main
          [:div.item
           [(-> @routes/current-view :data :view)]]])})))

(defn main-window []
  [:<>
   [:nav.topnav.blue-shade
    [:div#left-menu [navbar]]
    [:div#right-menu [auth-menu]]
    [:div#status [status]]]
   [:div#auth-forms [auth-forms]]
   [reagent-modals/modal-window]
   [pages]])

(defn mount []
  (rdom/render [main-window] (.getElementById js/document "main-content")))

(defn init! []
  (routes/init-routes!)
  (start-router!)
  (mount))
