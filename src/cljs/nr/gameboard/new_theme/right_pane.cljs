(ns nr.gameboard.new-theme.right-pane
  (:require [nr.appstate :refer [app-state]]
            [nr.gameboard.log :refer [log-pane-new-theme]]
            [nr.gameboard.replay :refer [notes-pane notes-shared-pane]]
            [nr.gameboard.settings :refer [settings-pane]]
            [nr.translations :refer [tr]]
            [reagent.core :as r]))

(defonce loaded-tabs (r/atom {}))
(defonce available-tabs
  {:log
   {:hiccup [log-pane-new-theme]
    :label (tr [:log.game-log "Game Log"])}

   :notes
   {:hiccup [notes-pane]
    :label (tr [:log.annotating "Annotating"])}

   :notes-shared
   {:hiccup [notes-shared-pane]
    :label (tr [:log.shared "Shared Annotations"])}

   :settings
   {:hiccup [settings-pane]
    :label (tr [:log.settings "Settings"])}

   :none {:hiccup [:div] :label (tr [:settings.none "Minimize"])}})

(defn- resize-card-zoom
  "Resizes the card zoom based on the values in the app-state"
  []
  (let [width (get-in @app-state [:options :log-width])
        top (get-in @app-state [:options :log-top])
        max-card-width (- width 5)
        max-card-height (- top 10)
        card-ratio (/ 418 300)]
    (if (> (/ max-card-height max-card-width) card-ratio)
      (-> ".card-zoom" js/$
          (.css "width" max-card-width)
          (.css "height" (int (* max-card-width card-ratio))))
      (-> ".card-zoom" js/$
          (.css "width" (int (/ max-card-height card-ratio)))
          (.css "height" max-card-height)))))

(defn- tab-selector [selected-tab]
  (fn []
    [:div.panel.panel-top.blue-shade.selector
     (doall (for [[tab {:keys [label]}] (seq @loaded-tabs)]
              [:a {:key tab
                   :class (when (= @selected-tab tab) "active")
                   :on-click #(reset! selected-tab tab)} label]))]))

(defn load-tab [tab]
  (let [{:keys [hiccup label]}
        (get available-tabs tab
             {:hiccup [:div.error "This should not happen"]
              :label "???"})]
    (swap! loaded-tabs assoc tab {:hiccup hiccup :label label})))

(defn clear-tabs []
  (reset! loaded-tabs {}))

(defn content-pane [& tabs]
  (let [selected-tab (r/atom nil)]
    (clear-tabs)
    (doseq [tab tabs]
      (load-tab tab))
    (reset! selected-tab (first tabs))
    (r/create-class
     {:display-name "content-pane"

      :component-did-mount
      (fn [this]
        (resize-card-zoom))

      :reagent-render
      (fn []
        [:div.content-pane {:style {:width (get-in @app-state [:options :log-width])}}
         [tab-selector selected-tab]
         [:div.panel {:class [:blue-shade :panel-bottom :content @selected-tab]}
          (get-in @loaded-tabs [@selected-tab :hiccup] "nothing here")]])})))
