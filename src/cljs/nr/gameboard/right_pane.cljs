(ns nr.gameboard.right-pane
  (:require [cljs.core.async :refer [put!]]
            [nr.appstate :refer [app-state]]
            [nr.gameboard.card-preview :refer [zoom-channel]]
            [nr.gameboard.diagrams :refer [run-timing-pane turn-timing-pane]]
            [nr.gameboard.log :refer [log-pane]]
            [nr.gameboard.replay :refer [notes-pane notes-shared-pane]]
            [nr.gameboard.state :refer [game-state]]
            [nr.gameboard.settings :refer [settings-pane]]
            [nr.translations :refer [tr]]
            [reagent.core :as r]))

(defonce loaded-tabs (r/atom {}))
(defonce available-tabs
  {:log
   {:hiccup [log-pane]
    :label (tr [:log.game-log "Game Log"])}

   :notes
   {:hiccup [notes-pane]
    :label (tr [:log.annotating "Annotating"])}

   :notes-shared
   {:hiccup [notes-shared-pane]
    :label (tr [:log.shared "Shared Annotations"])}

   :run-timing
   {:hiccup [run-timing-pane]
    :label (tr [:log.run-timing "Run Timing"])}

   :turn-timing
   {:hiccup [turn-timing-pane]
    :label (tr [:log.turn-timing "Turn Timing"])}

   :settings
   {:hiccup [settings-pane]
    :label (tr [:log.settings "Settings"])}})

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
        (.css "height" max-card-height)))
    (-> ".right-pane" js/$ (.css "width" width))
    (-> ".content-pane" js/$
      (.css "left" 0)
      (.css "top" top)
      (.css "height" "auto")
      (.css "width" width))))

(defn- pane-resize [event ui]
  "Resize the card zoom to fit the available space"
  (let [width (.. ui -size -width)
        top (.. ui -position -top)]
    (swap! app-state assoc-in [:options :log-width] width) ;;XXX: rename
    (swap! app-state assoc-in [:options :log-top] top)
    (.setItem js/localStorage "log-width" width)
    (.setItem js/localStorage "log-top" top)
    (resize-card-zoom)))

(defn- pane-start-resize [event ui]
  "Display a zoomed card when resizing so the user can visualize how the
  resulting zoom will look."
  (when-let [card (get-in @game-state [:runner :identity])]
    (put! zoom-channel card)))

(defn- pane-stop-resize [event ui]
  (put! zoom-channel false))

(defn- tab-selector [selected-tab]
  (fn []
    [:div.panel.panel-top.blue-shade.selector
     (doall (for [[tab {:keys [label]}] (seq @loaded-tabs)]
              [:a {:key tab
                   :on-click #(reset! selected-tab tab)} label]))]))

(defn load-tab [tab]
  (let [{:keys [hiccup label]}
        (get available-tabs tab
             {:hiccup [:div.error "This should not happen"]
              :label "???"})]
    (swap! loaded-tabs assoc tab {:hiccup hiccup :label label})))

(defn unload-tab [tab]
  (swap! loaded-tabs dissoc tab))

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
         (-> ".content-pane" js/$ (.resizable #js {:handles "w, n, nw"
                                                   :resize pane-resize
                                                   :start pane-start-resize
                                                   :stop pane-stop-resize}))
         (resize-card-zoom))

       :reagent-render
       (fn []
         [:div.content-pane
          [tab-selector selected-tab]
          [:div.panel.blue-shade.panel-bottom.content
           (get-in @loaded-tabs [@selected-tab :hiccup] "nothing here")]])})))
