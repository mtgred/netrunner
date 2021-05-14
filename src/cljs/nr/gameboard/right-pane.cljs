(ns nr.gameboard.right-pane
  (:require [cljs.core.async :refer [chan put!]]
            [clojure.string :as string]
            [nr.appstate :refer [app-state]]
            [nr.avatar :refer [avatar]]
            [nr.gameboard.log :refer [log-pane should-scroll scrolled-to-end?]]
            [nr.gameboard.replay :refer [notes-pane notes-shared-pane]]
            [nr.gameboard.state :refer [game-state not-spectator?]]
            [nr.help :refer [command-info]]
            [nr.translations :refer [tr]]
            [nr.utils :refer [influence-dot render-message]]
            [nr.ws :as ws]
            [reagent.core :as r]))

(defonce zoom-channel (chan))

(defonce log-mode (r/atom :log))

(defn resize-card-zoom []
  "Resizes the card zoom based on the values in the app-state"
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
      (.css "width" width))))

(defn log-resize [event ui]
  "Resize the card zoom to fit the available space"
  (let [width (.. ui -size -width)
        top (.. ui -position -top)]
    (println "ui:" ui)
    (swap! app-state assoc-in [:options :log-width] width)
    (swap! app-state assoc-in [:options :log-top] top)
    (.setItem js/localStorage "log-width" width)
    (.setItem js/localStorage "log-top" top)
    (resize-card-zoom)))

(defn log-start-resize [event ui]
  "Display a zoomed card when resizing so the user can visualize how the
  resulting zoom will look."
  (when-let [card (get-in @game-state [:runner :identity])]
    (put! zoom-channel card)))

(defn log-stop-resize [event ui]
  (put! zoom-channel false))

(defn log-selector []
  (fn []
    [:div.panel.panel-top.blue-shade.selector
     [:a {:on-click #(reset! log-mode :log)} (tr [:log.game-log "Game Log"])]
     " | "
     [:a {:on-click #(reset! log-mode :notes)} (tr [:log.annotating "Annotating"])]
     " | "
     [:a {:on-click #(reset! log-mode :notes-shared)} (tr [:log.shared "Shared Annotations"])]]))

(defn content-pane []
  (r/create-class
    (let [log (r/cursor game-state [:log])]
      {:display-name "content-pane"

       :component-did-mount
       (fn [this]
         (-> ".content-pane" js/$ (.resizable #js {:handles "w, n, nw"
                                                   :resize log-resize
                                                   :start log-start-resize
                                                   :stop log-stop-resize}))
         (resize-card-zoom))

       :reagent-render
       (fn []
         [:div.content-pane
          (case @log-mode
            :log
            [log-pane log zoom-channel]
            :notes
            [notes-pane]
            :notes-shared
            [notes-shared-pane])])})))
