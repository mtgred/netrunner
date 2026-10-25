(ns nr.gameboard.pane-size
  (:require [jinteki.settings :as settings]
            [nr.appstate :refer [app-state]]
            [nr.local-storage :as ls]))

(defn resize-card-zoom
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

(defn set-pane-size! [width top]
  (swap! app-state update :options assoc :log-width width :log-top top)
  (ls/save! "log-width" width)
  (ls/save! "log-top" top)
  (resize-card-zoom))

(defn reset-pane-size! []
  (let [{:keys [log-width log-top]} (settings/defaults)]
    (set-pane-size! log-width log-top)))
