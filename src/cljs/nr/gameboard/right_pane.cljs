(ns nr.gameboard.right-pane
  (:require [cljs.core.async :refer [put!]]
            [jinteki.utils :refer [clamp]]
            [nr.appstate :refer [app-state]]
            [nr.gameboard.card-preview :refer [zoom-channel]]
            [nr.gameboard.diagrams :refer [run-timing-pane turn-timing-pane]]
            [nr.gameboard.log :refer [log-pane]]
            [nr.gameboard.pane-size :refer [resize-card-zoom reset-pane-size! set-pane-size!]]
            [nr.gameboard.replay :refer [notes-pane notes-shared-pane]]
            [nr.gameboard.state :refer [game-state]]
            [nr.gameboard.settings :refer [settings-pane]]
            [nr.translations :refer [tr-span]]
            [reagent.core :as r]))

(defonce loaded-tabs (r/atom {}))
(defonce available-tabs
  {:log
   {:hiccup [log-pane]
    :label [:log_game-log "Game Log"]}

   :notes
   {:hiccup [notes-pane]
    :label [:log_annotating "Annotating"]}

   :notes-shared
   {:hiccup [notes-shared-pane]
    :label [:log_shared "Shared Annotations"]}

   :run-timing
   {:hiccup [run-timing-pane]
    :label [:log_run-timing "Run Timing"]}

   :turn-timing
   {:hiccup [turn-timing-pane]
    :label [:log_turn-timing "Turn Timing"]}

   :settings
   {:hiccup [settings-pane]
    :label [:log_settings "Settings"]}})

(defonce ^:private resize-state (atom nil))

(defonce ^:private last-resize-down (atom 0))

(defn- pane-start-resize
  "Display a zoomed card when resizing so the user can visualize how the
  resulting zoom will look. A double press resets the pane to its default size."
  [dir e]
  (.preventDefault e)
  (let [now (.-timeStamp e)
        double-press? (< (- now @last-resize-down) 350)]
    (reset! last-resize-down now)
    (if double-press?
      (reset-pane-size!)
      (do (.setPointerCapture (.-currentTarget e) (.-pointerId e))
          (reset! resize-state {:dir dir
                                :start-x (.-clientX e)
                                :start-y (.-clientY e)
                                :start-width (get-in @app-state [:options :log-width])
                                :start-top (get-in @app-state [:options :log-top])})
          (when-let [card (get-in @game-state [:runner :identity])]
            (put! zoom-channel card))))))

(defn- pane-resize [e]
  (when-let [{:keys [dir start-x start-y start-width start-top]} @resize-state]
    (let [width (if (#{:w :nw} dir)
                  (clamp (- start-width (- (.-clientX e) start-x))
                         100 (- (.-innerWidth js/window) 300))
                  start-width)
          top (if (#{:n :nw} dir)
                (clamp (+ start-top (- (.-clientY e) start-y))
                       0 (- (.-innerHeight js/window) 100))
                start-top)]
      (set-pane-size! width top))))

(defn- pane-stop-resize [_e]
  (when @resize-state
    (reset! resize-state nil)
    (put! zoom-channel false)))

(defn- resize-handle [dir]
  [:div {:class ["resize-handle" (str "resize-handle-" (name dir))]
         :on-pointer-down #(pane-start-resize dir %)
         :on-pointer-move pane-resize
         :on-pointer-up pane-stop-resize
         :on-pointer-cancel pane-stop-resize}])

(defn- tab-selector [selected-tab]
  [:div.panel.panel-top.blue-shade.selector
   (doall (for [[tab {:keys [label]}] (seq @loaded-tabs)]
            [:a {:key tab
                 :on-click #(reset! selected-tab tab)}
             [tr-span label]]))])

(defn load-tab [tab]
  (let [{:keys [hiccup label]}
        (get available-tabs tab
             {:hiccup [:div.error "This should not happen"]
              :label [:log_unknown "???"]})]
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
       (fn [_this]
         (resize-card-zoom))

       :reagent-render
       (fn []
         [:div.content-pane
          [resize-handle :w]
          [resize-handle :n]
          [resize-handle :nw]
          [tab-selector selected-tab]
          [:div.panel.blue-shade.panel-bottom.content
           (get-in @loaded-tabs [@selected-tab :hiccup] "nothing here")]])})))
