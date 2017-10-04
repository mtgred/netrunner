(ns netrunner.stats
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [<!] :as async]
            [netrunner.appstate :refer [app-state]]
            [netrunner.auth :refer [authenticated] :as auth]
            [netrunner.ajax :refer [POST GET]]
            [goog.string :as gstring]
            [goog.string.format]))

(defn num->percent
  "Converts an input number to a percent of the second input number for display"
  [num1 num2]
  (gstring/format "%.0f" (* 100 (float (/ num1 num2)))))

(defn notnum->zero
  "Converts a non-positive-number value to zero.  Returns the value if already a number"
  [input]
  (if (pos? (int input)) input 0))

(defn clear-user-stats [owner]
  (authenticated
    (fn [user]
      (let [data (:user @app-state)]
        (try (js/ga "send" "event" "user" "clearuserstats") (catch js/Error e))
        (go (let [result (<! (POST "/user/clearstats" data :json))])
            (om/set-state! owner :games-started 0)
            (om/set-state! owner :games-completed 0)
            (om/set-state! owner :wins 0)
            (om/set-state! owner :loses 0)
            (om/set-state! owner :dnf 0))))))

(defn refresh-user-stats [owner]
  (authenticated
    (fn [user]
      (try (js/ga "send" "event" "user" "refreshstats") (catch js/Error e))
      (go (let [result (-> (<! (GET (str "/user"))) :json first :stats)]
            (om/set-state! owner :games-started (:games-started result))
            (om/set-state! owner :games-completed (:games-completed result))
            (om/set-state! owner :wins (:wins result))
            (om/set-state! owner :loses (:loses result))
            (om/set-state! owner :dnf (- (:games-started result) (:games-completed result))))))))

(defn stats-view [user owner]
  (reify
    om/IInitState
    (init-state [this] {:flash-message ""})

    om/IWillMount
    (will-mount [this]
      (om/set-state! owner :games-started (get-in @app-state [:stats :games-started]))
      (om/set-state! owner :games-completed (get-in @app-state [:stats :games-completed]))
      (om/set-state! owner :wins (get-in @app-state [:stats :wins]))
      (om/set-state! owner :loses (get-in @app-state [:stats :loses]))
      (om/set-state! owner :dnf (- (om/get-state owner :games-started) (om/get-state owner :games-completed))))

    om/IRenderState
    (render-state [this state]
      (sab/html
        (let [started (notnum->zero (om/get-state owner :games-started))
              completed (notnum->zero (om/get-state owner :games-completed))
              pc (notnum->zero (num->percent completed started))
              win (notnum->zero (om/get-state owner :wins))
              pw (notnum->zero (num->percent win started))
              lose (notnum->zero (om/get-state owner :loses))
              pl (notnum->zero (num->percent lose started))
              incomplete (notnum->zero (om/get-state owner :dnf))
              pi (notnum->zero (num->percent incomplete started))]
          [:div
            [:div.panel.blue-shade
             [:h2 "Game Stats"]
              [:section
               [:div "Started: " started]
               [:div "Completed: " completed " (" pc "%)"]
               [:div "Not completed: " incomplete  " (" pi "%)"]
               [:div "Won: " win  " (" pw "%)"]
               [:div "Lost: " lose  " (" pl "%)"]]
             [:div.button-bar
              [:button {:on-click #(refresh-user-stats owner)} "Refresh Stats"]
              [:button {:on-click #(clear-user-stats owner)} "Clear Stats"]]]])))))

(defn stats [{:keys [user]} owner]
  (om/component
    (when user
      (om/build stats-view user))))

(om/root stats app-state {:target (. js/document (getElementById "stats"))})