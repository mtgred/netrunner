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
            (om/set-state! owner :dnf 0)
            ; Corp
            (om/set-state! owner :games-started-corp 0)
            (om/set-state! owner :games-completed-corp 0)
            (om/set-state! owner :wins-corp 0)
            (om/set-state! owner :loses-corp 0)
            (om/set-state! owner :dnf-corp 0)
            ; Runner
            (om/set-state! owner :games-started-runner 0)
            (om/set-state! owner :games-completed-runner 0)
            (om/set-state! owner :wins-runner 0)
            (om/set-state! owner :loses-runner 0)
            (om/set-state! owner :dnf-runner 0))))))

(defn refresh-user-stats [owner]
  (authenticated
    (fn [user]
      (try (js/ga "send" "event" "user" "refreshstats") (catch js/Error e))
      (go (let [result (-> (<! (GET (str "/user"))) :json first :stats)]
            (om/set-state! owner :games-started (:games-started result))
            (om/set-state! owner :games-completed (:games-completed result))
            (om/set-state! owner :wins (:wins result))
            (om/set-state! owner :loses (:loses result))
            (om/set-state! owner :dnf (- (:games-started result) (:games-completed result)))
            ; Corp
            (om/set-state! owner :games-started-corp (:games-started-corp result))
            (om/set-state! owner :games-completed-corp (:games-completed-corp result))
            (om/set-state! owner :wins-corp (:wins-corp result))
            (om/set-state! owner :loses-corp (:loses-corp result))
            (om/set-state! owner :dnf-corp (- (:games-started-corp result) (:games-completed-corp result)))
            ; Runner
            (om/set-state! owner :games-started-runner (:games-started-runner result))
            (om/set-state! owner :games-completed-runner (:games-completed-runner result))
            (om/set-state! owner :wins-runner (:wins-runner result))
            (om/set-state! owner :loses-runner (:loses-runner result))
            (om/set-state! owner :dnf-runner (- (:games-started-runner result) (:games-completed-runner result))))))))

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
      (om/set-state! owner :dnf (- (om/get-state owner :games-started) (om/get-state owner :games-completed)))
      ;; Corp
      (om/set-state! owner :games-started-corp (get-in @app-state [:stats :games-started-corp]))
      (om/set-state! owner :games-completed-corp (get-in @app-state [:stats :games-completed-corp]))
      (om/set-state! owner :wins-corp (get-in @app-state [:stats :wins-corp]))
      (om/set-state! owner :loses-corp (get-in @app-state [:stats :loses-corp]))
      (om/set-state! owner :dnf-corp (- (om/get-state owner :games-started-corp) (om/get-state owner :games-completed-corp)))
      ;; Runner
      (om/set-state! owner :games-started-runner (get-in @app-state [:stats :games-started-runner]))
      (om/set-state! owner :games-completed-runner (get-in @app-state [:stats :games-completed-runner]))
      (om/set-state! owner :wins-runner (get-in @app-state [:stats :wins-runner]))
      (om/set-state! owner :loses-runner (get-in @app-state [:stats :loses-runner]))
      (om/set-state! owner :dnf-runner (- (om/get-state owner :games-started-runner) (om/get-state owner :games-completed-runner))))

    om/IRenderState
    (render-state [this state]
      (sab/html
        (let [started (notnum->zero (om/get-state owner :games-started))
              completed (notnum->zero (om/get-state owner :games-completed))
              pc (notnum->zero (num->percent completed started))
              win (notnum->zero (om/get-state owner :wins))
              lose (notnum->zero (om/get-state owner :loses))
              pw (notnum->zero (num->percent win (+ win lose)))
              pl (notnum->zero (num->percent lose (+ win lose)))
              incomplete (notnum->zero (om/get-state owner :dnf))
              pi (notnum->zero (num->percent incomplete started))
              ;; Corp Stats
              started-corp (notnum->zero (om/get-state owner :games-started-corp))
              completed-corp (notnum->zero (om/get-state owner :games-completed-corp))
              pc-corp (notnum->zero (num->percent completed-corp started-corp))
              win-corp (notnum->zero (om/get-state owner :wins-corp))
              lose-corp (notnum->zero (om/get-state owner :loses-corp))
              pw-corp (notnum->zero (num->percent win-corp (+ win-corp lose-corp)))
              pl-corp (notnum->zero (num->percent lose-corp (+ win-corp lose-corp)))
              incomplete-corp (notnum->zero (om/get-state owner :dnf-corp))
              pi-corp (notnum->zero (num->percent incomplete-corp started-corp))
              ;; Runner Stats
              started-runner (notnum->zero (om/get-state owner :games-started-runner))
              completed-runner (notnum->zero (om/get-state owner :games-completed-runner))
              pc-runner (notnum->zero (num->percent completed-runner started-runner))
              win-runner (notnum->zero (om/get-state owner :wins-runner))
              lose-runner (notnum->zero (om/get-state owner :loses-runner))
              pw-runner (notnum->zero (num->percent win-runner (+ win-runner lose-runner)))
              pl-runner (notnum->zero (num->percent lose-runner (+ win-runner lose-runner)))
              incomplete-runner (notnum->zero (om/get-state owner :dnf-runner))
              pi-runner (notnum->zero (num->percent incomplete-runner started-runner))]
          [:div.blue-shade.panel.stats-main
            [:div.stats-left
             [:h2 "Game Stats"]
              [:section
               [:div "Started: " started]
               [:div "Completed: " completed " (" pc "%)"]
               [:div "Not completed: " incomplete  " (" pi "%)"]
               (if (get-in @app-state [:options :gamestats])
                 [:div [:div "Won: " win  " (" pw "%)"]
                  [:div "Lost: " lose  " (" pl "%)"]]
                 [:div [:br] [:br]])]
             [:div.button-bar
              [:button {:on-click #(refresh-user-stats owner)} "Refresh Stats"]
              [:button {:on-click #(clear-user-stats owner)} "Clear Stats"]]]
           [:div.stats-middle
            [:h2 "Corp Stats"]
            [:section
             [:div "Started: " started-corp]
             [:div "Completed: " completed-corp " (" pc-corp "%)"]
             [:div "Not completed: " incomplete-corp  " (" pi-corp "%)"]
             (when (get-in @app-state [:options :gamestats])
               [:div [:div "Won: " win-corp  " (" pw-corp "%)"]
                [:div "Lost: " lose-corp  " (" pl-corp "%)"]])]]
           [:div.stats-right
            [:h2 "Runner Stats"]
            [:section
             [:div "Started: " started-runner]
             [:div "Completed: " completed-runner " (" pc-runner "%)"]
             [:div "Not completed: " incomplete-runner  " (" pi-runner "%)"]
             (when (get-in @app-state [:options :gamestats])
               [:div [:div "Won: " win-runner  " (" pw-runner "%)"]
                [:div "Lost: " lose-runner  " (" pl-runner "%)"]])]]])))))

(defn stats [{:keys [user]} owner]
  (om/component
    (when user
      (om/build stats-view user))))

(om/root stats app-state {:target (. js/document (getElementById "stats"))})