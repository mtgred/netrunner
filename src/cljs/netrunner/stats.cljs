(ns netrunner.stats
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.appstate :refer [app-state]]
            [netrunner.deckbuilder :refer [process-decks num->percent]]
            [netrunner.auth :refer [authenticated] :as auth]
            [netrunner.ajax :refer [POST GET]]
            [goog.string :as gstring]
            [goog.string.format]))

(def stats-channel (chan))
(def stats-socket (.connect js/io (str js/iourl "/stats")))
(.on stats-socket "netrunner" #(put! stats-channel (js->clj % :keywordize-keys true)))

(defn notnum->zero
  "Converts a non-positive-number value to zero.  Returns the value if already a number"
  [input]
  (if (pos? (int input)) input 0))

(defn clear-user-stats []
  (authenticated
    (fn [user]
      (let [data (:user @app-state)]
        (try (js/ga "send" "event" "user" "clearuserstats") (catch js/Error e))
        (go (let [result (<! (POST "/user/clearstats" data :json))]
              (swap! app-state assoc :stats result)))))))

;; Go loop to receive messages from node server to refresh stats on game-end
(go (while true
      (let [msg (<! stats-channel)
            result (-> (<! (GET "/user")) :json first :stats)
            decks (process-decks (:json (<! (GET "/data/decks"))))]
        (swap! app-state assoc :stats result)
        (swap! app-state assoc :decks decks))))

(defn stat-view [{:keys [start-key complete-key win-key lose-key stats]} owner]
  (om/component
    (sab/html
      (let [started (notnum->zero (start-key stats))
            completed (notnum->zero (complete-key stats))
            pc (notnum->zero (num->percent completed started))
            win (notnum->zero (win-key stats))
            lose (notnum->zero (lose-key stats))
            pw (notnum->zero (num->percent win (+ win lose)))
            pl (notnum->zero (num->percent lose (+ win lose)))
            incomplete (notnum->zero (- started completed))
            pi (notnum->zero (num->percent incomplete started))]
        [:section
         [:div "Started: " started]
         [:div "Completed: " completed " (" pc "%)"]
         [:div "Not completed: " incomplete  " (" pi "%)"]
         (when-not (= "none" (get-in @app-state [:options :gamestats]))
           [:div [:div "Won: " win  " (" pw "%)"]
            [:div "Lost: " lose  " (" pl "%)"]])]))))

(defn stats [{:keys [stats] :as cursor} owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (sab/html
        [:div.blue-shade.content-page.panel
         [:div
          [:div
           [:h3 "Game Stats"]
           (om/build stat-view {:stats stats
                                :start-key :games-started :complete-key :games-completed
                                :win-key :wins :lose-key :loses})]
          [:div
           [:h3 "Corp Stats"]
           (om/build stat-view {:stats stats
                                :start-key :games-started-corp :complete-key :games-completed-corp
                                :win-key :wins-corp :lose-key :loses-corp})]
          [:div
           [:h3 "Runner Stats"]
           (om/build stat-view {:stats stats
                                :start-key :games-started-runner :complete-key :games-completed-runner
                                :win-key :wins-runner :lose-key :loses-runner})]]
         [:p
          [:button {:on-click #(clear-user-stats)} "Clear Stats"]]]))))

(om/root stats app-state {:target (. js/document (getElementById "stats"))})
