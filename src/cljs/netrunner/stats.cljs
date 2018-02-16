(ns netrunner.stats
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.appstate :refer [app-state]]
            [netrunner.deckbuilder :refer [process-decks num->percent]]
            [netrunner.auth :refer [authenticated] :as auth]
            [netrunner.ajax :refer [GET DELETE]]
            [netrunner.ws :as ws]
            [goog.string :as gstring]
            [goog.string.format]))

(defn update-deck-stats
  "Update the local app-state with a new version of deck stats"
  [deck-id stats]
  (let [deck (first (filter #(= (:_id %) deck-id) (:decks @app-state)))
        deck (assoc deck :stats stats)
        others (remove #(= (:_id %) deck-id) (:decks @app-state))]
    (swap! app-state assoc :decks (conj others deck))))

(ws/register-ws-handler!
  :stats/update
  #(do (swap! app-state assoc :stats (-> % :userstats))
       (update-deck-stats (-> % :deck-id) (-> % :deckstats))))

(defn notnum->zero
  "Converts a non-positive-number value to zero.  Returns the value if already a number"
  [input]
  (if (pos? (int input)) input 0))

(defn clear-user-stats []
  (authenticated
    (fn [user]
      (let [id (get-in @app-state [:user :_id])]
        (try (js/ga "send" "event" "user" "clearuserstats") (catch js/Error e))
        (go (let [result (<! (DELETE "/profile/stats/user"))]
              (swap! app-state assoc :stats result)))))))

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
