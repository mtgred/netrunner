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

(defn clear-user-stats [owner]
  (authenticated
    (fn [user]
      (let [data (:user @app-state)]
        (try (js/ga "send" "event" "user" "clearuserstats") (catch js/Error e))
        (go (let [result (<! (POST "/user/clearstats" data :json))]
              (swap! app-state assoc :stats result)))))))

;; Go loop to receive messages from node server to refresh stats on game-end
(go (while true
      (let [msg (<! stats-channel)
            result (-> (<! (GET (str "/user"))) :json first :stats)
            decks (process-decks (:json (<! (GET (str "/data/decks")))))]
        (try (js/ga "send" "event" "user" "refreshstats") (catch js/Error e))
        (swap! app-state assoc :stats result)
        (swap! app-state assoc :decks decks))))

(defn stats [{:keys [stats] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this] {:flash-message ""})

    om/IRenderState
    (render-state [this state]
      (sab/html
       (let [started (notnum->zero (:games-started stats))
              completed (notnum->zero (:games-completed stats))
              pc (notnum->zero (num->percent completed started))
              win (notnum->zero (:wins stats))
              lose (notnum->zero (:loses stats))
              pw (notnum->zero (num->percent win (+ win lose)))
              pl (notnum->zero (num->percent lose (+ win lose)))
              incomplete (notnum->zero (- started completed))
              pi (notnum->zero (num->percent incomplete started))
              ;; Corp Stats
              started-corp (notnum->zero (:games-started-corp stats))
              completed-corp (notnum->zero (:games-completed-corp stats))
              pc-corp (notnum->zero (num->percent completed-corp started-corp))
              win-corp (notnum->zero (:wins-corp stats))
              lose-corp (notnum->zero (:loses-corp stats))
              pw-corp (notnum->zero (num->percent win-corp (+ win-corp lose-corp)))
              pl-corp (notnum->zero (num->percent lose-corp (+ win-corp lose-corp)))
              incomplete-corp (notnum->zero (- started-corp completed-corp))
              pi-corp (notnum->zero (num->percent incomplete-corp started-corp))
              ;; Runner Stats
              started-runner (notnum->zero (:games-started-runner stats))
              completed-runner (notnum->zero (:games-completed-runner stats))
              pc-runner (notnum->zero (num->percent completed-runner started-runner))
              win-runner (notnum->zero (:wins-runner stats))
              lose-runner (notnum->zero (:loses-runner stats))
              pw-runner (notnum->zero (num->percent win-runner (+ win-runner lose-runner)))
              pl-runner (notnum->zero (num->percent lose-runner (+ win-runner lose-runner)))
              incomplete-runner (notnum->zero (- started-runner completed-runner))
              pi-runner (notnum->zero (num->percent incomplete-runner started-runner))]
         [:div.blue-shade.content-page.panel
          [:div
           [:div
            [:h3 "Game Stats"]
            [:section
             [:div "Started: " started]
             [:div "Completed: " completed " (" pc "%)"]
             [:div "Not completed: " incomplete  " (" pi "%)"]
             (when-not (= "none" (get-in @app-state [:options :gamestats]))
               [:div [:div "Won: " win  " (" pw "%)"]
                [:div "Lost: " lose  " (" pl "%)"]])]]
           [:div
            [:h3 "Corp Stats"]
            [:section
             [:div "Started: " started-corp]
             [:div "Completed: " completed-corp " (" pc-corp "%)"]
             [:div "Not completed: " incomplete-corp  " (" pi-corp "%)"]
             (when-not (= "none" (get-in @app-state [:options :gamestats]))
               [:div [:div "Won: " win-corp  " (" pw-corp "%)"]
                [:div "Lost: " lose-corp  " (" pl-corp "%)"]])]]
           [:div
            [:h3 "Runner Stats"]
            [:section
             [:div "Started: " started-runner]
             [:div "Completed: " completed-runner " (" pc-runner "%)"]
             [:div "Not completed: " incomplete-runner  " (" pi-runner "%)"]
             (when-not (= "none" (get-in @app-state [:options :gamestats]))
               [:div [:div "Won: " win-runner  " (" pw-runner "%)"]
                [:div "Lost: " lose-runner  " (" pl-runner "%)"]])]]]

          [:p
           [:button {:on-click #(clear-user-stats owner)} "Clear Stats"]]])))))

(om/root stats app-state {:target (. js/document (getElementById "stats"))})

