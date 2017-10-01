(ns netrunner.stats
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [<!] :as async]
            [netrunner.appstate :refer [app-state]]
            [netrunner.auth :refer [authenticated] :as auth]
            [netrunner.ajax :refer [POST GET]]))


(defn clear-user-stats [owner]
  (authenticated
    (fn [user]
      (let [data (dissoc (:user @app-state) :stats)]
        (try (js/ga "send" "event" "user" "clearuserstats") (catch js/Error e))
        (go (let [result (<! (POST "/clearuserstats" data :json))])
            (om/set-state! owner :games-started 0)
            (om/set-state! owner :games-completed 0)
            (om/set-state! owner :wins 0)
            (om/set-state! owner :loses 0)
            (om/set-state! owner :dnf 0))))))

(defn refresh-user-stats [owner]
  (authenticated
    (fn [user]
      (let [data (:user @app-state)]
        (try (js/ga "send" "event" "user" "refreshuserstats") (catch js/Error e))
        (go (let [result (<! (GET (str "/getuserstats" data :json)))]))))))

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
        [:div
          [:div.panel.blue-shade
           [:h2 "Game Stats"]
            [:section
             [:div "Games Started: "(om/get-state owner :games-started)]
             [:div "Games Completed: "(om/get-state owner :games-completed)]
             [:div "Win: "(om/get-state owner :wins)]
             [:div "Lose: "(om/get-state owner :loses)]
             [:div "Incomplete: "(om/get-state owner :dnf)]]]
        [:div.button-bar
         [:button {:on-click #(refresh-user-stats owner)} "Refresh Stats"]
         [:button {:on-click #(clear-user-stats owner)} "Clear Stats"]]]))))

(defn stats [{:keys [user]} owner]
  (om/component
    (when user
      (om/build stats-view user))))

(om/root stats app-state {:target (. js/document (getElementById "stats"))})



