(ns nr.replay-game
  (:require
    [nr.auth :refer [authenticated]]
    [nr.ws :as ws]
    [nr.translation-helpers :refer [tr]]
    [reagent.core :as r]))

(defn on-load [onload-ev]
  (let [replay (-> onload-ev .-target .-result)
        replay (js->clj (.parse js/JSON replay) :keywordize-keys true)
        history (:history replay)
        init-state (first history)
        init-state (assoc-in init-state [:options :spectatorhands] true)
        diffs (rest history)
        init-state (assoc init-state :replay-diffs diffs :gameid "local-replay")]
    (ws/event-msg-handler-wrapper
      {:id :game/start
       :?data (.stringify js/JSON (clj->js init-state))})))

(defn start-replay [replay-file]
  (let [reader (js/FileReader.)]
    (aset reader "onload" on-load)
    (.readAsText reader replay-file)))

(defn create-game [state]
  (authenticated
    (fn [_]
      (if-let [replay-file (:replay-file @state)]
        (do (swap! state assoc :flash-message nil)
            (start-replay replay-file))
        (swap! state assoc :flash-message true)))))

(defn start-replay-div [_lobby-state]
  (r/with-let [state (r/atom {:replay-file nil
                              :flash-message nil})]
    (fn [lobby-state]
      [:div
       [:div.button-bar
        [:button {:type "button"
                  :on-click #(do (.preventDefault %)
                                 (create-game state))}
         (tr [:lobby.start-replay "Start replay"])]
        [:button {:type "button"
                  :on-click #(do (.preventDefault %)
                                 (swap! lobby-state dissoc :replay))}
         (tr [:lobby.cancel "Cancel"])]]
       (when (:flash-message @state)
         [:p.flash-message
          (tr [:lobby.replay-invalid-file "Select a valid replay file."])])
       [:div [:input {:field :file
                      :type :file
                      :on-change #(swap! state assoc :replay-file (aget (.. % -target -files) 0))}]]])))
