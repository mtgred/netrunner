(ns nr.gameboard.actions
  (:require
   [differ.core :as differ]
   [nr.angel-arena.lobby :as angel-arena]
   [nr.appstate :refer [app-state current-gameid]]
   [nr.gameboard.replay :refer [init-replay]]
   [nr.gameboard.state :refer [check-lock? game-state get-side last-state
                               parse-state]]
   [nr.translations :refer [tr]]
   [nr.utils :refer [toastr-options]]
   [nr.ws :as ws]
   [reagent.core :as r]
   [reagent.ratom :as ratom]))

(defn reset-game! [state]
  (reset! game-state (assoc state :side (get-side state)))
  (reset! last-state @game-state)
  (reset! ws/lock false))

(defn init-game! [state]
  (let [side (get-side state)]
    (reset! game-state (dissoc state :replay-diffs :replay-jump-to))
    (swap! game-state assoc :side side)
    (reset! last-state @game-state)
    (reset! ws/lock false)
    (when (:replay-diffs state)
      (init-replay app-state state)
      (swap! app-state assoc-in [:current-game :started] true))))

(defn launch-game! [state]
  (init-game! state)
  (set! (.-onbeforeunload js/window) #(clj->js "Leaving this page will disconnect you from the game."))
  (-> "#gamelobby" js/$ .fadeOut)
  (-> "#gameboard" js/$ .fadeIn))

(defn leave-game! []
  (reset! game-state nil)
  (swap! app-state dissoc :current-game :start-shown)
  (set! (.-cursor (.-style (.-body js/document))) "default")
  (set! (.-onbeforeunload js/window) nil)
  (-> "#gameboard" js/$ .fadeOut)
  (-> "#gamelobby" js/$ .fadeIn))

(defn handle-diff! [{:keys [gameid diff]}]
  (when (= gameid (str (current-gameid app-state)))
    (swap! game-state #(differ/patch @last-state diff))
    (check-lock?)
    (reset! last-state @game-state)))

(declare toast)
(defn handle-timeout [gameid]
  (when (= gameid (current-gameid app-state))
    (toast (tr [:game.inactivity "Game closed due to inactivity"]) "error" {:time-out 0 :close-button true})
    (leave-game!)))

(defn handle-error []
  (toast (tr [:game.error "Internal Server Error. Please type /bug in the chat and follow the instructions."])
         "error"
         {:time-out 0
          :close-button true})
  (reset! ws/lock false))

(defmethod ws/event-msg-handler :game/start [{data :?data}]
  (reset! angel-arena/queueing false)
  (launch-game! (parse-state data)))
(defmethod ws/event-msg-handler :game/resync [{data :?data}] (reset-game! (parse-state data)))
(defmethod ws/event-msg-handler :game/diff [{data :?data}] (handle-diff! (parse-state data)))
(defmethod ws/event-msg-handler :game/timeout [{data :?data}] (handle-timeout data))
(defmethod ws/event-msg-handler :game/error [_] (handle-error))

(defn send-command
  ([command] (send-command command nil))
  ([command {:keys [no-lock card] :as args}]
   (when (and (not (:replay @game-state))
              (or (not @ws/lock) no-lock))
     (let [card (select-keys card [:cid :zone :side :host :type])
           args (merge args (when (seq card) {:card card}))]
       (when-not no-lock (reset! ws/lock true))
       (ws/ws-send! [:game/action {:gameid (current-gameid app-state)
                                   :command command
                                   :args args}])))))

(defn mute-spectators []
  (when (not (:replay @game-state))
    (ws/ws-send! [:game/mute-spectators {:gameid (current-gameid app-state)}])))

(defn stack-cards []
  (swap! app-state update-in [:options :stacked-cards] not))

; (defn flip-runner-board []
;   (let [layout (if (= "irl" (get-in @app-state [:options :runner-board-order])) "jnet" "irl")]
;     (swap! app-state assoc-in [:options :runner-board-order] layout)))

(defn concede []
  (when (not (:replay @game-state))
    (ws/ws-send! [:game/concede {:gameid (current-gameid app-state)}])))

(defn build-exception-msg [msg error]
  (letfn [(build-report-url [error]
            (js/escape (str "Please describe the circumstances of your error here.\n\n\nStack Trace:\n```clojure\n"
                         error
                         "\n```")))]
    (str "<div>"
      msg
      "<br/>"
      "<button type=\"button\" class=\"reportbtn\" style=\"margin-top: 5px\" "
      "onclick=\"window.open('https://github.com/mtgred/netrunner/issues/new?body="
      (build-report-url error)
      "');\">Report on GitHub</button></div>")))

(defn ack-toast ([id] (send-command "toast" {:id id})))

(defn toast
  "Display a toast warning with the specified message.
  Sends a command to clear any server side toasts."
  [msg toast-type options]
  (set! (.-options js/toastr) (toastr-options options))
  (let [f (aget js/toastr (if (= "exception" toast-type) "error" toast-type))]
    (f (if (= "exception" toast-type) (build-exception-msg msg (:last-error @game-state)) msg))))

(defonce side (r/cursor game-state [:side]))
(defonce me-toasts (ratom/reaction (get-in @game-state [@side :toast])))
(defn handle-toasts-changed [] (doseq [{:keys [id msg type options]} @me-toasts]
                                 (toast msg type options)
                                 (ack-toast id)))
(defonce watch-toasts (r/track! handle-toasts-changed))
