(ns nr.ws
  (:require
   [nr.ajax :refer [?csrf-token]]
   [nr.appstate :refer [app-state current-gameid]]
   [nr.utils :refer [non-game-toast]]
   [taoensso.sente  :as sente :refer [start-client-chsk-router!]]))

(defonce lock (atom false))

(if-not ?csrf-token
  (println "CSRF token NOT detected in HTML, default Sente config will reject requests")
  (let [{:keys [chsk ch-recv send-fn]}
        (sente/make-channel-socket-client!
          "/chsk"
          ?csrf-token
          {:type :auto
           :wrap-recv-evs? false})]
    (def chsk chsk)
    (def ch-chsk ch-recv)
    (defn ws-send!
      ([ev] (send-fn ev))
      ([ev ?timeout ?cb] (send-fn ev ?timeout ?cb)))))

(defmulti event-msg-handler
  "Multimethod to handle Sente `event-msg`s"
  :id)

(defn event-msg-handler-wrapper
  "Wraps `-msg-handler` with logging, error catching, etc."
  [event]
  (try
    (event-msg-handler event)
    (catch js/Object e
      (println "Caught an error in the message handler: " e))))

(defmethod event-msg-handler :default [event]
  (println (str "unknown event message"
                "\nid: " (:id event)
                "\nevent:" event)))

(defmethod event-msg-handler :chsk/handshake [_] (ws-send! [:lobby/list]))
(defmethod event-msg-handler :chsk/ws-ping [_])

(defmethod event-msg-handler :system/force-disconnect [_]
  (sente/chsk-reconnect! chsk))

(defn resync []
  (ws-send! [:game/resync {:gameid (current-gameid app-state)}]))

(defmethod event-msg-handler :chsk/state
  [{[old-state new-state] :?data}]
  (when (not (:first-open? new-state))
    (when (and (:open? old-state)
               (not (:open? new-state)))
      (reset! lock true)
      (non-game-toast "Lost connection to server. Reconnecting." "error" {:time-out 0 :close-button true}))
    (when (and (not (:open? old-state))
               (:open? new-state))
      (.clear js/toastr)
      (ws-send! [:lobby/list])
      (when (get-in @app-state [:current-game :started])
        (resync))
      (non-game-toast "Reconnected to server" "success" nil))))

(defonce router_ (atom nil))
(defn stop-router! [] (when-let [stop-fn @router_] (stop-fn)))
(defn start-router! []
  (stop-router!)
  (reset! router_
          (start-client-chsk-router!
            ch-chsk
            event-msg-handler-wrapper)))
