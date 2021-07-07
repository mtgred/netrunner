(ns nr.ws
  (:require-macros
    [cljs.core.async.macros :as asyncm :refer [go go-loop]])
  (:require
    [cljs.core.async :as async :refer [<! >! put! chan]]
    [nr.utils :refer [non-game-toast]]
    [nr.ajax :refer [?csrf-token]]
    [nr.appstate :refer [app-state]]
    [nr.gameboard.state :refer [lock game-state]]
    [taoensso.sente  :as sente :refer [start-client-chsk-router!]]))

(if-not ?csrf-token
  (println "CSRF token NOT detected in HTML, default Sente config will reject requests")
  (let [{:keys [chsk ch-recv send-fn state]}
        (sente/make-channel-socket! "/ws" ?csrf-token {:type :ws
                                                       :wrap-recv-evs? false})]
    (def chsk chsk)
    (def ch-chsk ch-recv)    ; ChannelSocket's receive channel
    (def ws-send! send-fn)   ; ChannelSocket's send API fn
    (def chsk-state state))) ; Watchable, read-only atom


(enable-console-print!)

(defmulti -msg-handler
  "Multimethod to handle Sente `event-msg`s"
  :id)

(defn event-msg-handler
  "Wraps `-msg-handler` with logging, error catching, etc."
  [event]
  (try
    (-msg-handler event)
    (catch js/Object e
      (println "Caught an error in the message handler: " e))))

(defmethod -msg-handler :default [event]
  (println "unknown event message" event))

(defmethod -msg-handler :chsk/handshake [event]
  nil)

(defmethod -msg-handler :chsk/ws-ping [event]
  nil)

(defn resync []
  (ws-send! [:netrunner/resync {:gameid-str (:gameid @game-state)}]))

(defmethod -msg-handler :chsk/state
  [{[old-state new-state] :?data}]
    (when (= (:type old-state) (:type new-state))
      (when (and (:open? old-state)
                 (not (:open? new-state))
                 (not (:first-open? new-state)))
        (reset! lock true)
        (non-game-toast "Lost connection to server. Reconnecting." "error" {:time-out 0 :close-button true}))
      (when (and (not (:open? old-state))
                 (:open? new-state)
                 (not (:first-open? new-state)))
        (.clear js/toastr)
        (ws-send! [:lobby/list])
        (when (and (:gameid @app-state)
                   (@game-state))
              (resync))
        (non-game-toast "Reconnected to server" "success" nil))))

(defonce router_ (atom nil))
(defn stop-router! [] (when-let [stop-fn @router_] (stop-fn)))
(defn start-router! []
  (stop-router!)
  (reset! router_
          (start-client-chsk-router!
            ch-chsk
            event-msg-handler)))

(start-router!)
