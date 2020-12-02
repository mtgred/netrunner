(ns web.ws
  (:require [clojure.core.async :refer [go <! >! timeout put! chan]]
            [aero.core :refer [read-config]]
            [buddy.sign.jwt :as jwt]
            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]))

(defonce ws-router (atom nil))

(let [{:keys [ch-recv send-fn connected-uids ajax-post-fn ajax-get-or-ws-handshake-fn]}
      (sente/make-channel-socket! (get-sch-adapter) {:user-id-fn (fn [ring-req] (:client-id ring-req))})]
  (defonce handshake-handler ajax-get-or-ws-handshake-fn)
  (defonce post-handler ajax-post-fn)
  (defonce <recv ch-recv)
  (defonce ^:private send! send-fn) ;; All access to send! should be through the internal buffer
  (defonce connected-uids connected-uids))

;; Maximum throughput is 25,000 client updates a second or 1024 pending broadcast-to!'s (asyncs limit for pending takes)
;; At a duration of 40ms, a maximum of 2 buffer sizes can be processed in one sente tick (sentes buffer window is 30ms)
(def buffer-clear-timer-ms 40)
;; If two buffers can be exhausted in one sente tick, we should use a max buffer size of roughly half the 1024 core.async limit
(def buffer-size 500)
(let [websocket-buffer (chan buffer-size)]
  (defonce ratelimiter (go
    (while true
      (<! (timeout (int buffer-clear-timer-ms))) 
      (dotimes [n buffer-size]
        (<! websocket-buffer)))))

  (defn broadcast-to!
    "Sends the given event and msg to all clients in the given uids sequence."
    [uids event msg]
    ;; TODO in high stress situations, multiple go blocks could be competing. This could result in out of order messages and thus a stale client.
    ;; To fix, we would want to keep the order of loading correct perhaps by blocking successive go blocks until the previous ones have completed
    (go
      (doseq [client uids
              :when (some? client)]
        (>! websocket-buffer true) ;; Block if we have recently sent a lot of messages. The data supplied is arbitrary
        (send! client [event msg])))))

(defn broadcast!
  "Sends the given event and msg to all connected clients."
  [event msg]
  (broadcast-to! (:ws @connected-uids) event msg))

(defonce ws-handlers (atom {}))

(defn handle-unknown
  [{:keys [id ?data uid client-id ?reply-fn] :as msg}]
  (println "Unhandled WS msg" id uid ?data)
  (when ?reply-fn
    (?reply-fn {:msg "Unhandled event"})))

(defn register-ws-handler!
  "Registers a function to handle a given event-id on the web socket.
  If multiple handlers are registered for an event, each is called until a
  truthy value is returned, and the rest are ignored."
  [event handler-fn]
  (swap! ws-handlers update event (partial cons handler-fn)))

(defn register-ws-handlers!
  "Utility to register a sequence of ws handlers. The sequence alternates
  *event* *handler* *event* *handler* ..."
  [& args]
  (let [handlers (partition 2 args)]
    (doseq [[event handler-fn] handlers]
      (register-ws-handler! event handler-fn))))

(defn handle-ws-msg
  "Called by the websocket router to process incoming messages."
  [{:keys [id ?data uid client-id ?reply-fn] :as msg}]
  (if-let [handlers (get @ws-handlers id)]
    ;; Call each handler in turn, until one returns a truthy value.
    (reduce (fn [_ handler]
              (when-let [result (handler msg)]
                (reduced result)))
            false
            handlers)
    (handle-unknown msg)))

(register-ws-handler! :chsk/ws-ping (fn [_])) ;; do nothing on ping messages

(defn stop-ws-router! []
  (when-let [stop-fn @ws-router]
    (stop-fn)))

(defn start-ws-router! []
  (stop-ws-router!)
  (reset! ws-router (sente/start-server-chsk-router! <recv handle-ws-msg)))
