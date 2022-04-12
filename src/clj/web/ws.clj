(ns web.ws
  (:require
   [clojure.core.async :refer [<! >! chan go timeout]]
   [web.app-state :refer [register-user! deregister-user!]]
   [web.user :refer [active-user?]]
   [taoensso.sente :as sente]
   [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]))

(let [chsk-server (sente/make-channel-socket-server!
                    (get-sch-adapter)
                    {:user-id-fn (fn [ring-req]
                                   (or (-> ring-req :session :uid)
                                       (:client-id ring-req)))})
      {:keys [ch-recv send-fn connected-uids
              ajax-post-fn ajax-get-or-ws-handshake-fn]} chsk-server]
  (defonce handshake-handler ajax-get-or-ws-handshake-fn)
  (defonce post-handler ajax-post-fn)
  (defonce connected-sockets connected-uids)
  (defonce ch-chsk ch-recv)
  (defn chsk-send! [uid ev] (send-fn uid ev)))

;; Maximum throughput is 25,000 client updates a second
;; or 1024 pending broadcast-to!'s (asyncs limit for pending takes).
;; At a duration of 40ms, a maximum of 2 buffer sizes can be processed
;; in one sente tick (sentes buffer window is 30ms)
(def buffer-clear-timer-ms 40)

;; If two buffers can be exhausted in one sente tick, we should use a max
;; buffer size of roughly half the 1024 core.async limit
(def buffer-size 500)
(def websocket-buffer (chan buffer-size))

(defn connected-uids [] (seq (:any @connected-sockets)))

(defonce ratelimiter
  (go (while true
        (<! (timeout (int buffer-clear-timer-ms)))
        (dotimes [_ buffer-size]
          (<! websocket-buffer)))))

(defn broadcast-to!
  "Sends the given event and msg to all clients in the given uids sequence."
  [uids event msg]
  ;; TODO in high stress situations, multiple go blocks could be competing.
  ;; This could result in out of order messages and thus a stale client.
  ;; To fix, we would want to keep the order of loading correct perhaps by blocking
  ;; successive go blocks until the previous ones have completed
  (go
    (doseq [client uids
            :when (some? client)]
      ;; Block if we have recently sent a lot of messages. The data supplied is arbitrary
      (>! websocket-buffer true)
      (chsk-send! client [event msg]))))

(defmulti -msg-handler
  "Multimethod to handle Sente `event-msg`s"
  :id)

(defmethod -msg-handler :default
  ;; Handles any hecked messages from the client
  [{:keys [id ?data uid ?reply-fn]}]
  (println "Unhandled WS msg" id uid (pr-str ?data))
  (when ?reply-fn
    (?reply-fn {:msg "Unhandled event"})))

(defmethod -msg-handler :chsk/ws-ping [_])
;; NOTE - :chsk/uidport-close is handled in game.clj
(defmethod -msg-handler :chsk/uidport-open
  [{uid :uid
    {user :user} :ring-req}]
  (when (active-user? user)
    (register-user! uid user)))

(defn event-msg-handler
  "Wraps `-msg-handler` with logging, error catching, etc."
  [event]
  (try
    (-msg-handler event)
    (catch Exception e
      (println "Caught an error in the message handler")
      (println (.printStackTrace e)))))
