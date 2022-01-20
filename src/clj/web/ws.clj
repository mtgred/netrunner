(ns web.ws
  (:require
   [clojure.core.async :refer [<! >! chan go timeout]]
   [executor.core :as executor]
   [taoensso.sente :as sente]
   [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]))

(let [chsk-server (sente/make-channel-socket-server!
                    (get-sch-adapter)
                    {:user-id-fn (fn [ring-req]
                                   (or (-> ring-req :session :uid)
                                       (:client-id ring-req)))})
      {:keys [ch-recv send-fn
              ajax-post-fn ajax-get-or-ws-handshake-fn]} chsk-server]
  (defn handshake-handler [request] (ajax-get-or-ws-handshake-fn request))
  (defn post-handler [request] (ajax-post-fn request))
  (def ch-chsk ch-recv)
  ;; redefined here to get clj-kondo checking
  (defn chsk-send! [uid ev] (send-fn uid ev)))

(executor/reg-fx :ws/chsk-send (fn [[uid ev]] (chsk-send! uid ev)))
(executor/reg-fx :ws/reply-fn (fn [[reply-fn value]] (when reply-fn (reply-fn value))))

;; Maximum throughput is 25,000 client updates a second
;; or 1024 pending broadcast-to!'s (asyncs limit for pending takes).
;; At a duration of 40ms, a maximum of 2 buffer sizes can be processed
;; in one sente tick (sentes buffer window is 30ms)
(def buffer-clear-timer-ms 40)

;; If two buffers can be exhausted in one sente tick, we should use a max
;; buffer size of roughly half the 1024 core.async limit
(def buffer-size 500)
(def websocket-buffer (chan buffer-size))

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

(def executor-namespaces
  #{"chsk" "lobby"})

(def executor-events
  #{
    })

(defn event-msg-handler
  "Wraps `-msg-handler` with logging, error catching, etc."
  [event]
  (try
    (cond
      (or (contains? executor-namespaces (namespace (:id event)))
          (contains? executor-events (:id event)))
      (executor/dispatch-sync [(:id event) event])
      :else
      (-msg-handler event))
    (catch Exception e
      (println "Caught an error in the message handler")
      (println (.printStackTrace e)))))
