(ns web.ws
  (:require
   [cljc.java-time.instant :as inst]
   [clojure.core.async :refer [<! >! chan close! go timeout]]
   [jinteki.msgpack-ext]
   [web.app-state :refer [register-user! deregister-user!]]
   [web.user :refer [active-user?]]
   [taoensso.sente :as sente]
   [taoensso.sente.packers.msgpack :as msgpack]
   [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]
   [taoensso.timbre :as timbre]
   [taoensso.trove :as trove]
   [taoensso.trove.timbre :as trove-timbre]))

(defn redact-uid-middleware
  "Timbre middelware to remove UIDs from Sente log lines"
  [data]
  (letfn [(filter-uid-from-log-arg [arg] (if (string? arg)
                                          (clojure.string/replace arg #"u_.*/c_" "u_[REDACTED]/c_")
                                          arg))]
    (assoc data :vargs (map filter-uid-from-log-arg (:vargs data )))))
(timbre/merge-config! {:middleware [redact-uid-middleware]})

;; Sente logs via trove, set it to use timbre.
(trove/set-log-fn! (trove-timbre/get-log-fn))

;; Maximum throughput is 25,000 client updates a second
;; or 1024 pending broadcast-to!'s (asyncs limit for pending takes).
;; At a duration of 40ms, a maximum of 2 buffer sizes can be processed
;; in one sente tick (sentes buffer window is 30ms)
(def buffer-clear-timer-ms 40)

;; If two buffers can be exhausted in one sente tick, we should use a max
;; buffer size of roughly half the 1024 core.async limit
(def buffer-size 500)

(defonce server_ (atom nil))

(defn start-server!
  [{:keys [packer]}]
  (let [server (sente/make-channel-socket-server!
                 (get-sch-adapter)
                 {:ws-kalive-ms 2500
                  :packer (if (= packer :edn)
                            :edn
                            (msgpack/get-packer))
                  :user-id-fn (fn [ring-req]
                                (or (-> ring-req :session :uid)
                                    (:client-id ring-req)))})
        buffer (chan buffer-size)]
    ;; Ratelimiter, exits when the buffer is closed by stop-server!
    (go (loop []
          (<! (timeout (int buffer-clear-timer-ms)))
          (when (loop [n buffer-size]
                  (or (zero? n)
                      (when (<! buffer)
                        (recur (dec n)))))
            (recur))))
    (reset! server_ (assoc server
                           :buffer buffer
                           :buffer-size buffer-size))))

(defn stop-server! []
  (when-let [{:keys [buffer]} @server_]
    (close! buffer))
  (reset! server_ nil))

(defn handshake-handler [& args]
  (when-let [{:keys [ajax-get-or-ws-handshake-fn]} @server_]
    (try (apply ajax-get-or-ws-handshake-fn args)
         (catch Exception ex (timbre/error ex "Caught an error in the handshake handler")))))

(defn post-handler [& args]
  (when-let [{:keys [ajax-post-fn]} @server_]
    (apply ajax-post-fn args)))

(defn ch-chsk []
  (:ch-recv @server_))

(defn chsk-send! [uid ev]
  (when-let [{:keys [send-fn]} @server_]
    (send-fn uid ev)))

(defn connected-sockets []
  (some-> @server_ :connected-uids deref))

(defn connections
  "internal sente info, ideally don't use this outside of debugging"
  []
  (some-> @server_ :private :conns_ deref))

(defn connected-uids [] (seq (:any (connected-sockets))))

(defn buffer-stats []
  (when-let [{:keys [buffer buffer-size]} @server_]
    {:pending (count (.buf buffer))
     :size buffer-size}))

(defn broadcast-to!
  "Sends the given event and msg to all clients in the given uids sequence."
  [uids event msg]
  ;; TODO in high stress situations, multiple go blocks could be competing.
  ;; This could result in out of order messages and thus a stale client.
  ;; To fix, we would want to keep the order of loading correct perhaps by blocking
  ;; successive go blocks until the previous ones have completed
  (let [{:keys [buffer]} @server_]
    (go
      (doseq [client uids
              :when (some? client)]
        ;; Block if we have recently sent a lot of messages. The data supplied is arbitrary
        (when buffer (>! buffer true))
        (chsk-send! client [event msg])))))

(defmulti -msg-handler
  "Multimethod to handle Sente `event-msg`s"
  :id)

(defmethod -msg-handler :default
  msg-handler--default
  ;; Handles any hecked messages from the client
  [{:keys [id ?data uid ?reply-fn]}]
  (timbre/error (str "Unhandled WS msg" id uid (pr-str ?data)))
  (when ?reply-fn
    (?reply-fn {:msg "Unhandled event"})))

(defmethod -msg-handler :chsk/ws-ping chsk--ws-ping [_])
(defmethod -msg-handler :chsk/ws-pong chsk--ws-pong [_])
;; NOTE - :chsk/uidport-close is handled in game.clj
(defmethod -msg-handler :chsk/uidport-open
  chsk--uidport-open
  [{uid :uid
    {user :user} :ring-req}]
  (when (active-user? user)
    (register-user! uid user)))

(defn event-msg-handler
  "Wraps `-msg-handler` with logging, error catching, etc."
  [event]
  (try
    (-msg-handler (assoc event :timestamp (inst/now)))
    (catch Exception e
      (timbre/error e "Caught an error in the message handler"))))
