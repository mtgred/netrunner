(ns tasks.stress.msgpack
  "The msgpack half of the stress client, kept apart so the harness also
  compiles against a server on the older edn wire: sente pre-1.20 has neither
  the msgpack packer nor the pluggable ws client used here. Only loaded (via
  requiring-resolve in tasks.stress.client) when the packer namespace exists."
  (:require
    [jinteki.msgpack-ext]
    [taoensso.sente.interfaces :as sente-interfaces]
    [taoensso.sente.packers.msgpack :as msgpack])
  (:import
    (java.nio ByteBuffer)
    (org.java_websocket.client WebSocketClient)))

(defn- make-binary-ws
  "Like taoensso.sente.java-ws-client/make-client-ws, but also accepts the
  binary frames a binary packer (msgpack) produces; sente's own Java client
  only handles text frames."
  [{:keys [uri-str headers on-error on-message on-close]}]
  (when-let [^WebSocketClient ws-client
             (try
               (proxy [WebSocketClient] [(java.net.URI. ^String uri-str) ^java.util.Map headers]
                 (onOpen [_] nil)
                 (onError [ex] (on-error ex))
                 (onMessage [msg]
                   (on-message
                    (if (instance? ByteBuffer msg)
                      (let [^ByteBuffer bb msg
                            arr (byte-array (.remaining bb))]
                        (.get bb arr)
                        arr)
                      msg)))
                 (onClose [code reason remote] (on-close code reason remote)))
               (catch Throwable t
                 (println "websocket client creation failed:" (.getMessage t))
                 nil))]
    (delay
      (.connect ws-client)
      (reify
        sente-interfaces/IClientWebSocket
        (cws-raw [_] ws-client)
        (cws-send [_ data] (.send ws-client data))
        (cws-close [_ code reason _clean?] (.close ws-client code reason))))))

(defn connect-opts
  "Extra make-channel-socket-client! opts for the msgpack wire."
  []
  {:packer (msgpack/get-packer)
   :ws-constructor make-binary-ws})
