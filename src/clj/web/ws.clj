(ns web.ws
  (:require [clojure.core.async :refer [go <! timeout]]
            [aero.core :refer [read-config]]
            [buddy.sign.jwt :as jwt]
            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.http-kit :refer (get-sch-adapter)]))

(defonce ws-router (atom nil))

(let [{:keys [ch-recv send-fn connected-uids ajax-post-fn ajax-get-or-ws-handshake-fn]}
      (sente/make-channel-socket! (get-sch-adapter) {:user-id-fn (fn [ring-req] (:client-id ring-req))})]
  (defonce handshake-handler ajax-get-or-ws-handshake-fn)
  (defonce post-handler ajax-post-fn)
  (defonce <recv ch-recv)
  (defonce send! send-fn)
  (defonce connected-uids connected-uids))

(defn broadcast-to!
  "Sends the given event and msg to all clients in the given uids sequence."
  [uids event msg]
  (doseq [client uids]
    (send! client [event msg])))

(defn broadcast!
  "Sends the given event and msg to all connected clients."
  [event msg]
  (broadcast-to! (:ws @connected-uids) event msg))

(let [ws-handlers (atom {})]
  (defn handle-unknown [{:keys [id ?data uid client-id ?reply-fn] :as msg}]
    (println "Unhandled WS msg" id uid ?data)
    (when ?reply-fn
      (?reply-fn {:msg "Unhandled event"})))

  (defn register-ws-handler!
    "Registers a function to handle a given event-id on the web socket.
    If multiple handlers are registered for an event, each is called until a
    truthy value is returned, and the rest are ignored."
    [event handler-fn]
    (swap! ws-handlers #(update-in % [event] (partial cons handler-fn))))

  (defn register-ws-handlers!
    "Utility to register a sequence of ws handlers. The sequence alternates
    *event* *handler* *event* *handler* ..."
    [& args]
    (let [handlers (partition 2 args)]
      (doseq [h handlers]
        (register-ws-handler! (first h) (second h)))))

  (defn handle-ws-msg
    "Called by the websocket router to process incoming messages."
    [{:keys [id ?data uid client-id ?reply-fn] :as msg}]
    (if-let [handlers (get @ws-handlers id)]
      ;; Call each handler in turn, until one returns a truthy value.
      (reduce (fn [prev handler] (or prev (handler msg))) false handlers)
      (handle-unknown msg))))

(register-ws-handler! :chsk/ws-ping (fn [_])) ;; do nothing on ping messages

(defn stop-ws-router! []
  (when-let [stop-fn @ws-router]
    (stop-fn)))

(defn start-ws-router! []
  (stop-ws-router!)
  (reset! ws-router (sente/start-server-chsk-router! <recv handle-ws-msg)))