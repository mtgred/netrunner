(ns netrunner.ws
  (:require-macros
    [cljs.core.async.macros :as asyncm :refer [go go-loop]])
  (:require
    ;; <other stuff>
    [cljs.core.async :as async :refer [<! >! put! chan]]
    [taoensso.sente  :as sente :refer [start-client-chsk-router!]]))


(let [{:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket! "/ws" {:type :ws})]
  (def chsk       chsk)
  (def <ws-recv ch-recv) ; ChannelSocket's receive channel
  (def ws-send! send-fn) ; ChannelSocket's send API fn
  (def chsk-state state)   ; Watchable, read-only atom
  )

(let [ws-handlers (atom {})]
  (defn register-ws-handler! [event handler-fn]
    (swap! ws-handlers assoc event handler-fn))

  (defn handle-ws-msg [{[res [event msg]] :event}]
    (if-let [handler (get @ws-handlers event)]
      (handler msg)
      (println "unknown socket msg" event msg))))

(start-client-chsk-router! <ws-recv handle-ws-msg)

