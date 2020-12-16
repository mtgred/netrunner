(ns nr.ws
  (:require-macros
    [cljs.core.async.macros :as asyncm :refer [go go-loop]])
  (:require
    [cljs.core.async :as async :refer [<! >! put! chan]]
    [nr.cardbrowser :refer [non-game-toast] :as cb]
    [nr.ajax :refer [?csrf-token]]
    [taoensso.sente  :as sente :refer [start-client-chsk-router!]]))

(if-not ?csrf-token
  (println "CSRF token NOT detected in HTML, default Sente config will reject requests")
  (let [{:keys [chsk ch-recv send-fn state]}
        (sente/make-channel-socket! "/ws" ?csrf-token {:type :ws})]
    (def chsk       chsk)
    (def <ws-recv ch-recv) ; ChannelSocket's receive channel
    (def ws-send! send-fn) ; ChannelSocket's send API fn
    (def chsk-state state)   ; Watchable, read-only atom
    ))


(enable-console-print!)

(let [ws-handlers (atom {})]
  (defn register-ws-handler! [event handler-fn]
    (swap! ws-handlers assoc event handler-fn))

  (defn handle-state-msg [[old-state new-state]]
    (when (= (:type old-state) (:type new-state))
      (when (and (:open? old-state)
                 (not (:open? new-state))
                 (not (:first-open? new-state)))
        (cb/non-game-toast "Lost connection to server. Reconnecting." "error" {:time-out 0 :close-button true}))
      (when (and (not (:open? old-state))
                 (:open? new-state)
                 (not (:first-open? new-state)))
        (.clear js/toastr)
        (cb/non-game-toast "Reconnected to server" "success" nil))))

  (defn handle-netrunner-msg [[event msg]]
    (let [handler (get @ws-handlers event)]
      (cond
        handler (handler msg)
        msg (println "unknown game socket msg" event msg))))

  (defn event-msg-handler [msg]
    (let [[event-type data] (:event msg)]
      (case event-type
        :chsk/handshake nil
        :chsk/state (handle-state-msg data)
        :chsk/recv (handle-netrunner-msg data)
        (println "unknown event message" event-type data)))))

(when ?csrf-token
  (start-client-chsk-router! <ws-recv event-msg-handler))
