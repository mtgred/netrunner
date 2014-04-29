(ns netrunner.socket
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.main :as main]))

(.log js/console "bar")

;; (.on js/socket "data" #(.log js/console %1))
;; (.emit js/socket "data" (js-obj "msg" "eggs"))

(defn socket-listen [msg-type]
  (let [c (chan 1)]
    (.on js/socket msg-type #(put! c %))
    c))

(defn process-msg [data]
  (.log js/console "async: " (aget data "msg")))

(let [c (socket-listen "data")]
  (go (while true
        (process-msg (<! c)))))
