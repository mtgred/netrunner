(ns netrunner.socket
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.main :as main]))

(.log js/console "bar")

;; (.on js/socket "data" #(.log js/console %1))
;; (.emit js/socket "data" "spam")

(defn socket-listen [msg-type]
  (let [c (chan 1)]
    (.on js/socket msg-type #(put! c %))
    c))

(defn process-msg [msg]
  (.log js/console "async: " msg))

(let [c (socket-listen "data")]
  (go (while true
        (process-msg (<! c)))))
