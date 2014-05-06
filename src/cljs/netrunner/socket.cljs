(ns netrunner.socket
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.main :as main]))

(def in-channel (chan))
(def out-channel (chan))

(def chat-channel (chan))
(def game-lobby-channel (chan))
(def game-channel (chan))

(go (while true
      (let [msg (<! out-channel)]
        (.emit js/socket "netrunner" msg))))

(.on js/socket "netrunner" #(put! in-channel %))

(defmulti process-msg #(aget % "type"))

(go (while true
      (process-msg (<! in-channel))))

(defmethod process-msg "chat" [msg]
  (put! chat-channel msg))

(defmethod process-msg "game-lobby" [msg]
  (put! game-lobby-channel msg))

(defmethod process-msg "game" [msg]
  (put! game-channel msg))
