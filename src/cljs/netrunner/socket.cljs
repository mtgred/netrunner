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
        (.log js/console msg)
        (.emit js/socket "netrunner" msg))))

(.on js/socket "netrunner" #(put! in-channel %))

(defmulti process-msg #(aget % "type"))

(go (while true
      (process-msg (<! in-channel))))

(defmethod process-msg "chat" [msg]
  (put! chat-channel msg))

(defmethod process-msg "game-lobby" [msg]
  (.log js/console "Game Lobby:" (aget msg "title")))

(defmethod process-msg "game" [msg]
  (.log js/console "New Game:" (aget msg "player1") "vs" (aget msg "player2")))
