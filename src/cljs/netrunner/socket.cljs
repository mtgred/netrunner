(ns netrunner.socket
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.main :as main]))

(def socket-channel (chan))

(.on js/socket "message" #(put! socket-channel %))

(defmulti process-msg #(aget % "type"))

(go (while true
      (process-msg (<! socket-channel))))

(defmethod process-msg "chat" [msg]
  (.log js/console "Chat: #" (aget msg "channel") (aget msg "msg")))

(defmethod process-msg "game-lobby" [msg]
  (.log js/console "Game Lobby:" (aget msg "title")))

(defmethod process-msg "game" [msg]
  (.log js/console "New Game:" (aget msg "player1") "vs" (aget msg "player2")))
