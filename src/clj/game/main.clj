(ns game.main
  (:import [org.zeromq ZMQ])
  (:require [cheshire.core :refer [parse-string generate-string]]
            [cheshire.generate :refer [add-encoder encode-str]]
            [game.macros :refer [effect]]
            [game.core :refer [game-states do! system-msg pay gain draw end-run] :as core])
  (:gen-class :main true))

(add-encoder java.lang.Object encode-str)

(def ctx (ZMQ/context 1))

(def commands
  {"say" core/say
   "system-msg" #(system-msg %1 %2 (:msg %3))
   "change" core/change
   "move" core/move-card
   "mulligan" core/mulligan
   "keep" core/keep-hand
   "start-turn" core/start-turn
   "end-turn" core/end-turn
   "draw" core/click-draw
   "credit" core/click-credit
   "purge" (do! {:cost [:click 3] :effect (effect (core/purge) (system-msg "purges viruses"))})
   "remove-tag" (do! {:cost [:click 1 :credit 2 :tag 1] :effect (effect (system-msg "removes 1 tag"))})
   "play" core/play
   "rez" #(core/rez %1 %2 (:card %3) nil)
   "derez" #(core/derez %1 %2 (:card %3))
   "run" core/click-run
   "no-action" core/no-action
   "continue" core/continue
   "access" core/successful-run
   "jack-out" core/jack-out
   "advance" core/advance
   "score" core/score
   "choice" core/resolve-prompt
   "select" core/select
   "shuffle" core/shuffle-deck
   "ability" core/play-ability})

(defn convert [args]
  (let [params (parse-string (String. args) true)]
    (if (or (get-in params [:args :card]))
      (update-in params [:args :card :zone] #(map (fn [k] (if (string? k) (keyword k) k)) %))
      params)))

(defn exec [{:keys [action gameid command side args text] :as msg}]
  (let [state (@game-states gameid)]
    (try (case action
           "start" (core/init-game msg)
           "do" ((commands command) state (keyword side) args)
           "quit" (system-msg state (keyword side) "left the game")
           "notification" (swap! state update-in [:log] #(conj % {:user "__system__" :text text})))
         (catch Exception e (println "Error: " e))))
  (assoc @(@game-states gameid) :action action))

(defn -main []
  (let [socket (.socket ctx ZMQ/REP)]
    (.bind socket "tcp://127.0.0.1:1043")
    (println "Listening on port 1043 for incoming commands...")
    (while true
      (let [data (convert (.recv socket))]
        ;; (println (:action data) data)
        (if (= (:action data) "remove")
          (do (swap! game-states dissoc (:gameid data))
              (.send socket (generate-string "ok")))
          (.send socket (generate-string (dissoc (exec data) :events))))))))
