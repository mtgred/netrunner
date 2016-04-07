(ns game.main
  (:import [org.zeromq ZMQ ZMQQueue])
  (:require [cheshire.core :refer [parse-string generate-string]]
            [cheshire.generate :refer [add-encoder encode-str]]
            [game.macros :refer [effect]]
            [game.core :refer [game-states system-msg pay gain draw end-run] :as core]
            [environ.core :refer [env]]
            [differ.core :as differ])
  (:gen-class :main true))

(add-encoder java.lang.Object encode-str)

(def last-states (atom {}))
(def ctx (ZMQ/context 1))

(def spectator-commands
  {"say" core/say})

(def commands
  {"say" core/say
   "concede" core/concede
   "system-msg" #(system-msg %1 %2 (:msg %3))
   "change" core/change
   "move" core/move-card
   "mulligan" core/mulligan
   "keep" core/keep-hand
   "start-turn" core/start-turn
   "end-phase-12" core/end-phase-12
   "end-turn" core/end-turn
   "draw" core/click-draw
   "credit" core/click-credit
   "purge" core/do-purge
   "remove-tag" core/remove-tag
   "play" core/play
   "rez" #(core/rez %1 %2 (:card %3) nil)
   "derez" #(core/derez %1 %2 (:card %3))
   "run" core/click-run
   "no-action" core/no-action
   "corp-phase-43" core/corp-phase-43
   "continue" core/continue
   "access" core/successful-run
   "jack-out" core/jack-out
   "advance" core/advance
   "score" core/score
   "choice" core/resolve-prompt
   "select" core/select
   "shuffle" core/shuffle-deck
   "ability" core/play-ability
   "trash-resource" core/trash-resource
   "auto-pump" core/auto-pump
   "toast" core/toast})

(defn convert [args]
  (try
    (let [params (parse-string (String. args) true)]
      (if (or (get-in params [:args :card]))
        (update-in params [:args :card :zone] #(map (fn [k] (if (string? k) (keyword k) k)) %))
        params))
    (catch Exception e
      (println "Convert error " e))))

(defn strip [state]
  (dissoc state :events :turn-events :per-turn :prevent :damage))

(defn not-spectator? [state user]
  "Returns true if the specified user in the specified state is not a spectator"
  (#{(get-in @state [:corp :user]) (get-in @state [:runner :user])} user))

(defn handle-do [user command state side args]
  "Ensures the user is allowed to do command they are trying to do"
  (if (not-spectator? state user)
    ((commands command) state (keyword side) args)
    (when-let [cmd (spectator-commands command)]
      (cmd state (keyword side) args))))

(defn run [socket]
  (while true
    (let [{:keys [gameid action command side user args text] :as msg} (convert (.recv socket))
          state (@game-states gameid)]
      (try
        (case action
          "start" (core/init-game msg)
          "remove" (do (swap! game-states dissoc gameid)
                       (swap! last-states dissoc gameid))
          "do" (handle-do user command state side args)
          "notification" (when state
                           (swap! state update-in [:log] #(conj % {:user "__system__" :text text}))))
        (if-let [new-state (@game-states gameid)]
          (do (case action
                ("start" "reconnect" "notification") (.send socket (generate-string {:action action :state (strip @new-state) :gameid gameid}))
                (let [diff (differ/diff (strip (@last-states gameid)) (strip @new-state))]
                  (.send socket (generate-string {:action action :diff diff :gameid gameid}))))
              (swap! last-states assoc gameid (strip @new-state)))
          (.send socket (generate-string {:action action :gameid gameid :state (strip @state)})))
        (catch Exception e
          (println "Error " action command (get-in args [:card :title]) e "\nStack trace:"
                   (java.util.Arrays/toString (.getStackTrace e)))
          (if (and state (#{"do" "start"} action))
            (.send socket (generate-string state))
            (.send socket (generate-string "error"))))))))

(def zmq-url (str "tcp://" (or (env :zmq-host) "127.0.0.1") ":1043"))

(defn dev []
  (println "[Dev] Listening on port 1043 for incoming commands...")
  (let [socket (.socket ctx ZMQ/REP)]
    (.bind socket zmq-url)
    (run socket)))

(defn -main []
  (println "[Prod] Listening on port 1043 for incoming commands...")
  (let [worker-url "inproc://responders"
        router (doto (.socket ctx ZMQ/ROUTER) (.bind zmq-url))
        dealer (doto (.socket ctx ZMQ/DEALER) (.bind worker-url))]
    (dotimes [n 2]
      (.start
       (Thread.
        (fn []
          (let [socket (.socket ctx ZMQ/REP)]
            (.connect socket worker-url)
            (run socket))))))

    (.start (Thread. #(.run (ZMQQueue. ctx router dealer))))))
