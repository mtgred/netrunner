(ns game.main
  (:import [org.zeromq ZMQ ZMQQueue])
  (:require [cheshire.core :refer [parse-string generate-string]]
            [cheshire.generate :refer [add-encoder encode-str]]
            [game.macros :refer [effect]]
            [game.core :refer [all-cards all-cards-alt game-states old-states system-msg pay gain draw end-run toast show-error-toast
                               card-is-public?] :as core]
            [game.utils :refer [card-is? private-card]]
            [environ.core :refer [env]]
            [differ.core :as differ])
  (:gen-class :main true))

(add-encoder java.lang.Object encode-str)

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
   "runner-ability" core/play-runner-ability
   "subroutine" core/play-subroutine
   "trash-resource" core/trash-resource
   "auto-pump" core/auto-pump
   "toast" core/toast
   "view-deck" core/view-deck
   "close-deck" core/close-deck})

(defn convert [args]
  (try
    (let [params (parse-string (String. args) true)]
      (if (or (get-in params [:args :card]))
        (update-in params [:args :card :zone] #(map (fn [k] (if (string? k) (keyword k) k)) %))
        params))
    (catch Exception e
      (println "Convert error " e))))

(defn strip [state]
  (dissoc state :events :turn-events :per-turn :prevent :damage :effect-completed))

(defn not-spectator? [state user]
  "Returns true if the specified user in the specified state is not a spectator"
  (and state (#{(get-in @state [:corp :user]) (get-in @state [:runner :user])} user)))

(defn handle-do [user command state side args]
  "Ensures the user is allowed to do command they are trying to do"
  (if (not-spectator? state user)
    ((commands command) state (keyword side) args)
    (when-let [cmd (spectator-commands command)]
      (cmd state (keyword side) args))))

(defn- private-card-vector [state side cards]
  (vec (map (fn [card]
              (cond
                (not (card-is-public? state side card)) (private-card card)
                (:hosted card) (update-in card [:hosted] #(private-card-vector state side %))
                :else card))
            cards)))

(defn- make-private-runner [state]
  (-> (:runner @state)
      (update-in [:hand] #(private-card-vector state :runner %))
      (update-in [:discard] #(private-card-vector state :runner %))
      (update-in [:deck] #(private-card-vector state :runner %))
      (update-in [:rig :facedown] #(private-card-vector state :runner %))
      (update-in [:rig :resource] #(private-card-vector state :runner %))))

(defn- make-private-corp [state]
  (let [zones (concat [[:hand]] [[:discard]] [[:deck]]
                      (for [server (keys (:servers (:corp @state)))] [:servers server :ices])
                      (for [server (keys (:servers (:corp @state)))] [:servers server :content]))]
    (loop [s (:corp @state)
           z zones]
      (if (empty? z)
        s
        (recur (update-in s (first z) #(private-card-vector state :corp %)) (next z))))))

(defn- make-private-deck [state side deck]
  (if (:view-deck (side @state))
    deck
    (private-card-vector state side deck)))

(defn- private-states [state]
  ;; corp, runner, spectator
  (let [corp-private (make-private-corp state)
        runner-private (make-private-runner state)
        corp-deck (update-in (:corp @state) [:deck] #(make-private-deck state :corp %))
        runner-deck (update-in (:runner @state) [:deck] #(make-private-deck state :runner %))]
    [(assoc @state :runner runner-private
                   :corp corp-deck)
     (assoc @state :corp corp-private
                   :runner runner-deck)
     (if (get-in @state [:options :spectatorhands])
       (assoc @state :corp (assoc-in corp-private [:hand] (get-in @state [:corp :hand]))
                     :runner (assoc-in runner-private [:hand] (get-in @state [:runner :hand])))
       (assoc @state :corp corp-private :runner runner-private))]))

(defn- reset-all-cards
  [cards]
  (let [;; split the cards into regular cards and alt-art cards
        [regular alt] ((juxt filter remove) #(not= "Alternates" (:setname %)) cards)
        regular (into {} (map (juxt :title identity) regular))
        alt (into {} (map (juxt :title identity)
                          ;; take the regular version of this card and change its code to the alt code
                          (map #(assoc (regular (:title %))
                                 :code (:code %)) alt)))]
    (reset! all-cards regular)
    (reset! all-cards-alt alt)))

(defn- handle-command
  "Apply the given command to the given state. Return true if the state should be sent
  back across the socket (if the command was successful or a resolvable exception was
  caught), or false if an error string should."
  [{:keys [gameid action command side user args text cards] :as msg} state]

  (try (do (case action
             "initialize" (reset-all-cards cards);; creates a map from card title to card data
             "start" (core/init-game msg)
             "remove" (do (swap! game-states dissoc gameid)
                          (swap! old-states dissoc gameid))
             "do" (handle-do user command state side args)
             "notification" (when state
                              (swap! state update-in [:log] #(conj % {:user "__system__" :text text}))))

           true)
       (catch Exception e
         (do (println "Error " action command (get-in args [:card :title]) e)
             (try (if state
                    (do (show-error-toast state (keyword side))
                        (swap! state assoc :last-error (str "Error " action " " command " "
                                                            (or (get-in args [:card :title])
                                                                (get-in args [:choice]))
                                                            " " (pr-str e)))
                        true)
                    false)
                  (catch Exception e
                    (do (println "Toast Error " action command (get-in args [:card :title]) e)
                        false)))))))

(defn run [socket]
  "Main thread for handling commands from the UI server. Attempts to apply a command,
  then returns the resulting game state, or another message as appropriate."
  (while true
      ;; Attempt to handle the command. If true is returned, then generate a successful
      ;; message. Otherwise generate an error message.
      (try
        (let [{:keys [gameid action command args] :as msg} (convert (.recv socket))]
          (if (= action "alert")
            (do (doseq [state (vals @game-states)]
                  (doseq [side [:runner :corp]]
                    (toast state side command "warning" {:time-out 0 :close-button true})))
                (.send socket (generate-string "ok")))
            (let [state (@game-states (:gameid msg))
                  old-state (when state (@old-states (:gameid msg)))
                  [old-corp old-runner old-spect] (when old-state (private-states (atom old-state)))]
              (if (handle-command msg state)
                (if (= action "initialize")
                  (.send socket (generate-string "ok"))
                  (if-let [new-state (@game-states gameid)]
                    (let [[new-corp new-runner new-spect] (private-states new-state)]
                      (do
                        (swap! old-states assoc (:gameid msg) @new-state)
                        (if (#{"start" "reconnect" "notification"} action)
                          ;; send the whole state, not a diff
                          (.send socket (generate-string {:action      action
                                                          :runnerstate (strip new-runner)
                                                          :corpstate   (strip new-corp)
                                                          :spectstate  (strip new-spect)
                                                          :gameid      gameid}))
                          ;; send a diff
                          (let [runner-diff (differ/diff (strip old-runner) (strip new-runner))
                                corp-diff (differ/diff (strip old-corp) (strip new-corp))
                                spect-diff (differ/diff (strip old-spect) (strip new-spect))]
                            (.send socket (generate-string {:action     action
                                                            :runnerdiff runner-diff
                                                            :corpdiff   corp-diff
                                                            :spectdiff  spect-diff
                                                            :gameid     gameid}))))))
                    (.send socket (generate-string {:action action :state old-state :gameid gameid}))))
                (.send socket (generate-string "error"))))))
        (catch Exception e
          (try (do (println "Inner Error " e)
                   (.send socket (generate-string "error")))
               (catch Exception e
                 (println "Socket Error " e)))))))

(def zmq-url (str "tcp://" (or (env :zmq-host) "127.0.0.1") ":1043"))

(defn dev []
  (println "[Dev] Listening on port 1043 for incoming commands...")
  (let [socket (.socket ctx ZMQ/REP)]
    (.bind socket zmq-url)
    (run socket)))

(defn -main []
  (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException [_ thread ex]
        (println "UNCAUGHT EXCEPTION " ex))))

  (println "[Prod] Listening on port 1043 for incoming commands...")
  (let [worker-url "inproc://responders"
        router (doto (.socket ctx ZMQ/ROUTER) (.bind zmq-url))
        dealer (doto (.socket ctx ZMQ/DEALER) (.bind worker-url))]
    (.start
      (Thread.
        (fn []
          (let [socket (.socket ctx ZMQ/REP)]
            (.connect socket worker-url)
            (run socket)))))

    (.start (Thread. #(.run (ZMQQueue. ctx router dealer))))))
