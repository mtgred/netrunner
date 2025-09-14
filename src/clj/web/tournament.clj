(ns web.tournament
  (:require
   [cheshire.core :as json]
   [clj-uuid :as uuid]
   [cljc.java-time.instant :as inst]
   [cljc.java-time.duration :as duration]
   [cljc.java-time.temporal.chrono-unit :as chrono]
   [clojure.string :as str]
   [clojure.core.async :as async]
   [game.core :as core]
   [jinteki.utils :refer [str->int]]
   [monger.operators :refer :all]
   [org.httpkit.client :as http]
   ;; [web.lobby :refer [all-games refresh-lobby close-lobby]]
   [web.mongodb :refer [find-maps-case-insensitive]]
   [web.app-state :as app-state]
   [web.lobby :as lobby]
   [web.stats :refer [fetch-elapsed]]
   [web.utils :refer [response]]
   [web.game :refer [handle-message-and-send-diffs!]]
   [web.ws :as ws]))

(defn auth [_] (response 200 {:message "ok"}))

(defn wrap-with-to-handler
  "Wrap a function in a handler which checks that the user is a tournament organizer."
  [handler]
  (fn [{{user :user} :ring-req
        reply-fn :?reply-fn
        :as msg}]
    (if (:tournament-organizer user)
      (do (handler msg)
          (when reply-fn (reply-fn 200)))
      (when reply-fn (reply-fn 403)))))

(defn- view-tables
  [{uid :uid}]
  ;; find all tables in the tournament lobbie
  ;; strip them to just:
  ;;   id, player1, player2, title, time-extension
  (let [strip-players (fn [players] (mapv #(select-keys % [:uid :side]) players))
        comp-lobbies (->> (app-state/get-lobbies)
                          (filter #(= (:room %) "competitive"))
                          (map #(select-keys % [:gameid :title :players :time-extension :excluded?]))
                          (map #(update % :players strip-players)))]
    (ws/broadcast-to! [uid] :tournament/view-tables {:competitive-lobbies (vec comp-lobbies)
                                                     :tournament-state (app-state/tournament-state)})))

(defonce tasks (atom {}))

(defn cancel-task!
  "Cancel scheduled task by key"
  [keyvec]
  (when-let [{:keys [stop-chan]} (get-in @tasks keyvec nil)]
    (async/close! stop-chan)
    (swap! tasks dissoc key)))

(defn cancel-tasks-for-lobby!
  "Cancel tasks for a given lobby"
  [{:keys [gameid] :as lobby}]
  (doseq [k (keys (get tasks gameid []))]
    (cancel-task! [gameid k])))

(defn cancel-all-tasks!
  "Cancel all pending tasks and make a clean atom"
  []
  (doseq [outer (keys @tasks)
          inner (keys (get @tasks outer []))]
    (cancel-task! [outer inner])
    (reset! tasks {})))

(defn schedule-task
  "schedules a task under `keyvec` to occur at `time`.
  If the task exists already, it will be cancelled/rescheduled"
  [keyvec time f]
  ;; do not schedule tasks in the past
  (let [now (inst/now)]
    (when-not (inst/is-before time now)
      (cancel-task! keyvec)
      (let [stop-chan (async/chan)
            delay-ms (* 1000  (- (inst/get-epoch-second time) (inst/get-epoch-second now)))
            task-chan (async/go
                        (let [timeout-chan (async/timeout delay-ms)]
                          (async/alt!
                            timeout-chan (do (f) :done)
                            stop-chan :cancelled)))]
        (swap! tasks assoc-in keyvec {:stop-chan stop-chan :task-chan task-chan})
        task-chan))))

(defn- alert-lobby
  [{:keys [gameid] :as lobby} msg]
  (when-let [actual-lobby (app-state/get-lobby gameid)]
    (if (:excluded? actual-lobby)
      (cancel-tasks-for-lobby! actual-lobby)
      (if (:started actual-lobby)
        ;; in game - use the in-game thing
        (handle-message-and-send-diffs! actual-lobby nil nil (str "[!] " msg))
        (lobby/lobby-thread
          (let [timestamp (inst/now)
                message (core/make-message {:user {:username "TOURNAMENT SCHEDULER" :uid "TOURNAMENT SCHEDULER"} :text msg})
                new-app-state (swap! app-state/app-state
                                     update :lobbies #(-> %
                                                          (lobby/handle-send-message gameid message)
                                                          (lobby/handle-set-last-update gameid "TOURNAMENT SCHEDULER")))
                lobby? (get-in new-app-state [:lobbies gameid])]
            (lobby/send-lobby-state lobby?)
            (lobby/log-delay! timestamp :tournament-alert-lobby)))))))

(defn- offset-time
  [time minutes seconds]
  (inst/plus (inst/plus time (duration/of-minutes (or minutes 0))) (duration/of-seconds (or seconds 0))))

(defn- schedule-lobby!
  [{:keys [gameid time-extension] :as lobby}]
  (when-let [{:keys [round-start round-start-alert round-start-1m-alert
                     round-20m-warning round-5m-warning round-1m-warning round-end
                     round-time-call round-time-explainer report-match]
              :as round} (app-state/tournament-state)]
    (when round-start-alert    (schedule-task [gameid :round-start] round-start (fn [] (alert-lobby lobby "The round has begun!"))))
    (when round-start-1m-alert (schedule-task [gameid :round-start-1m] (inst/minus round-start (duration/of-minutes 1)) (fn [] (alert-lobby lobby "The round will begin in one minute."))))

    (when round-end            (schedule-task [gameid :round-end]      (offset-time round-end time-extension 0) (fn [] (alert-lobby lobby round-time-call))))
    (when round-time-explainer (schedule-task [gameid :round-explain]  (offset-time round-end time-extension 5) (fn [] (alert-lobby lobby round-time-explainer))))
    (when round-1m-warning     (schedule-task [gameid :round-1m-warn]  (inst/plus round-end (duration/of-minutes (- (or time-extension 0) 1))) (fn [] (alert-lobby lobby "1 minute remaining in the round"))))
    (when round-5m-warning     (schedule-task [gameid :round-5m-warn]  (inst/plus round-end (duration/of-minutes (- (or time-extension 0) 5))) (fn [] (alert-lobby lobby "5 minutes remaining in the round"))))
    (when round-20m-warning    (schedule-task [gameid :round-20m-warn] (inst/plus round-end (duration/of-minutes (- (or time-extension 0) 20))) (fn [] (alert-lobby lobby "20 minutes remaining in the round"))))
    (when report-match         (schedule-task [gameid :report-match]   (offset-time round-end time-extension 10) (fn [] (alert-lobby lobby (str "Report your match here: " report-match)))))))

(defmethod lobby/assign-tournament-properties :default [{:keys [gameid] :as lobby}]
  (when-let [lobby? (app-state/get-lobby gameid)]
    (when (and (= "competitive" (:room lobby))
               (not (:exclude? lobby)))
      (schedule-lobby! lobby?))))

(defn- conclude-round
  [{uid :uid}]
  (swap! app-state/app-state assoc :tournament nil)
  (cancel-all-tasks!)
  (view-tables {:uid uid}))

(defn- declare-round
  [{{:keys [tournament-settings]} :?data
    uid :uid}]
  (if (app-state/tournament-state)
    (do (ws/broadcast-to! [uid] :tournament/declare-round {:error "A round is already underway"})
        (view-tables {:uid uid}))
    ;; we need to convert the tournament settings to timestamps
    (let [now (inst/truncated-to (inst/now) chrono/seconds)
          start-in (get-in tournament-settings [:round-start :start-in] 0)
          ;; start of round?
          round-start          (inst/plus now (duration/of-minutes start-in))
          round-start-alert    (get-in tournament-settings [:round-start :alert] nil)
          round-start-1m-alert (when (pos? start-in)
                                 (get-in tournament-settings [:round-start :one-minute-warning] nil))
          ;; round itself
          round-length         (get-in tournament-settings [:round :time-in-round] 0)
          round-end            (inst/plus (inst/plus now (duration/of-minutes round-length)) (duration/of-minutes start-in))
          round-20-minute-warning (when (get-in tournament-settings [:round :twenty-minute-warning] false) (inst/minus round-end (duration/of-minutes 20)))
          round-5-minute-warning  (when (get-in tournament-settings [:round :five-minute-warning]    true) (inst/minus round-end (duration/of-minutes 5)))
          round-1-minute-warning  (when (get-in tournament-settings [:round :one-minute-warning]    false) (inst/minus round-end (duration/of-minutes 1)))
          round-time-call        (get-in tournament-settings [:round :time-expiry-text] "-- TIME IN ROUND --")
          round-explain-time-resolution? (get-in tournament-settings [:round :explain-time-resolution] true)
          round-time-expiry-rules-text (get-in tournament-settings [:round :time-expiry-rules-text] "Time has been called. The active player finishes their turn, then the opposing player takes a turn. If the game has not concluded by the end of that turn, then the game is decided on agenda points.")

          ;; reporting of the matches
          show-report-match-url? (get-in tournament-settings [:reporting :self-reporting] false)
          report-match-url (get-in tournament-settings [:reporting :self-reporting-url] nil)

          ;; final config
          tournament-config {:source-uid           uid
                             :round-start          round-start
                             :round-start-alert    round-start-alert
                             :round-start-1m-alert round-start-1m-alert
                             :round-end            round-end
                             :round-20m-warning    round-20-minute-warning
                             :round-5m-warning     round-5-minute-warning
                             :round-1m-warning     round-1-minute-warning
                             :round-time-call      round-time-call
                             :round-time-explainer (when round-explain-time-resolution? round-time-expiry-rules-text)
                             :report-match         (when show-report-match-url? report-match-url)
                             }]
      (when-not (app-state/tournament-state)
        (swap! app-state/app-state assoc :tournament tournament-config)
        (doseq [lobby (->> (app-state/get-lobbies)
                           (filter #(= (:room %) "competitive")))]
          (schedule-lobby! lobby)))
        ;; schedule
      (view-tables {:uid uid}))))

(defmethod ws/-msg-handler :tournament/conclude-round
  tournament--conclude-round
  [event]
  ((wrap-with-to-handler conclude-round) event))

(defmethod ws/-msg-handler :tournament/declare-round
  tournament--declare-round
  [event]
  ((wrap-with-to-handler declare-round) event))

;; gets a list of all competitive lobbies
(defmethod ws/-msg-handler :tournament/view-tables
  tournament--view-tables
  [event]
  ((wrap-with-to-handler view-tables) event))

(defn- update-tables
  [{{:keys [competitive-lobbies]} :?data
    uid :uid}]
  (let [competitive-lobbies (mapv #(select-keys % [:gameid :excluded? :time-extension]) competitive-lobbies)
        to-update (into {} (map (juxt :gameid identity) competitive-lobbies))]
    (swap! app-state/app-state update :lobbies
           #(merge-with merge % (select-keys to-update (keys %))))
    (doseq [lobby (->> (app-state/get-lobbies)
                       (filter #(= (:room %) "competitive")))]
      (schedule-lobby! lobby))
    (view-tables {:uid uid})))

(defmethod ws/-msg-handler :tournament/update-tables
  tournament--update-tables
  [event]
  ((wrap-with-to-handler update-tables) event))
