(ns web.angel-arena
  (:require
   [cljc.java-time.duration :as duration]
   [cljc.java-time.instant :as inst]
   [clojure.string :refer [lower-case] :as str]
   [game.core.diffs :as diffs]
   [game.core.say :refer [make-system-message system-msg]]
   [game.core.winning :refer [win]]
   [game.utils :refer [in-coll?]]
   [jinteki.utils :refer [other-side]]
   [monger.operators :refer :all]
   [monger.query :as mq]
   [web.angel-arena.runs :refer [add-new-match! finish-run! start-run!]]
   [web.angel-arena.utils :refer [get-deck-from-id get-runs
                                  inactive-period-countdown inactive-period-warning
                                  max-inactivity-count supported-formats]]
   [web.app-state :as app-state]
   [web.game :as game]
   [web.lobby :as lobby]
   [web.stats :as stats]
   [web.utils :refer [average]]
   [web.ws :as ws]))

(defonce arena-queue (atom []))
(defonce arena-queue-times
  (atom (into {} (for [form supported-formats]
                   [form {:corp [] :runner []}]))))

(defmethod ws/-msg-handler :angel-arena/fetch-runs
  angel-arena--fetch-runs
  [{{db :system/db
     user :user} :ring-req
    ?reply-fn :?reply-fn}]
  (when user
    (when-let [runs (get-runs db (:username user))]
      (?reply-fn runs))))

(def coll "angel-arena")

(defmethod ws/-msg-handler :angel-arena/fetch-history
  angel-arena--fetch-history
  [{{db :system/db
     user :user} :ring-req
    ?reply-fn :?reply-fn}]
  (when user
    (let [runs (mq/with-collection db coll
                 (mq/find (select-keys user [:username]))
                 (mq/sort (array-map :run-finished -1))
                 (mq/limit 5))]
      (?reply-fn runs))))

(defmethod ws/-msg-handler :angel-arena/fetch-queue-times
  angel-arena--fetch-queue-times
  [{{user :user} :ring-req
    ?reply-fn :?reply-fn}]
  (when user
    (let [times @arena-queue-times
          response (into {} (for [form supported-formats
                                  :let [corp-times (map duration/to-millis (get-in times [form :corp] []))
                                        runner-times (map duration/to-millis (get-in times [form :runner] []))]]
                              [form {:corp (quot (average corp-times) 1000)
                                     :runner (quot (average runner-times) 1000)}]))]
      (?reply-fn response))))

(defmethod ws/-msg-handler :angel-arena/start-run
  angel-arena--start-run
  [{{db :system/db
     {:keys [username]} :user} :ring-req
    {:keys [deck-id]} :?data}]
  (when username
    (try
      (let [runs (get-runs db username)
            deck (get-deck-from-id db username deck-id)
            form (keyword (lower-case (get-in deck [:status :format])))
            side (keyword (lower-case (get-in deck [:identity :side])))]
        (when-not (get-in runs [form side]) ; when not already running on this side and format
          (start-run! db username runs deck)))
      (catch Exception e
        (println "Caught exception while starting a new run: " (.getMessage e))))))

(defmethod ws/-msg-handler :angel-arena/abandon-run
  angel-arena--abandon-run
  [{{db :system/db
     {:keys [username]} :user} :ring-req
    uid :uid
    {:keys [deck-id]} :?data}]
  (when username
    (try
      (let [runs (get-runs db username)
            deck (get-deck-from-id db username deck-id)
            form (keyword (lower-case (get-in deck [:status :format])))
            side (keyword (lower-case (get-in deck [:identity :side])))]
        (when (get-in runs [form side]) ; there's a run in this side and format
          (finish-run! db username runs deck)
          (ws/chsk-send! uid [:angel-arena/run-update])))
      (catch Exception e
        (println "Caught exception while abandoning run: " (.getMessage e))))))

(defn- remove-from-queue! [username]
  (swap! arena-queue (partial remove #(= username (get-in % [:user :username])))))

(defn- add-queue-time! [player]
  (let [side (keyword (lower-case (:side player)))
        form (:format player)
        queue-time (duration/between (:queue-start player) (inst/now))]
    ; keep the latest 5 wait times
    (if (< 5 (count (get-in @arena-queue-times [form side])))
      (swap! arena-queue-times update-in [form side] #(conj (next %) queue-time))
      (swap! arena-queue-times update-in [form side] conj queue-time))))

(def angel-arena-created-message
  (make-system-message "Angel Arena lobby has been created."))

(def angel-arena-intro-message
  (make-system-message
    (str/join
      " " ["This game is played in the Angel Arena, a competitive matchmaking system."
           "Wins and losses of your run are being tracked."
           "If by any error, the game should prematurely register a win, please use the"
           "/clear-win command to continue playing the game."
           "Good luck and have fun!"])))

(defn create-new-angel-arena-lobby
  [player1 player2 form]
  (let [title (str "Match between "
                   (get-in player1 [:user :username])
                   " and "
                   (get-in player2 [:user :username]))
        game (-> (lobby/create-new-lobby
                   {:options
                    {:allow-spectator true
                     :api-access      true
                     :format          (name form)
                     :mute-spectators true
                     :password        nil
                     :room            "angel-arena"
                     :save-replay     true
                     :spectatorhands  false
                     :timer           false
                     :title           title}})
                 (assoc :players [player1 player2]
                        :original-players [player1 player2])
                 (lobby/send-message angel-arena-created-message)
                 (lobby/send-message angel-arena-intro-message))]
    game))

(defn find-eligible-player [player form]
  (let [side (:side player)
        username (-> player :user :username)
        other-side (if (= side "Corp") "Runner" "Corp")
        played-them-fn (fn [other-player]
                         (in-coll?
                           (map #(get-in % [:opponent :username])
                                (filter :winner (get-in player [:run-info :games])))
                           (get-in other-player [:user :username])))
        they-played-us-fn (fn [other-player]
                            (in-coll?
                              (map #(get-in % [:opponent :username])
                                   (filter :winner (get-in other-player [:run-info :games])))
                              username))
        eligible-players (->> @arena-queue
                              ; Players in the same format playing the other side
                              (filter #(and (= form (:format %))
                                            (= other-side (:side %))))
                              ; Players that we didn't already play
                              (remove played-them-fn)
                              ; Players that didn't already play us
                              (remove they-played-us-fn)
                              ; Players that didn't block us
                              (remove #(in-coll?
                                         (get-in % [:user :options :blocked-users])
                                         username))
                              ; Players that we didn't block
                              (remove #(in-coll?
                                         (get-in player [:user :options :blocked-users])
                                         (get-in % [:user :username]))))]
    (first eligible-players)))

(defn update-angel-arena-db! [db player opponent gameid]
  (remove-from-queue! (get-in opponent [:user :username]))
  (add-queue-time! player)
  (add-queue-time! opponent)
  (add-new-match! db player opponent gameid)
  (add-new-match! db opponent player gameid))

(defmethod ws/-msg-handler :angel-arena/queue
  angel-arena--queue
  [{{db :system/db user :user} :ring-req
    uid :uid
    {:keys [deck-id]} :?data}]
  (when user
    (let [username (:username user)
          runs (get-runs db username)
          deck (get-deck-from-id db username deck-id)
          form (keyword (lower-case (get-in deck [:status :format])))
          side (keyword (lower-case (get-in deck [:identity :side])))
          run-info (get-in runs [form side])]
      (when (and runs deck form side
                 ; check that player isn't already queueing
                 (empty? (filter #(= username (:username %)) @arena-queue)))
        (let [player {:user user
                      :uid uid
                      :format form
                      :side (if (= :corp side) "Corp" "Runner")
                      :deck deck
                      :run-info run-info
                      :queue-start (inst/now)}
              opponent (find-eligible-player player form)]
          (prn username (:uid opponent))
          ;; if we find an opponent, create a new lobby object, start the game using
          ;; the :game/start logic, update the db with player info and stats, and then
          ;; signal to both players the game has started
          (if opponent
            (let [lobby (create-new-angel-arena-lobby
                         (dissoc player :queue-start)
                         (dissoc opponent :queue-start)
                         form)
                  gameid (:gameid lobby)
                  players [(dissoc player :queue-start)
                           (dissoc opponent :queue-start)]
                  ;; duplicate logic from :game/start
                  ;; instead of trying to find some awkward way to call both
                  new-app-state
                  (swap! app-state/app-state
                         update :lobbies
                         #(-> %
                              (lobby/register-lobby lobby uid)
                              (game/handle-start-game gameid players (:date lobby))
                              (lobby/handle-set-last-update gameid uid)))
                  lobby? (get-in new-app-state [:lobbies gameid])]
              (if lobby?
                (do (update-angel-arena-db! db player opponent gameid)
                    (stats/game-started db lobby?)
                    (lobby/send-lobby-state lobby?)
                    (lobby/broadcast-lobby-list)
                    (game/send-state-to-participants :game/start lobby? (diffs/public-states (:state lobby?))))
                (swap! arena-queue conj player)))
            ;; Otherwise, enqueue the player and wait for a match
            (swap! arena-queue conj player)))))))

(defmethod ws/-msg-handler :angel-arena/dequeue
  angel-arena--dequeue
  [{{{:keys [username]} :user} :ring-req}]
  (when username
    (remove-from-queue! username)))

(defn is-maybe-inactive? [last-update]
  (inst/is-after (inst/now) (inst/plus-seconds last-update inactive-period-warning)))

(defn is-inactive? [last-update]
  (inst/is-after (inst/now) (inst/plus-seconds last-update inactive-period-countdown)))

(defn strip-user [user]
  (select-keys user [:username :emailhash]))

(defn set-inactive-left [state user side now]
  (swap! state assoc-in [:angel-arena-info :inactivity-warning]
         {:stage :inactive-left
          :inactive-user (strip-user user)
          :inactive-side side
          :warning-time now}))

(defn set-inactive-pre-start [state now]
  (swap! state assoc-in [:angel-arena-info :inactivity-warning]
         {:stage :inactive-pre-start
          :inactive-side nil
          :inactive-user nil
          :warning-time now
          :period-to-react -1}))

(defn set-inactive-start [state user side now]
  (swap! state assoc-in [:angel-arena-info :inactivity-warning]
         {:stage :inactive-warning
          :inactive-user (strip-user user)
          :inactive-side side
          :warning-time now
          :period-to-react inactive-period-countdown}))

(defn set-inactive-countdown [state]
  (swap! state assoc-in [:angel-arena-info :inactivity-warning :stage] :inactive-countdown))

(defn reset-inactive [state]
  (swap! state update :angel-arena-info dissoc :inactivity-warning))

(defn player-left-lobby [lobby]
  (let [{:keys [original-players players]} lobby
        active-username (get-in (first players) [:user :username])
        {inactive-user :user
         inactive-side :side} (first (remove #(= active-username (get-in % [:user :username])) original-players))
        inactive-side (keyword (lower-case inactive-side))]
    (game/update-and-send-diffs!
      set-inactive-left lobby inactive-user inactive-side (inst/now))))

(defn check-for-inactivity
  "Called by a background thread to notify lobbies without activity."
  [_db]
  (let [changed? (volatile! false)]
    ; TODO: Turn this into an option for all games, if it is liked by the community
    (doseq [{:keys [state gameid last-update players] :as lobby}
            (filter #(= "angel-arena" (:room %)) (app-state/get-lobbies))]
      (let [inactive-side (if (:end-turn @state)
                            (other-side (:active-player @state))
                            (:active-player @state))
            inactive-user (get-in @state [inactive-side :user])]
        (if (= 1 (count players))
          ; Player leaves
          (when (is-inactive? last-update)
            (vreset! changed? true)
            (player-left-lobby lobby))
          ; Player inactive
          (when-not (or (nil? gameid)
                        (:run @state))
            (if (zero? (:turn @state))
              (when (is-inactive? last-update)
                (game/update-and-send-diffs!
                  set-inactive-pre-start lobby (inst/now)))

              (case (get-in @state [:angel-arena-info :inactivity-warning :stage] :inactive-start)
                :inactive-start
                (when (is-maybe-inactive? last-update)
                  ; no action for longer than first inactivity-period
                  (game/update-and-send-diffs!
                    set-inactive-start lobby inactive-user inactive-side (inst/now)))

                :inactive-warning
                (when-let [{:keys [warning-time period-to-react]}
                           (get-in @state [:angel-arena-info :inactivity-warning])]
                  (if (inst/is-after last-update warning-time)
                    ; there was an action after the warning
                    (game/update-and-send-diffs! reset-inactive lobby)
                    ; still no action
                    (when (inst/is-after (inst/now) (inst/plus-seconds warning-time period-to-react))
                      ; reaction period over
                      (game/update-and-send-diffs! set-inactive-countdown lobby))))

                ;; everything else
                (when-let [warning-time (get-in @state [:angel-arena-info :inactivity-warning :warning-time])]
                  (when (inst/is-after last-update warning-time)
                    ; there was an action after the warning
                    (game/update-and-send-diffs! reset-inactive lobby)))))))))
    (when @changed?
      (lobby/broadcast-lobby-list))))

(defn request-more-time [state inactive-side]
  (-> state
      (update :angel-arena-info dissoc :inactivity-warning)
      (update-in [:angel-arena-info :inactivity-counter inactive-side] (fnil dec max-inactivity-count))))

(defmethod ws/-msg-handler :angel-arena/more-time
  angel-arena--more-time
  [{{{:keys [username]} :user} :ring-req
    uid :uid
    {:keys [gameid]} :?data}]
  (let [{:keys [state] :as lobby} (app-state/get-lobby gameid)
        {:keys [inactive-side inactive-user] :as inactive-state} (get-in @state [:angel-arena-info :inactivity-warning])]
    (when (and state
               inactive-state
               (or (= username (get-in @state [:corp :user :username]))
                   (= username (get-in @state [:runner :user :username])))
               (= username (:username inactive-user))
               (pos? (get-in @state [:angel-arena-info :inactivity-counter inactive-side] 1)))
      (system-msg state inactive-side (str "has asked for more time ("
                                           (get-in @state [:angel-arena-info :inactivity-counter inactive-side])
                                           " remaining)"))
      (swap! app-state/app-state
                         update :lobbies lobby/handle-set-last-update gameid uid)
      (game/update-and-send-diffs! request-more-time lobby inactive-side))))

(defmethod ws/-msg-handler :angel-arena/claim-victory
  angel-arena--claim-victory
  [{{db :system/db
     {:keys [username]} :user} :ring-req
    {:keys [gameid]} :?data}]
  (let [{:keys [state] :as lobby} (app-state/get-lobby gameid)
        {:keys [stage inactive-side] :as inactive-state} (get-in @state [:angel-arena-info :inactivity-warning])]
    (when (and state
               inactive-state
               (or (= username (get-in @state [:corp :user :username]))
                   (= username (get-in @state [:runner :user :username])))
               (or (= stage :inactive-left)
                   (and (= stage :inactive-countdown)
                        (= username (get-in @state [(other-side inactive-side) :user :username])))))
      (let [old-state @state]
        (system-msg state (other-side inactive-side) "claims a victory")
        (win state (other-side inactive-side) "Claim")
        (lobby/close-lobby! db lobby)
        (game/send-state-diffs lobby (diffs/public-diffs old-state state))))))

(defmethod ws/-msg-handler :angel-arena/cancel-match
  angel-arena--cancel-match
  [{{db :system/db
     {:keys [username]} :user} :ring-req
    {:keys [gameid]} :?data}]
  (let [{:keys [state] :as lobby} (app-state/get-lobby gameid)
        {:keys [stage inactive-side] :as inactive-state} (get-in @state [:angel-arena-info :inactivity-warning])]
    (when (and state
               inactive-state
               (or (= username (get-in @state [:corp :user :username]))
                   (= username (get-in @state [:runner :user :username])))
               (or (= stage :inactive-pre-start)
                   (= stage :inactive-left)
                   (and (= stage :inactive-countdown)
                        (= username (get-in @state [(other-side inactive-side) :user :username])))))
      (let [old-state @state]
        (system-msg state (other-side inactive-side) "cancels the match")
        (lobby/close-lobby! db lobby)
        (game/send-state-diffs lobby (diffs/public-diffs old-state state))))))
