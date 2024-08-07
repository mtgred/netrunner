(ns web.stats
  (:require
   [cheshire.core :as json]
   [cljc.java-time.instant :as inst]
   [clojure.string :refer [lower-case]]
   [game.utils :refer [dissoc-in]]
   [jinteki.cards :refer [all-cards]]
   [monger.collection :as mc]
   [monger.operators :refer :all]
   [monger.query :as mq]
   [monger.result :refer [acknowledged?]]
   [ring.util.request :refer [request-url]]
   ;;[web.angel-arena.stats :as angel-arena-stats]
   [web.mongodb :refer [->object-id]]
   [web.pages :as pages]
   [web.user :refer [active-user?]]
   [web.utils :refer [json-response response mongo-time-to-utc-string]]
   [web.ws :as ws]))

(defn clear-userstats-handler
  "Clear any statistics for a given user-id contained in a request"
  [{db :system/db
    {:keys [_id]} :user}]
  (if (acknowledged? (mc/update db :users {:_id (->object-id _id)} {$unset {:stats ""}}))
    (response 200 {:message "Deleted"})
    (response 403 {:message "Forbidden"})))

(defn clear-deckstats-handler
  "Clear any statistics for a given deck-id contained in a request"
  [{db :system/db
    {id :id} :path-params}]
  (if id
    (if (acknowledged? (mc/update db :decks {:_id (->object-id id)} {$unset {:stats ""}}))
      (response 200 {:message "Deleted"})
      (response 403 {:message "Forbidden"}))
    (response 401 {:message "Unauthorized"})))

(defn stats-for-deck
  "Get statistics for a given deck id"
  [db deck-id]
  (mc/find-one-as-map db :decks {:_id (->object-id deck-id)} ["stats"]))

(defn stats-for-user
  "Get statistics for a given user id"
  [db user-id]
  (mc/find-one-as-map db :users {:_id (->object-id user-id)} ["stats"]))

(defn build-stats-kw
  "Take a stats prefix and add a side to it"
  [prefix side]
  (keyword (str prefix (lower-case (or side "Corp")))))

(defn inc-deck-stats
  "Update deck stats for a given counter"
  [db deck-id record]
  (when record
    (mc/update db :decks {:_id (->object-id deck-id)} {$inc record})))

(defn deck-record-end
  [state player]
  (let [enable-deckstats (get-in player [:user :options :deckstats])
        deck-id (get-in player [:deck :_id])
        winning-deck (:winning-deck-id @state)
        losing-deck (:losing-deck-id @state)
        record (merge (when (and enable-deckstats deck-id)
                        {:stats.games-completed 1})
                      (when (and enable-deckstats deck-id (= winning-deck deck-id))
                        {:stats.wins 1})
                      (when (and enable-deckstats deck-id (= losing-deck deck-id))
                        {:stats.loses 1}))]
    record))

(defn update-deck-stats
  "Update stats for player decks on game ending"
  [db {:keys [original-players ending-players state]}]
  (doseq [player original-players]
    (let [enable-deckstats (get-in player [:user :options :deckstats])
          deck-id (get-in player [:deck :_id])]
      (when (and enable-deckstats deck-id)
        (inc-deck-stats db deck-id {:stats.games-started 1}))))
  (doseq [player ending-players]
    (inc-deck-stats db (get-in player [:deck :_id]) (deck-record-end state player))))

(defn inc-game-stats
  "Update user's game stats for a given counter"
  [db user-id record]
  (mc/update db :users {:_id (->object-id user-id)} {$inc record}))

(defn game-record-start
  [p]
  (merge {:stats.games-started 1}
         {(build-stats-kw "stats.games-started-" (:side p)) 1}))

(defn game-record-end
  [state player]
  (let [username (get-in player [:user :username])
        enable-userstats (get-in player [:user :options :gamestats])
        winning-user (:winning-user @state)
        losing-user (:losing-user @state)
        side-str (:side player)
        record (merge {:stats.games-completed 1}
                      {(build-stats-kw "stats.games-completed-" side-str) 1}
                      (when (and (= username winning-user) enable-userstats)
                        {:stats.wins 1 (build-stats-kw "stats.wins-" side-str) 1})
                      (when (and (= username losing-user) enable-userstats)
                        {:stats.loses 1 (build-stats-kw "stats.loses-" side-str) 1}))]
    record))

(defn update-game-stats
  "Update game stats for users on game ending"
  [db {:keys [gameid original-players ending-players state]}]
  (doseq [player original-players]
    (if (:side player)
      (inc-game-stats db (get-in player [:user :_id]) (game-record-start player))
      (println "NULL start player side in stats for gameid" gameid)))
  (doseq [player ending-players]
    (if (:side player)
      (inc-game-stats db (get-in player [:user :_id]) (game-record-end state player))
      (println "NULL end player side in stats for gameid" gameid))))

(defn push-stats-update
  "Gather updated deck and user stats and send via web socket to clients"
  [db {:keys [ending-players]}]
  (doseq [player ending-players]
    (let [user-id (get-in player [:user :_id])
          deck-id (get-in player [:deck :_id])
          userstats (:stats (stats-for-user db user-id))
          deckstats (:stats (stats-for-deck db deck-id))]
      (ws/chsk-send! (:uid player) [:stats/update {:userstats userstats
                                                   :deck-id (str deck-id)
                                                   :deckstats deckstats}]))))

(defn game-started
  [db {:keys [gameid date start-date title room players format]}]
  (let [corp (some #(when (= "Corp" (:side %)) %) players)
        runner (some #(when (= "Runner" (:side %)) %) players)]
    (mc/insert db :game-logs {:gameid (str gameid)
                              :title title
                              :room room
                              :creation-date date
                              :start-date start-date
                              :format format
                              :corp {:player (select-keys (:user corp) [:username :emailhash])
                                     :deck-name (get-in corp [:deck :name])
                                     :identity (get-in corp [:deck :identity :title])}
                              :runner {:player (select-keys (:user runner) [:username :emailhash])
                                       :deck-name (get-in runner [:deck :name])
                                       :identity (get-in runner [:deck :identity :title])}})))

(def game-log-coll "game-logs")

(defn delete-old-replay
  [db {:keys [username]}]
  (let [games (mq/with-collection db game-log-coll
                (mq/find {$and [{$or [{:corp.player.username username}
                                      {:runner.player.username username}]}
                                {:replay {$exists true}}
                                {:replay-shared false}]})
                (mq/fields [:gameid])
                (mq/sort (array-map :start-date -1))
                (mq/skip 15))]
    (doseq [game games]
      (mc/update db :game-logs
                 {:gameid (:gameid game)}
                 {$unset {:replay nil}
                  $set {:has-replay false}}))))

(defn generate-replay [state]
  (json/generate-string
    {:metadata {:winner (:winner @state)
                :reason (:reason @state)
                :end-date (inst/now)
                :stats (-> (:stats @state)
                           (dissoc-in [:time :started])
                           (dissoc-in [:time :ended]))
                :turn (:turn @state)
                :corp.agenda-points (get-in @state [:corp :agenda-point])
                :runner.agenda-points (get-in @state [:runner :agenda-point])}
     :history (:history @state)}))

(defn game-finished
  [db {:keys [state gameid room] :as game}]
  (when state
    (let [should-save-replay (or (get-in @state [:options :save-replay])
                                 (:bug-reported @state))
          should-share-replay (:bug-reported @state)]
      (try
        (mc/update db :game-logs
                   {:gameid (str gameid)}
                   {$set {:winner (:winner @state)
                          :reason (:reason @state)
                          :end-date (inst/now)
                          :stats (-> (:stats @state)
                                     (dissoc-in [:time :started])
                                     (dissoc-in [:time :ended]))
                          :turn (:turn @state)
                          :corp.agenda-points (get-in @state [:corp :agenda-point])
                          :runner.agenda-points (get-in @state [:runner :agenda-point])
                          :bug-reported (:bug-reported @state)
                          :replay (when should-save-replay (generate-replay state))
                          :has-replay (get-in @state [:options :save-replay] false)
                          :replay-shared should-share-replay
                          :log (:log @state)}})
        (when (and should-save-replay (not should-share-replay))
          (delete-old-replay db (get-in @state [:corp :user]))
          (delete-old-replay db (get-in @state [:corp :runner])))
        ;; (when (and (= "angel-arena" room)
        ;;            (:winner @state))
        ;;   (angel-arena-stats/game-finished db game))
        (catch Exception e
          (println "Caught exception saving game stats: " (.getMessage e))
          (println "Stats: " (:stats @state)))))))

(defn history
  [{db :system/db
    {username :username :as user} :user}]
  (if (active-user? user)
    (let [games (->> (mq/with-collection db game-log-coll
                       (mq/find {$or [{:corp.player.username username}
                                      {:runner.player.username username}]})
                       (mq/fields {:replay 0 :log 0 :_id 0})
                       (mq/sort (array-map :start-date -1))
                       (mq/limit 100))
                     (into []))
          games (map #(update % :creation-date mongo-time-to-utc-string) games)
          games (map #(update % :start-date mongo-time-to-utc-string) games)
          games (map #(update % :end-date mongo-time-to-utc-string) games)
          ]
      (response 200 games))
    (response 401 {:message "Unauthorized"})))

(defn fetch-log
  [{db :system/db
    user :user
    {:keys [gameid]} :path-params}]
  (if (active-user? user)
    (let [{:keys [log]} (mc/find-one-as-map db :game-logs {:gameid gameid} ["log"])]
      (response 200 (or log {})))
    (response 401 {:message "Unauthorized"})))

(defn fetch-annotations
  [{db :system/db
    {username :username :as user} :user
    {:keys [gameid]} :path-params}]
  (if (active-user? user)
    (let [{:keys [corp runner replay-shared annotations]}
          (mc/find-one-as-map db game-log-coll {:gameid gameid} ["corp" "runner" "replay-shared" "annotations"])]
      (if (or replay-shared
              (= username (get-in corp [:player :username]))
              (= username (get-in runner [:player :username])))
        (json-response 200 (or annotations []))
        (response 401 {:message "Unauthorized"})))
    (response 401 {:message "Unauthorized"})))

(defn fetch-elapsed [db gameid]
  (let [stats (mc/find-one-as-map db :game-logs {:gameid (str gameid)} ["stats"])]
    (-> stats :stats :time :elapsed)))

(defn check-annotations-size [annotations]
  ; Not more than 50k characters text
  (>= 50000
      (+ (reduce + (map #(count (:notes %)) (vals (get-in annotations [:turns :corp]))))
         (reduce + (map #(count (:notes %)) (vals (get-in annotations [:turns :runner]))))
         (reduce + (map #(count (:notes %)) (vals (:clicks annotations)))))))

(defn publish-annotations
  [{db :system/db
    {username :username} :user
    {:keys [gameid]} :path-params
    body :body}]
  (let [{:keys [corp runner replay replay-shared annotations]}
        (mc/find-one-as-map db :game-logs {:gameid gameid} ["corp" "runner" "replay" "replay-shared" "annotations"])
        replay (or replay {})]
    (if (or replay-shared
            (or (= username (get-in corp [:player :username]))
                (= username (get-in runner [:player :username]))))
      (if (empty? replay)
        (response 404 {:message "Replay not found"})
        (if (check-annotations-size body)
          (let [new-annotations (conj annotations
                                      {:username username
                                       :date (:date body)
                                       :turns {:corp (get-in body [:turns :corp])
                                               :runner (get-in body [:turns :runner])}
                                       :clicks (:clicks body)})]
            (mc/update db :game-logs
                       {:gameid (str gameid)}
                       {$set {:annotations new-annotations}})
            (response 200 {:message "Annotations published"}))
          (response 413 {:message "File too large"})))
      (response 401 {:message "Unauthorized"}))))

(defn delete-annotations
  [{db :system/db
    {username :username} :user
    {:keys [gameid date]} :path-params}]
  (let [{:keys [corp runner replay annotations]}
        (mc/find-one-as-map db :game-logs {:gameid gameid} ["corp" "runner" "replay" "replay-shared" "annotations"])
        replay (or replay {})
        annotations (or annotations [])
        [ind anno] (first (filter #(= date (str (:date (second %)))) (map-indexed vector annotations)))]
    (if (or (= username (:username anno))
            (= username (get-in runner [:player :username]))
            (= username (get-in corp [:player :username])))
      (if (empty? replay)
        (response 404 {:message "Replay not found"})
        (if (and ind anno)
          (let [new-annotations
                (vec (concat (subvec annotations 0 ind)
                             (subvec annotations (inc ind))))]
            (mc/update db :game-logs
                       {:gameid (str gameid)}
                       {$set {:annotations new-annotations}})
            (response 200 {:message "Annotations deleted"}))
          (response 404 {:message "Annotations not found"})))
      (response 401 {:message "Unauthorized"}))))

(defn fetch-replay
  [{db :system/db
    {username :username} :user
    {:keys [gameid]} :path-params}]
  (let [{:keys [corp runner replay replay-shared bug-reported]}
        (mc/find-one-as-map db :game-logs {:gameid gameid} ["corp" "runner" "replay" "replay-shared" "bug-reported"])
        replay (or replay {})]
    (if (or bug-reported
            replay-shared
            (or (= username (get-in corp [:player :username]))
                (= username (get-in runner [:player :username]))))
      (if (empty? replay)
        (response 404 {:message "Replay not found"})
        (json-response 200 replay))
      (response 401 {:message "Unauthorized"}))))

(defn share-replay
  [{db :system/db
    {username :username} :user
    {:keys [gameid]} :path-params}]
  (if username
    (try
      (mc/update db :game-logs
                 {$and [{:gameid (str gameid)}
                        {$or [{:corp.player.username username}
                              {:runner.player.username username}]}]}
                 {$set {:replay-shared true}})
      (response 200 {:message "Shared"})
      (catch Exception e
        (println "Caught exception sharing game: " (.getMessage e))
        (response 500 {:message "Server error"})))
    (response 401 {:message "Unauthorized"})))

(defn- get-winner-card
  [winner corp runner host]
  (let [default-img (str host "/img/icons/jinteki_167.png")]
    (if winner
      (let [win-id (:identity ((keyword winner) {:corp corp :runner runner}))
            win-card-img (get-in (@all-cards win-id) [:images :en :default :stock])]
        (if win-card-img
          (str host win-card-img)
          default-img))
      default-img)))

(defn replay-handler
  [{db :system/db
    {:keys [gameid bugid]}        :path-params
    {:keys [n d b]}               :query-params
    scheme                        :scheme
    headers                       :headers
    :as req}]
  (let [{:keys [replay winner corp runner title]} (mc/find-one-as-map db :game-logs {:gameid (or gameid bugid)})
        replay (or replay {})
        gameid-str (cond ; different string for replays and bug-reports
                     gameid (if (and n d) (str gameid "?n=" n "&d=" d) gameid)
                     bugid (str bugid "?bug-report" (when b (str "&b=" b))))]
    (if (empty? replay)
      (response 404 {:message "Replay not found"})
      (let [corp-user (get-in corp [:player :username] "Unknown")
            corp-id (:identity corp)
            runner-user (get-in runner [:player :username] "Unknown")
            runner-id (:identity runner)
            host (str (name scheme) "://" (get headers "host"))
            og {:type "website"
                :url (request-url req)
                :image (get-winner-card winner corp runner host)
                :title (str (cond
                              gameid "REPLAY: "
                              bugid "BUG-REPORT: ")
                            title)
                :site_name "jinteki.net"
                :description (str corp-user " (" corp-id ") vs. " runner-user " (" runner-id ")")}]
        (pages/index-page req og gameid-str)))))
