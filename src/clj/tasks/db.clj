(ns tasks.db
  "Database maintenance tasks"
  (:require
    ;; external
    [monger.collection :as mc]
    [monger.db]
    [monger.operators :refer :all]
    [clj-uuid :as uuid]
    ;; internal
    [jinteki.validator :refer [calculate-deck-status]]
    [tasks.setup :refer [connect disconnect]]
    [web.auth :refer [create-user]]
    [web.mongodb :refer [object-id]]
    [web.decks :refer [hash-deck update-deck prepare-deck-for-db]]
    [web.nrdb :refer [download-public-decklist]]
    [web.utils :refer [md5]]))

(defn- get-deck-status
  [deck]
  (if (or (nil? (:identity deck)) (empty? (:identity deck)))
    (throw (Exception. "Nil/Empty identity"))
    (let [updated-deck (update-deck deck)]
      (calculate-deck-status updated-deck))))

(defn update-all-decks
  "Run after fetching the data to update all decks"
  [& args]
  (let [{{:keys [db]} :mongodb/connection :as system} (connect)
        cnt (atom 0)]
    (doseq [deck (mc/find-maps db "decks" nil)]
      (let [deck-id (:_id deck)]
        (swap! cnt inc)
        (when (zero? (mod @cnt 1000)) (do (print ".") (flush)))
        (try
          (let [status (get-deck-status deck)]
            (mc/update db "decks"
                       {:_id (object-id deck-id)}
                       {"$set" {"status" status}}))
          (catch Exception e (do (println "Something got hecked" (.getMessage e))
                                 (println "Deck id:" deck-id))))))
    (newline)
    (println "Updated" @cnt "decks")
    (disconnect)))

(defn- get-all-users
  "Get all users in the database. Takes a list of fields."
  [db fields]
  (mc/find-maps db "users" {} fields))

(defn- delete-user
  "Delete a user by Mongo document id"
  [db id]
  (mc/remove-by-id db "users" id))

(defn delete-duplicate-users
  "Delete entries in the users table that share a username. Leave the first registered entry found in the collection."
  [& args]
  (try
    (let [{{:keys [db]} :mongodb/connection :as system} (connect)
          dry-run (some #{"--dry-run"} args)
          users (get-all-users db [:email :username :registrationDate :lastConnection])
          grouped (vals (group-by :username users))
          duplicates (filter #(> (count %) 1) grouped)]
      (when dry-run
        (println "DRY RUN: not deleting accounts"))
      (println "Found" (count users) "user accounts.")
      (println "Found" (count duplicates) "duplicated usernames.")
      (doseq [d duplicates]
        (let [[f & r] (sort-by :registrationDate d)]
          (println "Found username:" (:username f))
          (println "\tKeeping:" (:email f) "," (:registrationDate f))
          (if dry-run
            (println "\tWould delete:")
            (println "\tDeleting:"))
          (doseq [del r]
            (println "\t\t" (:email del) "," (:registrationDate del))
            (when (not dry-run)
              (delete-user db (:_id del)))))))
    (catch Exception e (do
                         (println "Delete duplicate users failed" (.getMessage e))
                         (.printStackTrace e)))
    (finally (disconnect))))

(defn- prepare-sample-decks
  [nrdb-urls]
  (vec
   (for [url nrdb-urls]
     (let [deck (assoc (download-public-decklist url)
                       :date (java.util.Date.)
                       :format "standard")
           updated-deck (update-deck deck)
           status (calculate-deck-status updated-deck)
           deck-hash (hash-deck updated-deck)]
       (if (> (count (:cards deck)) 0)
         {:deck deck
          :deck-hash deck-hash
          :status status}
         (throw (Exception. "A deck contains no cards. Did you forget to run `lein fetch`?")))))))

(defn- create-sample-user
  [username template]
  (let [email      (str username "@example.com")
        email-hash (md5 email)]
    (assoc template
           :username username
           :email email
           :email-hash email-hash)))

(defn- create-sample-deck
  [username decks]
  (let [{deck :deck, deck-hash :deck-hash, status :status} (rand-nth decks)]
    (prepare-deck-for-db deck username status deck-hash)))

(defn- create-sample-game-log
  [username]
  (let [email (str username "@example.com")
        players {:runner {:player {:username "<nobody>", :emailhash (md5 "nobody@example.com")},
                          :deck-name "Firestorm (Worlds 110th)",
                          :identity "Ele \"Smoke\" Scovak: Cynosure of the Net",
                          :agenda-points 0}
                 :corp {:player {:username "<nobody>", :emailhash (md5 "nobody@example.com")},
                        :deck-name "That One SYNC Deck -- 35th at Worlds",
                        :identity "SYNC: Everything, Everywhere",
                        :agenda-points 0}}
        side (rand-nth (keys players))
        players (update players side
                        #(assoc % :player {:username username :emailhash (md5 email)}))]
    {:gameid (uuid/v4),
     :format "standard",
     :replay-shared false,
     :end-date (java.util.Date.),
     :winner "runner",
     :replay nil,
     :title (str username "'s game"),
     :turn 0,
     :reason "Concede",
     :creation-date (java.util.Date.),
     :runner (:runner players),
     :corp (:corp players),
     :room "casual",
     :start-date (java.util.Date.),
     :log
     [{:user "__system__", :text "<username> has created the game."}
      {:user "__system__", :text "<username> joined the game."}
      {:user "__system__", :text "[hr]"}
      {:user "__system__", :text "<username> keeps their hand."}
      {:user "__system__", :text "<username> keeps their hand."}
      {:user "__system__", :text "<username> concedes."}
      {:user "__system__", :text "<username> wins the game."}
      {:user "__system__", :text "<username> has left the game."}],
     :stats
     {:time {:elapsed 0}, :corp {:gain {:card 5}}, :runner {:gain {:card 5}}}}))

(defn- create-sample-message
  [username messages]
  (let [email (str username "@example.com")
        message (rand-nth messages)]
    {:username username
     :emailhash (md5 email)
     :msg message
     :channel "general"
     :date (java.util.Date.)}))

(defn- samples-for-user
  "Number of samples to be created for a specific user.

  This will always yield at least `min-total-samples` when adding all sample
  counts together (see `total-samples`). Due to rounding issues, it will
  usually produce more samples."
  [user-index users min-total-samples]
  (let [common-case (/ min-total-samples (+ 998 users))]
    (cond
      (<= users 1)      min-total-samples
      (= 0 user-index)  0
      (= 1 user-index)  (int (Math/ceil (- min-total-samples (* (- users 2) common-case))))
      :else             (int (Math/ceil common-case)))))

(defn- total-samples
  "Sum of samples returned by `samples-for-user`"
  [users samples]
  (+ (samples-for-user 1 users samples)
     (* (- users 2) (samples-for-user 2 users samples))))

(defn- sample-data-batches
  "Return a lazy sequence of lists of maps containing documents for the database."
  [username-prefix users decks game-logs messages]
  (let [batch-size 10
        user-template (create-user "" "password" "")
        sample-decks (prepare-sample-decks
                      ["https://netrunnerdb.com/en/decklist/62104/that-one-sync-deck-35th-at-worlds"
                       "https://netrunnerdb.com/en/decklist/62113/firestorm-worlds-110th-"])
        sample-messages ["Hello ""¡Hola!" "Grüß Gott" "Hyvää päivää" "Tere õhtust" "⠓⠑⠇⠇⠕"
                         "Bonġu Cześć!" "Dobrý den" "Здравствуйте!" "Γειά σας" "გამარჯობა"]]
    (for [b (range (Math/ceil (/ users batch-size)))]
      (apply merge-with concat
             (for [k (range (* b batch-size) (min (* (+ b 1) batch-size) users))]
               (let [username (str username-prefix k)]
                 {:users [(create-sample-user username user-template)]
                  :decks (for [i (range (samples-for-user k users decks))]
                           (create-sample-deck username sample-decks))
                  :game-logs (for [i (range (samples-for-user k users game-logs))]
                               (create-sample-game-log username))
                  :messages (for [i (range (samples-for-user k users messages))]
                              (create-sample-message username sample-messages))}))))))

(defn create-sample-data
  "Create sample data to benchmark queries.

  This creates users, decks, game-logs and messages in the same order of
  magnitude as the live instance at jinteki.net. These (massive) amounts of
  data can then be used to profile database queries or just get a feeling for
  how real workloads affect the system's performance.

  On my machine, this takes about 3 minutes and consumes around 600 MB of disk
  space.

  The users are named \"<username-prefix><INDEX>\", e.g. \"sample0\",
  \"sample1\", ... and their passwords are \"password\".

  The created documents are all tied to users, i.e. as long as you use
  different username prefixes, you can run this function multiple times if
  needed.

  The created users include two extreme cases. \"sample0\" has no decks, no
  games and no messages, while \"sample1\" has approx. a thousand times the
  amount of decks, games and messages as the median."
  [& [users username-prefix avg-decks avg-game-logs avg-messages]]
  (let [{{:keys [db]} :mongodb/connection :as system} (connect)]
    (try
      (let [username-prefix (or username-prefix "sample")
            ;; Those numbers are from January 2021, obviously rounded:
            users (Integer/parseInt (or users "50000"))
            decks (* users (Integer/parseInt (or avg-decks "11")))
            game-logs (* users (Integer/parseInt (or avg-game-logs "5")))
            messages (* users (Integer/parseInt (or avg-messages "5")))]
        (println "This will take a few minutes to create"
                 users "users,"
                 (total-samples users decks) "decks,"
                 (total-samples users game-logs) "game-logs and"
                 (total-samples users messages) "messages.")
        (println "Press any key to continue.")
        (read-line)
        (doseq [batch (sample-data-batches username-prefix users decks game-logs messages)]
          (do
            (mc/insert-batch db "users" (:users batch))
            (mc/insert-batch db "decks" (:decks batch))
            (mc/insert-batch db "game-logs" (:game-logs batch))
            (mc/insert-batch db "messages" (:messages batch))))
        (println "Successfully created sample data.")
        (println "You can now login with e.g. username"
                 (format "\"%s%d\"," username-prefix 1)
                 "password \"password\"."))
      (catch Exception e (do
                           (println "Create sample data failed:" (.getMessage e))
                           (.printStackTrace e)))
      (finally (disconnect system)))))
