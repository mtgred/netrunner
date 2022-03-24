(ns tasks.load-generator
  (:require
    [clj-uuid :as uuid]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.tools.cli :refer [parse-opts]]
    [gniazdo.core :as ws]
    [monger.collection :as mc]
    [monger.operators :refer [$in]]
    [org.httpkit.client :as http]
    [web.lobby :refer [all-games]]
    [web.system :refer [start stop]]
    [web.ws :as game-ws-handler])
  (:import
    java.net.URLEncoder))

;; This print guarantees a coherent print (i.e. parallel prints will not be interleaved)
(defn safe-println [& more]
  (.write *out* (str (str/join " " more) "\n")))

(defn add-test-users
  [{{:keys [db]} :mongodb/connection} max-users]
  (let [playing-users ["TestCorp" "TestRunner"]
        watching-users (for [n (range max-users)] (str "TestUser" n))
        all-users (into playing-users watching-users)
        existing-users (->> (mc/find-maps db "users" {:username {$in all-users}} [:username])
                            (keep :username)
                            (into #{}))
        users-not-in-db (->> existing-users
                             (set/difference (into #{} all-users))
                             (mapv (fn [username] {:username username
                                                   :password "password"
                                                   :email (str username "@mailinator.com")}))
                             (not-empty))]
    (when users-not-in-db
      (mc/insert-batch db "users" users-not-in-db))))

(comment
  (def system (start))
  (add-test-users system 1002)
  (stop system)
  ,)

(defn login
  [username password]
  (let [options {:form-params {:username username
                               :password password}}
        post-res @(http/post "http://localhost:1042/login" options)]
    (if (or (post-res :error)
            (= 401 (post-res :status)))
      (println "Failed, exception is " (or (post-res :error) (post-res :status)))
      (let [get-res @(http/get "http://localhost:1042" {:as :text
                                                        :headers {"Cookie" (str (:set-cookie (post-res :headers)))}})]
        (if (or (get-res :error)
                (= 401 (get-res :status)))
          (println "Failed, exception is " (or (get-res :error) (get-res :status)))
          {"Origin" "http://localhost:1042"
           "Cookie" (str (:set-cookie (post-res :headers)) ";" (:set-cookie (get-res :headers)))
           "X-CSRF-Token" (second (re-find #"data-csrf-token=\"(.*?)\"" (str (get-res :body))))})))))

(defn create-game
  [{{:keys [db]} :mongodb/connection :as system} max-users]
  (add-test-users system max-users)
  (let [client (ws/client)
        corp-client-id (uuid/to-string (uuid/v1))
        runner-client-id (uuid/to-string (uuid/v1))
        corp-login (login "TestCorp" "password")
        runner-login (login "TestRunner" "password")
        corp-decks (mc/find-maps db "decks" {:username "TestCorp"})
        runner-decks (mc/find-maps db "decks" {:username "TestRunner"})]
    (.setMaxTextMessageSize (.getPolicy client) (* 1024 1024))
    (.start client)

    (safe-println "Login Corp")
    (ws/connect (str "ws://localhost:1042/chsk"
                     "?client-id=" corp-client-id
                     "&csrf-token=" (URLEncoder/encode (get corp-login "X-CSRF-Token")))
                :client client
                :extensions ["permessage-deflate"]
                :on-error #(safe-println "corp error" %)
                :on-connect (fn [n] (safe-println "Corp Connected"))
                :on-close (fn [x y] (safe-println "Corp Disconnected"))
                :headers corp-login)
    (safe-println "Login Runner")
    (ws/connect (str "ws://localhost:1042/chsk"
                     "?client-id=" runner-client-id
                     "&csrf-token=" (URLEncoder/encode (get runner-login "X-CSRF-Token")))
                :client client
                :on-error #(safe-println "runner error" %)
                :on-connect (fn [n] (safe-println "Runner Connected"))
                :on-close (fn [x y] (safe-println "Runner Disconnected"))
                :headers runner-login)
    (safe-println "Create lobby")

    ;; Corp create lobby
    (game-ws-handler/-msg-handler {:id :lobby/create :ring-req {:user {:username "TestCorp"}}
                                   :client-id corp-client-id
                                   :uid "TestCorp"
                                   :?data {:title "Performance Game"
                                           :format "standard"
                                           :allow-spectator true
                                           :spectatorhands false
                                           :password ""
                                           :room "casual"
                                           :side "Corp"
                                           :options {}}})

    (let [game-id (first (first @all-games))]
      ;;  Runner join
      (game-ws-handler/-msg-handler {:id :lobby/join
                                     :ring-req {:system/db db
                                                :user {:username "TestRunner"}}
                                     :client-id runner-client-id
                                     :uid "TestRunner"
                                     :?data {:gameid game-id
                                             :password ""}})
      ;; Select decks
      (game-ws-handler/-msg-handler {:id :lobby/deck
                                     :ring-req {:system/db db
                                                :user {:username "TestCorp"}}
                                     :uid "TestCorp"
                                     :client-id corp-client-id
                                     ;; find one deck where :identity :side "Corp", then get the _id
                                     :?data (str (some #(if (= "Corp" ((% :identity) :side)) (% :_id)) corp-decks))})
      (game-ws-handler/-msg-handler {:id :lobby/deck
                                     :ring-req {:system/db db
                                                :user {:username "TestRunner"}}
                                     :uid "TestRunner"
                                     :client-id runner-client-id
                                     ;; find one deck where :identity :side "Runner", then get the _id
                                     :?data (str (some #(if (= "Runner" ((% :identity) :side)) (% :_id)) runner-decks))})
      (doall
        (pmap
          (fn [n]
            (let [userClientID (uuid/to-string (uuid/v1))
                  creds (login (str "TestUser" n) "password")
                  socket (ws/connect (str "ws://localhost:1042/chsk"
                                          "?client-id=" userClientID
                                          "&csrf-token=" (URLEncoder/encode (get creds "X-CSRF-Token")))
                                     :client client
                                     :on-error #(safe-println "spectator error" %)
                                     :on-close #(safe-println 'closed %1 %2)
                                     :headers creds)]
              (game-ws-handler/-msg-handler {:id :lobby/watch
                                             :ring-req {:system/db db
                                                        :user {:username (str "TestUser" n)}}
                                             :client-id userClientID
                                             :?data {:gameid game-id
                                                     :password ""}})
              socket))
          (range 1 max-users)))

      (safe-println "Spectators connected")
      (game-ws-handler/-msg-handler {:id :game/start
                                     :ring-req {:system/db db
                                                :user {:username "TestCorp"}}
                                     :uid "TestCorp"
                                     :client-id corp-client-id})
      (safe-println "Started game"))))

(defn usage
  [options-summary]
  (->> [""
        "Usage: lein load-generator [options]"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(def cli-options
  [["-n" "--num NUM" "Number of spectators to connect to game"
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]
    :default 1000]])

(defn exit [status msg]
  (binding [*out* *err*]
    (println msg))
  (System/exit status))

(defn command
  "This is not really a load test per se, but a development tool to assist debugging performance issues.
  This will start the server and create a single game on your environment
  between users TestCorp (Corp) and TestRunner (Runner).
  One thousand spectators will be connected to the game by default, from users sample4-sample1003.
  The load generator must be run AFTER creating sample users."
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (if (or errors
            (not-empty arguments))
      (exit 1 (str/join \newline (conj errors (usage summary))))
      (let [system (start)]
        (create-game system (options :num))
        (.addShutdownHook (Runtime/getRuntime) (Thread. #(stop system)))))))
