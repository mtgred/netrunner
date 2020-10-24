(ns tasks.load-test
  "Load test for large numbers of websockets"
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [web.db :refer [db] :as webdb]
            [clj-async-profiler.core :as prof]
            [gniazdo.core :as ws]
            [clj-uuid :as uuid]
            [org.httpkit.client :as http]
            [web.game :refer :all]
            [crypto.password.bcrypt :as password]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]
            [monger.operators :refer :all]
            [web.core :refer :all]
            [game.core-test :refer :all]
            [game.macros-test :refer :all]
            [web.lobby :refer :all]))

(defn safe-println [& more]
  (.write *out* (str (clojure.string/join " " more) "\n")))

(defn add-test-users [maxUsers]
  (webdb/connect)
  (when (not (mc/find-one-as-map db "users" {:username "TestCorp"}))
    (mc/insert db "users"
                    {:username         "TestCorp"
                      :email            "TestCorp@mailinator.com"
                      :password         (password/encrypt "password")
                      :isadmin          false
                      :options          {}}))

  (when (not (mc/find-one-as-map db "users" {:username "TestRunner"}))
    (mc/insert db "users"
                    {:username         "TestRunner"
                      :email            "TestRunner@mailinator.com"
                      :password         (password/encrypt "password")
                      :isadmin          false
                      :options          {}}))
  (doall
    (pmap
      (fn [n]
        (when (not (mc/find-one-as-map db "users" {:username (str "TestUser" n)}))
          (mc/insert db
                    "users"
                    {:username         (str "TestUser" n)
                      :email            (str "TestUser" n "@mailinator.com")
                      :password         (password/encrypt "password")
                      :isadmin          false
                      :options          {}})))
      (range maxUsers))))

(defn login
  [username password]
  (let [options {:form-params {:username username :password password}}
      {:keys [status error headers body]} @(http/post "http://localhost:1042/login" options)]
  (if (or error (= 401 status))
    (println "Failed, exception is " (or error status))
    (:set-cookie headers))))

(defn create-game [maxUsers]
  (def corp-client-id (uuid/to-string (uuid/v1)))
  (def runner-client-id (uuid/to-string (uuid/v1)))
  (safe-println "Login with test users")
  (def corp-socket (ws/connect
        (str "ws://localhost:1042/ws?client-id=" corp-client-id)
        ; :on-receive #(safe-println 'received %)
        :on-error #(safe-println "corp error" %)
        :on-connect (fn [n] (safe-println "Corp Connected"))
        :on-close (fn [x y] (safe-println "Corp Disconnected"))
        :headers {"Cookie" (login "TestCorp" "password")}
        ))
  (def runner-socket (ws/connect
        (str "ws://localhost:1042/ws?client-id=" runner-client-id)
        ; :on-receive #(safe-println 'received %)
        :on-error #(safe-println "runner error" %)
        :on-connect (fn [n] (safe-println "Runner Connected"))
        :on-close (fn [x y] (safe-println "Runner Disconnected"))
        :headers {"Cookie" (login "TestRunner" "password")}))
  (safe-println "Create lobby")
  (handle-lobby-create {:ring-req {:user {:username "TestCorp"}}
                        :client-id corp-client-id
                        :?data {:title "Performance Game" :format "standard" :allow-spectator true :spectatorhands false :password "" :room "casual" :side "Corp" :options {}}})
  
  (def game-id (first (first @all-games)))

  (handle-lobby-join {:ring-req {:user {:username "TestRunner"}}
                        :client-id runner-client-id
                        :?data {:gameid game-id :password ""}})

  (def sockets (doall (pmap
    (fn [n]
      (def userClientID (uuid/to-string (uuid/v1)))
      (def socket (ws/connect
        (str "ws://localhost:1042/ws?client-id=" userClientID)
        ; :on-receive #(if (> (count %) 20000) (safe-println 'received %))
        :on-error #(safe-println "spectator error" %)
        :on-close #(safe-println 'closed %1 %2)
        ; :on-connect (fn [n] (safe-println "Connected"))
        :headers {"Cookie" (login (str "TestUser" n) "password")}))
      (handle-lobby-watch {:ring-req {:user {:username (str "TestUser" n)}}
                        :client-id userClientID
                        :?data {:gameid game-id :password ""}})
      socket)
    (range maxUsers))))

  (safe-println "Spectators connected")
  (handle-game-start {:ring-req {:user {:username "TestCorp"}}
                        :client-id corp-client-id})
  (safe-println "Started game"))

(defn command
  []
    (-main "dev")

    (def maxUsers 100)
    (add-test-users maxUsers)
    (safe-println "Users created")

    (create-game maxUsers)
)
