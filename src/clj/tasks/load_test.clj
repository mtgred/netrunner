(ns tasks.load-test
  "Load test for large numbers of websockets"
  (:require [clojure.string :refer [join]]
            [web.db :refer [db] :as webdb]
            [gniazdo.core :as ws]
            [clj-uuid :as uuid]
            [org.httpkit.client :as http]
            [crypto.password.bcrypt :as password]
            [monger.collection :as mc]
            [web.core :refer [-main]]
            [web.game :refer [handle-game-start]]
            [web.lobby :refer [handle-lobby-create handle-lobby-join handle-lobby-watch all-games]]))

;; This print guarantees a coherent print (i.e. parallel prints will not be interleaved)
(defn safe-println [& more]
  (.write *out* (str (join " " more) "\n")))

(defn add-test-users [maxUsers]
  (webdb/connect)
  (when (not (mc/find-one-as-map db "users" {:username "TestCorp"}))
    (mc/insert db "users" {:username "TestCorp"
                           :email "TestCorp@mailinator.com"
                           :password (password/encrypt "password")
                           :isadmin false
                           :options {}}))

  (when (not (mc/find-one-as-map db "users" {:username "TestRunner"}))
    (mc/insert db "users" {:username "TestRunner"
                           :email "TestRunner@mailinator.com"
                           :password (password/encrypt "password")
                           :isadmin false
                           :options {}}))
  (doall
    (pmap
      (fn [n]
        (when (not (mc/find-one-as-map db "users" {:username (str "TestUser" n)}))
          (mc/insert db "users" {:username (str "TestUser" n)
                                 :email (str "TestUser" n "@mailinator.com")
                                 :password (password/encrypt "password")
                                 :isadmin false
                                 :options {}})))
      (range maxUsers))))

(defn login
  [username password]
  (let [options {:form-params {:username username
                               :password password}}
        {:keys [status error headers]} @(http/post "http://localhost:1042/login" options)]
    (if (or error (= 401 status))
      (println "Failed, exception is " (or error status))
      (:set-cookie headers))))

(defn create-game
  [maxUsers]
  (let [client (ws/client)
        corp-client-id (uuid/to-string (uuid/v1))
        runner-client-id (uuid/to-string (uuid/v1))]
    (safe-println "Login with test users")
    (.setMaxTextMessageSize (.getPolicy client) (* 1024 1024))
    (.start client)

    (ws/connect (str "ws://localhost:1042/ws?client-id=" corp-client-id)
                :client client
                :on-error #(safe-println "corp error" %)
                :on-connect (fn [n] (safe-println "Corp Connected"))
                :on-close (fn [x y] (safe-println "Corp Disconnected"))
                :headers {"Cookie" (login "TestCorp" "password")})
    (ws/connect (str "ws://localhost:1042/ws?client-id=" runner-client-id)
                :client client
                :on-error #(safe-println "runner error" %)
                :on-connect (fn [n] (safe-println "Runner Connected"))
                :on-close (fn [x y] (safe-println "Runner Disconnected"))
                :headers {"Cookie" (login "TestRunner" "password")})
    (safe-println "Create lobby")
    (handle-lobby-create {:ring-req {:user {:username "TestCorp"}}
                          :client-id corp-client-id
                          :?data {:title "Performance Game"
                                  :format "standard"
                                  :allow-spectator true
                                  :spectatorhands false
                                  :password ""
                                  :room "casual"
                                  :side "Corp"
                                  :options {}}})

    (let [game-id (first (first @all-games))]
      (handle-lobby-join {:ring-req {:user {:username "TestRunner"}}
                            :client-id runner-client-id
                            :?data {:gameid game-id
                                    :password ""}})

      (doall
        (pmap
          (fn [n]
            (let [userClientID (uuid/to-string (uuid/v1))
                  socket (ws/connect (str "ws://localhost:1042/ws?client-id=" userClientID)
                                     :client client
                                     :on-error #(safe-println "spectator error" %)
                                     :on-close #(safe-println 'closed %1 %2)
                                     :headers {"Cookie" (login (str "TestUser" n) "password")})]
              (handle-lobby-watch {:ring-req {:user {:username (str "TestUser" n)}}
                                    :client-id userClientID
                                    :?data {:gameid game-id
                                            :password ""}})
              socket))
          (range maxUsers)))

      (safe-println "Spectators connected")
      (handle-game-start {:ring-req {:user {:username "TestCorp"}}
                          :client-id corp-client-id})
      (safe-println "Started game"))))

(defn command
  []
  (-main "dev")

  (def maxUsers 1000)
  (add-test-users maxUsers)
  (safe-println "Users created")

  (create-game maxUsers))
