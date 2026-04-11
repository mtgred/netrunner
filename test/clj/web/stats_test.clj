(ns web.stats-test
  (:require
   [clojure.test :refer :all]
   [monger.collection :as mc]
   [monger.result :refer [acknowledged?]]
   [web.mongodb :refer [->object-id]]
   [web.stats :refer :all]))

(deftest strip-opponent-deck-name-test
  (let [game {:corp   {:player {:username "alice"} :deck-name "HB Fast Advance"}
              :runner {:player {:username "bob"}   :deck-name "Stealth Andy"}}]
    (testing "corp user: runner deck-name is stripped, corp deck-name is kept"
      (let [result (strip-opponent-deck-name game "alice")]
        (is (= "HB Fast Advance" (get-in result [:corp :deck-name])))
        (is (nil? (get-in result [:runner :deck-name])))))
    (testing "runner user: corp deck-name is stripped, runner deck-name is kept"
      (let [result (strip-opponent-deck-name game "bob")]
        (is (= "Stealth Andy" (get-in result [:runner :deck-name])))
        (is (nil? (get-in result [:corp :deck-name])))))))

(deftest clear-deckstats-handler-ownership-test
  (testing "only allow clearing deck stats for your decks"
    (with-redefs [mc/find-one-as-map (fn [& _] nil)  ; deck not found for this user
                  mc/update          (fn [& _] :mock-result)
                  acknowledged?      (fn [_] true)
                  ->object-id        identity]
      (let [;; alice is authenticated but targets a deck that belongs to bob
            request {:system/db   :mock-db
                     :user        {:username "alice"}
                     :path-params {:id "bobs-deck-id"}}
            result  (clear-deckstats-handler request)]
        (is (= 401 (:status result))
            "handler must reject requests where the user does not own the deck"))))
  (testing "owner can clear stats for their own deck"
    (with-redefs [mc/find-one-as-map (fn [& _] {:_id "alices-deck-id" :username "alice"})
                  mc/update          (fn [& _] :mock-result)
                  acknowledged?      (fn [_] true)
                  ->object-id        identity]
      (let [request {:system/db   :mock-db
                     :user        {:username "alice"}
                     :path-params {:id "alices-deck-id"}}
            result  (clear-deckstats-handler request)]
        (is (= 200 (:status result)))))))

(def game-with-alice-and-bob
  {:corp   {:player {:username "alice"}}
   :runner {:player {:username "bob"}}
   :log    [{:text "some log entry"}]})

(def public-msg {:user "__system__" :text "public message"})
(def corp-msg {:user "__system__" :text "corp only"})
(def runner-msg {:user "__system__" :text "runner only"})

(def game-with-new-format-log
  {:corp   {:player {:username "alice"}}
   :runner {:player {:username "bob"}}
   :log    [{:public public-msg}
            {:corp corp-msg}
            {:runner runner-msg}]})

(def game-with-old-format-log
  {:corp   {:player {:username "alice"}}
   :runner {:player {:username "bob"}}
   :log    [{:user "__system__" :text "Game started"}
            {:user {:username "alice" :emailhash "abc"} :text "Hello"}]})

(deftest fetch-log-ownership-test
  (testing "non-player cannot fetch a game log"
    (with-redefs [mc/find-one-as-map (fn [& _] game-with-alice-and-bob)]
      (let [request {:system/db   :mock-db
                     :user        {:username "eve" :_id "eve-id"}
                     :path-params {:gameid "some-game-id"}}
            result  (fetch-log request)]
        (is (= 401 (:status result))
            "handler must reject non-players from viewing game logs"))))
  (testing "corp player can fetch their game log"
    (with-redefs [mc/find-one-as-map (fn [& _] game-with-alice-and-bob)]
      (let [request {:system/db   :mock-db
                     :user        {:username "alice" :_id "alice-id"}
                     :path-params {:gameid "some-game-id"}}
            result  (fetch-log request)]
        (is (= 200 (:status result))))))
  (testing "runner player can fetch their game log"
    (with-redefs [mc/find-one-as-map (fn [& _] game-with-alice-and-bob)]
      (let [request {:system/db   :mock-db
                     :user        {:username "bob" :_id "bob-id"}
                     :path-params {:gameid "some-game-id"}}
            result  (fetch-log request)]
        (is (= 200 (:status result)))))))

(deftest fetch-log-new-format-test
  (testing "corp player receives public and corp-only messages, not runner-only"
    (with-redefs [mc/find-one-as-map (fn [& _] game-with-new-format-log)]
      (let [request {:system/db   :mock-db
                     :user        {:username "alice"}
                     :path-params {:gameid "g1"}}
            result  (fetch-log request)
            log     (:body result)]
        (is (= 200 (:status result)))
        (is (= 2 (count log)))
        (is (some #(= "public message" (:text %)) log))
        (is (some #(= "corp only" (:text %)) log))
        (is (not (some #(= "runner only" (:text %)) log))))))
  (testing "runner player receives public and runner-only messages, not corp-only"
    (with-redefs [mc/find-one-as-map (fn [& _] game-with-new-format-log)]
      (let [request {:system/db   :mock-db
                     :user        {:username "bob"}
                     :path-params {:gameid "g1"}}
            result  (fetch-log request)
            log     (:body result)]
        (is (= 200 (:status result)))
        (is (= 2 (count log)))
        (is (some #(= "public message" (:text %)) log))
        (is (some #(= "runner only" (:text %)) log))
        (is (not (some #(= "corp only" (:text %)) log)))))))

(deftest fetch-log-old-format-test
  (testing "old-format logs are returned unchanged for corp player"
    (with-redefs [mc/find-one-as-map (fn [& _] game-with-old-format-log)]
      (let [request {:system/db   :mock-db
                     :user        {:username "alice"}
                     :path-params {:gameid "g2"}}
            result  (fetch-log request)
            log     (:body result)]
        (is (= 200 (:status result)))
        (is (= 2 (count log)))
        (is (= "Game started" (:text (first log)))))))
  (testing "old-format logs are returned unchanged for runner player"
    (with-redefs [mc/find-one-as-map (fn [& _] game-with-old-format-log)]
      (let [request {:system/db   :mock-db
                     :user        {:username "bob"}
                     :path-params {:gameid "g2"}}
            result  (fetch-log request)
            log     (:body result)]
        (is (= 200 (:status result)))
        (is (= 2 (count log)))
        (is (= "Game started" (:text (first log))))))))
