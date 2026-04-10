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
