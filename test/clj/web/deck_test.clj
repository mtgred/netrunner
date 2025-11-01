(ns web.deck-test
  (:require
   [clojure.test :refer :all]
   [web.decks :refer :all]))

(deftest bulk-delete-handler-validation-test
  (testing "Bulk delete handler validation and responses"

    (testing "unauthorized request without username"
      (let [request {:system/db nil
                     :user {}
                     :body {:deck-ids ["deck1" "deck2"]}}
            response (decks-bulk-delete-handler request)]
        (is (= 401 (:status response)))
        (is (= "Unauthorized or invalid request" (get-in response [:body :message])))))

    (testing "invalid request without deck-ids"
      (let [request {:system/db nil
                     :user {:username "testuser"}
                     :body {}}
            response (decks-bulk-delete-handler request)]
        (is (= 401 (:status response)))
        (is (= "Unauthorized or invalid request" (get-in response [:body :message])))))

    (testing "invalid request with non-sequential deck-ids"
      (let [request {:system/db nil
                     :user {:username "testuser"}
                     :body {:deck-ids "not-an-array"}}
            response (decks-bulk-delete-handler request)]
        (is (= 401 (:status response)))
        (is (= "Unauthorized or invalid request" (get-in response [:body :message])))))

    (testing "empty deck-ids array"
      (let [request {:system/db nil
                     :user {:username "testuser"}
                     :body {:deck-ids []}}
            response (decks-bulk-delete-handler request)]
        (is (= 200 (:status response)))
        (is (= [] (:body response)))))))
