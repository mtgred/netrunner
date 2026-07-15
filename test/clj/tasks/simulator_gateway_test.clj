(ns tasks.simulator-gateway-test
  (:require
    [clojure.edn :as edn]
    [clojure.test :refer [deftest is]]
    [tasks.simulator-gateway :as simulator-gateway]))

(deftest checked-in-manifest-matches-repository-sources
  (let [cards (edn/read-string (slurp simulator-gateway/default-card-data))
        expected (simulator-gateway/manifest-json cards)
        actual (slurp simulator-gateway/default-output)]
    (is (= expected actual)
        "Run `lein simulator-gateway` after card or beginner deck changes.")))
