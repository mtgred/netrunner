(ns game.quotes-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [game.quotes :as sut]
   [game.test-framework]
   [jinteki.cards :refer [all-cards]]))

(deftest load-quotes-test
  (is (sut/load-quotes!))
  (doseq [id (keys @sut/identity-quotes)]
    (is (get @all-cards id)))
  (doseq [[id pairs] @sut/identity-quotes
          [pair-id pair-quotes] pairs
          :when (not (#{"Default" "Anarch" "Criminal" "Shaper"
                        "Haas-Bioroid" "Jinteki" "NBN" "Weyland Consortium"} pair-id))
          :let [pair? (get @all-cards pair-id)]]
    (testing (format "checking %s quotes" id)
      (is pair? (format "%s is mispelled" (pr-str pair-id)))
      (is (vector? pair-quotes))
      (doseq [quote pair-quotes]
        (is (not (str/blank? quote)))
        (is (= quote (str/trim quote)))))))
