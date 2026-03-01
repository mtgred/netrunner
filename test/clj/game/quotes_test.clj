(ns game.quotes-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [game.quotes :as sut]
   [game.test-framework]
   [jinteki.cards :refer [all-cards]]))

(deftest load-quotes-test
  (is (sut/load-quotes!))
  (doseq [id (keys @sut/identity-quotes)
          :let [known-id? (get @all-cards id)]]
    (is known-id? (format "%s is mispelled" (pr-str id))))
  (doseq [[id pairs] @sut/identity-quotes
          [pair-id pair-quotes] pairs
          :let [pair? (or (get @all-cards pair-id)
                          (#{"Default" "Anarch" "Criminal" "Shaper"
                             "Haas-Bioroid" "Jinteki" "NBN" "Weyland Consortium"} pair-id))]]
    (testing (format "checking %s quotes" id)
      (is pair? (format "%s is mispelled" (pr-str pair-id)))
      (is (vector? pair-quotes))
      (doseq [quote pair-quotes]
        (is (not (str/blank? quote)))
        (is (= quote (str/trim quote)))))))
