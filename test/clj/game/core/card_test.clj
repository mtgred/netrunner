(ns game.core.card-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.test-framework :refer :all]))

(deftest has-subtype?-test
  (before-each [contaminate {:subtypes []}
                stimhack {:subtypes ["Run"]}
                laamb {:subtypes ["Icebreaker" "Fracter"]}]
    (testing "no subtypes"
      (is (not (has-subtype? contaminate "Run"))))
    (testing "one subtype"
      (is (has-subtype? stimhack "Run"))
      (is (not (has-subtype? stimhack "Mod"))))
    (testing "multiple subtypes"
      (is (has-subtype? laamb "Icebreaker"))
      (is (has-subtype? laamb "Fracter"))
      (is (not (has-subtype? stimhack "Mod"))))))

(deftest has-any-subtype?-test
  (before-each [contaminate {:subtypes []}
                stimhack {:subtypes ["Run"]}
                laamb {:subtypes ["Icebreaker" "Fracter"]}]
    (testing "one is present returns true"
      (is (has-any-subtype? stimhack ["Test" "Two" "Run"])))
    (testing "multiple are present but not all returns true"
      (is (has-any-subtype? laamb ["Test" "Two" "Icebreaker" "Fracter" "False"])))
    (testing "none are present"
      (is (not (has-any-subtype? contaminate ["Test" "Two" "Icebreaker" "Fracter" "False"]))))))

(deftest has-all-subtypes?-test
  (before-each [contaminate {:subtypes []}
                stimhack {:subtypes ["Run"]}
                laamb {:subtypes ["Icebreaker" "Fracter"]}]
     (testing "Interaction when card has one subtype"
       (is (has-all-subtypes? stimhack ["Run"]))
       (is (not (has-all-subtypes? stimhack ["Test" "Two" "Run"]))))
     (testing "Interaction when card has two subtypes"
       (is (has-all-subtypes? laamb ["Icebreaker" "Fracter"]))
       (is (has-all-subtypes? laamb ["Icebreaker"]))
       (is (not (has-all-subtypes? laamb ["Icebreaker" "Fracter" "Test"]))))
     (testing "Interaction when card has no subtypes"
       (is (not (has-all-subtypes? contaminate ["Test" "Two" "Icebreaker" "Fracter" "False"]))))))
