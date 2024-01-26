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
