(ns game.core.card-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

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
