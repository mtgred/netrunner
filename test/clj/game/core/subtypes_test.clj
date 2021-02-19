(ns game.core.subtypes-test
  (:require [game.core :as core]
            [game.core.subtypes :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest has-subtypes?-test
  (do-game
    (new-game {:runner {:hand ["Stimhack" "Contaminate" "Laamb"]}})
    (let [stimhack (find-card "Stimhack" (:hand (get-runner)))
          contaminate (find-card "Contaminate" (:hand (get-runner)))
          laamb (find-card "Laamb" (:hand (get-runner)))]
      (testing "no subtypes"
        (is (not (has-subtypes? contaminate "Run"))))
      (testing "one subtype"
        (is (has-subtypes? stimhack "Run"))
        (is (not (has-subtypes? stimhack "Mod"))))
      (testing "multiple subtypes"
        (is (has-subtypes? laamb "Icebreaker"))
        (is (has-subtypes? laamb "Fracter"))
        (is (not (has-subtypes? stimhack "Mod")))))))

(deftest add-subtype-test
  (do-game
    (new-game)
    (let [sg (first (:hand (get-runner)))]
      (testing "accepts strings and keywords"
        (is (add-subtype sg "type"))
        (is (add-subtype sg :type)))
      (testing "converts input to string"
        (let [sg (add-subtype sg :type)]
          (is (= "Type" (first (:subtypes sg))))))
      (testing "throws if given subtype isn't a keyword or string"
        (is (thrown? java.lang.AssertionError (add-subtype sg []))))
      (testing "doesn't update the card"
        (is (not= (add-subtype sg "Mod") (refresh sg))))
      )))

(deftest add-subtype!-test
  (do-game
    (new-game {:runner {:hand ["Stimhack" "Contaminate" "Laamb"]}})
    (let [stimhack (find-card "Stimhack" (:hand (get-runner)))
          contaminate (find-card "Contaminate" (:hand (get-runner)))
          laamb (find-card "Laamb" (:hand (get-runner)))]
      )))

(deftest remove-subtype-test
  (do-game
    (new-game {:runner {:hand ["Stimhack" "Contaminate" "Laamb"]}})
    (let [stimhack (find-card "Stimhack" (:hand (get-runner)))
          contaminate (find-card "Contaminate" (:hand (get-runner)))
          laamb (find-card "Laamb" (:hand (get-runner)))]
      )))

(deftest remove-subtype!-test
  (do-game
    (new-game {:runner {:hand ["Stimhack" "Contaminate" "Laamb"]}})
    (let [stimhack (find-card "Stimhack" (:hand (get-runner)))
          contaminate (find-card "Contaminate" (:hand (get-runner)))
          laamb (find-card "Laamb" (:hand (get-runner)))]
      )))

(deftest subtypes-for-card-test
  (do-game
    (new-game {:runner {:hand ["Stimhack" "Contaminate" "Laamb"]}})
    (let [stimhack (find-card "Stimhack" (:hand (get-runner)))
          contaminate (find-card "Contaminate" (:hand (get-runner)))
          laamb (find-card "Laamb" (:hand (get-runner)))]
      )))

(deftest update-subtypes-for-card-test
  (do-game
    (new-game {:runner {:hand ["Stimhack" "Contaminate" "Laamb"]}})
    (let [stimhack (find-card "Stimhack" (:hand (get-runner)))
          contaminate (find-card "Contaminate" (:hand (get-runner)))
          laamb (find-card "Laamb" (:hand (get-runner)))]
      )))

(deftest update-all-subtypes-test
  (do-game
    (new-game {:runner {:hand ["Stimhack" "Contaminate" "Laamb"]}})
    (let [stimhack (find-card "Stimhack" (:hand (get-runner)))
          contaminate (find-card "Contaminate" (:hand (get-runner)))
          laamb (find-card "Laamb" (:hand (get-runner)))]
      )))
