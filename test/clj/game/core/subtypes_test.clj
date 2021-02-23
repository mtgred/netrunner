(ns game.core.subtypes-test
  (:require [game.core :as core]
            [game.macros :refer [req]]
            [game.core.subtypes :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest has-subtypes?-test
  (before-each [contaminate {:subtypes []}
                stimhack {:subtypes ["Run"]}
                laamb {:subtypes ["Icebreaker" "Fracter"]}]
    (testing "no subtypes"
      (is (not (has-subtypes? contaminate "Run"))))
    (testing "one subtype"
      (is (has-subtypes? stimhack "Run"))
      (is (not (has-subtypes? stimhack "Mod"))))
    (testing "multiple subtypes"
      (is (has-subtypes? laamb "Icebreaker"))
      (is (has-subtypes? laamb "Fracter"))
      (is (not (has-subtypes? stimhack "Mod"))))))

(deftest add-subtype-test
  (before-each [sg {:subtypes []}]
    (testing "accepts strings and keywords"
      (is (add-subtype sg "type"))
      (is (add-subtype sg :type)))
    (testing "performs no change on keyword input"
      (is (= "type" (first (:subtypes (add-subtype sg :type))))))
    (testing "throws if given subtype isn't a keyword or string"
      (is (thrown? java.lang.AssertionError (add-subtype sg nil))))))

(deftest add-subtype!-test
  (do-game
    (new-game {:runner {:hand ["Sure Gamble"]}})
    (testing "updates the card"
      (let [sg (first (:hand (get-runner)))]
        (is (= (add-subtype! state nil sg "Mod") (refresh sg)))))))

(deftest remove-subtype-test
  (before-each [sg {:subtypes ["Type"]}]
    (testing "accepts strings and keywords"
      (is (remove-subtype sg "type"))
      (is (remove-subtype sg :type)))
    (testing "performs no change on keyword input"
      (is (= "Type" (first (:subtypes (remove-subtype sg :type))))))
    (testing "throws if given subtype isn't a keyword or string"
      (is (thrown? java.lang.AssertionError (remove-subtype sg nil))))))

(deftest remove-subtype!-test
  (do-game
    (new-game {:runner {:hand ["Stimhack"]}})
    (testing "updates the card"
      (let [sg (first (:hand (get-runner)))]
        (is (= (remove-subtype! state nil sg "Run") (refresh sg)))))))

(deftest subtypes-for-card-test
  (before-each [state (new-game {:runner {:hand ["Stimhack"]}})
                stimhack (find-card "Stimhack" (get-in @state [:runner :hand]))]
    (testing "Returns subtypes for a given card"
      (do-game state
        (is (= ["Run"] (subtypes-for-card state (refresh stimhack))))))
    (testing "Returns printed subtypes"
      (do-game state
        (swap! state assoc-in [:corp :hand 0 :subtypes] [])
        (is (= ["Run"] (subtypes-for-card state (refresh stimhack))))))
    (testing "Concats applicable :subtype effects"
      (do-game state
        (swap! state assoc :effects [{:type :subtypes :value "Mod"}])
        (is (= ["Run" "Mod"] (subtypes-for-card state (refresh stimhack))))))))

(deftest update-subtypes-for-card-test
  (before-each [state (new-game {:runner {:hand ["Stimhack"]}})
                stimhack (find-card "Stimhack" (get-in @state [:runner :hand]))]
    (testing "Returns false when nothing changes"
      (do-game state
        (is (not (update-subtypes-for-card state nil stimhack)))))
    (testing "Returns true when subtypes change"
      (do-game state
        (swap! state assoc :effects [{:type :subtypes :value "Mod"}])
        (is (update-subtypes-for-card state nil stimhack))
        (is (= ["Run" "Mod"] (:subtypes (refresh stimhack))) "Card is updated")))))

(deftest update-all-subtypes-test
  (before-each [state (new-game {:runner {:hand ["Stimhack" "Contaminate" "Laamb"]}})]
    (testing "Returns false when nothing changes"
      (do-game state
        (is (not (update-all-subtypes state)))))
    (testing "Returns true when at least 1 card is updated"
      (do-game state
        (swap! state assoc :effects [{:type :subtypes
                                      :req (req (= "Stimhack" (:title target)))
                                      :value "Mod"}])
        (is (update-all-subtypes state))))))
