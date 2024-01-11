(ns game.core.subtypes-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.subtypes :refer :all]
   [game.macros :refer [req]]
   [game.test-framework :refer :all]))

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
        (swap! state assoc :effects [{:type :gain-subtype :value "Mod"}])
        (is (= #{"Run" "Mod"} (into #{} (subtypes-for-card state (refresh stimhack)))))))))

(deftest update-subtypes-for-card-test
  (before-each [state (new-game {:runner {:hand ["Stimhack"]}})
                stimhack (find-card "Stimhack" (get-in @state [:runner :hand]))]
    (testing "Returns false when nothing changes"
      (do-game state
        (is (not (update-subtypes-for-card state nil stimhack)))))
    (testing "Returns true when subtypes change"
      (do-game state
        (swap! state assoc :effects [{:type :gain-subtype :value "Mod"}])
        (is (update-subtypes-for-card state nil stimhack))
        (is (= #{"Mod" "Run"} (into #{} (:subtypes (refresh stimhack)))) "Card is updated")))))

(deftest update-all-subtypes-test
  (before-each [state (new-game {:runner {:hand ["Stimhack" "Contaminate" "Laamb"]}})
                _ (update-all-subtypes state)]
    (testing "Returns false when nothing changes"
      (do-game state
        (is (not (update-all-subtypes state)))))
    (testing "Returns true when at least 1 card is updated"
      (do-game state
        (swap! state assoc :effects [{:type :gain-subtype
                                      :req (req (= "Stimhack" (:title target)))
                                      :value "Mod"}])
        (is (update-all-subtypes state))))))
