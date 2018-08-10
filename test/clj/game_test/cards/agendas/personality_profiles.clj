(ns game-test.cards.agendas.personality-profiles
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest personality-profiles
  ;; Personality Profiles
  (testing "basic test"
    (do-game
      (new-game {:corp {:deck ["Personality Profiles"]}
                 :runner {:deck ["Self-modifying Code" "Clone Chip"
                                 "Corroder" (qty "Patron" 2)]}})
      (starting-hand state :runner ["Self-modifying Code" "Clone Chip" "Patron" "Patron"])
      (play-and-score state "Personality Profiles")
      (take-credits state :corp)
      (play-from-hand state :runner "Self-modifying Code")
      (play-from-hand state :runner "Clone Chip")
      (let [smc (get-program state 0)]
        (card-ability state :runner smc 0)
        (click-prompt state :runner (find-card "Corroder" (:deck (get-runner))))
        (is (= 2 (count (:discard (get-runner))))))
      (let [chip (get-hardware state 0)]
        (card-ability state :runner chip 0)
        (click-card state :runner (find-card "Self-modifying Code" (:discard (get-runner))))
        (is (second-last-log-contains? state "Patron")
            "Personality Profiles trashed card name is in log")
        (is (= 3 (count (:discard (get-runner))))))))
  (testing "Ensure effects still fire with an empty hand, #1840"
    (do-game
      (new-game {:corp {:deck ["Personality Profiles"]}
                 :runner {:deck ["Self-modifying Code" "Clone Chip"
                                 "Corroder"]}})
      (starting-hand state :runner ["Self-modifying Code" "Clone Chip"])
      (play-and-score state "Personality Profiles")
      (take-credits state :corp)
      (play-from-hand state :runner "Self-modifying Code")
      (play-from-hand state :runner "Clone Chip")
      (let [smc (get-program state 0)]
        (card-ability state :runner smc 0)
        (click-prompt state :runner (find-card "Corroder" (:deck (get-runner)))))
      (let [cor (get-program state 0)]
        (is (some? cor))
        (is (= (:title cor) "Corroder"))
        (is (= "Self-modifying Code" (:title (first (:discard (get-runner)))))))
      (let [chip (get-hardware state 0)]
        (card-ability state :runner chip 0)
        (click-card state :runner (find-card "Self-modifying Code" (:discard (get-runner)))))
      (let [smc (get-program state 1)]
        (is (some? smc))
        (is (= (:title smc) "Self-modifying Code"))
        (is (= "Clone Chip" (:title (first (:discard (get-runner))))))))))
