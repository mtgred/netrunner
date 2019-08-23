(ns game-test.engine.events
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest first-trash
  (doseq [first-trash [:runner :corp]
          second-trash [:runner :corp]]
    (testing (str "Base test for " (name first-trash) " followed by " (name second-trash))
      (do-game
        (new-game {:corp {:hand (qty "Hedge Fund" 5)}})
        (is (not (core/first-trash? state)))
        (core/trash state first-trash (find-card "Hedge Fund" (:hand (get-corp))))
        (is (core/first-trash? state))
        (core/trash state second-trash (find-card "Hedge Fund" (:hand (get-corp))))
        (is (not (core/first-trash? state)))))))
