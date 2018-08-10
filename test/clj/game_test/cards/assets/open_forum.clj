(ns game-test.cards.assets.open-forum
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest open-forum
  ;; Open Forum
  (do-game
    (new-game {:corp {:deck ["Open Forum" "Ice Wall" "Fire Wall" "Enigma"]}})
    (play-from-hand state :corp "Open Forum" "New remote")
    (core/move state :corp (find-card "Ice Wall" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Fire Wall" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Enigma" (:hand (get-corp))) :deck)
    (is (-> @state :corp :hand count zero?))
    (let [forum (get-content state :remote1 0)]
      (core/rez state :corp forum)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (last-log-contains? state "Fire Wall") "Mandatory Draw was Ice Wall, Open Forum should reveal Fire Wall")
      (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
      (is (= 2 (-> @state :corp :deck count)) "Two cards should remain in R&D")
      (is (= "Ice Wall" (-> @state :corp :deck first :title)) "Top card in R&D should be Ice Wall"))))
