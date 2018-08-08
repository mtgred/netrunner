(ns game-test.cards.assets.malia-z0l0k4
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest malia-z0l0k4
  ;; Malia Z0L0K4 - blank an installed non-virtual runner resource
  (do-game
    (new-game {:corp {:deck [(qty "Malia Z0L0K4" 2)
                             "Mausolus"]}
               :runner {:deck ["Rachel Beckman"
                               "Daily Casts"
                               "Rumor Mill"]}})
    (play-from-hand state :corp "Malia Z0L0K4" "New remote")
    (play-from-hand state :corp "Malia Z0L0K4" "New remote")
    (play-from-hand state :corp "Mausolus" "HQ")
    (take-credits state :corp)
    (let [malia1 (get-content state :remote1 0)
          malia2 (get-content state :remote2 0)
          mausolus (get-ice state :hq 0)]
      (play-from-hand state :runner "Daily Casts")
      (take-credits state :runner)
      (let [N (:credit (get-runner))]
        (core/rez state :corp malia1)
        (click-card state :corp (get-resource state 0))
        (take-credits state :corp)
        (is (= N (:credit (get-runner))) "Daily casts did not trigger when blanked"))
      (take-credits state :runner)
      (core/derez state :corp malia1)
      (let [N (:credit (get-runner))]
        (take-credits state :corp)
        (is (= (+ N 2) (:credit (get-runner))) "Daily casts triggers again when unblanked"))
      (play-from-hand state :runner "Rachel Beckman")
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks after playing Beckman")
      (core/rez state :corp malia1)
      (click-card state :corp (get-resource state 1))
      (is (= 3 (:click (get-runner))) "Runner has 3 clicks after Beckman is blank")
      (core/derez state :corp malia1)
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks after Beckman is unblanked")
      (core/rez state :corp malia1)
      (click-card state :corp (get-resource state 1))
      (core/rez state :corp mausolus)
      (card-subroutine state :corp mausolus 2)
      (is (and (= 1 (:tag (get-runner)))
               (zero? (count (:discard (get-runner))))) "Runner has 1 tag, but Rachel Beckman not trashed")
      (take-credits state :runner)
      (is (zero? (count (:hand (get-corp)))) "Malia is not in hand")
      (core/move-card state :corp {:card malia1 :server "HQ"})
      (is (= 1 (count (:hand (get-corp)))) "Malia is in hand")
      (is (= 1 (count (:discard (get-runner)))) "Rachel Beckman got trashed on unblanking")
      (core/rez state :corp malia2)
      (click-card state :corp (get-resource state 0))
      (let [N (:credit (get-runner))]
        (take-credits state :corp)
        (is (= N (:credit (get-runner))) "Daily casts is blank, so no drip")))
    (play-from-hand state :runner "Rumor Mill")
    (take-credits state :runner)
    (let [N (:credit (get-runner))]
      (take-credits state :corp)
      (is (= (+ N 2) (:credit (get-runner)))))))
