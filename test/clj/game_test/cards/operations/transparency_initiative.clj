(ns game-test.cards.operations.transparency-initiative
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest transparency-initiative
  ;; Transparency Initiative - Full test
  (do-game
    (new-game {:corp {:deck ["Transparency Initiative" "Oaktown Renovation"
                             "Project Atlas" "Hostile Takeover" "Casting Call"]}})
    (core/gain state :corp :click 5)
    (play-from-hand state :corp "Oaktown Renovation" "New remote")
    (play-from-hand state :corp "Casting Call")
    (click-card state :corp (find-card "Project Atlas" (:hand (get-corp))))
    (click-prompt state :corp "New remote")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (let [oaktown (get-content state :remote1 0)
          atlas (get-content state :remote2 0)
          hostile (get-content state :remote3 0)]
      (play-from-hand state :corp "Transparency Initiative")
      (click-card state :corp (refresh oaktown))
      ;; doesn't work on face-up agendas
      (is (zero? (count (:hosted (refresh oaktown)))))
      (click-card state :corp (refresh atlas))
      (is (= 1 (count (:hosted (refresh atlas)))) "Casting Call")
      ;; works on facedown agenda
      (click-card state :corp (refresh hostile))
      (is (= 1 (count (:hosted (refresh hostile)))))
      ;; gains Public subtype
      (is (core/has-subtype? (refresh hostile) "Public"))
      ;; gain 1 credit when advancing
      (is (= 5 (:credit (get-corp))))
      (core/advance state :corp {:card (refresh hostile)})
      (is (= 5 (:credit (get-corp))))
      ;; make sure advancing other agendas doesn't gain 1
      (core/advance state :corp {:card (refresh oaktown)})
      (is (= 6 (:credit (get-corp))) "Transparency initiative didn't fire")
      (core/advance state :corp {:card (refresh atlas)})
      (is (= 5 (:credit (get-corp))) "Transparency initiative didn't fire"))))
