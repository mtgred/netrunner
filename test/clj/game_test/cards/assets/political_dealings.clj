(ns game-test.cards.assets.political-dealings
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest political-dealings
  ;; Political Dealings
  (testing "Full test"
    (do-game
      (new-game {:corp {:deck ["Political Dealings" "Medical Breakthrough" "Oaktown Renovation"]}})
      (core/move state :corp (find-card "Medical Breakthrough" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Oaktown Renovation" (:hand (get-corp))) :deck)
      (play-from-hand state :corp "Political Dealings" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      ;; Install Medical Breakthrough
      (core/draw state :corp)
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "New remote")
      (is (= "Medical Breakthrough" (:title (get-content state :remote2 0)))
          "Medical Breakthrough installed by Political Dealings")
      ;; Install Oaktown Renovation
      (core/draw state :corp)
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "New remote")
      (is (= "Oaktown Renovation" (:title (get-content state :remote3 0)))
          "Oaktown Renovation installed by Political Dealings")
      (is (:rezzed (get-content state :remote3 0))
          "Oaktown Renovation installed face up")))
  (testing "Daily Business Show interaction - Draw 2 agendas, install both of them but return 1 to bottom of R&D"
    (do-game
      (new-game {:corp {:deck ["Political Dealings" "Daily Business Show" "Turtlebacks"
                               "Breaking News" "Project Beale"]}})
      (starting-hand state :corp ["Political Dealings" "Daily Business Show" "Turtlebacks"])
      (core/gain state :corp :credit 3)
      (play-from-hand state :corp "Political Dealings" "New remote")
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (play-from-hand state :corp "Turtlebacks" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/rez state :corp (get-content state :remote2 0))
      (core/rez state :corp (get-content state :remote3 0))
      (take-credits state :corp)
      (is (zero? (count (:hand (get-corp)))))
      (let [agenda1 (first (:deck (get-corp)))
            agenda2 (second (:deck (get-corp)))]
        (take-credits state :runner)
        ;; Install first agenda
        (is (= 2 (count (:hand (get-corp)))))
        (is (zero? (:credit (get-corp))))
        (click-prompt state :corp "Yes")
        (click-prompt state :corp "New remote")
        (is (= (:cid agenda1) (:cid (get-content state :remote4 0))))
        (is (= 1 (:credit (get-corp))) "Turtlebacks triggered")
        ;; Install second agenda
        (click-prompt state :corp "Yes")
        (click-prompt state :corp "New remote")
        (is (= (:cid agenda2) (:cid (get-content state :remote5 0))))
        (is (= 2 (:credit (get-corp))) "Turtlebacks triggered")
        ;; DBS - put first agenda at bottom of R&D
        (click-card state :corp (get-content state :remote4 0))
        (is (zero? (count (:hand (get-corp)))))
        (is (= (:cid agenda1) (:cid (last (:deck (get-corp))))))))))
