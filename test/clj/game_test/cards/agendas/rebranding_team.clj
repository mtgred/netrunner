(ns game-test.cards.agendas.rebranding-team
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest rebranding-team
  ;; Rebranding Team
  (do-game
    (new-game {:corp {:deck ["Rebranding Team" "Launch Campaign" "City Surveillance"
                             "Jackson Howard" "Museum of History" "Advanced Assembly Lines"]}})
    (play-and-score state "Rebranding Team")
    (core/click-draw state :runner 1)
    (is (core/has-subtype? (find-card "Advanced Assembly Lines" (:hand (get-corp))) "Advertisement"))
    ; #2608 part 2 - retain Advertisement always
    (trash-from-hand state :corp "Advanced Assembly Lines")
    (is (core/has-subtype? (find-card "Advanced Assembly Lines" (:discard (get-corp))) "Advertisement"))
    (is (core/has-subtype? (find-card "Launch Campaign" (:hand (get-corp))) "Advertisement"))
    (is (core/has-subtype? (find-card "City Surveillance" (:hand (get-corp))) "Advertisement"))
    (is (core/has-subtype? (find-card "Jackson Howard" (:hand (get-corp))) "Advertisement"))
    (is (core/has-subtype? (find-card "Jackson Howard" (:hand (get-corp))) "Executive"))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Advertisement"))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Alliance"))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Ritzy"))
    (core/move state :corp (find-card "Rebranding Team" (:scored (get-corp))) :deck)
    (is (core/has-subtype? (find-card "Launch Campaign" (:hand (get-corp))) "Advertisement"))
    (is (not (core/has-subtype? (find-card "Advanced Assembly Lines" (:discard (get-corp))) "Advertisement")))
    (is (not (core/has-subtype? (find-card "City Surveillance" (:hand (get-corp))) "Advertisement")))
    (is (not (core/has-subtype? (find-card "Jackson Howard" (:hand (get-corp))) "Advertisement")))
    (is (core/has-subtype? (find-card "Jackson Howard" (:hand (get-corp))) "Executive"))
    (is (not (core/has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Advertisement")))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Alliance"))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Ritzy"))))
