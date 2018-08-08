(ns game-test.cards.agendas.director-haas-pet-project
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest director-haas-pet-project
  ;; Director Haas' Pet Project
  (do-game
    (new-game {:corp {:deck ["Director Haas' Pet Project"
                             "Adonis Campaign"
                             "Strongbox"
                             "Eli 1.0"
                             (qty "Hedge Fund" 5)]}})
    (starting-hand state :corp ["Director Haas' Pet Project" "Adonis Campaign" "Strongbox"])
    (core/move state :corp (find-card "Eli 1.0" (:deck (get-corp))) :discard)
    (play-and-score state "Director Haas' Pet Project")
    (click-prompt state :corp "Yes")
    (click-card state :corp (find-card "Adonis Campaign" (:hand (get-corp))))
    (click-card state :corp (find-card "Strongbox" (:hand (get-corp))))
    (click-card state :corp (find-card "Eli 1.0" (:discard (get-corp))))))
