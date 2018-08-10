(ns game-test.cards.resources.film-critic
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest film-critic
  ;; Film Critic
  (testing "Prevent Corp-trashed execs going to Runner scored. Issues #1181/#1042"
    (do-game
      (new-game {:corp {:deck [(qty "Director Haas" 3) (qty "Project Vitruvius" 3) "Hedge Fund"]}
                 :runner {:deck ["Film Critic"]}})
      (play-from-hand state :corp "Project Vitruvius" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (let [fc (first (get-resource state))]
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Yes")
        (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
        (take-credits state :runner)
        (trash-from-hand state :corp "Director Haas")
        (is (= 1 (count (:discard (get-corp)))) "Director Haas stayed in Archives")
        (is (zero? (:agenda-point (get-runner))) "No points gained by Runner")
        (is (empty? (:scored (get-runner))) "Nothing in Runner scored"))))
  (testing "Fetal AI interaction"
    (do-game
      (new-game {:corp {:deck [(qty "Fetal AI" 3)]}
                 :runner {:deck ["Film Critic" (qty "Sure Gamble" 3)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (let [fc (first (get-resource state))]
        (run-empty-server state "HQ")
        ;; should not have taken damage yet
        (is (= 3 (count (:hand (get-runner)))) "No damage dealt yet")
        (click-prompt state :runner "Yes")
        (is (= 3 (count (:hand (get-runner)))) "No damage dealt")
        (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
        (card-ability state :runner fc 0)
        (is (= 1 (count (:scored (get-runner)))) "Agenda added to runner scored")
        (is (= 3 (count (:hand (get-runner)))) "No damage dealt"))))
  (testing "Do not take a net damage when a hosted agenda is trashed due to film critic trash #2382"
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Infrastructure" 3) "Project Vitruvius"]}
                 :runner {:deck ["Film Critic" (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (play-from-hand state :corp "Project Vitruvius" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (let [fc (first (get-resource state))]
        (run-empty-server state :remote2)
        (click-prompt state :runner "Yes")
        (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
        (take-credits state :runner)
        (core/gain state :corp :credit 10)
        (core/trash-resource state :corp nil)
        (click-card state :corp fc)
        (is (= 1 (count (:discard (get-runner)))) "FC trashed")
        (is (= 1 (count (:discard (get-corp)))) "Agenda trashed")
        (is (= 3 (count (:hand (get-runner)))) "No damage dealt"))))
  (testing "required hosted cards to be an agenda before firing ability"
    (do-game
      (new-game {:corp {:deck ["MCA Informant"]}
                 :runner {:deck ["Film Critic"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (let [fc (first (get-resource state))]
        (take-credits state :runner)
        (play-from-hand state :corp "MCA Informant")
        (click-card state :corp fc)
        (is (= 1 (count (:hosted (refresh fc)))) "MCA Informant hosted on FC")
        (take-credits state :corp)
        (card-ability state :runner fc 0)
        (is (= 1 (count (:hosted (refresh fc)))) "MCA Informant still hosted on FC")))))
