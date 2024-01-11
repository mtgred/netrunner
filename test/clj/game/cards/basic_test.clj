(ns game.cards.basic-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.test-framework :refer :all]))

(deftest corp-basic-actions-gain-1-credit
    ;; Gain 1 credit
    (do-game
      (new-game)
      (is (changed? [(:credit (get-corp)) 1]
            (click-credit state :corp))
          "Gain 1 credit")))

(deftest corp-basic-actions-draw-card
    ;; Draw card
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]}})
      (is (changed? [(count (:hand (get-corp))) 1]
            (click-draw state :corp))
          "Drew 1 card")))

(deftest corp-basic-actions-install-agenda
    ;; Install agenda
    (do-game
      (new-game {:corp {:deck ["Project Beale"]}})
      (play-from-hand state :corp "Project Beale" "New remote")
      (is (= "Project Beale" (:title (get-content state :remote1 0))) "Project Beale installed")))

(deftest corp-basic-actions-install-asset
    ;; Install asset
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (is (= "PAD Campaign" (:title (get-content state :remote1 0))) "PAD Campaign installed")))

(deftest corp-basic-actions-install-upgrade
    ;; Install upgrade
    (do-game
      (new-game {:corp {:deck ["Breaker Bay Grid"]}})
      (play-from-hand state :corp "Breaker Bay Grid" "New remote")
      (is (= "Breaker Bay Grid" (:title (get-content state :remote1 0))) "Breaker Bay Grid installed")))

(deftest corp-basic-actions-install-ice
    ;; Install ice
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (is (= "Ice Wall" (:title (get-ice state :remote1 0))) "Ice Wall installed")))

(deftest corp-basic-actions-play-operation
    ;; Play operation
    (do-game
      (new-game {:corp {:deck ["Hedge Fund"]}})
      (is (changed? [(:credit (get-corp)) 4]
            (play-from-hand state :corp "Hedge Fund"))
          "Gained 4c from Hedge Fund")))

(deftest corp-basic-actions-advance-installed-ice
    ;; Advance installed ice
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (click-advance state :corp (get-ice state :hq 0))
      (is (= 1 (get-counters (get-ice state :hq 0) :advancement)) "Placed 1 advancement on Ice Wall")))

(deftest corp-basic-actions-advance-agenda
    ;; Advance agenda
    (do-game
      (new-game {:corp {:deck ["Project Beale"]}})
      (play-from-hand state :corp "Project Beale" "New remote")
      (click-advance state :corp (get-content state :remote1 0))
      (is (= 1 (get-counters (get-content state :remote1 0) :advancement)) "Placed 1 advancement on Project Beale")))

(deftest corp-basic-actions-trash-resource-if-runner-is-tagged
    ;; Trash resource if runner is tagged
    (do-game
      (new-game {:runner {:deck ["Fan Site"]}
                 :options {:start-as :runner}})
      (play-from-hand state :runner "Fan Site")
      (let [fs (get-resource state 0)]
        (take-credits state :runner)
        (gain-tags state :runner 1)
        (trash-resource state)
        (click-card state :corp fs)
        (is (= 1 (count (:discard (get-runner)))) "Fan Site got trashed"))))

(deftest corp-basic-actions-purge
    ;; Purge
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:deck ["Clot" "Imp" "Botulus"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Clot")
      (play-from-hand state :runner "Imp")
      (play-from-hand state :runner "Botulus")
      (click-card state :runner (get-ice state :hq 0))
      (take-credits state :runner)
      (let [imp (get-program state 1)
            bot (first (:hosted (get-ice state :hq 0)))]
        (is (= 2 (get-counters (refresh imp) :virus)) "Imp starts with 2 virus counters")
        (is (= 1 (get-counters (refresh bot) :virus)) "Botulus starts with 1 virus counter")
        (purge state :corp)
        (is (= 1 (count (:discard (get-runner)))) "Clot got trashed")
        (is (= 0 (get-counters (refresh imp) :virus)) "Imp has zero counters after purge")
        (is (= 0 (get-counters (refresh bot) :virus)) "Botulus has zero counters after purge")
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh bot) :virus)) "Botulus gains 1 counter at start of turn"))))

(deftest runner-basic-actions-gain-1-credit
    ;; Gain 1 credit
    (do-game
      (new-game {:options {:start-as :runner}})
      (is (changed? [(:credit (get-runner)) 1]
            (click-credit state :runner))
          "Gain 1 credit")))

(deftest runner-basic-actions-draw-card
    ;; Draw card
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:deck [(qty "Sure Gamble" 10)]}})
      (is (changed? [(count (:hand (get-runner))) 1]
            (click-draw state :runner))
          "Drew 1 card")))

(deftest runner-basic-actions-install-program
    ;; Install program
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:deck ["Misdirection"]}})
      (play-from-hand state :runner "Misdirection")
      (is (= "Misdirection" (:title (get-program state 0))) "Misdirection installed")))

(deftest runner-basic-actions-install-resource
    ;; Install resource
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:deck ["Fan Site"]}})
      (play-from-hand state :runner "Fan Site")
      (is (= "Fan Site" (:title (get-resource state 0))) "Fan Site installed")))

(deftest runner-basic-actions-install-hardware
    ;; Install hardware
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:deck ["Bookmark"]}})
      (play-from-hand state :runner "Bookmark")
      (is (= "Bookmark" (:title (get-hardware state 0))) "Bookmark installed")))

(deftest runner-basic-actions-play-operation
    ;; Play operation
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:deck ["Sure Gamble"]}})
      (is (changed? [(:credit (get-runner)) 4]
            (play-from-hand state :runner "Sure Gamble"))
          "Gained 4c from Sure Gamble")))

(deftest runner-basic-actions-run-hq
    ;; Run HQ
    (do-game
      (new-game {:options {:start-as :runner}})
      (run-on state :hq)
      (is (:run @state) "Run initiated")))

(deftest runner-basic-actions-remove-tag
    ;; Remove tag
    (do-game
      (new-game {:options {:start-as :runner}})
      (gain-tags state :runner 1)
      (is (= 1 (count-tags state)) "Runner has 1 tag")
      (remove-tag state :runner)
      (is (= 0 (count-tags state)) "Runner removed tag")))
