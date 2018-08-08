(ns game-test.cards.events.drive-by
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest drive-by
  ;; Drive By - Expose card in remote server and trash if asset or upgrade
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Eve Campaign" 2)
                               "Product Placement"
                               "Project Atlas"]}
                 :runner {:deck [(qty "Drive By" 2)]}})
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Eve Campaign" "New remote")
      (play-from-hand state :corp "Eve Campaign" "New remote")
      (play-from-hand state :corp "Project Atlas" "New remote")
      (play-from-hand state :corp "Product Placement" "HQ")
      (take-credits state :corp)
      (let [eve1 (get-content state :remote1 0)
            eve2 (get-content state :remote2 0)
            atl (get-content state :remote3 0)
            pp (get-content state :hq 0)]
        (core/rez state :corp eve1)
        (play-from-hand state :runner "Drive By")
        (click-card state :runner pp)
        (is (= 1 (count (get-in @state [:corp :servers :hq :content])))
            "Upgrades in root of central servers can't be targeted")
        (click-card state :runner (refresh eve1))
        (is (= 1 (count (get-in @state [:corp :servers :remote1 :content])))
            "Rezzed cards can't be targeted")
        (click-card state :runner eve2)
        (is (= 2 (:click (get-runner))) "Spent 2 clicks")
        (is (and (= 1 (count (:discard (get-corp))))
                 (= 5 (:credit (get-runner))))
            "Eve trashed at no cost")
        (is (nil? (get-in @state [:corp :servers :remote2 :content])) "Server 2 no longer exists")
        (play-from-hand state :runner "Drive By")
        (click-card state :runner atl)
        (is (zero? (:click (get-runner))) "Runner has 0 clicks left")
        (is (= 1 (count (get-in @state [:corp :servers :remote3 :content])))
            "Project Atlas not trashed from Server 3"))))
  (testing "Psychic Field trashed after psi game. Issue #2127."
    (do-game
      (new-game {:corp {:deck ["Psychic Field"]}
                 :runner {:deck [(qty "Drive By" 3)]}})
      (play-from-hand state :corp "Psychic Field" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Drive By")
      (click-card state :runner (get-content state :remote1 0))
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (empty? (get-content state :remote1)) "Psychic Field trashed")))
  (testing "Turn on reprisal cards. Issue #3755."
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:deck ["Drive By"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Drive By")
      (click-card state :runner "PAD Campaign")
      (is (empty? (get-content state :remote1)) "PAD Campaign trashed")
      (is (get-in (get-runner) [:register :trashed-card]) "Registered as runner trashed a card"))))
