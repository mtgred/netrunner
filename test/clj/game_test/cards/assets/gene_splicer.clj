(ns game-test.cards.assets.gene-splicer
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest gene-splicer
  ;; Gene Splicer
  (testing "Runner accesses an unadvanced Gene Splicer and doesn't trash
    ;; No net damage is dealt and Gene Splicer remains installed"
    (do-game
      (new-game {:corp {:deck ["Gene Splicer"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (is (zero? (count (:discard (get-runner)))) "Runner took no net damage")
      (is (= "Gene Splicer" (:title (get-content state :remote1 0))) "Gene Splicer was not trashed")
      (is (= 5 (:credit (get-runner))) "Runner spent no credits")))
  (testing "Runner accesses an unadvanced Gene Splicer and trashes it.
    No net damage is dealt and Gene Splicer is trashed"
    (do-game
      (new-game {:corp {:deck ["Gene Splicer"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (zero? (count (:discard (get-runner)))) "Runner took no net damage")
      (is (= nil (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
      (is (= (:title (last (:discard (get-corp)))) "Gene Splicer") "Gene Splicer trashed")
      (is (= 4 (:credit (get-runner))) "Runner spent 1 credit to trash Gene Splicer")))
  (testing "Runner accesses a single-advanced Gene Splicer and doesn't trash.
    1 net damage is dealt and Gene Splicer remains installed"
    (do-game
      (new-game {:corp {:deck ["Gene Splicer"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (core/add-counter state :corp (get-content state :remote1 0) :advancement 1)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (is (= 1 (count (:discard (get-runner)))) "Runner took 1 net damage")
      (is (= "Gene Splicer" (:title (get-content state :remote1 0))) "Gene Splicer was not trashed")
      (is (= 5 (:credit (get-runner))) "Runner spent no credits")))
  (testing "Runner accesses a single-advanced Gene Splicer and trashes it.
    1 net damage is dealt and Gene Splicer is trashed"
    (do-game
      (new-game {:corp {:deck ["Gene Splicer"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (core/add-counter state :corp (get-content state :remote1 0) :advancement 1)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (= 1 (count (:discard (get-runner)))) "Runner took 1 net damage")
      (is (= nil (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
      (is (= (:title (last (:discard (get-corp)))) "Gene Splicer") "Gene Splicer trashed")
      (is (= 4 (:credit (get-runner))) "Runner spent 1 credit to trash Gene Splicer")))
  (testing "Runner accesses a double-advanced Gene Splicer and doesn't trash
    2 net damage is dealt and Gene Splicer remains installed"
    (do-game
      (new-game {:corp {:deck ["Gene Splicer"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (core/add-counter state :corp (get-content state :remote1 0) :advancement 2)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net damage")
      (is (= "Gene Splicer" (:title (get-content state :remote1 0))) "Gene Splicer was not trashed")
      (is (= 5 (:credit (get-runner))) "Runner spent no credits")))
  (testing "Runner accesses a double-advanced Gene Splicer and trashes it.
    2 net damage is dealt and Gene Splicer is trashed"
    (do-game
      (new-game {:corp {:deck ["Gene Splicer"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (core/add-counter state :corp (get-content state :remote1 0) :advancement 2)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net damage")
      (is (= nil (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
      (is (= (:title (last (:discard (get-corp)))) "Gene Splicer") "Gene Splicer trashed")
      (is (= 4 (:credit (get-runner))) "Runner spent 1 credit to trash Gene Splicer")))
  (testing "Corp triple-advances a Gene Splicer and uses its ability to add to their score area as a 1 point agenda"
    (do-game
      (new-game {:corp {:deck [(qty "Gene Splicer" 2) (qty "Ice Wall" 3) (qty "Vanilla" 2)]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (let [gs (get-content state :remote1 0)]
        (core/add-counter state :corp gs :advancement 2)
        (take-credits state :runner)
        (core/add-counter state :corp (refresh gs) :advancement 1)
        (core/rez state :corp (refresh gs))
        (card-ability state :corp (refresh gs) 0)
        (is (= nil (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
        (is (= 1 (:agendapoints (get-scored state :corp 0))) "Gene Splicer added to Corp score area")))))
