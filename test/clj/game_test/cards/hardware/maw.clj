(ns game-test.cards.hardware.maw
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest maw
  ;; Maw - Once per turn, first time runner declines to steal or trash, trash a HQ card at random
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "BOOM!" 5)]}
                 :runner {:deck ["Maw"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (zero? (count (:discard (get-corp)))) "No HQ card in discard before Maw installed")
      (play-from-hand state :runner "Maw")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (zero? (count (:discard (get-corp)))) "HQ card not trashed by Maw as first decline already happened")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (= 1 (count (:discard (get-corp)))) "HQ card trashed by Maw")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (= 1 (count (:discard (get-corp)))) "2nd HQ card on same turn not trashed by Maw")))
  (testing "Check trashed card is trashed face-up if it's the card that is accessed, issue #2695"
    ;; Also checks Maw auto-trashes on Operation with no trash cost
    (do-game
      (new-game {:corp {:deck ["Hedge Fund"]}
                 :runner {:deck ["Maw"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Maw")
      (run-empty-server state :hq)
      (is (zero? (count (:discard (get-corp)))) "HQ card not trashed by Maw yet")
      (click-prompt state :runner "No action")
      (is (= 1 (count (:discard (get-corp)))) "HQ card trashed by Maw now")
      (is (:seen (first (:discard (get-corp)))) "Trashed card is registered as seen since it was accessed")))
  (testing "with Hiro in hand - Hiro not moved to runner scored area on trash decline. #2638"
    (do-game
      (new-game {:corp {:deck ["Chairman Hiro"]}
                 :runner {:deck ["Maw"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Maw")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (zero? (count (:scored (get-runner)))) "Hiro not scored")
      (is (= 1 (count (:discard (get-corp)))) "Hiro trashed by Maw")))
  (testing "Maw shouldn't trigger on stolen agenda. #3433"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"
                               (qty "Ice Wall" 5)]}
                 :runner {:deck ["Maw"]}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Maw")
      (run-empty-server state :remote1)
      (click-prompt state :runner "Steal")
      (is (zero? (count (:discard (get-corp)))) "No HQ card in discard as agenda was stolen")))
  (testing "Maw shouldn't trigger when accessing a card in archives. #3388"
    (do-game
      (new-game {:corp {:deck ["Rashida Jaheem" "Cyberdex Virus Suite" (qty "Ice Wall" 4)]}
                 :runner {:id "Alice Merchant: Clan Agitator"
                          :deck ["Maw" "Imp"]}})
      (core/move state :corp (find-card "Rashida Jaheem" (:hand (get-corp))) :deck)
      (trash-from-hand state :corp "Cyberdex Virus Suite")
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Maw")
      (play-from-hand state :runner "Imp")
      (run-empty-server state :archives)
      (click-prompt state :corp (find-card "Ice Wall" (:hand (get-corp)))) ;; Alice's ability
      (click-prompt state :runner "Cyberdex Virus Suite")
      (click-prompt state :corp "Yes")
      (run-empty-server state :rd)
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (= 3 (count (:discard (get-corp)))) "Ice Wall, CVS, and Rashida")
      (is (empty? (:prompt (get-runner))) "No more prompts for runner")))
  (testing "Maw should trigger when declining to steal. #3388"
    (do-game
      (new-game {:corp {:deck [(qty "Obokata Protocol" 2) (qty "Ice Wall" 4)]}
                 :runner {:id "Alice Merchant: Clan Agitator"
                          :deck ["Maw" "Archives Interface"]}})
      (trash-from-hand state :corp "Ice Wall")
      (starting-hand state :corp ["Obokata Protocol" "Obokata Protocol"])
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Maw")
      (play-from-hand state :runner "Archives Interface")
      (run-empty-server state :archives)
      (click-prompt state :corp (find-card "Obokata Protocol" (:hand (get-corp))))
      (click-prompt state :runner "Yes")
      (click-prompt state :runner (find-card "Ice Wall" (:discard (get-corp))))
      (click-prompt state :runner "No action")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (= 2 (count (:discard (get-corp)))) "Ice Wall and Obokata"))))
