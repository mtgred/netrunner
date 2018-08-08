(ns game-test.cards.resources.district-99
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest district-99
  ;; District 99 - Gains power counters on hardware/program trashes, can spend 3 power counters to recur a card matching identity
  (testing "Trashes by both sides and manual triggers"
    (do-game
      (new-game {:corp {:deck ["Bio-Ethics Association"]}
                 :runner {:deck ["District 99" (qty "Spy Camera" 2) "Faerie"]}})
      (play-from-hand state :corp "Bio-Ethics Association" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "District 99")
      (let [d99 (get-resource state 0)
            bea (get-content state :remote1 0)]
        (card-ability state :runner (refresh d99) 1) ; manually add power counter
        (is (= 1 (get-counters (refresh d99) :power)) "1 power counter was added manually")
        (card-ability state :runner (refresh d99) 1) ; try to manually add power counter twice
        (is (= 1 (get-counters (refresh d99) :power)) "Manual power counter addition is only possible once per turn")
        (play-from-hand state :runner "Spy Camera")
        (card-ability state :runner (get-hardware state 0) 1) ; pop spy camera
        (click-prompt state :runner "OK")
        (is (= 1 (get-counters (refresh d99) :power)) "Manual power counter addition suppressed later trigger")
        (play-from-hand state :runner "Spy Camera")
        (is (= 1 (count (:hand (get-runner)))) "Faerie in hand")
        (is (= "Faerie" (:title (first (:hand (get-runner))))))
        (core/rez state :corp bea)
        (take-credits state :runner)
        (is (= 0 (count (:hand (get-runner)))) "Faerie was trashed")
        (is (= 2 (get-counters (refresh d99) :power)) "Trashing Faerie from grip placed a counter")
        (card-ability state :runner (get-hardware state 0) 1) ; pop spy camera
        (click-prompt state :runner "OK")
        (is (= 2 (get-counters (refresh d99) :power)) "Trashing Spy Camera after Faerie did not place a counter"))))
  (testing "Rebirth interaction, basic functionality"
    (do-game
      (new-game {:corp {:deck ["Grim"]}
                 :runner {:id "Armand \"Geist\" Walker: Tech Lord"
                          :deck ["District 99" (qty "Spy Camera" 3) "Faerie" "Rebirth" "Sure Gamble"]}})
      (play-from-hand state :corp "Grim" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (core/click-draw state :runner nil)
      (core/click-draw state :runner nil)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "District 99")
      (play-from-hand state :runner "Rebirth")
      (let [khan "Khan: Savvy Skiptracer"]
        (click-prompt state :runner khan)
        (is (= khan (-> (get-runner) :identity :title)) "Rebirthed into Khan"))
      (play-from-hand state :runner "Spy Camera")
      (play-from-hand state :runner "Faerie")
      (let [d99 (get-resource state 0)
            faerie (get-program state 0)
            spycam (get-hardware state 0)
            grim (get-ice state :hq 0)]
        (run-on state :hq)
        (core/rez state :corp grim)
        (card-subroutine state :corp (refresh grim) 0)
        (is (= 0 (get-counters (refresh d99) :power)) "No power counters before Faerie is trashed")
        (click-card state :corp faerie)
        (is (= 1 (get-counters (refresh d99) :power)) "1 power counter was added for Faerie being trashed")
        (card-ability state :runner spycam 1) ; pop spycam
        (click-prompt state :runner "OK")
        (is (= 1 (get-counters (refresh d99) :power)) "Trashing Spy Camera after Faerie did not add a second power counter")
        (card-ability state :runner (refresh d99) 2) ; manually add counter
        (is (= 1 (get-counters (refresh d99) :power)) "Can't manually add power counter after one has already been added")
        (run-jack-out state)
        (play-from-hand state :runner "Spy Camera")
        (take-credits state :runner)
        (card-ability state :runner (get-hardware state 0) 1) ; pop spycam
        (click-prompt state :runner "OK")
        (is (= 2 (get-counters (refresh d99) :power)) "Trashing Spy Camera on Corp turn added a second power counter")
        (take-credits state :corp)
        (play-from-hand state :runner "Spy Camera")
        (take-credits state :runner)
        (card-ability state :runner (get-hardware state 0) 1) ; pop spycam
        (click-prompt state :runner "OK")
        (take-credits state :corp)
        (is (= 3 (get-counters (refresh d99) :power)) "Trashing Spy Camera on Runner turn added a third power counter")
        (let [faerie (first (filter #(= (:title %) "Faerie") (:discard (get-runner))))]
          (doseq [c ["Sure Gamble" "Faerie" "Spy Camera"]]
            (is (some? (filter #(= (:title %) c) (:hand (get-runner)))) (str c " is in the discard")))
          (is (zero? (count (:hand (get-runner)))) "Faerie is not in hand")
          (card-ability state :runner (refresh d99) 0)  ; Retrieve card from Archives
          (is (= 2 (count (:choices (prompt-map :runner)))) "Runner can choose between Spy Camera and Faerie only")
          (click-prompt state :runner faerie)
          (is (= 1 (count (:hand (get-runner)))) "1 card added to hand")
          (is (= "Faerie" (-> (get-runner) :hand first :title)) "Faerie added to hand")
          (is (zero? (get-counters (refresh d99) :power)) "Picking up Faerie removed 3 counters"))))))
