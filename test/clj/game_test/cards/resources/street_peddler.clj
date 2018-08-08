(ns game-test.cards.resources.street-peddler
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest street-peddler
  ;; Street Peddler
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Street Peddler" "Gordian Blade"
                                 "Torch" (qty "Sure Gamble" 2)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler" "Sure Gamble"])
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)]
        (is (= 3 (count (:hosted sp))) "Street Peddler is hosting 3 cards")
        (card-ability state :runner sp 0)
        (click-prompt state :runner (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
        (is (= "Gordian Blade" (:title (get-program state 0)))
            "Gordian Blade was installed")
        (is (= 3 (core/available-mu state)) "Gordian cost 1 mu"))))
  (testing "Can't afford install"
    (do-game
      (new-game {:runner {:deck ["Street Peddler" (qty "Gordian Blade" 3)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler"])
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)]
        (card-ability state :runner sp 0)
        (core/lose state :runner :credit 3)
        (is (= 2 (count (:choices (first (:prompt (get-runner))))))
            "1 card and 1 cancel option on Street Peddler")
        (click-prompt state :runner (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
        (is (zero? (count (get-program state)))
            "Gordian Blade was not installed")
        (is (and (:installed (refresh sp)) (= 3 (count (:hosted (refresh sp))))
                 "Street Peddler still installed with 3 hosted cards")))))
  (testing "Interaction with Kate discount"
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["Street Peddler"
                                 "Gordian Blade"
                                 (qty "Sure Gamble" 2)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler"])
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)]
        ;; should still be able to afford Gordian w/ Kate discount
        (core/lose state :runner :credit 3)
        (card-ability state :runner sp 0)
        (is (= 2 (count (:choices (first (:prompt (get-runner))))))
            "Only 1 choice (plus Cancel) to install off Peddler")
        (click-prompt state :runner (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
        (is (= "Gordian Blade" (:title (get-program state 0)))
            "Gordian Blade was installed")
        (is (= 3 (core/available-mu state)) "Gordian cost 1 mu"))))
  (testing "Programs should cost memory. Issue #708"
    (do-game
      (new-game {:runner {:deck ["Street Peddler" (qty "Corroder" 3)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler"])
      (play-from-hand state :runner "Street Peddler")
      (is (= 4 (core/available-mu state)) "No memory cost for hosting on Street Peddler")
      (let [sp (get-resource state 0)]
        (is (= "Corroder" (:title (first (:hosted sp)))) "Street Peddler is hosting Corroder")
        (card-ability state :runner sp 0)
        (click-prompt state :runner (first (:hosted sp))) ; choose to install Gordian
        (is (= "Corroder" (:title (get-program state 0)))
            "Corroder was installed")
        (is (= 3 (core/available-mu state)) "Corroder cost 1 mu"))))
  (testing "Muertos/Brain Chip uninstall effect not fired when removed off peddler/hosting Issue #2294, #2358"
    (do-game
      (new-game {:corp {:deck ["Jackson Howard"]}
                 :runner {:deck [(qty "Street Peddler" 2) "Muertos Gang Member" "Brain Chip"]}})
      (core/move state :runner (find-card "Muertos Gang Member" (:hand (get-runner))) :deck {:front true})
      (core/move state :runner (find-card "Brain Chip" (:hand (get-runner))) :deck {:front true})
      (core/move state :runner (find-card "Street Peddler" (:hand (get-runner))) :deck {:front true})
      (play-from-hand state :corp "Jackson Howard" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Street Peddler")
      (core/gain state :runner :agenda-point 1)
      (let [jh (get-content state :remote1 0)
            sp (get-resource state 0)]
        (core/rez state :corp jh)
        (card-ability state :runner sp 0)
        (click-prompt state :runner (find-card "Street Peddler" (:hosted sp))) ; choose to another Peddler
        (is (empty? (:prompt (get-corp))) "Corp not prompted to rez Jackson")
        (is (= 4 (core/available-mu state)) "Runner has 4 MU"))))
  (testing "Trashing hardware should not reduce :in-play values"
    (do-game
      (new-game {:runner {:deck ["Street Peddler" (qty "HQ Interface" 3)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler"])
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)]
        (card-ability state :runner sp 0)
        (click-prompt state :runner (first (:hosted sp))) ; choose to install HQ Interface
        (is (= 2 (:hq-access (get-runner)))
            "HQ Access increased by 1 from installed HQI and not reduced by the 2 trashed ones"))))
  (testing "Installing Parasite with only 1cr. Issue #491."
    (do-game
      (new-game {:corp {:deck [(qty "Pop-up Window" 3)]}
                 :runner {:deck ["Street Peddler" (qty "Parasite" 3)]}})
      (play-from-hand state :corp "Pop-up Window" "HQ")
      (take-credits state :corp 2)
      (starting-hand state :runner ["Street Peddler"])
      (core/lose state :runner :credit 4) ; go down to 1 credit
      (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)
            pu (get-ice state :hq 0)]
        (core/rez state :corp pu)
        (card-ability state :runner sp 0)
        (click-prompt state :runner (first (:hosted sp))) ; choose to install Parasite
        (is (= "Parasite" (:title (:card (first (get-in @state [:runner :prompt])))))
            "Parasite target prompt")
        (click-card state :runner pu)
        (is (= 4 (count (:discard (get-runner)))) "3 Parasite, 1 Street Peddler in heap")
        (is (= 1 (count (:discard (get-corp)))) "Pop-up Window in archives"))))
(testing "Tech Trader install"
  (do-game
    (new-game {:runner {:deck ["Street Peddler"
                               "Tech Trader"]}})
    (take-credits state :corp)
    (starting-hand state :runner ["Street Peddler"])
    (play-from-hand state :runner "Street Peddler")
    (let [sp (get-resource state 0)]
      (is (= 1 (count (:hosted sp))) "Street Peddler is hosting 1 card")
      (card-ability state :runner sp 0)
      (click-prompt state :runner (find-card "Tech Trader" (:hosted sp))) ; choose to install Tech Trader
      (is (= "Tech Trader" (:title (get-resource state 0)))
          "Tech Trader was installed")
      (is (= 5 (:credit (get-runner))) "Did not gain 1cr from Tech Trader ability")))))
(deftest-pending street-peddler-trash-while-choosing-card
  ;; Street Peddler - trashing Street Peddler while choosing which card to
  ;; discard should dismiss the choice prompt. Issue #587.
  (do-game
    (new-game {:runner {:deck ["Street Peddler"
                               "Gordian Blade"
                               "Torch"
                               (qty "Sure Gamble" 2)]}})
    (take-credits state :corp)
    (starting-hand state :runner ["Street Peddler" "Sure Gamble"])
    (play-from-hand state :runner "Street Peddler")
    (let [street-peddler (get-resource state 0)]
      (is (= 3 (count (:hosted street-peddler))) "Street Peddler is hosting 3 cards")
      (card-ability state :runner street-peddler 0)
      (trash-resource state "Street Peddler")
      (is (zero? (count (get-in @state [:runner :prompt])))))))
