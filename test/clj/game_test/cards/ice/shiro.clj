(ns game-test.cards.ice.shiro
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest shiro
  ;; Shiro
  (testing "Full test"
    (do-game
      (new-game {:corp {:deck ["Shiro" "Caprice Nisei"
                               "Quandary" "Jackson Howard"]}
                 :runner {:deck ["R&D Interface"]}})
      (starting-hand state :corp ["Shiro"])
      (play-from-hand state :corp "Shiro" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "R&D Interface")
      (let [shiro (get-ice state :hq 0)]
        (run-on state :hq)
        (core/rez state :corp shiro)
        (card-subroutine state :corp shiro 0)
        (click-prompt state :corp (find-card "Caprice Nisei" (:deck (get-corp))))
        (click-prompt state :corp (find-card "Quandary" (:deck (get-corp))))
        (click-prompt state :corp (find-card "Jackson Howard" (:deck (get-corp))))
        ;; try starting over
        (click-prompt state :corp "Start over")
        (click-prompt state :corp (find-card "Jackson Howard" (:deck (get-corp))))
        (click-prompt state :corp (find-card "Quandary" (:deck (get-corp))))
        (click-prompt state :corp (find-card "Caprice Nisei" (:deck (get-corp)))) ;this is the top card of R&D
        (click-prompt state :corp "Done")
        (is (= "Caprice Nisei" (:title (first (:deck (get-corp))))))
        (is (= "Quandary" (:title (second (:deck (get-corp))))))
        (is (= "Jackson Howard" (:title (second (rest (:deck (get-corp)))))))
        (card-subroutine state :corp shiro 1)
        (click-prompt state :runner "Card from deck")
        (is (= (:cid (first (:deck (get-corp))))
               (:cid (:card (first (:prompt (get-runner)))))) "Access the top card of R&D")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Card from deck")
        (is (= (:cid (second (:deck (get-corp))))
               (:cid (:card (first (:prompt (get-runner)))))) "Access another card due to R&D Interface"))))
  (testing "with Mwanza City Grid, should access additional 3 cards"
    (do-game
      (new-game {:corp {:deck ["Shiro" "Mwanza City Grid"
                               (qty "Ice Wall" 10)]}
                 :runner {:deck ["R&D Interface"]}})
      (starting-hand state :corp ["Shiro" "Mwanza City Grid"])
      (play-from-hand state :corp "Mwanza City Grid" "R&D")
      (play-from-hand state :corp "Shiro" "R&D")
      (take-credits state :corp)
      (core/gain state :corp :credit 100)
      (play-from-hand state :runner "R&D Interface")
      (let [shiro (get-ice state :rd 0)
            mwanza (get-content state :rd 0)]
        (run-on state :rd)
        (core/rez state :corp shiro)
        (core/rez state :corp mwanza)
        (let [credits (:credit (get-corp))]
          (card-subroutine state :corp shiro 1)
          (is (= 3 (-> @state :run :access-bonus)) "Should access an additional 3 cards")
          (dotimes [_ 5]
            (click-prompt state :runner "Card from deck")
            (click-prompt state :runner "No action"))
          (run-jack-out state)
          (is (= (+ credits 10) (:credit (get-corp))) "Corp should only gain money once"))))))
