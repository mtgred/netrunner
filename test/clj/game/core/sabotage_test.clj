(ns game.core.sabotage-test
  (:require [game.core :as core]
            [game.core.eid :as eid]
            [game.core.sabotage :as s]
            [game.core.mark :as m]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest sabotage-test
  (testing "Choosing only from HQ"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 15)]}})
      (core/resolve-ability state :runner (eid/make-eid state)
                            (s/sabotage-ability 3) (:identity (get-runner)) nil)
      (is (and (= 1 (count (:prompt (get-runner))))
               (= :waiting (prompt-type :runner))) "Runner is waiting on Corp")
      (is (= 0 (count (:disard (get-corp)))) "Archives is empty")
      (let [prev-cards-in-rd (-> (get-corp) :deck count)
            prev-cards-in-hq (-> (get-corp) :hand count)]
        (click-card state :corp (nth (:hand (get-corp)) 0))
        (click-card state :corp (nth (:hand (get-corp)) 1))
        (click-card state :corp (nth (:hand (get-corp)) 2))
        (is (= prev-cards-in-rd
               (-> (get-corp) :deck count))
            "No cards from R&D trashed")
        (is (= (- prev-cards-in-hq 3)
               (-> (get-corp) :hand count))
            "3 cards from HQ trashed"))
      (is (empty (:prompt (get-corp))) "no more Corp prompt")
      (is (empty (:prompt (get-runner))) "no more Runner waiting prompt")
      (is (= 3 (count (:discard (get-corp)))) "Archives has 3 cards")))
  (testing "Choosing only from R&D"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 15)]}})
      (core/resolve-ability state :runner (eid/make-eid state)
                            (s/sabotage-ability 3) (:identity (get-runner)) nil)
      (is (and (= 1 (count (:prompt (get-runner))))
               (= :waiting (prompt-type :runner))) "Runner is waiting on Corp")
      (is (= 0 (count (:disard (get-corp)))) "Archives is empty")
      (let [prev-cards-in-rd (-> (get-corp) :deck count)
            prev-cards-in-hq (-> (get-corp) :hand count)]
        (click-prompt state :corp "Done")
        (is (= (- prev-cards-in-rd 3)
               (-> (get-corp) :deck count))
            "3 cards from R&D trashed")
        (is (= prev-cards-in-hq
               (-> (get-corp) :hand count))
            "No cards from HQ trashed"))
      (is (empty (:prompt (get-corp))) "no more Corp prompt")
      (is (empty (:prompt (get-runner))) "no more Runner waiting prompt")
      (is (= 3 (count (:discard (get-corp)))) "Archives has 3 cards")))
  (testing "Choosing a mix from HQ and R&D"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 15)]}})
      (core/resolve-ability state :runner (eid/make-eid state)
                            (s/sabotage-ability 3) (:identity (get-runner)) nil)
      (is (and (= 1 (count (:prompt (get-runner))))
               (= :waiting (prompt-type :runner))) "Runner is waiting on Corp")
      (is (= 0 (count (:disard (get-corp)))) "Archives is empty")
      (let [prev-cards-in-rd (-> (get-corp) :deck count)
            prev-cards-in-hq (-> (get-corp) :hand count)]
        (click-card state :corp (nth (:hand (get-corp)) 0))
        (click-card state :corp (nth (:hand (get-corp)) 1))
        (click-prompt state :corp "Done")
        (is (= (- prev-cards-in-rd 1)
               (-> (get-corp) :deck count))
            "1 card from R&D trashed")
        (is (= (- prev-cards-in-hq 2)
               (-> (get-corp) :hand count))
            "2 cards from HQ trashed"))
      (is (empty (:prompt (get-corp))) "no more Corp prompt")
      (is (empty (:prompt (get-runner))) "no more Runner waiting prompt")
      (is (= 3 (count (:discard (get-corp)))) "Archives has 3 cards")))
  (testing "Forced to trash some cards from HQ"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 7)]}})
      (core/resolve-ability state :runner (eid/make-eid state)
                            (s/sabotage-ability 3) (:identity (get-runner)) nil)
      (is (and (= 1 (count (:prompt (get-runner))))
               (= :waiting (prompt-type :runner))) "Runner is waiting on Corp")
      (is (= 0 (count (:disard (get-corp)))) "Archives is empty")
      (is (= "Choose at least 2 cards and up to 3 cards to trash from HQ. Remainder will be trashed from top of R&D." (:msg (prompt-map :corp))))
      (is (= "Done" (-> (prompt-map :corp) :choices first :value)) "Got Done choice in prompt")
      (is (= 0 (count (-> @state :corp :toast))) "Got no toasts")
      (click-prompt state :corp "Done")
      (is (= 1 (count (-> @state :corp :toast))) "Got a message to select more cards")
      (is (= :select (prompt-type :corp)) "Corp still needs to select cards")
      (click-card state :corp (nth (:hand (get-corp)) 0))
      (click-prompt state :corp "Done")
      (is (= 2 (count (-> @state :corp :toast))) "Got a message to select more cards")
      (is (= :select (prompt-type :corp)) "Corp still needs to select cards")
      (click-card state :corp (nth (:hand (get-corp)) 1))
      (click-prompt state :corp "Done")
      (is (= 2 (count (-> @state :corp :toast))) "Got no further message to select more cards")
      (is (empty (:prompt (get-corp))) "no Corp prompt")
      (is (empty (:prompt (get-runner))) "no Runner waiting prompt")))
  (testing "Forced to trash entire HQ and R&D"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 7)]}})
      (core/resolve-ability state :runner (eid/make-eid state)
                            (s/sabotage-ability 100) (:identity (get-runner)) nil)
      (is (empty (:prompt (get-corp))) "no Corp prompt")
      (is (empty (:prompt (get-runner))) "no Runner waiting prompt")
      (is (= 0 (count (:hand (get-corp)))) "HQ is empty")
      (is (= 0 (count (:deck (get-corp)))) "R&D is empty")
      (is (= 7 (count (:discard (get-corp)))) "Archives has 7 cards"))))
