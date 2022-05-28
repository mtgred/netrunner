(ns game.core.borealis-test
  (:require [game.core :as core]
            [game.core.eid :as eid]
            [game.core.sabotage :as s]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest sabotage
  (testing "Choosing only from HQ"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 15)]}})
      (core/resolve-ability state :runner (eid/make-eid state)
                            (s/sabotage-ability 2) (:identity (get-runner)) nil)
      (is (and (= 1 (count (:prompt (get-runner))))
               (= :waiting (prompt-type :runner))) "Runner is waiting on Corp")
      (is (= 0 (count (:disard (get-corp)))) "Archives is empty")
      (let [prev-cards-in-rd (-> (get-corp) :deck count)
            prev-cards-in-hq (-> (get-corp) :hand count)]
        (click-card state :corp (first (:hand (get-corp))))
        (click-card state :corp (second (:hand (get-corp))))
        (is (= prev-cards-in-rd
               (-> (get-corp) :deck count))
            "No cards from R&D trashed")
        (is (= (- prev-cards-in-hq 2)
               (-> (get-corp) :hand count))
            "2 cards from HQ trashed"))
      (is (empty (:prompt (get-corp))) "no more Corp prompt")
      (is (empty (:prompt (get-runner))) "no more Runner waiting prompt")
      (is (= 2 (count (:discard (get-corp)))) "Archives has 2 cards")
      (println (prompt-fmt :corp))
      (println (clojure.string/join "\n" (map :text (:log @state))))
      (is false "boop")
      )))
