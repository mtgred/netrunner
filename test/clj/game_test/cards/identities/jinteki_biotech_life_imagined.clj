(ns game-test.cards.identities.jinteki-biotech-life-imagined
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest jinteki-biotech-life-imagined
  ;; Jinteki Biotech
  (testing "Brewery net damage"
    (do-game
      (new-game {:corp {:id "Jinteki Biotech: Life Imagined"
                        :deck ["Braintrust"]}
                 :options {:dont-start-turn true}})
      (click-prompt state :corp "The Brewery")
      (core/start-turn state :corp nil)
      (card-ability state :corp (:identity (get-corp)) 1)
      (is (= 1 (count (:hand (get-runner)))) "Runner took 2 net damage from Brewery flip")))
  (testing "Greenhouse four advancement tokens"
    (do-game
      (new-game {:corp {:id "Jinteki Biotech: Life Imagined"
                        :deck ["Braintrust"]}
                 :options {:dont-start-turn true}})
      (click-prompt state :corp "The Greenhouse")
      (core/start-turn state :corp nil)
      (play-from-hand state :corp "Braintrust" "New remote")
      (take-credits state :corp)
      (take-credits state :runner)
      (let [bt (get-content state :remote1 0)]
        (is (zero? (get-counters (refresh bt) :advancement)) "No advancement counters on agenda")
        (card-ability state :corp (:identity (get-corp)) 1)
        (click-card state :corp (refresh bt))
        (is (= 4 (get-counters (refresh bt) :advancement)) "Four advancement counters on agenda"))))
  (testing "Tank shuffle Archives into R&D"
    (do-game
      (new-game {:corp {:id "Jinteki Biotech: Life Imagined"
                        :deck [(qty "Hedge Fund" 3)]}
                 :options {:dont-start-turn true}})
      (click-prompt state :corp "The Tank")
      (core/start-turn state :corp nil)
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :runner)
      (is (= 3 (count (:discard (get-corp)))) "Archives started with 3 cards")
      (is (zero? (count (:deck (get-corp)))) "R&D started empty")
      (card-ability state :corp (:identity (get-corp)) 1)
      (is (zero? (count (:discard (get-corp)))) "Archives ended empty")
      (is (= 3 (count (:deck (get-corp)))) "R&D ended with 3 cards"))))
