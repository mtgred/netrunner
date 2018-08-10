(ns game-test.cards.assets.alexa-belsky
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest alexa-belsky
  ;; Alexa Belsky
  (do-game
    (new-game {:corp {:deck ["Alexa Belsky" "Hedge Fund" "Breaking News"
                             "Gutenberg" "Product Placement" "Jackson Howard"]}})
    (play-from-hand state :corp "Alexa Belsky" "New remote")
    (let [alexa (get-content state :remote1 0)]
      (core/rez state :corp alexa)
      (card-ability state :corp alexa 0)
      (is (= 1 (count (:discard (get-corp)))) "Alexa Belsky trashed")
      (is (= 5 (count (:hand (get-corp)))))
      (is (zero? (count (:deck (get-corp)))))
      (click-prompt state :runner "5") ;Runner chooses to pay 5 credits so 2 cards are prevented from being shuffled
      (is (= 2 (count (:hand (get-corp)))))
      (is (= 3 (count (:deck (get-corp)))))
      (is (zero? (:credit (get-runner)))))))
