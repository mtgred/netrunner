(ns game-test.cards.assets.api-s-keeper-isobel
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest api-s-keeper-isobel
  ;; API-S Keeper Isobel
  (do-game
    (new-game {:corp {:deck ["API-S Keeper Isobel" "Ice Wall"]}})
    (play-from-hand state :corp "API-S Keeper Isobel" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [ap (get-content state :remote1 0)
          iw (get-ice state :hq 0)]
      (core/rez state :corp (refresh ap))
      (core/rez state :corp (refresh iw))
      (core/advance state :corp {:card (refresh iw)})
      (is (= 1 (get-counters (refresh iw) :advancement)) "Ice Wall has 1 advancement token")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 1 (:credit (get-corp))) "Corp starts with 1 credits")
      (card-ability state :corp (refresh ap) 0)
      (click-card state :corp (refresh iw))
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall loses an advancement token")
      (is (= 4 (:credit (get-corp))) "Corp gains 3 credits"))))
