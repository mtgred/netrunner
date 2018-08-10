(ns game-test.cards.agendas.genetic-resequencing
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest genetic-resequencing
  ;; Genetic Resequencing
  (do-game
    (new-game {:corp {:deck ["Genetic Resequencing" (qty "Braintrust" 2)]}})
    (play-from-hand state :corp "Braintrust" "New remote")
    (play-from-hand state :corp "Braintrust" "New remote")
    (play-from-hand state :corp "Genetic Resequencing" "New remote")
    (let [bt1 (get-content state :remote1 0)
          bt2 (get-content state :remote2 0)
          gr (get-content state :remote3 0)]
      (score-agenda state :corp bt1)
      (let [btscored (get-scored state :corp 0)]
        (is (zero? (get-counters (refresh btscored) :agenda)) "No agenda counters on scored Braintrust")
        (score-agenda state :corp gr)
        (click-card state :corp bt2)
        (is (zero? (get-counters (refresh bt2) :agenda))
            "No agenda counters on installed Braintrust; not a valid target")
        (click-card state :corp btscored)
        (is (= 1 (get-counters (refresh btscored) :agenda))
            "1 agenda counter placed on scored Braintrust")))))
