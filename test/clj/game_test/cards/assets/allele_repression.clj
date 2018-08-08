(ns game-test.cards.assets.allele-repression
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest allele-repression
  ;; Allele Repression
  (do-game
    (new-game {:corp {:deck ["Allele Repression"]}})
    (play-from-hand state :corp "Allele Repression" "New remote")
    (let [ar (get-content state :remote1 0)]
      (core/advance state :corp (refresh ar))
      (core/advance state :corp (refresh ar))
      (card-ability state :corp ar 0)
      (is (= 1 (count (:discard (get-corp)))) "Allele Repression is trashed"))))
