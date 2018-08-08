(ns game-test.cards.agendas.ancestral-imager
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ancestral-imager
  ;; Ancestral Imager
  (do-game
    (new-game {:corp {:deck [(qty "Ancestral Imager" 3)]}})
    (play-and-score state "Ancestral Imager")
    (take-credits state :corp)
    (let [grip (count (:hand (get-runner)))]
      (is (= grip (count (:hand (get-runner)))) (str "Runner has " grip " cards in hand"))
      (run-on state :hq)
      (run-jack-out state)
      (is (= (dec grip) (count (:hand (get-runner)))) "Runner took 1 net damage"))))
