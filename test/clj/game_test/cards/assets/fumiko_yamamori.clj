(ns game-test.cards.assets.fumiko-yamamori
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest fumiko-yamamori
  ;; Fumiko Yamamori
  (do-game
    (new-game {:corp {:deck ["Fumiko Yamamori"]}})
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "Fumiko Yamamori" "New remote")
    (let [fumiko (get-content state :remote1 0)]
      (core/rez state :corp (refresh fumiko))
      (core/psi-game state :corp (refresh fumiko)
                     {:equal  {:msg "resolve equal bets effect"}
                      :not-equal {:msg "resolve unequal bets effect"}})
      (click-prompt state :corp "2 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 1 (-> (get-runner) :discard count)) "Runner should discard a card to meat damage"))))
