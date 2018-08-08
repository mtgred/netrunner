(ns game-test.cards.assets.echo-chamber
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest echo-chamber
  ;; Echo Chamber - 3 clicks to become 1 point agenda
  (do-game
    (new-game {:corp {:deck ["Echo Chamber"]}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Echo Chamber" "New remote")
    (let [ec (get-content state :remote1 0)]
      (core/rez state :corp ec)
      (card-ability state :corp ec 0))
    (is (= 1 (:agendapoints (get-scored state :corp 0))) "Echo Chamber added to Corp score area")))
