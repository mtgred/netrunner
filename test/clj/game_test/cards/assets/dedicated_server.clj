(ns game-test.cards.assets.dedicated-server
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest dedicated-server
  ;; Dedicated Servers
  (do-game
    (new-game {:corp {:deck ["Dedicated Server"]}})
    (play-from-hand state :corp "Dedicated Server" "New remote")
    (let [servers (get-content state :remote1 0)]
      (core/rez state :corp servers)
      (is (= 2 (get-counters (refresh servers) :recurring)) "Should have 2 recurring credits"))))
