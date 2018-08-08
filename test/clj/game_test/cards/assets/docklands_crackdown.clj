(ns game-test.cards.assets.docklands-crackdown
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest docklands-crackdown
  ;; Docklands Crackdown
  (letfn [(dlcd-test [number]
            (do-game
              (new-game {:corp {:deck ["Docklands Crackdown"]}
                         :runner {:deck ["Cache"]}})
              (play-from-hand state :corp "Docklands Crackdown" "New remote")
              (let [dlcd (get-content state :remote1 0)]
                (core/rez state :corp dlcd)
                (core/add-counter state :corp dlcd :power number)
                (take-credits state :corp)
                (play-from-hand state :runner "Cache")
                (is (= (- 4 number) (:credit (get-runner)))))))]
    (doall (map dlcd-test [0 1 2 3 4]))))
