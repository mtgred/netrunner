(ns game-test.cards.resources.gene-conditioning-shoppe
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest gene-conditioning-shoppe
  ;; Gene Conditioning Shoppe - set :genetics-trigger-twice flag
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)]}
                 :runner {:deck ["Gene Conditioning Shoppe"
                                 "Adjusted Chronotype"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Adjusted Chronotype")
      (is (not (core/has-flag? state :runner :persistent :genetics-trigger-twice)))
      (play-from-hand state :runner "Gene Conditioning Shoppe")
      (is (core/has-flag? state :runner :persistent :genetics-trigger-twice))
      (core/trash state :runner (get-resource state 1))
      (is (not (core/has-flag? state :runner :persistent :genetics-trigger-twice)))))
  (testing "set :genetics-trigger-twice flag - ensure redundant copies work"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Gene Conditioning Shoppe" 2)
                                 "Adjusted Chronotype"]}})
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Adjusted Chronotype")
      (let [adjusted-chronotype (get-resource state 0)]
        (is (not (core/has-flag? state :runner :persistent :genetics-trigger-twice)))
        (play-from-hand state :runner "Gene Conditioning Shoppe")
        (play-from-hand state :runner "Gene Conditioning Shoppe")
        (let [gcs1 (get-resource state 1)
              gcs2 (get-resource state 2)]
          (is (core/has-flag? state :runner :persistent :genetics-trigger-twice))
          (core/trash state :runner gcs1)
          (is (core/has-flag? state :runner :persistent :genetics-trigger-twice))
          (core/trash state :runner gcs2)
          (is (not (core/has-flag? state :runner :persistent :genetics-trigger-twice))))))))
