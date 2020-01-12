(ns game.engine.abilities-test
  (:require [game.core :as core]
            [game.core.eid :as eid]
            [game.core.card :refer :all]
            [game.cards.ice :as ice]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [game.core.card-defs :refer :all]
            [jinteki.cards :refer [all-cards]]
            [clojure.test :refer :all]))

(deftest combine-abilities
  (testing "Combining 2 abilities"
    (do-game
      (new-game {:corp {:deck ["Enigma"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (let [cr (:credit (get-corp))]
        (core/resolve-ability state :corp (eid/make-eid state)
                              (core/combine-abilities (ice/gain-credits-sub 1)
                                                      (ice/gain-credits-sub 2))
                              (get-ice state :hq 0) nil)
        (is (= (+ 3 cr) (:credit (get-corp))) "Corp gained 3 credits"))))
  (testing "Combining 3 abilities"
    (do-game
      (new-game {:corp {:deck ["Enigma"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (let [cr (:credit (get-corp))]
        (core/resolve-ability state :corp (eid/make-eid state)
                              (core/combine-abilities (ice/gain-credits-sub 1)
                                                      (ice/gain-credits-sub 2)
                                                      (ice/gain-credits-sub 3))
                              (get-ice state :hq 0) nil)
        (is (= (+ 6 cr) (:credit (get-corp))) "Corp gained 6 credits"))))
  (testing "Combining trace abilities"
    (do-game
      (new-game {:corp {:deck ["Enigma"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (let [cr (:credit (get-corp))]
        (core/resolve-ability state :corp (eid/make-eid state)
                              (core/combine-abilities (ice/trace-ability 1 (ice/gain-credits-sub 1))
                                                      (ice/trace-ability 2 (ice/gain-credits-sub 2))
                                                      (ice/trace-ability 3 (ice/gain-credits-sub 3)))
                              (get-ice state :hq 0) nil)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= (+ 1 cr) (:credit (get-corp))) "Corp gained 1 credit")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= (+ 3 cr) (:credit (get-corp))) "Corp gained 2 credits")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= (+ 6 cr) (:credit (get-corp))) "Corp gained 3 credits")))))

(deftest trash-icon
  (doseq [card (->> @all-cards
                    vals
                    (filter #(re-find #"(?i)\[trash\].*:" (:text % ""))))]
    (is (core/has-trash-ability? card) (str (:title card) " needs either :cost [:trash] or :trash-icon true"))))
