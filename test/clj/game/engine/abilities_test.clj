(ns game.engine.abilities-test
  (:require [game.core :as core]
            [game.core.eid :as eid]
            [game.core.card :refer :all]
            [game.core.card-defs :refer [card-def]]
            [game.cards.ice :as ice]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [game.core.card-defs :refer :all]
            [jinteki.cards :refer [all-cards]]
            [jinteki.utils :refer [add-cost-to-label]]
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

(deftest label
  (doseq [[title abilities]
          (->> @all-cards
               vals
               (sort-by (juxt :type :title))
               (map (juxt :title card-def))
               (filter (comp :abilities second)))
          [idx ability] (map-indexed (fn [idx itm] [idx itm]) (:abilities abilities))]
    (is (string? (or (:label ability) (:msg ability)))
        (str title ": Ability " (inc idx) " doesn't have an appropriate label"))))

(deftest cost-label
  (testing "Conditional costs"
    (do-game
      (new-game {:runner {:hand ["Simulchip" "Gorman Drip v1"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Simulchip")
      ;; Needs to be called manually because it's only run in main.clj and we don't interact
      ;; with that in tests
      (core/update-all-card-labels state)
      (is (= "[trash], trash 1 installed program: Install a program from the heap"
             (add-cost-to-label (first (:abilities (get-hardware state 0))))))
      (play-from-hand state :runner "Gorman Drip v1")
      (card-ability state :runner (get-program state 0) 0)
      (core/update-all-card-labels state)
      (is (= "[trash]: Install a program from the heap"
             (add-cost-to-label (first (:abilities (get-hardware state 0))))))))
  (testing "trash icon"
    (do-game
      (new-game {:runner {:hand ["Recon Drone"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Recon Drone")
      (core/update-all-card-labels state)
      (is (= "[trash]: Prevent damage"
             (add-cost-to-label (first (:abilities (get-hardware state 0)))))))))
