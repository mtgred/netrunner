(ns game.core.abilities-test
  (:require
   [clojure.test :refer :all]
   [game.cards.ice :as ice]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.card-defs :refer :all]
   [game.core.eid :as eid]
   [game.test-framework :refer :all]
   [jinteki.cards :refer [all-cards]]
   [jinteki.utils :refer [add-cost-to-label]]))

(deftest combine-abilities
  (testing "Combining 2 abilities"
    (do-game
      (new-game {:corp {:deck ["Enigma"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (rez state :corp (get-ice state :hq 0))
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
      (rez state :corp (get-ice state :hq 0))
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
      (rez state :corp (get-ice state :hq 0))
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
  (doseq [card (->> (vals @all-cards)
                    (filter #(re-find #"(?i)\[trash\].*:" (:text % ""))))
          :when (not-empty (card-def card))]
    (is (core/has-trash-ability? card) (str (:title card) " needs either :cost [:trash-can] or :trash-icon true"))))

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

(defn generate-label
  [card]
  (add-cost-to-label (first (:abilities card))))

(deftest cost-label
  (testing "Conditional costs"
    (do-game
      (new-game {:runner {:hand ["Simulchip" "Gorman Drip v1"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Simulchip")
      (is (= "[trash], trash 1 installed program: Install a program from the heap"
             (generate-label (get-hardware state 0))))
      (play-from-hand state :runner "Gorman Drip v1")
      (card-ability state :runner (get-program state 0) 0)
      (is (= "[trash]: Install a program from the heap"
             (generate-label (get-hardware state 0))))))
  (testing "trash icon"
    (do-game
      (new-game {:runner {:hand ["Bankroll"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Bankroll")
      (is (= "[trash]: Take all hosted credits" (generate-label (get-program state 0))))))
  (testing "x credits"
    (do-game
      (new-game {:runner {:hand ["Misdirection"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Misdirection")
      (is (= "[Click][Click], X [Credits]: Remove X tags" (generate-label (get-program state 0))))))
  (testing "x power counters"
    (do-game
      (new-game {:corp {:hand ["Lakshmi Smartfabrics"]}})
      (play-from-hand state :corp "Lakshmi Smartfabrics" "New remote")
      (let [lak (get-content state :remote1 0)]
        (rez state :corp lak)
        (is (= "X hosted power counters: Reveal an agenda worth X points from HQ"
               (generate-label (refresh lak))))))))
