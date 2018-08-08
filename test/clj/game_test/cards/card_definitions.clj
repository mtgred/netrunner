(ns game-test.cards.card-definitions
  (:require [game.core :as core]
            [game.utils :as utils]
            [jinteki.cards :refer [all-cards card-definitions]]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest do-game-test
  (do-game
    (new-game)
    (play-from-hand state :corp "Hedge Fund")
    (is (= 9 (:credit (get-corp))))))

(deftest virus-counter-flags
  (testing "Set counter flag when virus card enters play with counters"
    (do-game
      (new-game {:runner {:deck ["Surge" "Imp" "Crypsis"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (let [imp (get-program state 0)]
        (is (get-in imp [:added-virus-counter]) "Counter flag was set on Imp"))))
  (testing "Set counter flag when add-prop is called on a virus"
    (do-game
      (new-game {:runner {:deck ["Crypsis"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Crypsis")
      (let [crypsis (get-program state 0)]
        (card-ability state :runner crypsis 2) ;click to add a virus counter
        (is (= 1 (get-counters (refresh crypsis) :virus)) "Crypsis added a virus token")
        (is (get-in (refresh crypsis) [:added-virus-counter])
            "Counter flag was set on Crypsis"))))
  (testing "Clear the virus counter flag at the end of each turn"
    (do-game
      (new-game {:runner {:deck ["Crypsis"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Crypsis")
      (let [crypsis (get-program state 0)]
        (card-ability state :runner crypsis 2) ; click to add a virus counter
        (take-credits state :runner 2)
        (take-credits state :corp 1)
        (is (not (get-in (refresh crypsis) [:added-virus-counter]))
            "Counter flag was cleared on Crypsis")))))

(deftest end-the-run-test
  ;; Since all ETR ice share a common ability, we only need one test
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3) (qty "Hedge Fund" 3) (qty "Restructure" 2)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp 2)
    (run-on state "HQ")
    (is (= [:hq] (get-in @state [:run :server])))
    (let [iwall (get-ice state :hq 0)]
      (core/rez state :corp iwall)
      (card-subroutine state :corp iwall 0)
      (is (not (:run @state)) "Run is ended")
      (is (get-in @state [:runner :register :unsuccessful-run]) "Run was unsuccessful"))))

(deftest auto-pump-breakers
  ;; Breaker get a dynamic ability that matches the strength of the encountered ice
  (testing "Single pump"
    (do-game
      (new-game {:corp {:deck ["Masvingo"]}
                 :runner {:deck ["Laamb"]}})
      (play-from-hand state :corp "Masvingo" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Laamb")
      (run-on state "HQ")
      (let [laamb (get-program state 0)]
        (is (= 2 (:current-strength (refresh laamb))) "Laamb starts at 2 strength")
        (is (= 6 (:credit (get-runner))) "Spent 4 to install")
        (core/play-dynamic-ability state :runner {:dynamic "auto-pump" :card (refresh laamb)})
        (is (= 8 (:current-strength (refresh laamb))) "Laamb is at 8 strength")
        (is (= 3 (:credit (get-runner))) "Spent 3 to pump"))))
  (testing "Multi pump"
    (do-game
      (new-game {:corp {:deck ["Masvingo"]}
                 :runner {:deck ["Ankusa"]}})
      (play-from-hand state :corp "Masvingo" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Ankusa")
      (run-on state "HQ")
      (let [ank (get-program state 0)]
        (is (zero? (:current-strength (refresh ank))) "Ankusa starts at 1 strength")
        (is (= 4 (:credit (get-runner))) "Spent 6 to install")
        (core/play-dynamic-ability state :runner {:dynamic "auto-pump" :card (refresh ank)})
        (is (= 3 (:current-strength (refresh ank))) "Ankusa is at 3 strength")
        (is (= 1 (:credit (get-runner))) "Spent 3 to pump")))))

; (defn make-label
    ;   [ab]
    ;   (seq (utils/make-label ab)))

; (deftest displayed-abilities-require-lables
    ;   (doseq [[title card] (sort-by #(type->dir (val %)) @all-cards)
                ;           :let [card (@card-definitions title)
                                  ;                 abilities (:abilities card)
                                  ;                 subroutines (:subroutines card)
                                  ;                 runner-abilities (:runner-abilities card)
                                  ;                 corp-abilities (:corp-abilities card)]]
          ;     (when abilities
                  ;       (doseq [[idx ab] (map-indexed vector abilities)]
                            ;         (is (make-label ab) (str title " ability " idx " needs a label"))))
          ;     (when subroutines
                  ;       (doseq [[idx sub] (map-indexed vector subroutines)]
                            ;         (is (make-label sub) (str title " subroutine " idx " needs a label"))))
          ;     (when runner-abilities
                  ;       (doseq [[idx ab] (map-indexed vector runner-abilities)]
                            ;         (is (make-label ab) (str title " runner-ability " idx " needs a label"))))
          ;     (when corp-abilities
                  ;       (doseq [[idx ab] (map-indexed vector corp-abilities)]
                            ;         (is (make-label ab) (str title " corp-ability " idx " needs a label"))))))
