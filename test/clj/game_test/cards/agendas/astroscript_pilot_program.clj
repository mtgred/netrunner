(ns game-test.cards.agendas.astroscript-pilot-program
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest astroscript-pilot-program
  ;; AstroScript token placement
  (do-game
    (new-game {:corp {:deck [(qty "AstroScript Pilot Program" 3) (qty "Ice Wall" 2)]}})
    (core/gain state :corp :click 3)
    (letfn [(try-place [from to]
              (card-ability state :corp (refresh from) 0)
              (click-card state :corp (refresh to)))
            (should-not-place [from to msg]
              (try-place from to)
              (click-prompt state :corp "Done")
              (is (= 1 (get-counters (refresh from) :agenda))
                  (str (:title from)" token was not used on " (:title to) msg))
              (is (zero? (get-counters (refresh to) :advancement))
                  (str "Advancement token not placed on " (:title to) msg)))
            (should-place [from to msg]
              (try-place from to)
              (is (zero? (get-counters (refresh from) :agenda))
                  (str (:title from) " token was used on " (:title to) msg))
              (is (= 1 (get-counters (refresh to) :advancement))
                  (str "Advancement token placed on " (:title to) msg)))]
      (play-and-score state "AstroScript Pilot Program")
      (play-from-hand state :corp "AstroScript Pilot Program" "New remote")
      (let [scored-astro (get-scored state :corp 0)
            installed-astro (get-content state :remote2 0)
            hand-astro (find-card "AstroScript Pilot Program" (:hand (get-corp)))]
        (should-not-place scored-astro hand-astro " in hand")
        (should-place scored-astro installed-astro " that is installed")
        (advance state installed-astro 2)
        (core/score state :corp {:card (refresh installed-astro)}))
      (play-from-hand state :corp "Ice Wall" "HQ")
      (let [no-token-astro (get-scored state :corp 0)
            token-astro (get-scored state :corp 1)
            hand-ice-wall (find-card "Ice Wall" (:hand (get-corp)))
            installed-ice-wall (get-ice state :hq 0)]
        (should-not-place token-astro no-token-astro " that is scored")
        (should-not-place token-astro hand-ice-wall " in hand")
        (should-place token-astro installed-ice-wall " that is installed")))))
