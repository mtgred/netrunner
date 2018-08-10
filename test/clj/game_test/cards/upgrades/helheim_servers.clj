(ns game-test.cards.upgrades.helheim-servers
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest helheim-servers
  ;; Helheim Servers - Full test
  (do-game
    (new-game {:corp {:deck ["Helheim Servers" "Gutenberg" "Vanilla"
                             "Jackson Howard" "Hedge Fund"]}})
    (play-from-hand state :corp "Helheim Servers" "R&D")
    (play-from-hand state :corp "Gutenberg" "R&D")
    (play-from-hand state :corp "Vanilla" "R&D")
    (take-credits state :corp)
    (run-on state "R&D")
    (is (:run @state))
    (let [helheim (get-content state :rd 0)
          gutenberg (get-ice state :rd 0)
          vanilla (get-ice state :rd 1)]
      (core/rez state :corp helheim)
      (core/rez state :corp gutenberg)
      (core/rez state :corp vanilla)
      (is (= 6 (:current-strength (refresh gutenberg))))
      (is (zero? (:current-strength (refresh vanilla))))
      (card-ability state :corp helheim 0)
      (click-card state :corp (find-card "Jackson Howard" (:hand (get-corp))))
      (is (= 1 (count (:discard (get-corp)))))
      (is (= 8 (:current-strength (refresh gutenberg))))
      (is (= 2 (:current-strength (refresh vanilla))))
      (card-ability state :corp helheim 0)
      (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp))))
      (is (= 2 (count (:discard (get-corp)))))
      (is (= 10 (:current-strength (refresh gutenberg))))
      (is (= 4 (:current-strength (refresh vanilla))))
      (run-jack-out state)
      (is (not (:run @state)))
      (is (= 6 (:current-strength (refresh gutenberg))))
      (is (zero? (:current-strength (refresh vanilla)))))))
