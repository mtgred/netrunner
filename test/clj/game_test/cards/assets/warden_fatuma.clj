(ns game-test.cards.assets.warden-fatuma
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest warden-fatuma
  ;; Warden Fatuma - rezzed bioroid ice gains an additional sub
  (do-game
    (new-game {:corp {:deck ["Warden Fatuma" "Kakugo"
                             "Eli 2.0" "Ichi 2.0"]}})
    (core/gain state :corp :credit 20 :click 5)
    (play-from-hand state :corp "Kakugo" "Archives")
    (play-from-hand state :corp "Eli 2.0" "HQ")
    (play-from-hand state :corp "Ichi 2.0" "R&D")
    (play-from-hand state :corp "Warden Fatuma" "New remote")
    (let [wf (get-content state :remote1 0)
          kak (get-ice state :archives 0)
          eli (get-ice state :hq 0)
          ichi (get-ice state :rd 0)]
      (core/rez state :corp kak)
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo starts with 1 sub")
      (core/rez state :corp eli)
      (is (= 2 (count (:subroutines (refresh eli)))) "Eli 2.0 starts with 2 subs")
      (is (zero? (count (:subroutines (refresh ichi)))) "Unrezzed Ichi 2.0 starts with 0 subs")
      (core/rez state :corp wf)
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo stays at 1 sub")
      (is (= 3 (count (:subroutines (refresh eli)))) "Eli 2.0 gains 1 sub")
      (is (zero? (count (:subroutines (refresh ichi)))) "Unrezzed Ichi 2.0 stays at 0 subs")
      (core/rez state :corp ichi)
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo stays at 1 sub")
      (is (= 3 (count (:subroutines (refresh eli)))) "Eli 2.0 stays at 1 sub")
      (is (= 3 (count (:subroutines (refresh ichi)))) "Ichi 2.0 rezzes with 3 subs")
      (core/derez state :corp (refresh wf))
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo stays at 1 sub")
      (is (= 2 (count (:subroutines (refresh eli)))) "Eli 2.0 reverts")
      (is (= 2 (count (:subroutines (refresh ichi)))) "Ichi 2.0 reverts"))))
