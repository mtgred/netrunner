(ns game-test.cards.upgrades.surat-city-grid
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest surat-city-grid
  ;; Surat City Grid - Trigger on rez of a card in/protecting same server to rez another card at 2c discount
  (do-game
    (new-game {:corp {:deck [(qty "Surat City Grid" 2) (qty "Cyberdex Virus Suite" 2)
                             "Enigma" "Wraparound"]}})
    (core/gain state :corp :credit 15 :click 8)
    (play-from-hand state :corp "Surat City Grid" "New remote")
    (play-from-hand state :corp "Wraparound" "Server 1")
    (play-from-hand state :corp "Cyberdex Virus Suite" "Server 1")
    (let [scg1 (get-content state :remote1 0)
          cvs1 (get-content state :remote1 1)
          wrap (get-ice state :remote1 0)]
      (core/rez state :corp scg1)
      (core/rez state :corp cvs1)
      (is (= 15 (:credit (get-corp))))
      (is (= (:cid scg1) (-> (get-corp) :prompt first :card :cid)) "Surat City Grid triggered from upgrade in same remote")
      (click-prompt state :corp "Yes")
      (click-card state :corp wrap)
      (is (:rezzed (refresh wrap)) "Wraparound is rezzed")
      (is (= 15 (:credit (get-corp))) "Wraparound rezzed for free with 2c discount from SCG")
      (play-from-hand state :corp "Surat City Grid" "HQ")
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Cyberdex Virus Suite" "HQ")
      (let [scg2 (get-content state :hq 0)
            cvs2 (get-content state :hq 1)
            enig (get-ice state :hq 0)]
        (core/rez state :corp scg2)
        (core/rez state :corp cvs2)
        (is (empty? (:prompt (get-corp))) "SCG didn't trigger, upgrades in root of same central aren't considered in server")
        (core/derez state :corp (refresh wrap))
        (core/rez state :corp enig)
        (is (= (:cid scg2) (-> (get-corp) :prompt first :card :cid)) "SCG did trigger for ICE protecting HQ")))))
