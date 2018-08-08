(ns game-test.cards.icebreakers.ika
  (:require [game.core :as core]
            [game.utils :as utils]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ika
  ;; Ika
  (testing "Can be hosted on both rezzed/unrezzed ice, respects no-host, is blanked by Magnet"
    (do-game
      (new-game {:corp {:deck ["Tithonium" "Enigma" "Magnet"]}
                 :runner {:deck ["Ika"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Tithonium" "Archives")
      (play-from-hand state :corp "Magnet" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Ika")
      (core/gain state :runner :credit 100)
      (core/gain state :corp :credit 100)
      (let [ika (get-program state 0)
            enigma (get-ice state :hq 0)
            tithonium (get-ice state :archives 0)
            magnet (get-ice state :rd 0)]
        (let [creds (:credit (get-runner))]
          (card-ability state :runner ika 2) ; host on a piece of ice
          (click-card state :runner tithonium)
          (is (utils/same-card? ika (first (:hosted (refresh tithonium)))) "Ika was rehosted")
          (is (= (- creds 2) (:credit (get-runner))) "Rehosting from rig cost 2 creds"))
        (run-on state :archives)
        (let [creds (:credit (get-runner))
              ika (first (:hosted (refresh tithonium)))]
          (card-ability state :runner ika 2)
          (click-card state :runner enigma)
          (is (utils/same-card? ika (first (:hosted (refresh enigma)))) "Ika was rehosted")
          (is (= (- creds 2) (:credit (get-runner))) "Rehosting from ice during run cost 2 creds"))
        (core/rez state :corp tithonium)
        (let [creds (:credit (get-runner))
              ika (first (:hosted (refresh enigma)))]
          (card-ability state :runner ika 2)
          (click-card state :runner tithonium)
          (is (= 0 (count (:hosted (refresh tithonium)))) "Ika was not hosted on Tithonium")
          (is (= creds (:credit (get-runner))) "Clicking invalid targets is free")
          (click-prompt state :runner "Done")
          (core/rez state :corp magnet)
          (click-card state :corp ika)
          (is (= 0 (count (:hosted (refresh enigma)))) "Ika was removed from Enigma")
          (is (= 1 (count (:hosted (refresh magnet)))) "Ika was hosted onto Magnet")
          (let [ika (first (:hosted (refresh magnet)))]
            (is (= 0 (count (:abilities ika))) "Ika was blanked")))))))
