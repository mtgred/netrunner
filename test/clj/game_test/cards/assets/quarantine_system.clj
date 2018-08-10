(ns game-test.cards.assets.quarantine-system
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest quarantine-system
  ;; Forfeit agenda to rez up to 3 ICE with 2 credit discount per agenda point
  (do-game
    (new-game {:corp {:deck [(qty "Chiyashi" 3) "Quarantine System" "Project Beale"]}})
    (core/gain state :corp :credit 100)
    (core/gain state :corp :click 100)
    (play-from-hand state :corp "Chiyashi" "HQ")
    (play-from-hand state :corp "Chiyashi" "HQ")
    (play-from-hand state :corp "Chiyashi" "HQ")
    (play-from-hand state :corp "Quarantine System" "New remote")
    (play-from-hand state :corp "Project Beale" "New remote")
    (is (= 102 (:credit (get-corp))) "Corp has 102 creds")
    (let [ch1 (get-ice state :hq 0)
          ch2 (get-ice state :hq 1)
          ch3 (get-ice state :hq 2)
          qs (get-content state :remote1 0)
          beale (get-content state :remote2 0)]
      (core/rez state :corp qs)
      (card-ability state :corp qs 0)
      (is (empty? (:prompt (get-corp))) "No prompt to rez ICE")
      (score-agenda state :corp beale)
      ; 1 on rez
      (is (= 101 (:credit (get-corp))) "Corp has 101 creds")
      (card-ability state :corp qs 0)
      (click-card state :corp (get-scored state :corp 0))
      (click-card state :corp ch1)
      (click-card state :corp ch2)
      (click-card state :corp ch3)
      ; pay 8 per Chiyashi - 24 total
      (is (= 77 (:credit (get-corp))) "Corp has 77 creds")
      (is (empty? (:prompt (get-corp))) "No prompt to rez ICE"))))
