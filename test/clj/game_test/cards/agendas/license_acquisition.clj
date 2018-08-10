(ns game-test.cards.agendas.license-acquisition
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest license-acquisition
  ;; License Acquisition
  (do-game
    (new-game {:corp {:deck [(qty "License Acquisition" 4)
                             "Adonis Campaign" "Eve Campaign"
                             "Strongbox" "Corporate Troubleshooter"]}})
    (testing "Set up"
      (starting-hand state :corp ["License Acquisition" "License Acquisition" "License Acquisition" "License Acquisition"
                                  "Adonis Campaign" "Strongbox"])
      (core/move state :corp (find-card "Eve Campaign" (:deck (get-corp))) :discard)
      (core/move state :corp (find-card "Corporate Troubleshooter" (:deck (get-corp))) :discard)
      (core/gain state :corp :click 4))
    (testing "Asset & HQ"
      (play-and-score state "License Acquisition")
      (click-card state :corp (find-card "Adonis Campaign" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (is (some? (get-content state :remote2 0))))
    (testing "Upgrade & HQ"
      (play-and-score state "License Acquisition")
      (click-card state :corp (find-card "Strongbox" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (is (some? (get-content state :remote4 0))))
    (testing "Asset & Archives"
      (play-and-score state "License Acquisition")
      (click-card state :corp (find-card "Eve Campaign" (:discard (get-corp))))
      (click-prompt state :corp "New remote")
      (is (some? (get-content state :remote6 0))))
    (testing "Upgrade & Archives"
      (play-and-score state "License Acquisition")
      (click-card state :corp (find-card "Corporate Troubleshooter" (:discard (get-corp))))
      (click-prompt state :corp "New remote")
      (is (some? (get-content state :remote8 0))))))
