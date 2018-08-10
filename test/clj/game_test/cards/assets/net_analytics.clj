(ns game-test.cards.assets.net-analytics
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest net-analytics
  ;; Draw a card when runner avoids or removes 1 or more tags
  (do-game
    (new-game {:corp {:deck [(qty "Ghost Branch" 3) (qty "Net Analytics" 3)]}
               :runner {:deck [(qty "New Angeles City Hall" 3)]}})
    (starting-hand state :corp ["Net Analytics" "Ghost Branch"])
    (play-from-hand state :corp "Ghost Branch" "New remote")
    (play-from-hand state :corp "Net Analytics" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "New Angeles City Hall")
    (take-credits state :runner)
    (let [gb (get-content state :remote1 0)
          net (get-content state :remote2 0)
          nach (get-resource state 0)]
      (core/rez state :corp (refresh net))
      (core/advance state :corp {:card (refresh gb)})
      (is (= 1 (get-counters (refresh gb) :advancement)))
      (take-credits state :corp)
      (is (= 1 (count (:hand (get-corp)))) "Corp hand size is 1 before run")
      (run-empty-server state "Server 1")
      (click-prompt state :corp "Yes") ; Ghost Branch ability
      (card-ability state :runner nach 0)
      (click-prompt state :runner "Done")
      (click-prompt state :corp "Yes") ; Draw from Net Analytics
      (click-prompt state :runner "No action")
      (is (empty? (:prompt (get-runner))) "Runner waiting prompt is cleared")
      (is (zero? (:tag (get-runner))) "Avoided 1 Ghost Branch tag")
      (is (= 2 (count (:hand (get-corp)))) "Corp draw from NA")
      ; tag removal
      (core/gain-tags state :runner 1)
      (click-prompt state :runner "Done") ; Don't prevent the tag
      (core/remove-tag state :runner 1)
      (click-prompt state :corp "Yes") ; Draw from Net Analytics
      (is (= 3 (count (:hand (get-corp)))) "Corp draw from NA"))))
