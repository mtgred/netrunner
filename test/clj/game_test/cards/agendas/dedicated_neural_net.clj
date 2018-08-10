(ns game-test.cards.agendas.dedicated-neural-net
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest dedicated-neural-net
  ;; Dedicated Neural Net
  (do-game
    (new-game {:corp {:deck ["Dedicated Neural Net" (qty "Scorched Earth" 2)
                             "Hedge Fund" "Caprice Nisei"]}
               :runner {:deck ["HQ Interface"]}})
    (play-from-hand state :corp "Caprice Nisei" "HQ")
    (play-and-score state "Dedicated Neural Net")
    (take-credits state :corp)
    (run-empty-server state :hq)
    (click-prompt state :runner "0 [Credits]")
    (click-prompt state :corp "1 [Credits]")
    (is (-> @state :run :run-effect :replace-access) "Replace-access tiggered")
    (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp))))
    (click-prompt state :runner "Card from hand")
    (is (accessing state "Hedge Fund") "Runner accessing Hedge Fund")
    (click-prompt state :runner "No action")
    ;; test for #2376
    (click-prompt state :runner "Unrezzed upgrade in HQ")
    (is (accessing state "Caprice Nisei") "Runner accessing Caprice")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run completed")
    (run-empty-server state :hq)
    (click-prompt state :runner "Card from hand")
    (click-prompt state :runner "No action")
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "HQ Interface")
    (run-empty-server state :hq)
    (click-prompt state :runner "0 [Credits]")
    (click-prompt state :corp "1 [Credits]")
    (is (= 2 (-> (get-corp) :selected first :max)) "Corp chooses 2 cards for Runner to access")))
