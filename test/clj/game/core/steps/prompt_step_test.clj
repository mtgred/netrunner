(ns game.core.steps.prompt-step-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core-test :refer :all]
   [game.core.pipeline :refer :all]
   [game.core.steps.prompt-step :as sut]
   [game.macros-test :refer :all]
   [game.utils-test :refer :all]))

(deftest basic-test
  (before-each [state (new-game {:corp {:deck [(qty "Hedge Fund" 10)]}
                                 :dont-start-game true})]
    (testing "heck"
      (queue-step!
        state
        (sut/->PromptStep))
      (println (get-in @state [:corp :prompt-state]))
      )
    ))
