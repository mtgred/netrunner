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
        (sut/->PromptStep
          :corp
          (fn keep-mulligan-prompt [_state]
            {:prompt-title "Corp Mulligan"
             :prompt-text "Keep or mulligan this hand?"
             :buttons [{:text "Keep" :arg "keep"}
                       {:text "Mulligan" :arg "mulligan"}]})
          (fn keep-mulligan-waiting [_state]
            {:prompt-title "Waiting for Corp to keep or mulligan starting hand"})))
      (continue-gp! state)
      (println (get-in @state [:corp :prompt-state]))
      )
    ))
