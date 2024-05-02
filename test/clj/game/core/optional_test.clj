(ns game.core.optional-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.initializing :refer [make-card]]
   [game.macros :refer [req]]
   [game.test-framework :refer :all]))

(deftest optional-req
  (let [spy (atom [])]
    (do-game (new-game)
      (core/resolve-ability state :corp
                            {:req (req (swap! spy conj "outer") true)
                             :optional
                             {:req (req (swap! spy conj "inner") true)
                              :prompt "Yes or no"
                              :yes-ability {:effect (req true)}}}
                            (make-card {:title "test"}) nil)
      (is (= ["inner"] @spy) "Only the inner req of optional should be checked"))))
