(ns game.core.optional-test
  (:require [game.core :as core]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [game.macros :refer [req]]
            [clojure.test :refer :all]))

(deftest optional-req
  (let [spy (atom [])]
    (do-game (new-game)
      (core/resolve-ability state :corp
                            {:req (req (swap! spy conj "outer") true)
                             :optional
                             {:req (req (swap! spy conj "inner") true)
                              :prompt "Yes or no"
                              :yes-ability {:effect (req true)}}}
                            {} nil)
      (is (= ["inner"] @spy) "Only the inner req of optional should be checked"))))
