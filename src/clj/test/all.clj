(ns test.all
  (:require [clojure.test :refer :all]
            test.core
            test.cards.agendas
            test.cards.assets
            test.cards.events
            test.cards.hardware
            test.cards.ice
            test.cards.icebreakers
            test.cards.identities
            test.cards.operations
            test.cards.programs
            test.cards.resources
            test.cards.upgrades))

(deftest all-tests
  (run-tests 'test.core)
  (run-all-tests #"test.cards.*"))
