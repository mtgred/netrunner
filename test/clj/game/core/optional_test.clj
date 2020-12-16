(ns game.core.optional-test
  (:require [game.core :as core]
            [game.core-test :refer :all]
            [game.core.card-defs :refer [card-def]]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [game.macros :refer [req]]
            [clojure.test :refer :all]
            [clojure.stacktrace :refer [print-stack-trace]]
            [jinteki.cards :refer [all-cards]]))

(deftest optional-req
  (let [spy (atom [])
        og-card-def card-def]
    (with-redefs [all-cards all-cards
                  card-def (fn [card]
                             (if-not (= "Optional Card" (:title card))
                               (og-card-def card)
                               {:req (req (swap! spy conj "outer") true)
                                :optional
                                {:req (req (swap! spy conj "inner") true)
                                 :prompt "Yes or no"
                                 :yes-ability {:effect (req true)}}}))]
      (swap! all-cards assoc "Optional Card" {:title "Optional Card"
                                              :side "Corp"
                                              :type "Asset"
                                              :cost 0})
      (do-game
        (new-game {:corp {:hand ["Optional Card"]}})
        (play-from-hand state :corp "Optional Card" "New remote")
        (rez state :corp (get-content state :remote1 0))
        (is (= ["inner"] @spy) "Only the inner req of optional should be checked")
        ))))
