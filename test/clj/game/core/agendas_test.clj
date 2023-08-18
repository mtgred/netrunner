(ns game.core.agendas-test
  (:require [game.core :as core]
            [game.macros :refer [req]]
            [game.core.agendas :refer [agenda-points]]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest agenda-points-test
  (defmethod core/defcard-impl "Test Card" [_] {})
  (before-each [state (new-game)
                test-card {:title "Test Card" :agendapoints 1}]
    (testing "basic functionality"
      (is (nil? (agenda-points state nil nil)) "requires card to exist"))
    (testing "base points"
      (is (= 0 (agenda-points state nil (dissoc test-card :agendapoints))))
      (is (= 1 (agenda-points state nil test-card)))
      (is (= 5 (agenda-points state nil (assoc test-card :agendapoints 5)))))
    (testing "agenda-value"
      (core/register-lingering-effect state nil test-card {:type :agenda-value :value 1})
      (is (= 2 (agenda-points state nil test-card))))
    (testing "all together"
      (let [test-card (assoc test-card :agendapoints 5)]
        (core/register-lingering-effect state nil test-card {:type :agenda-value :value 1})
        (is (= 6 (agenda-points state nil test-card))) "5 + 1"))
    (testing "points-fn"
      (defmethod core/defcard-impl "Test Card" [_]
        {:agendapoints-corp (req 5)
         :agendapoints-runner (req 10)})
      (is (= 5 (agenda-points state :corp test-card)))
      (is (= 10 (agenda-points state :runner test-card)))
      (core/register-lingering-effect state nil test-card {:type :agenda-value :value 1})
      (is (= 6 (agenda-points state :corp test-card)))
      (is (= 11 (agenda-points state :runner test-card)))))
  (remove-method core/defcard-impl "Test Card"))
