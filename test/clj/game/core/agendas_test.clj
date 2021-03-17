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
                vp {:title "Test Card" :agendapoints 1}]
    (testing "basic functionality"
      (is (nil? (agenda-points state nil nil)) "requires card to exist"))
    (testing "base points"
      (is (= 0 (agenda-points state nil (dissoc vp :agendapoints))))
      (is (= 1 (agenda-points state nil vp)))
      (is (= 5 (agenda-points state nil (assoc vp :agendapoints 5)))))
    (testing "as-agenda-points"
      (is (= 2 (agenda-points state nil (assoc vp :as-agenda-points 1))))
      (is (= 0 (agenda-points state nil (assoc vp :as-agenda-points -1)))))
    (testing "agenda-value"
      (core/register-floating-effect state nil vp {:type :agenda-value :value 1})
      (is (= 2 (agenda-points state nil vp))))
    (testing "all together"
      (let [vp (assoc vp :agendapoints 5 :as-agenda-points 3)]
        (core/register-floating-effect state nil vp {:type :agenda-value :value 1})
        (is (= 9 (agenda-points state nil vp))) "5 + 3 + 1"))
    (testing "points-fn"
      (defmethod core/defcard-impl "Test Card" [_]
        {:agendapoints-corp (req 5)
         :agendapoints-runner (req 10)})
      (is (= 5 (agenda-points state :corp vp)))
      (is (= 10 (agenda-points state :runner vp)))
      (core/register-floating-effect state nil vp {:type :agenda-value :value 1})
      (is (= 6 (agenda-points state :corp vp)))
      (is (= 11 (agenda-points state :runner vp)))))
  (remove-method core/defcard-impl "Test Card"))
