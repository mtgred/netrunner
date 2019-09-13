(ns game-test.engine.effects
  (:require [game.core :as core]
            [game.core.effects :as e]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest gather-effects
  (let [start {:active-player :corp
               :eid 0
               :req-called 0}
        state (atom start)
        side :corp
        c1 {:cid 1
            :side :corp
            :title "Test Card 1"}
        c2 {:cid 2
            :side :corp
            :title "Test Card 2"}
        f1 {:type :test-type
            :duration :persistent
            :effect 1}
        f2 {:type :test-type
            :duration :persistent
            :effect (fn [state side eid card targets] 2)}]

    ;; This is testing if the :req is not present, so no need to retest
    (testing ":effect static values"
      (reset! state start)
      (e/register-floating-effect
        state side c1
        f1)
      (let [effects (e/get-effects state :corp c2 :test-type)]
        (is (= [1] effects) "Should return the static value")))

    (testing ":effect function values"
      (reset! state start)
      (e/register-floating-effect
        state side c1
        f2)
      (let [effects (e/get-effects state :corp c2 :test-type)]
        (is (= [2] effects) "Should return the value returned by the function")))

    (testing ":req is present"
      (testing "and is called"
        (reset! state start)
        (e/register-floating-effect
          state side c1
          (assoc f1 :req (fn [s1 s2 e c t] (swap! state update :req-called inc))))
        (is (zero? (:req-called @state)) "The req hasn't been called")
        (e/get-effects state :corp c2 :test-type)
        (is (= 1 (:req-called @state)) "The req has been called once")
        (e/get-effects state :corp c2 :test-type)
        (is (= 2 (:req-called @state)) "The req has been called a second time"))

      (testing "and returns true"
        (reset! state start)
        (e/register-floating-effect
          state side c1
          (assoc f1 :req (fn [s1 s2 e c t] true)))
        (let [effects (e/get-effects state :corp c2 :test-type)]
          (is (= [1] effects) "Should return the effect value")))

      (testing "and returns a truthy value"
        (reset! state start)
        (e/register-floating-effect
          state side c1
          (assoc f1 :req (fn [s1 s2 e c t] :true)))
        (let [effects (e/get-effects state :corp c2 :test-type)]
          (is (= [] effects) "Should not return the effect value")))

      (testing "and returns a falsey value"
        (reset! state start)
        (e/register-floating-effect
          state side c1
          (assoc f1 :req (fn [s1 s2 e c t] nil)))
        (let [effects (e/get-effects state :corp c2 :test-type)]
          (is (= [] effects) "Should not return the effect value"))))))
