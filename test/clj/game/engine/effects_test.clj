(ns game.engine.effects-test
  (:require [game.core :as core]
            [game.core.effects :as e]
            [game.utils :refer [same-card?]]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest gather-effects
  (let [start {:active-player :corp
               :eid 0
               :req-called 0}
        state (atom start)
        side :corp
        corp-card {:cid 1
                   :side :corp
                   :title "Test Card 1"}
        runner-card {:cid 2
                     :side :runner
                     :title "Test Card 2"}
        type-1 {:type :test-type
                :duration :constant
                :value 1}
        type-2 {:type :test-type-2
                :duration :constant
                :value (fn [state side eid card targets] 2)}]

    (testing "Effect type filtering"
      (reset! state start)
      (e/register-floating-effect state side corp-card type-1)
      (e/register-floating-effect state side corp-card type-2)
      (e/register-floating-effect state side corp-card type-1)
      (e/register-floating-effect state side corp-card type-2)
      (let [effects (e/gather-effects state :corp :test-type)]
        (is (= 2 (count effects)) "Only 2 effects are returned")))

    (testing "Effect sorting"
      (reset! state start)
      (is (= :corp (:active-player @state)) "Corp is active player")
      (e/register-floating-effect state side corp-card type-1)
      (e/register-floating-effect state side runner-card type-1)
      (e/register-floating-effect state side corp-card type-1)
      (e/register-floating-effect state side runner-card type-1)
      (let [effects (e/gather-effects state :corp :test-type)]
        (is (= [:corp :corp :runner :runner] (map #(get-in % [:card :side]) effects))
            "Effects are sorted by active player first")))))

(deftest get-effects
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
            :duration :constant
            :value 1}
        f2 {:type :test-type
            :duration :constant
            :value (fn [state side eid card targets] 2)}]

    ;; This is testing if the :req is not present, so no need to retest
    (testing ":value static values"
      (reset! state start)
      (e/register-floating-effect state side c1 f1)
      (let [effects (e/get-effects state :corp c2 :test-type)]
        (is (= [1] effects) "Should return the static value")))

    (testing ":value function values"
      (reset! state start)
      (e/register-floating-effect state side c1 f2)
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

      (testing "and returns a truthy value"
        (reset! state start)
        (e/register-floating-effect
          state side c1
          (assoc f1 :req (fn [s1 s2 e c t] :true)))
        (let [effects (e/get-effects state :corp c2 :test-type)]
          (is (= [1] effects) "Should return the effect value")))

      (testing "and returns a falsey value"
        (reset! state start)
        (e/register-floating-effect
          state side c1
          (assoc f1 :req (fn [s1 s2 e c t] nil)))
        (let [effects (e/get-effects state :corp c2 :test-type)]
          (is (= [] effects) "Should not return the effect value"))))))

(deftest sum-effects
  (let [start {:active-player :corp
               :eid 0
               :req-called 0}
        state (atom start)
        side :corp
        card {:cid 1
              :side :corp
              :title "Test Card"}
        f (fn [n]
            {:type :test-type
             :duration :constant
             :value n})]

    (testing "Handles non-numbers"
      (reset! state start)
      (e/register-floating-effect state side card (f 1))
      (e/register-floating-effect state side card (f 2))
      (e/register-floating-effect state side card (f nil))
      (e/register-floating-effect state side card (f 3))
      (e/register-floating-effect state side card (f 4))
      (is (= 10 (e/sum-effects state side card :test-type)) "Doesn't fail on nils"))))
