(ns game.core.l10n-test 
  (:require
   [clojure.test :refer [deftest is]]
   [game.core.l10n :as sut]))

(deftest i18n-tests
  (is (= "CoolRunner uses \"Knickknack\" O'Brian to trash Daily Casts and gain 3 [Credit] and draw 1 card."
         (sut/build-msg
          (sut/->base-msg
           {:msg/type :use-card
            :username "CoolRunner"
            :title "\"Knickknack\" O'Brian"
            :msg/effect-msgs [{:effect/type :trash-card
                               :effect/title "Daily Casts"}
                              {:effect/type :gain-credits
                               :effect/value 3}
                              {:effect/type :draw-cards
                               :effect/value 1}]}))))
  (is (= "BadRunner spends 1 [Click] and pays 0 [Credit] to use \"Knickknack\" O'Brian to trash Daily Casts and gain 3 [Credit] and draw 1 card."
         (sut/build-msg
          (sut/->use-card-msg
           {:title "\"Knickknack\" O'Brian"}
           [{:effect/type :trash-card
             :effect/title "Daily Casts"}
            {:effect/type :gain-credits
             :effect/value 3}
            {:effect/type :draw-cards
             :effect/value 1}]
           [{:paid/type :click
             :paid/value 1}
            {:paid/type :credit
             :paid/value 0}]
           {:username "BadRunner" :side :runner}))))
  (is (= "SillyCorp pays 6 [Credit] from [their] credit pool and pays 3 [Credit] from Primary Transmission Dish to increase trace strength to 12."
         (sut/build-msg
          (sut/->base-msg
           {:msg/type :increase-trace-strength
            :username "SillyCorp"
            :side :runner
            :value 12
            :msg/payments
            [{:paid/type :credit
              :paid/value 9
              :paid/targets [{:pick-counters/type :credit-pool
                              :value 6}
                             {:pick-counters/type :card
                              :title "Primary Transmission Dish"
                              :value 3}]}]}))))
  (is (= "AngryRunner pays 1 [Credit] from [their] credit pool and pays 1 [Credit] from bad publicity to use New Angeles City Hall to avoid 1 tag."
         (sut/build-msg
          (sut/->base-msg
           {:msg/type :pay-use-card
            :username "AngryRunner"
            :title "New Angeles City Hall"
            :msg/effect-msgs [{:effect/type :avoid-tags
                               :effect/count 1}]
            :msg/payments [{:paid/type :credit
                            :paid/value 2
                            :paid/targets [{:pick-counters/type :credit-pool
                                            :value 1}
                                           {:pick-counters/type :bad-publicity
                                            :value 1}]}]})))))
