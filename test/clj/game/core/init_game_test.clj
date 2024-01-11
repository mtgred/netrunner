(ns game.core.init-game-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.test-framework :refer :all]))

(deftest default-identity
  (testing "Both are chosen"
    (do-game
      (new-game {:corp {:id "Jinteki: Personal Evolution"}
                 :runner {:id "Khan: Savvy Skiptracer"}})
      (is (= "Jinteki: Personal Evolution" (get-in @state [:corp :identity :title])))
      (is (= "Khan: Savvy Skiptracer" (get-in @state [:runner :identity :title])))))
  (testing "Only the Corp ID is chosen"
    (do-game
      (new-game {:corp {:id "Jinteki: Personal Evolution"}})
      (is (= "Jinteki: Personal Evolution" (get-in @state [:corp :identity :title])))
      (is (= "The Professor: Keeper of Knowledge" (get-in @state [:runner :identity :title])))))
  (testing "Only the Runner ID is chosen"
    (do-game
      (new-game {:runner {:id "Khan: Savvy Skiptracer"}})
      (is (= "Custom Biotics: Engineered for Success" (get-in @state [:corp :identity :title])))
      (is (= "Khan: Savvy Skiptracer" (get-in @state [:runner :identity :title])))))
  (testing "Both are not chosen"
    (do-game
      (new-game)
      (is (= "Custom Biotics: Engineered for Success" (get-in @state [:corp :identity :title])))
      (is (= "The Professor: Keeper of Knowledge" (get-in @state [:runner :identity :title]))))))
