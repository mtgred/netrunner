(ns game-test.engine.init-game
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

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
      (is (= "The Masque: Cyber General" (get-in @state [:runner :identity :title])))))
  (testing "Only the Runner ID is chosen"
    (do-game
      (new-game {:runner {:id "Khan: Savvy Skiptracer"}})
      (is (= "The Shadow: Pulling the String" (get-in @state [:corp :identity :title])))
      (is (= "Khan: Savvy Skiptracer" (get-in @state [:runner :identity :title])))))
  (testing "Both are not chosen"
    (do-game
      (new-game)
      (is (= "The Shadow: Pulling the String" (get-in @state [:corp :identity :title])))
      (is (= "The Masque: Cyber General" (get-in @state [:runner :identity :title]))))))
