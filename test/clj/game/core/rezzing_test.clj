(ns game.core.rezzing-test
  (:require [game.core :as core]
            [game.core.rezzing :as rezzing]
            [game.core.card-defs :refer [card-def]]
            [game.core.card :refer :all]
            [game.core.cost-fns :refer [rez-additional-cost-bonus]]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest get-rez-cost-test
  (before-each [state (new-game)
                card {:title "No match" :cost 5}]
    (testing "ignoring all costs"
      (is (= [[:credit 0]] (rezzing/get-rez-cost state nil nil {:ignore-cost :all-costs}))))
    (testing "as an alternative cost"
      (is (= [[:click 1]] (rezzing/get-rez-cost state nil nil {:alternative-cost [:click 1]}))))
    (testing "base cost"
      (is (= [[:credit 5]] (rezzing/get-rez-cost state nil card nil))))
    (testing "ignoring cost"
      (is (= () (rezzing/get-rez-cost state nil card {:ignore-cost true})))))
  (before-each [state (new-game)
                card {:title "No match" :cost 5 :additional-cost [:trash]}]
    (testing "ignoring cost with additional costs"
      (is (= [[:trash 1]] (rezzing/get-rez-cost state nil card {:ignore-cost true}))))
    (testing "with additional costs"
      (is (= [[:credit 5] [:trash 1]] (rezzing/get-rez-cost state nil card nil))))
    (testing "with additional costs and card disabled"
      (let [card (assoc card :disabled true)]
        (is (= [[:credit 5]] (rezzing/get-rez-cost state nil card nil)))))))

(deftest simultaneous-rez-test
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand ["Advanced Assembly Lines" "Surat City Grid" "NGO Front"]}})
    (play-from-hand state :corp "Surat City Grid" "New remote")
    (play-from-hand state :corp "Advanced Assembly Lines" "Server 1")
    (play-from-hand state :corp "NGO Front" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (changes-val-macro
      -1 (:credit (get-corp))
      "Corp pays 1 credit to rez AAL"
      (rez state :corp (get-content state :remote1 1)))
    (is (= :waiting (prompt-type :runner)))
    (is (= "Choose a trigger to resolve" (:msg (prompt-map :corp))))
    (is (= ["Advanced Assembly Lines" "Surat City Grid"] (sort (prompt-titles :corp))))
    (changes-val-macro
      3 (:credit (get-corp))
      "Corp gains 3 from choosing AAL first"
      (click-prompt state :corp "Advanced Assembly Lines"))
    (click-prompt state :corp "Yes")
    (click-card state :corp "NGO Front")
    (is (rezzed? (get-content state :remote1 0)))
    (is (rezzed? (get-content state :remote1 1)))
    (is (rezzed? (get-content state :remote2 0)))))
