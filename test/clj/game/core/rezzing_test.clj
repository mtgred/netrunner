(ns game.core.rezzing-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.rezzing :as rezzing]
   [game.test-framework :refer :all]
   [game.core.payment :refer [->c]]))

(deftest get-rez-cost-test
  (before-each [state (new-game)
                card {:title "No match" :cost 5}]
    (testing "ignoring all costs"
      (is (= [(->c :credit 0)] (rezzing/get-rez-cost state nil nil {:ignore-cost :all-costs}))))
    (testing "as an alternative cost"
      (is (= [(->c :click 1)] (rezzing/get-rez-cost state nil nil {:alternative-cost [(->c :click 1)]}))))
    (testing "base cost"
      (is (= [(->c :credit 5)] (rezzing/get-rez-cost state nil card nil))))
    (testing "ignoring cost"
      (is (= () (rezzing/get-rez-cost state nil card {:ignore-cost true})))))
  (before-each [state (new-game)
                card {:title "No match" :cost 5 :additional-cost [(->c :trash-can)]}]
    (testing "with additional costs and card disabled"
      (let [card (assoc card :disabled true)]
        (is (= [(->c :credit 5)] (rezzing/get-rez-cost state nil card nil)))))))

(deftest simultaneous-rez-test
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand ["Advanced Assembly Lines" "Surat City Grid" "NGO Front"]}})
    (play-from-hand state :corp "Surat City Grid" "New remote")
    (play-from-hand state :corp "Advanced Assembly Lines" "Server 1")
    (play-from-hand state :corp "NGO Front" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (is (changed? [(:credit (get-corp)) -1]
          (rez state :corp (get-content state :remote1 1)))
        "Corp pays 1 credit to rez AAL")
    (is (waiting? state :runner))
    (is (= "Choose a trigger to resolve" (:msg (prompt-map :corp))))
    (is (= ["Advanced Assembly Lines" "Done" "Surat City Grid"] (sort (prompt-titles :corp))))
    (is (changed? [(:credit (get-corp)) 3]
          (click-prompt state :corp "Advanced Assembly Lines"))
        "Corp gains 3 from choosing AAL first")
    (click-prompt state :corp "Yes")
    (click-card state :corp "NGO Front")
    (is (rezzed? (get-content state :remote1 0)))
    (is (rezzed? (get-content state :remote1 1)))
    (is (rezzed? (get-content state :remote2 0)))))
