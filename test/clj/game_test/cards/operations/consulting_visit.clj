(ns game-test.cards.operations.consulting-visit
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest consulting-visit
  ;; Consulting Visit - Only show single copies of operations corp can afford as choices. Play chosen operation
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Consulting Visit"
                               (qty "Beanstalk Royalties" 2)
                               "Green Level Clearance"
                               "Breaking News"
                               "Hedge Fund"]}})
      (is (= 5 (:credit (get-corp))))
      (starting-hand state :corp ["Consulting Visit"])
      (play-from-hand state :corp "Consulting Visit")
      (let [get-prompt (fn [] (first (#(get-in @state [:corp :prompt]))))
            prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
        (is (= (list "Beanstalk Royalties" "Green Level Clearance" nil) (prompt-names)))
        (click-prompt state :corp (find-card "Beanstalk Royalties" (:deck (get-corp))))
        (is (= 6 (:credit (get-corp)))))))
  (testing "Works properly when played with Mumbad City Hall"
    (do-game
      (new-game {:corp {:deck ["Mumbad City Hall"
                               "Beanstalk Royalties"
                               "Green Level Clearance"
                               "Breaking News"
                               "Hedge Fund"
                               "Consulting Visit"
                               "Mumba Temple"]}})
      (is (= 5 (:credit (get-corp))))
      (starting-hand state :corp ["Mumbad City Hall"])
      (play-from-hand state :corp "Mumbad City Hall" "New remote")
      (let [hall (get-content state :remote1 0)
            get-prompt (fn [] (first (#(get-in @state [:corp :prompt]))))
            prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
        (card-ability state :corp hall 0)
        (is (= (list "Consulting Visit" "Mumba Temple" nil) (prompt-names)))
        (click-prompt state :corp (find-card "Consulting Visit" (:deck (get-corp))))
        (is (= 3 (:credit (get-corp))))
        (is (= (list "Beanstalk Royalties" "Green Level Clearance" nil) (prompt-names)))
        (click-prompt state :corp (find-card "Green Level Clearance" (:deck (get-corp))))
        (is (= 5 (:credit (get-corp))))))))
