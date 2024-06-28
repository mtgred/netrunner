(ns game.core.stats-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.test-framework :refer :all]))

(deftest click-count
  (testing "clicks gained"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                        :hand []}
                 :runner {:deck [(qty "Sure Gamble" 10)]}})
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 9 (get-in @state [:stats :corp :gain :click])) "Corp has started 3 turns")
      (is (= 8 (get-in @state [:stats :runner :gain :click])) "Runner has started 2 turns")
      (is (= 8 (get-in @state [:stats :corp :gain :card])) "Corp drew 3 times")
      (is (= 5 (get-in @state [:stats :runner :gain :card])) "Runner did not draw")
      (is (= 6 (get-in @state [:stats :corp :gain :credit])) "Corp gained 6 credits")
      (is (= 8 (get-in @state [:stats :runner :gain :credit])) "Runner gained 8 credits")
      (is (= 8 (get-in @state [:stats :runner :gain :click])) "Runner has started 2 turns")
      (is (= 6 (get-in @state [:stats :corp :click :credit])) "Corp clicked for 6 credits")
      (is (= 8 (get-in @state [:stats :runner :click :credit])) "Runner clicked for 8 credits")
      (play-from-hand state :corp "Hedge Fund")
      (is (= 6 (get-in @state [:stats :corp :click :credit])) "Corp clicked for 6 credits")
      (is (= 15 (get-in @state [:stats :corp :gain :credit])) "Corp gained 15 credits")
      (is (= 5 (get-in @state [:stats :corp :spent :credit])) "Corp spent 5 credits")
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (is (= 8 (get-in @state [:stats :runner :click :credit])) "Runner clicked for 8 credits")
      (is (= 17 (get-in @state [:stats :runner :gain :credit])) "Runner gained 17 credits")
      (is (= 5 (get-in @state [:stats :runner :spent :credit])) "Runner spent 5 credits"))))

(deftest tags-count
  (testing "tags gained"
    (do-game
      (new-game {:corp {:hand ["Breaking News"]}})
      (play-and-score state "Breaking News")
      (take-credits state :corp)
      (remove-tag state :runner)
      (is (= 2 (get-in @state [:stats :runner :gain :tag :base])) "Runner gained 2 tags"))))

(deftest credits-from-cards
  (testing "Take from Liberated Account"
    (do-game
      (new-game {:runner {:deck [(qty "Liberated Account" 10)]}})
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 4 (get-in @state [:stats :runner :click :credit])) "Runner clicked for 4 credits")
      (is (= 4 (get-in @state [:stats :runner :gain :credit])) "Runner starts with 4 credits")
      (play-from-hand state :runner "Liberated Account")
      (card-ability state :runner (get-resource state 0) 0)
      (card-ability state :runner (get-resource state 0) 0)
      (card-ability state :runner (get-resource state 0) 0)
      (is (= 4 (get-in @state [:stats :runner :click :credit])) "Runner clicked for 4 credits")
      (is (= 16 (get-in @state [:stats :runner :gain :credit])) "Runner gained 12 credits from LA")
      (is (= 6 (get-in @state [:stats :runner :spent :credit])) "Runner spent 6 credits")))
  (testing "Take from Adonis Campaign"
    (do-game
      (new-game {:corp {:hand ["Adonis Campaign"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (let [ac (get-content state :remote1 0)]
        (rez state :corp ac)
        (take-credits state :corp)
        (is (= 2 (get-in @state [:stats :corp :click :credit])) "Corp clicked for 2 credits")
        (is (= 2 (get-in @state [:stats :corp :gain :credit])) "Corp starts with 2 credits")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 5 (get-in @state [:stats :corp :click :credit])) "Corp clicked for 5 credits")
        (is (= 8 (get-in @state [:stats :corp :gain :credit])) "Corp gained 3 credits from AC")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 8 (get-in @state [:stats :corp :click :credit])) "Corp clicked for 8 credits")
        (is (= 4 (get-in @state [:stats :corp :spent :credit])) "Corp spent 4 credits")
        (is (= 14 (get-in @state [:stats :corp :gain :credit])) "Corp gained 6 credits (total) from AC")))))

(deftest companion-credits
  (testing "Take from Mystic Maemi"
    (do-game
      (new-game {:corp {:deck ["Project Vitruvius"]}
                 :runner {:deck [(qty "Sure Gamble" 3) "Mystic Maemi"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Mystic Maemi")
      (let [mm (get-resource state 0)]
        (run-empty-server state "HQ")
        (click-prompt state :runner "Steal")
        (take-credits state :runner)
        (is (= 1 (get-in @state [:stats :runner :spent :credit])) "Runner spent 1 credits")
        (take-credits state :corp)
        (take-credits state :runner)
        (take-credits state :corp)
        (play-from-hand state :runner "Sure Gamble")
        (click-card state :runner mm)
        (click-card state :runner mm)
        (click-prompt state :runner "Done")
        (is (= 6 (get-in @state [:stats :runner :click :credit])) "Runner clicked for 6 credits")
        (is (= 6 (get-in @state [:stats :runner :spent :credit])) "Runner spent 6 credits")
        (is (= 15 (get-in @state [:stats :runner :gain :credit])) "Runner gained HF credits")))))
