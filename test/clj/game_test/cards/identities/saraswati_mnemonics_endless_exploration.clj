(ns game-test.cards.identities.saraswati-mnemonics-endless-exploration
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest saraswati-mnemonics-endless-exploration
  ;; Saraswati Mnemonics
  (do-game
    (new-game {:corp {:id "Saraswati Mnemonics: Endless Exploration"
                      :deck ["Gene Splicer" "House of Knives"]}})
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (click-card state :corp (find-card "Gene Splicer" (:hand (get-corp))))
    (click-prompt state :corp "New remote")
    (let [splicer (get-content state :remote1 0)]
      (is (= 1 (get-counters (refresh splicer) :advancement)) "1 advancements placed on Gene Splicer")
      (core/rez state :corp (refresh splicer))
      (is (not (:rezzed (refresh splicer))) "Gene Splicer did not rez")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/rez state :corp (refresh splicer))
      (is (:rezzed (refresh splicer)) "Gene Splicer now rezzed")
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (click-card state :corp (find-card "House of Knives" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (let [house (get-content state :remote2 0)]
        (advance state house)
        (advance state house)
        (core/score state :corp (refresh house))
        (is (empty? (:scored (get-corp))) "House of Knives not scored")
        (is (zero? (:agenda-point (get-corp))))
        (take-credits state :corp)
        (take-credits state :runner)
        (core/score state :corp (refresh house))
        (is (= 1 (:agenda-point (get-corp))) "House of Knives was able to be scored")))))
