(ns game-test.cards.assets.full-immersion-recstudio
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest full-immersion-recstudio
  ;; Full Immmersion RecStudio - install directly, and via Interns
  (testing "Full test"
    (do-game
      (new-game {:corp {:deck ["Full Immersion RecStudio"
                               (qty "Interns" 2)
                               (qty "Launch Campaign" 3)]}})
      (play-from-hand state :corp "Full Immersion RecStudio" "New remote")
      (let [fir (get-content state :remote1 0)]
        (core/rez state :corp fir)
        (card-ability state :corp fir 0)
        (click-card state :corp (find-card "Launch Campaign" (:hand (get-corp))))
        (let [lc (first (:hosted (refresh fir)))]
          (is lc "Launch Campaign hosted on Full Immersion RecStudio")
          (core/rez state :corp lc)
          (is (and (:installed (refresh lc)) (:rezzed (refresh lc))) "Rezzed Launch Campaign")
          (take-credits state :corp)
          (take-credits state :runner)
          (is (= 5 (:credit (get-corp))) "Gained 2cr from Launch Campaign")
          (is (= 4 (get-counters (refresh lc) :credit)) "4cr left on Launch Campaign")
          (play-from-hand state :corp "Interns")
          (click-card state :corp (find-card "Launch Campaign" (:hand (get-corp))))
          (click-prompt state :corp (refresh fir))
          (is (= 2 (count (:hosted (refresh fir)))) "Interns installed onto FIR")))))
  (testing "hosting an asset with events does not double-register events. Issue #1827"
    (do-game
      (new-game {:corp {:deck ["Full Immersion RecStudio" "Sandburg" "Vanilla"
                               "Oaktown Renovation"]}})
      (play-from-hand state :corp "Full Immersion RecStudio" "New remote")
      (play-from-hand state :corp "Vanilla" "HQ")
      (let [fir (get-content state :remote1 0)
            van (get-ice state :hq 0)]
        (core/rez state :corp fir)
        (core/rez state :corp van)
        (card-ability state :corp fir 0)
        (click-card state :corp (find-card "Sandburg" (:hand (get-corp))))
        (core/gain state :corp :credit 7 :click 3)
        (core/rez state :corp (first (:hosted (refresh fir))))
        (is (= 2 (:current-strength (refresh van))) "Vanilla at 2 strength")
        (card-ability state :corp fir 0)
        (click-card state :corp (find-card "Oaktown Renovation" (:hand (get-corp))))
        (core/advance state :corp {:card (last (:hosted (refresh fir)))})
        (is (= 11 (:credit (get-corp))) "Gained 1cr from advancing Oaktown")))))
