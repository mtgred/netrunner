(ns game-test.cards.operations.success
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest success
  ;; Success
  (testing "Works with bad publicity"
    (do-game
      (new-game {:corp {:deck ["NAPD Contract" "Project Beale" "Success"]}})
      (play-from-hand state :corp "NAPD Contract" "New remote")
      (play-from-hand state :corp "Project Beale" "New remote")
      (core/gain state :corp :bad-publicity 9)
      (core/gain state :corp :credit 8)
      (core/gain state :corp :click 15)
      (let [napd (get-content state :remote1 0)
            beale (get-content state :remote2 0)]
        (dotimes [_ 13] (core/advance state :corp {:card (refresh napd)}))
        (is (= 13 (get-counters (refresh napd) :advancement)))
        (core/score state :corp {:card (refresh napd)})
        (is (= 2 (:agenda-point (get-corp))))
        (play-from-hand state :corp "Success")
        (click-card state :corp (get-scored state :corp 0))
        (is (= "NAPD Contract" (:title (first (:rfg (get-corp))))))
        (click-card state :corp (refresh beale))
        (is (= 13 (get-counters (refresh beale) :advancement)))
        (core/score state :corp {:card (refresh beale)})
        (is (= 7 (:agenda-point (get-corp)))))))
  (testing "Works with public agendas"
    (do-game
      (new-game {:corp {:deck ["Oaktown Renovation" "Vanity Project" "Success"]}})
      (core/gain state :corp :click 1)
      (score-agenda state :corp (find-card "Vanity Project" (:hand (get-corp))))
      (is (= 4 (:agenda-point (get-corp))))
      (play-from-hand state :corp "Oaktown Renovation" "New remote")
      (is (= 5 (:credit (get-corp))))
      (play-from-hand state :corp "Success")
      (click-card state :corp (get-scored state :corp 0))
      (is (= "Vanity Project" (:title (first (:rfg (get-corp))))))
      (let [oaktown (get-content state :remote1 0)]
        (click-card state :corp (refresh oaktown))
        (is (= 6 (get-counters (refresh oaktown) :advancement)))
        (is (= 19 (:credit (get-corp))) "Gain 2 + 2 + 2 + 2 + 3 + 3 = 14 credits for advancing Oaktown")
        (core/score state :corp {:card (refresh oaktown)})
        (is (= 2 (:agenda-point (get-corp)))))))
  (testing "interaction with Jemison, regression test for issue #2704"
    (do-game
      (new-game {:corp {:id "Jemison Astronautics: Sacrifice. Audacity. Success."
                        :deck ["Success"
                               "High-Risk Investment"
                               "Government Takeover"]}})
      (core/gain state :corp :click 1)
      (score-agenda state :corp (find-card "High-Risk Investment" (:hand (get-corp))))
      (play-from-hand state :corp "Government Takeover" "New remote")
      (play-from-hand state :corp "Success")
      (click-card state :corp (get-in (get-corp) [:scored 0]))
      (let [gto (get-content state :remote1 0)]
        ;; Prompt for Jemison
        (click-card state :corp (refresh gto))
        (is (= 4 (get-counters (refresh gto) :advancement)) "Added 4 counters from Jemison trigger")
        ;; Prompt for Success
        (click-card state :corp (refresh gto))
        (is (= (+ 4 5) (get-counters (refresh gto) :advancement)) "Advance 5 times from Success")))))
