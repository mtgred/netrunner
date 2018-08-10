(ns game-test.cards.assets.false-flag
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest false-flag
  ;; False Flag
  (testing "when the corp attempts to score False Flag"
    (testing "and False Flag has 7 advancements"
      (do-game
        (new-game {:corp {:deck ["False Flag"]}})
        (play-from-hand state :corp "False Flag" "New remote")
        (let [ff (get-content state :remote1 0)]
          (core/add-counter state :corp ff :advancement 7)
          (core/rez state :corp (refresh ff))
          (card-ability state :corp (refresh ff) 0)
          (is (nil? (get-content state :remote1 0))
              "False Flag is no longer in remote")
          (is (= 3 (:agendapoints (get-scored state :corp 0)))
              "the corp can score False Flag")
          (is (= 1 (:click (get-corp)))
              "scoring False Flag costs one click"))))
    (testing "and False Flag has less than 7 advancements"
      (do-game
        (new-game {:corp {:deck ["False Flag"]}})
        (play-from-hand state :corp "False Flag" "New remote")
        (let [ff (get-content state :remote1 0)]
          (core/add-counter state :corp ff :advancement 6)
          (core/rez state :corp (refresh ff))
          (card-ability state :corp (refresh ff) 0)
          (is (not (nil? (get-content state :remote1 0)))
              "False Flag remains in the remote")
          (is (nil? (:agendapoints (get-scored state :corp 0)))
              "the corp cannot score false flag")
          (is (= 2 (:click (get-corp)))
              "the corp does not lose a click")))))
  (testing "when the runner accesses False Flag"
    (letfn [(false-flag-tags-test
              [[advancements expected-tags]]
              (testing (str "and False Flag has " advancements " advancements")
                (do-game
                  (new-game {:corp {:deck ["False Flag"]}})
                  (play-from-hand state :corp "False Flag" "New remote")
                  (core/add-prop state :corp
                                 (get-content state :remote1 0)
                                 :advance-counter advancements)
                  (take-credits state :corp)
                  (run-empty-server state "Server 1")
                  (click-prompt state :runner "No action")
                  (let [tags (:tag (get-runner))]
                    (is (= expected-tags tags)
                        (str "the runner recieves " tags " tags"))))))]
      (doall (map false-flag-tags-test
                  [[0 0]
                   [2 1]
                   [5 2]
                   [10 5]])))))
