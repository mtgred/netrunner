(ns game.core.set-up-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.test-framework :refer :all]))

(deftest mulligan-responses
  (let [setup {:corp {:deck ["Ice Wall" "Hedge Fund" "IPO" "NGO Front" "PAD Campaign"
                             "Jackson Howard" "Enigma" "Merger" "SanSan City Grid"]}
               :runner {:deck ["Sure Gamble" "Drug Dealer" "Corroder" "Nfr"
                               "Kasi String" "Stimhack" "Desperado" "Jak Sinclair"]}
               :options {:dont-start-game true}}]
    (testing "keep"
      (do-game
        (new-game setup)
        (let [corp-hand (:hand (get-corp))]
          (click-prompt state :corp "Keep")
          (is (= corp-hand (:hand (get-corp))))
          (is (last-log-contains? state "Corp keeps their hand")))
        (let [runner-hand (:hand (get-runner))]
          (click-prompt state :runner "Keep")
          (is (= runner-hand (:hand (get-runner))))
          (is (last-log-contains? state "Runner keeps their hand")))))
    (testing "mulligan"
      (do-game
        (new-game setup)
        (let [corp-hand (:hand (get-corp))]
          (click-prompt state :corp "Mulligan")
          (is (not (identical? corp-hand (:hand (get-corp)))))
          (is (last-log-contains? state "Corp takes a mulligan")))
        (let [runner-hand (:hand (get-runner))]
          (click-prompt state :runner "Mulligan")
          (is (not (identical? runner-hand (:hand (get-runner)))))
          (is (last-log-contains? state "Runner takes a mulligan")))))))
