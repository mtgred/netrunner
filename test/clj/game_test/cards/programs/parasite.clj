(ns game-test.cards.programs.parasite
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest parasite
  (testing "Basic functionality: Gain 1 counter every Runner turn"
    (do-game
      (new-game {:corp {:deck [(qty "Wraparound" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Parasite" 3) (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Wraparound" "HQ")
      (let [wrap (get-ice state :hq 0)]
        (core/rez state :corp wrap)
        (take-credits state :corp)
        (play-from-hand state :runner "Parasite")
        (click-card state :runner wrap)
        (is (= 3 (core/available-mu state)) "Parasite consumes 1 MU")
        (let [psite (first (:hosted (refresh wrap)))]
          (is (zero? (get-counters psite :virus)) "Parasite has no counters yet")
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 1 (get-counters (refresh psite) :virus))
              "Parasite gained 1 virus counter at start of Runner turn")
          (is (= 6 (:current-strength (refresh wrap))) "Wraparound reduced to 6 strength")))))
  (testing "Installed facedown w/ Apex"
    (do-game
      (new-game {:runner {:id "Apex: Invasive Predator"
                          :deck ["Parasite"]}})
      (take-credits state :corp)
      (core/end-phase-12 state :runner nil)
      (click-card state :runner (find-card "Parasite" (:hand (get-runner))))
      (is (empty? (:prompt (get-runner))) "No prompt to host Parasite")
      (is (= 1 (count (get-runner-facedown state))) "Parasite installed face down")))
  (testing "Installed on untrashable Architect should keep gaining counters past 3 and make strength go negative"
    (do-game
      (new-game {:corp {:deck [(qty "Architect" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Parasite" 3) "Grimoire"]}})
      (play-from-hand state :corp "Architect" "HQ")
      (let [arch (get-ice state :hq 0)]
        (core/rez state :corp arch)
        (take-credits state :corp)
        (play-from-hand state :runner "Grimoire")
        (play-from-hand state :runner "Parasite")
        (click-card state :runner arch)
        (let [psite (first (:hosted (refresh arch)))]
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (take-credits state :runner)
          (take-credits state :corp)
          (take-credits state :runner)
          (take-credits state :corp)
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 4 (get-counters (refresh psite) :virus)) "Parasite has 4 counters")
          (is (= -1 (:current-strength (refresh arch))) "Architect at -1 strength")))))
  (testing "Should stay on hosted card moved by Builder"
    (do-game
      (new-game {:corp {:deck [(qty "Builder" 3) "Ice Wall"]}
                 :runner {:deck [(qty "Parasite" 3)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Builder" "Archives")
      (let [builder (get-ice state :archives 0)]
        (core/rez state :corp builder)
        (take-credits state :corp)
        (play-from-hand state :runner "Parasite")
        (click-card state :runner builder)
        (let [psite (first (:hosted (refresh builder)))]
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 3 (:current-strength (refresh builder))) "Builder reduced to 3 strength")
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (take-credits state :runner))
        (let [orig-builder (refresh builder)]
          (card-ability state :corp builder 0)
          (click-prompt state :corp "HQ")
          (let [moved-builder (get-ice state :hq 1)]
            (is (= (:current-strength orig-builder) (:current-strength moved-builder)) "Builder's state is maintained")
            (let [orig-psite (dissoc (first (:hosted orig-builder)) :host)
                  moved-psite (dissoc (first (:hosted moved-builder)) :host)]
              (is (= orig-psite moved-psite) "Hosted Parasite is maintained"))
            (take-credits state :corp)
            (let [updated-builder (refresh moved-builder)
                  updated-psite (first (:hosted updated-builder))]
              (is (= 2 (:current-strength updated-builder)) "Builder strength still reduced")
              (is (= 2 (get-counters (refresh updated-psite) :virus)) "Parasite counters still incremented")))))))
  (testing "Use Hivemind counters when installed; instantly trash ICE if counters >= ICE strength"
    (do-game
      (new-game {:corp {:deck [(qty "Enigma" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck ["Parasite"
                                 "Grimoire"
                                 "Hivemind"
                                 "Sure Gamble"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (let [enig (get-ice state :hq 0)]
        (core/rez state :corp enig)
        (take-credits state :corp)
        (play-from-hand state :runner "Sure Gamble")
        (play-from-hand state :runner "Grimoire")
        (play-from-hand state :runner "Hivemind")
        (let [hive (get-program state 0)]
          (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind has 2 counters")
          (play-from-hand state :runner "Parasite")
          (click-card state :runner enig)
          (is (= 1 (count (:discard (get-corp)))) "Enigma trashed instantly")
          (is (= 4 (core/available-mu state)))
          (is (= 2 (count (:discard (get-runner)))) "Parasite trashed when Enigma was trashed")))))
  (testing "Trashed along with host ICE when its strength has been reduced to 0"
    (do-game
      (new-game {:corp {:deck [(qty "Enigma" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Parasite" 3) "Grimoire"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (let [enig (get-ice state :hq 0)]
        (core/rez state :corp enig)
        (take-credits state :corp)
        (play-from-hand state :runner "Grimoire")
        (play-from-hand state :runner "Parasite")
        (click-card state :runner enig)
        (let [psite (first (:hosted (refresh enig)))]
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (is (= 1 (:current-strength (refresh enig))) "Enigma reduced to 1 strength")
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 1 (count (:discard (get-corp)))) "Enigma trashed")
          (is (= 1 (count (:discard (get-runner)))) "Parasite trashed when Enigma was trashed"))))))
