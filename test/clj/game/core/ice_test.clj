(ns game.core.ice-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.test-framework :refer :all]))

(deftest auto-pump-and-break-test
  (testing "update after ice updates subs"
    (do-game
      (new-game {:corp {:hand ["Tour Guide" (qty "PAD Campaign" 2)]
                        :credits 10}
                 :runner {:hand ["Bukhgalter"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "Tour Guide" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Bukhgalter")
      (let [p1 (get-content state :remote1 0)
            p2 (get-content state :remote2 0)
            tg (get-ice state :hq 0)
            buk (get-program state 0)]
        (rez state :corp p1)
        (rez state :corp tg)
        (is (= 1 (count (:subroutines (refresh tg)))))
        (run-on state :hq)
        (is (= "Add 1 strength" (-> (refresh buk) :abilities last :label)) "Not encountered an ice yet")
        (rez state :corp p2)
        (run-continue state)
        (is (= "Fully break Tour Guide" (-> (refresh buk) :abilities last :label)))
        (is (= 2 (count (:subroutines (refresh tg))))))))
  (testing "Also works on second encounter"
    (do-game
      (new-game {:corp {:hand ["Tour Guide" (qty "PAD Campaign" 2)]
                        :credits 10}
                 :runner {:hand ["Bukhgalter"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "Tour Guide" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Bukhgalter")
      (let [p1 (get-content state :remote1 0)
            tg (get-ice state :hq 0)
            buk (get-program state 0)]
        (rez state :corp p1)
        (rez state :corp tg)
        (run-on state :hq)
        (run-continue state)
        (is (= "Fully break Tour Guide" (-> (refresh buk) :abilities last :label)))
        (fire-subs state tg)
        (take-credits state :runner)
        (take-credits state :corp)
        (run-on state :hq)
        (run-continue state)
        (is (= "Fully break Tour Guide" (-> (refresh buk) :abilities last :label))))))
  (testing "Breaking restrictions on auto-pump-and-break - No auto pumping if (:breakable sub) does not return :unrestricted"
    (do-game
      (new-game {:corp {:hand ["Afshar"]}
                 :runner {:hand ["Gordian Blade"]
                          :credits 10}})
      (play-from-hand state :corp "Afshar" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Gordian Blade")
      (run-on state :hq)
      (let [afshar (get-ice state :hq 0)
            gord (get-program state 0)]
        (rez state :corp afshar)
        (run-continue state)
        (is (empty? (filter #(= :auto-pump-and-break (:dynamic %)) (:abilities (refresh gord)))) "No auto break dynamic ability"))))
  (testing "Breaking restrictions on auto-pump-and-break - Auto pumping if (:breakable sub) returns :unrestricted"
    (do-game
      (new-game {:corp {:hand ["Afshar"]}
                 :runner {:hand ["Gordian Blade"]
                          :credits 10}})
      (play-from-hand state :corp "Afshar" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Gordian Blade")
      (run-on state :rd)
      (let [afshar (get-ice state :rd 0)
            gord (get-program state 0)]
        (rez state :corp afshar)
        (run-continue state)
        (is (not-empty (filter #(= :auto-pump-and-break (:dynamic %)) (:abilities (refresh gord)))) "Autobreak is active")
        (auto-pump-and-break state (refresh gord))
        (is (empty? (remove :broken (:subroutines (refresh afshar)))) "All subroutines broken"))))
  (testing "Auto break handles pump abilities with variable strength"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["DNA Tracker"]
                        :credits 20}
                 :runner {:deck [(qty "Unity" 3)]
                          :credits 20}})
      (play-from-hand state :corp "DNA Tracker" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Unity")
      (play-from-hand state :runner "Unity")
      (play-from-hand state :runner "Unity")
      (let [unity (get-program state 0)
            ice (get-ice state :hq 0)]
        (run-on state :hq)
        (run-continue state)
        (is (= "5 [Credits]" (get-in (refresh unity) [:abilities 2 :cost-label])) "Auto Break label lists cost as 5 credits")
        (is (changed? [(:credit (get-runner)) -5]
              (auto-pump-and-break state (refresh unity)))
            "Auto break costs 5")
        (is (= 7 (:current-strength (refresh unity))) "Unity's strength is 7 after pumping twice")
        (is (zero? (count (remove :broken (:subroutines (refresh ice))))) "All subroutines have been broken"))))
  (testing "Auto break handles break abilities with variable cost"
    (do-game
      (new-game {:runner {:hand [(qty "Marjanah" 2)]
                          :credits 20}
                 :corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]
                        :credits 20}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Marjanah")
      (run-on state :hq)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state :encounter-ice)
      (let [marjanah (get-program state 0)]
        (is (= "2 [Credits]" (get-in (refresh marjanah) [:abilities 2 :cost-label])) "Auto Break label lists cost as 2 credits")
        (is (changed? [(:credit (get-runner)) -2]
              (card-ability state :runner (refresh marjanah) 2))
            "Break costs 2")
        (run-continue state :movement)
        (run-continue state nil)
        (run-on state :hq)
        (run-continue state :encounter-ice)
        (is (= "1 [Credits]" (get-in (refresh marjanah) [:abilities 2 :cost-label])) "Auto Break label lists cost as 1 credit")
        (is (changed? [(:credit (get-runner)) -1]
              (card-ability state :runner (refresh marjanah) 2))
            "Break costs 1 after run"))))
  (testing "Basic auto pump test"
    (do-game
      (new-game {:runner {:hand ["Corroder"]
                          :credits 20}
                 :corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Fire Wall"]
                        :credits 20}})
      (play-from-hand state :corp "Fire Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (run-on state :hq)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state :encounter-ice)
      (let [corroder (get-program state 0)
            fire-wall (get-ice state :hq 0)]
        (is (not-empty (filter #(= :auto-pump (:dynamic %)) (:abilities (refresh corroder)))) "Auto pump is active")
        (is (= "3 [Credits]" (get-in (refresh corroder) [:abilities 3 :cost-label])) "Auto pump label lists cost as 3 credits")
        (is (changed? [(:credit (get-runner)) -3]
              (auto-pump state (refresh corroder)))
            "Pump costs 3")
        (is (= 5 (:current-strength (refresh corroder))) "Breaker strength equals ice strength")
        (is (not (some #{:broken} (:subroutines fire-wall))) "No subroutines have been broken")
        (is (empty? (filter #(= :auto-pump (:dynamic %)) (:abilities (refresh corroder)))) "No auto pump ability since breaker is at strength"))))
  (testing "Auto pump handles pump abilities with variable strength"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["DNA Tracker"]
                        :credits 20}
                 :runner {:deck [(qty "Unity" 3)]
                          :credits 20}})
      (play-from-hand state :corp "DNA Tracker" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Unity")
      (play-from-hand state :runner "Unity")
      (play-from-hand state :runner "Unity")
      (let [unity (get-program state 0)]
        (run-on state :hq)
        (run-continue state)
        (is (= "2 [Credits]" (get-in (refresh unity) [:abilities 3 :cost-label])) "Auto Break label lists cost as 2 credits")
        (is (changed? [(:credit (get-runner)) -2]
              (auto-pump state (refresh unity)))
            "Auto pump costs 2")
        (is (= 7 (:current-strength (refresh unity))) "Unity's strength is 7 after pumping twice"))))
  (testing "Auto pump available even with no active break ability"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["DNA Tracker"]
                        :credits 20}
                 :runner {:deck ["Utae"]
                          :credits 20}})
      (play-from-hand state :corp "DNA Tracker" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Utae")
      (let [utae (get-program state 0)]
        (run-on state :hq)
        (run-continue state)
        (is (not-empty (filter #(= :auto-pump (:dynamic %)) (:abilities (refresh utae)))) "Auto pump is active")
        (is (empty? (filter #(= :auto-pump-and-break (:dynamic %)) (:abilities (refresh utae)))) "No auto break dynamic ability")))))

(deftest bioroid-break-abilities-test
  ;; The click-to-break ablities on bioroids shouldn't create an undo-click
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Eli 1.0"]}})
    (play-from-hand state :corp "Eli 1.0" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 0))
    (run-continue state)
    (let [undo-click (:click-state @state)
          clicks (:click (get-runner))]
      (card-side-ability state :runner (get-ice state :hq 0) 0)
      (click-prompt state :runner "End the run")
      (is (= (dec clicks) (:click (get-runner))) "Runner has spent 1 click on the bioroid-break ability")
      (core/command-undo-click state :runner)
      (is (= (inc clicks) (:click (get-runner))) "Runner regains clicks spent on break ability and run")
      (is (not (:run @state)) "Undoing a click resets to before the run began"))))
