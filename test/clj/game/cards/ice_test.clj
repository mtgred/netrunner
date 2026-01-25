(ns game.cards.ice-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.utils-test :refer [error-wrapper is']]
   [game.test-framework :refer :all]))

(defn does-damage-sub-impl
  [cname sub damage]
  (let [state (subroutine-test cname sub {:runner {:hand [(qty "Ika" (inc damage))]}})]
    (is' (= (count (get-in @state [:runner :discard])) damage)
         (str cname " did " damage " damage"))
    state))

(defmacro does-damage-sub
  [cname sub damage]
  `(error-wrapper (does-damage-sub-impl ~cname ~sub ~damage)))

(defn etr-sub-impl
  [cname sub]
  (let [state (subroutine-test cname sub)]
    (is' (not (:run @state)) (str cname " ended the run"))
    state))

(defmacro etr-sub
  [cname sub]
  `(error-wrapper (etr-sub-impl ~cname ~sub)))

(deftest ablative-barrier-no-threat
  (do-game
    (new-game {:corp {:hand ["Ablative Barrier" "Ablative Barrier" "City Works Project"]
                      :discard ["Vanilla"]}})
    (play-from-hand state :corp "Ablative Barrier" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 0))
    (is (no-prompt? state :corp) "no prompt to pull a card with ablative")))

(deftest ablative-barrier-threat-trigger
  (do-game
    (new-game {:corp {:hand [(qty "Ablative Barrier" 2) "Ice Wall" "City Works Project"]
                      :discard ["Vanilla"]
                      :credits 10}})
    (play-from-hand state :corp "Ablative Barrier" "HQ")
    (play-from-hand state :corp "Ablative Barrier" "HQ")
    (play-and-score state "City Works Project")
    (take-credits state :corp)
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 1))
    (click-card state :corp "Vanilla")
    (click-prompt state :corp "Archives")
    (is (= "Vanilla" (:title (get-ice state :archives 0))))
    (run-continue state)
    (rez state :corp (get-ice state :hq 0))
    (click-card state :corp "Ice Wall")
    (click-prompt state :corp "R&D")))

(deftest afshar-subroutines
  ;; Subroutines
  (do-game
    (new-game {:corp {:hand ["Afshar"]}})
    (play-from-hand state :corp "Afshar" "HQ")
    (let [afshar (get-ice state :hq 0)]
      (take-credits state :corp)
      (run-on state "HQ")
      (rez state :corp afshar)
      (run-continue state)
      (card-subroutine state :corp afshar 0)
      (is (= 3 (:credit (get-runner))) "Runner should lose 2 credits")
      (card-subroutine state :corp afshar 1)
      (is (not (:run @state)) "Run is ended"))))

(deftest afshar-breaking-restriction
  ;; Breaking restriction
  (do-game
    (new-game {:corp {:hand ["Afshar"]}
               :runner {:hand ["Gordian Blade"]
                        :credits 10}})
    (play-from-hand state :corp "Afshar" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Gordian Blade")
    (let [afshar (get-ice state :hq 0)
          gord (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp afshar)
      (run-continue state)
      (is (empty? (filter #(:dynamic %) (:abilities (refresh gord)))) "No auto break dynamic ability")
      (card-ability state :runner gord 0)
      (click-prompt state :runner "End the run")
      (is (no-prompt? state :runner) "No prompt for further breaking")
      (card-ability state :runner gord 0)
      (is (no-prompt? state :runner) "Can't use break ability"))))

(deftest afshar-no-breaking-restriction-on-other-servers
  ;; No breaking restriction on other servers
  (do-game
    (new-game {:corp {:hand ["Afshar"]}
               :runner {:hand ["Gordian Blade"]
                        :credits 10}})
    (play-from-hand state :corp "Afshar" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Gordian Blade")
    (run-on state "R&D")
    (let [afshar (get-ice state :rd 0)
          gord (get-program state 0)]
      (rez state :corp afshar)
      (run-continue state)
      (card-ability state :runner gord 0)
      (click-prompt state :runner "End the run")
      (is (not (no-prompt? state :runner)) "Can break more subs")
      (click-prompt state :runner "Make the Runner lose 2 [Credits]"))))

(deftest afshar-breaking-restriction-also-on-the-second-encounter
  ;; Breaking restriction also on the second encounter
  (do-game
    (new-game {:corp {:hand ["Afshar"]}
               :runner {:hand ["Gordian Blade"]
                        :credits 10}})
    (play-from-hand state :corp "Afshar" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Gordian Blade")
    (run-on state "HQ")
    (let [afshar (get-ice state :hq 0)
          gord (get-program state 0)]
      (rez state :corp afshar)
      (run-continue state)
      (is (empty? (filter #(= :auto-pump-and-break (:dynamic %)) (:abilities (refresh gord)))) "No auto break dynamic ability")
      (card-ability state :runner gord 0)
      (click-prompt state :runner "Make the Runner lose 2 [Credits]")
      (fire-subs state afshar)
      (is (not (:run @state)) "Run ended")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :runner gord 0)
      (click-prompt state :runner "Make the Runner lose 2 [Credits]")
      (is (no-prompt? state :runner) "No prompt for further breaking")
      (card-ability state :runner gord 0)
      (is (no-prompt? state :runner) "Can't use break ability"))))

(deftest aimor
  ;; Aimor - trash the top 3 cards of the stack, trash Aimor
  (do-game
    (new-game {:corp {:deck ["Aimor"]}
               :runner {:deck ["Sure Gamble" "Desperado" "Corroder" "Patron"]
                        :hand ["Sure Gamble"]}})
    (play-from-hand state :corp "Aimor" "HQ")
    (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "Aimor installed")
    (take-credits state :corp)
    (let [aim (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp aim)
      (run-continue state)
      (card-subroutine state :corp aim 0)
      (is (= 3 (count (:discard (get-runner)))) "Runner trashed 3 cards")
      (is (= 1 (count (:deck (get-runner)))) "Runner has 1 card in deck")
      (is (nil? (refresh aim)) "Aimor is trashed"))))

(deftest akhet-akhet-gains-strength-at-3-advancements
  ;; Akhet gains strength at 3 advancements
  (do-game
    (new-game {:corp {:deck ["Akhet"]}})
    (play-from-hand state :corp "Akhet" "HQ")
    (core/gain state :corp :click 1 :credit 1)
    (let [akhet (get-ice state :hq 0)]
      (rez state :corp akhet)
      (is (= 0 (get-counters (refresh akhet) :advancement)) "Akhet has no adv tokens")
      (is (= 2 (get-strength (refresh akhet))) "Akhet starts at 2 strength")
      (dotimes [n 2]
        (advance state akhet)
        (is (= (inc n) (get-counters (refresh akhet) :advancement)) (str "Akhet has " (inc n) " adv tokens"))
        (is (= 2 (get-strength (refresh akhet))) "Akhet stays at 2 strength"))
      (advance state akhet)
      (is (= 3 (get-counters (refresh akhet) :advancement)) "Akhet has 3 adv tokens")
      (is (= 5 (get-strength (refresh akhet))) "Akhet is now at 5 strength"))))

(deftest akhet-akhet-subroutines
  ;; Akhet subroutines
  (do-game
    (new-game {:corp {:deck ["Akhet"]}})
    (play-from-hand state :corp "Akhet" "HQ")
    (take-credits state :corp)
    (let [akhet (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp akhet)
      (run-continue state)
      (fire-subs state akhet)
      (is (= 0 (get-counters (refresh akhet) :advancement)) "Akhet has no adv tokens")
      (click-card state :corp (refresh akhet))
      (is (= 1 (get-counters (refresh akhet) :advancement)) "Akhet gained 1 adv tokens")
      (is (not (:run @state)) "Run has ended"))))

(deftest akhet-breaking-restriction
  ;; Breaking restriction
  (do-game
    (new-game {:corp {:hand ["Akhet"]}
               :runner {:hand ["Corroder"]}})
    (play-from-hand state :corp "Akhet" "HQ")
    (let [akhet (get-ice state :hq 0)]
      (advance state akhet 2)
      (is (= 2 (get-counters (refresh akhet) :advancement)) "Akhet has 2 adv tokens")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (run-on state :hq)
      (let [cor (get-program state 0)]
        (rez state :corp (refresh akhet))
        (run-continue state)
        (card-ability state :runner cor 0)
        (click-prompt state :runner "End the run")
        (is (not (no-prompt? state :runner)) "Prompt to break second sub open")
        (click-prompt state :runner "Gain 1 [Credit]. Place 1 advancement counter")
        (is (no-prompt? state :runner) "Prompt now closed")
        (is (empty? (remove :broken (:subroutines (refresh akhet)))) "All subroutines broken")
        (run-continue state :movement)
        (run-jack-out state)
        (take-credits state :runner)
        (core/gain state :corp :credit 1)
        (advance state akhet)
        (is (= 3 (get-counters (refresh akhet) :advancement)) "Akhet now has 3 adv tokens")
        (take-credits state :corp)
        (core/gain state :runner :credit 5)
        (run-on state :hq)
        (run-continue state)
        (auto-pump state cor)
        (card-ability state :runner (refresh cor) 0)
        (click-prompt state :runner "End the run")
        (is (no-prompt? state :runner) "No option to break second sub")))))

(deftest anansi-3-net-damage-when-bypassing
  ;; 3 net damage when bypassing
  (do-game
    (new-game {:corp {:deck ["Anansi"]}
               :runner {:deck [(qty "Sure Gamble" 4) "Inside Job"]}})
    (play-from-hand state :corp "Anansi" "HQ")
    (core/gain state :corp :credit 8)
    (take-credits state :corp)
    (let [anansi (get-ice state :hq 0)]
      (play-from-hand state :runner "Inside Job")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (rez state :corp anansi)
      (is (changed? [(count (:hand (get-runner))) -3]
            (run-continue state))
          "3 net damage from passing Anansi"))))

(deftest anansi-no-net-damage-when-bypassing-and-derezzing-ice-with-capybara
  ;; No net damage when bypassing and derezzing with Capybara
  (do-game
    (new-game {:corp {:deck ["Anansi"]}
               :runner {:hand [(qty "Sure Gamble" 4) "Inside Job" "Capybara"]}})
    (play-from-hand state :corp "Anansi" "HQ")
    (core/gain state :corp :credit 8)
    (take-credits state :corp)
    (let [anansi (get-ice state :hq 0)]
      (play-from-hand state :runner "Capybara")
      (play-from-hand state :runner "Inside Job")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (rez state :corp anansi)
      (is (changed? [(count (:hand (get-runner))) 0]
            (run-continue state)
            (click-prompt state :runner "Yes"))
          "No net damage from bypassing and derezzing Anansi"))))

(deftest anansi-no-net-damage-when-breaking-all-subs
  ;; no net damage when breaking all subs
  (do-game
    (new-game {:corp {:deck ["Anansi"]
                      :credits 15}
               :runner {:deck [(qty "Sure Gamble" 4) "Mongoose"]
                        :credits 15}})
    (play-from-hand state :corp "Anansi" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Mongoose")
    (let [anansi (get-ice state :hq 0)
          mongoose (get-program state 0)]
      (run-on state :hq)
      (rez state :corp anansi)
      (run-continue state :encounter-ice)
      (auto-pump-and-break state mongoose)
      (is (changed? [(count (:hand (get-runner))) 0]
            (run-continue state))
          "3 net damage from passing Anansi"))))

(deftest anansi-anansi-and-border-control-issue-4769
  ;; Anansi and Border Control. Issue #4769
  (do-game
    (new-game {:corp {:hand ["Anansi" "Border Control"]
                      :credits 20}
               :runner {:hand [(qty "Sure Gamble" 6) "Corroder"]
                        :credits 90}})
    (play-from-hand state :corp "Border Control" "HQ")
    (play-from-hand state :corp "Anansi" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (let [anansi (get-ice state :hq 1)
          border (get-ice state :hq 0)
          corroder (get-program state 0)]
      (run-on state :hq)
      (rez state :corp anansi)
      (run-continue state :encounter-ice)
      (is (changed? [(count (:hand (get-runner))) -3]
            (run-continue state :movement))
          "3 net damage from passing Anansi")
      (rez state :corp border)
      (run-continue state :approach-ice)
      (run-continue state :encounter-ice)
      (auto-pump-and-break state corroder)
      (is (changed? [(count (:hand (get-runner))) 0]
            (card-ability state :corp (refresh border) 0))
          "No further net damage")
      (is (nil? (get-run)) "Run ended"))))

(deftest anansi-runner-has-to-pay-2c-to-draw-card-issue-5335
  ;; Runner has to pay 2c to draw card. Issue #5335
  (do-game
    (new-game {:corp {:hand ["Anansi"]
                      :credits 15}
               :runner {:credits 0
                        :deck [(qty "Sure Gamble" 5)]
                        :hand [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Anansi" "HQ")
    (take-credits state :corp)
    (let [anansi (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp anansi)
      (run-continue state)
      (card-subroutine state :corp anansi 1)
      (click-prompt state :corp "No")
      (is (changed? [(count (:hand (get-runner))) 0]
            (is (= ["No"] (prompt-buttons :runner)) "No option to pay")
            (click-prompt state :runner "No"))
          "No new card from Anansi")
      (is (no-prompt? state :corp) "corp has no prompts from Anansi"))))

(deftest anansi-2nd-sub-test-runner-clicks-yes
  ;; 2nd sub test - runner clicks YES
  (do-game
    (new-game {:corp {:hand ["Anansi"]
                      :credits 15}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Anansi" "HQ")
    (take-credits state :corp)
    (let [anansi (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp anansi)
      (run-continue state)
      (card-subroutine state :corp anansi 1)
      (click-prompt state :corp "No")
      (is (changed? [(count (:hand (get-runner))) 1]
            (click-prompt state :runner "Yes"))
          "New card from Anansi")
      (is (no-prompt? state :corp) "corp has no prompts from Anansi"))))

(deftest anansi-2nd-sub-test-runner-clicks-no
  ;; 2nd sub test - runner clicks NO
  (do-game
    (new-game {:corp {:hand ["Anansi"]
                      :credits 15}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Anansi" "HQ")
    (take-credits state :corp)
    (let [anansi (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp anansi)
      (run-continue state)
      (card-subroutine state :corp anansi 1)
      (click-prompt state :corp "No")
      (is (changed? [(count (:hand (get-runner))) 0]
            (click-prompt state :runner "No"))
          "No new card from Anansi")
      (is (no-prompt? state :corp) "corp has no prompts from Anansi"))))

(deftest anemone-happy-path
  ;; Anemone
  (do-game
    (new-game {:corp {:hand [(qty "Anemone" 2) "Hedge Fund"]
                      :credits 50}
               :runner {:hand [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Anemone" "HQ")
    (let [anem (get-ice state :hq 0)]
      (take-credits state :corp)
      (is (changed? [(count (:discard (get-runner))) 2
             (count (:discard (get-corp))) 1]
            (run-on state :hq)
            (rez state :corp anem)
            (click-prompt state :corp "Yes")
            (is (= :select (prompt-type :corp)))
            (click-card state :corp "Hedge Fund"))
          "Runner takes 2 net damage"))))

(deftest anemone-wrong-server
  (do-game
    (new-game {:corp {:hand [(qty "Anemone" 2) "Hedge Fund"]
                      :credits 50}
               :runner {:hand [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Anemone" "HQ")
    (let [anem (get-ice state :hq 0)]
      (take-credits state :corp)
      (run-on state :rd)
      (rez state :corp anem)
      (is (no-prompt? state :corp) "Anemone not active outside attacked server"))))

(deftest anemone-outside-run
  (do-game
    (new-game {:corp {:hand [(qty "Anemone" 2) "Hedge Fund"]
                      :credits 50}
               :runner {:hand [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Anemone" "HQ")
    (let [anem (get-ice state :hq 0)]
      (rez state :corp anem)
      (is (no-prompt? state :corp) "Anemone not active outside run"))))

(deftest anemone-cant-afford
  (do-game
    (new-game {:corp {:hand ["Anemone"]
                      :credits 50}
               :runner {:hand [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Anemone" "HQ")
    (let [anem (get-ice state :hq 0)]
      (take-credits state :corp)
      (run-on state :hq)
      (rez state :corp anem)
      (is (no-prompt? state :corp) "Anemone not active if corp can't pay the cost"))))

(deftest ansel-1-0
  ;; Ansel 1.0
  (before-each
    [state (new-game {:corp {:hand ["Ansel 1.0" "NGO Front" "Merger"]
                             :discard ["Adonis Campaign"]
                             :credits 100}
                      :runner {:hand ["Corroder" "Trick Shot"]
                               :credits 100}})
     _ (do (core/gain state :corp :click 100)
           (play-from-hand state :corp "Ansel 1.0" "New remote"))
     ansel (get-ice state :remote1 0)]
    (testing "Has 1.0 Bioroid runner ability"
      (do-game state
        (take-credits state :corp)
        (run-on state :remote1)
        (rez state :corp ansel)
        (run-continue state :encounter-ice)
        (is (changed? [(:click (get-runner)) -1]
              (card-side-ability state :runner (refresh ansel) 0)
              (is (= "Break a subroutine" (:msg (prompt-map :runner))))
              (click-prompt state :runner "Trash an installed Runner card"))
            "Runner loses a single click")
        (is (:broken (first (:subroutines (refresh ansel)))))))
    (testing "First sub is trash installed card"
      (do-game state
        (take-credits state :corp)
        (play-from-hand state :runner "Corroder")
        (run-on state :remote1)
        (rez state :corp ansel)
        (run-continue state :encounter-ice)
        (card-subroutine state :corp ansel 0)
        (is (= "Choose an installed card to trash" (:msg (prompt-map :corp))))
        (is (= :select (prompt-type :corp)))
        (click-card state :corp "Corroder")
        (is (not (find-card "Corroder" (get-program state))))
        (is (find-card "Corroder" (:discard (get-runner))))))
    (testing "Second sub is install from hq or archives: HQ"
      (do-game state
        (take-credits state :corp)
        (run-on state :remote1)
        (rez state :corp ansel)
        (run-continue state :encounter-ice)
        (card-subroutine state :corp ansel 1)
        (is (= "Choose a card to install from HQ or Archives" (:msg (prompt-map :corp))))
        (is (= :select (prompt-type :corp)))
        (click-card state :corp "NGO Front")
        (is (= "Choose a location to install NGO Front" (:msg (prompt-map :corp))))
        (click-prompt state :corp "New remote")
        (is (= "NGO Front" (:title (get-content state :remote2 0))))
        (is (not (find-card "NGO Front" (:hand (get-corp)))))))
    (testing "Second sub is install from hq or archives: Archives"
      (do-game state
        (take-credits state :corp)
        (run-on state :remote1)
        (rez state :corp ansel)
        (run-continue state :encounter-ice)
        (card-subroutine state :corp ansel 1)
        (is (= "Choose a card to install from HQ or Archives" (:msg (prompt-map :corp))))
        (is (= :select (prompt-type :corp)))
        (click-card state :corp "Adonis Campaign")
        (is (= "Choose a location to install Adonis Campaign" (:msg (prompt-map :corp))))
        (click-prompt state :corp "New remote")
        (is (= "Adonis Campaign" (:title (get-content state :remote2 0))))
        (is (not (find-card "Adonis Campaign" (:discard (get-corp)))))))
    (testing "Third sub blocks stealing and trashing: stealing"
      (do-game state
        (play-from-hand state :corp "Merger" "Server 1")
        (take-credits state :corp)
        (run-on state :remote1)
        (rez state :corp ansel)
        (run-continue state :encounter-ice)
        (card-subroutine state :corp ansel 2)
        (is (last-log-contains? state "prevent the Runner from stealing or trashing"))
        (run-continue state :movement)
        (run-continue state :success)
        (is (accessing state "Merger"))
        (is (= ["No action"] (prompt-buttons :runner)))))
    (testing "Third sub blocks stealing and trashing: trashing"
      (do-game state
        (play-from-hand state :corp "NGO Front" "Server 1")
        (take-credits state :corp)
        (run-on state :remote1)
        (rez state :corp ansel)
        (run-continue state :encounter-ice)
        (card-subroutine state :corp ansel 2)
        (is (last-log-contains? state "prevent the Runner from stealing or trashing"))
        (run-continue state :movement)
        (run-continue state :success)
        (is (accessing state "NGO Front"))
        (is (= ["No action"] (prompt-buttons :runner))))))
  (testing "preventing trash with Ansel, issue #7343"
    (do-game (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                               :hand ["Spin Doctor" "Ansel 1.0"]
                               :credits 10}
                        :runner {:hand ["Trick Shot"]
                                 :credits 10}})
      (play-from-hand state :corp "Ansel 1.0" "R&D")
      (play-from-hand state :corp "Spin Doctor" "New remote")
      (take-credits state :corp)
      (let [ansel (get-ice state :rd 0)]
        (play-from-hand state :runner "Trick Shot")
        (run-continue state)
        (rez state :corp ansel)
        (run-continue state :encounter-ice)
        (fire-subs state (refresh ansel))
        ;;(click-prompt state :corp "Done")
        (click-prompt state :corp "Done")
        (is (last-log-contains? state "cannot steal or trash"))
        (run-continue-until state :success)
        (is (last-log-contains? state "accesses an unseen card"))
        (is (= ["No action"] (prompt-buttons :runner))
            "Ansel's third sub applies during the initial run")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Server 1")
        (run-continue-until state :success)
        (is (= ["Pay 2 [Credits] to trash" "No action"] (prompt-buttons :runner))
            "Ansel's third sub doesn't last into 'end of run' runs")))))

(deftest ansel-1-0-vs-run-amok
  (do-game
    (new-game {:corp {:hand ["Ansel 1.0"]}
               :runner {:hand ["Run Amok"]}})
    (play-from-hand state :corp "Ansel 1.0" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Run Amok")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (rez state :corp (get-ice state :hq 0))
    (run-continue state :encounter-ice)
    (card-subroutine state :corp (get-ice state :hq 0) 2)
    (is (last-log-contains? state "prevent the Runner from stealing or trashing"))
    (run-continue state :movement)
    (run-jack-out state)
    (click-card state :runner "Ansel 1.0")
    (is (= 1 (count (:discard (get-corp)))) "Trashed ansel")))

(deftest ansel-1-0-vs-virtuoso
  (do-game
    (new-game {:corp {:hand ["Ansel 1.0" "Hostile Takeover"]}
               :runner {:hand ["Virtuoso"]}})
    (play-from-hand state :corp "Ansel 1.0" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Virtuoso")
    (run-on state :archives)
    (rez state :corp (get-ice state :archives 0))
    (run-continue state :encounter-ice)
    (card-subroutine state :corp (get-ice state :archives 0) 2)
    (is (last-log-contains? state "prevent the Runner from stealing or trashing"))
    (core/set-mark state :archives)
    (run-continue-until state :success)
    (click-prompt state :runner "Steal")
    (is (= 1 (count (:scored (get-runner)))) "Stole agenda")))

(deftest ansel-1-0-access-after-no-steal
  (do-game
    (new-game {:corp {:hand ["Ansel 1.0" "Ganked!" "Merger"]
                      :discard ["Adonis Campaign"]
                      :credits 100}
               :runner {:hand ["Corroder"]
                        :credits 100}})
    (core/gain state :corp :click 100)
    (play-from-hand state :corp "Ansel 1.0" "New remote")
    (play-from-hand state :corp "Ganked!" "Server 1")
    (play-from-hand state :corp "Merger" "Server 1")
    (take-credits state :corp)
    (let [ansel (get-ice state :remote1 0)]
      (run-on state :remote1)
      (rez state :corp ansel)
      (run-continue state :encounter-ice)
      (card-subroutine state :corp ansel 2)
      (is (last-log-contains? state "prevent the Runner from stealing or trashing"))
      (run-continue state :movement)
      (run-continue state :success)
      (click-card state :runner "Merger")
      (is (accessing state "Merger"))
      (is (= ["No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (waiting? state :runner)
          "Runner has prompt to wait for Corp to use Ganked!"))))

(deftest anvil
  (do-game
    (new-game {:corp {:hand ["Anvil" "Ice Wall"]}
               :runner {:hand ["Unity"]
                        :credits 50}})
    (play-from-hand state :corp "Anvil" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Unity")
    (let [unity (get-program state 0)
          anvil (get-ice state :hq 0)
          iwall (get-ice state :rd 0)]
      (run-on state :hq)
      (rez state :corp (refresh anvil))
      (run-continue state)
      (click-prompt state :corp "No")
      (is (changed? [(:credit (get-runner)) -4]
            (auto-pump-and-break state unity)
            (core/process-action "continue" state :corp nil))
          "spent 4 to break")
      (run-jack-out state)
      (run-on state :hq)
      (run-continue state :encounter-ice)
      (click-prompt state :corp "Yes")
      (click-card state :corp (refresh iwall))
      (is (changed? [(:credit (get-runner)) 0]
            (auto-pump-and-break state unity))
          "can't break")
      (is (changed? [(:credit (get-corp)) 1
             (:credit (get-runner)) -1]
            (fire-subs state (core/get-current-ice state)))
          "gained 1")
      (click-card state :runner (refresh unity))
      (is (= 1 (count (:discard (get-runner)))) "trashed unity"))))

(deftest archangel
  ;; Archangel - accessing from R&D does not cause run to hang.
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Archangel"]}
               :runner {:hand ["Bank Job"]}})
    (play-from-hand state :corp "Archangel" "HQ")
    (let [archangel (get-ice state :hq 0)]
      (take-credits state :corp)
      (rez state :corp archangel)
      (play-from-hand state :runner "Bank Job")
      (run-on state "HQ")
      (run-continue state)
      (card-subroutine state :corp archangel 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp (get-resource state 0))
      (is (nil? (get-resource state 0)) "Bank Job is trashed"))))

(deftest archangel-access-test
  ;; Access test
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Archangel"]}
               :runner {:hand ["Bank Job"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Bank Job")
    (run-empty-server state "HQ")
    (click-prompt state :corp "Yes")
    (is (= "Archangel" (:title (core/get-current-ice state))) "The Runner is encountering Archangel")
    (fire-subs state (core/get-current-ice state))
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-resource state 0))
    (is (nil? (get-resource state 0)) "Bank Job is trashed")
    (encounter-continue state)
    (is (accessing state "Archangel") "Return to accessing Archangel")))

(deftest architect-architect-is-untrashable-while-installed-and-rezzed-but-trashable-if-derezzed-or-from-hq
  ;; Architect is untrashable while installed and rezzed, but trashable if derezzed or from HQ
  (do-game
    (new-game {:corp {:deck [(qty "Architect" 3)]}})
    (play-from-hand state :corp "Architect" "HQ")
    (let [architect (get-ice state :hq 0)]
      (rez state :corp architect)
      (trash state :corp (refresh architect))
      (is (get-ice state :hq 0) "Architect was trashed, but should be untrashable")
      (derez state :corp (refresh architect))
      (trash state :corp (refresh architect))
      (is (nil? (get-ice state :hq 0)) "Architect was not trashed, but should be trashable")
      (trash state :corp (get-in @state [:corp :hand 0]))
      (is (= (get-in @state [:corp :discard 0 :title]) "Architect"))
      (is (= (get-in @state [:corp :discard 1 :title]) "Architect")))))

(deftest ashigaru-gaining-losing-subs
  ;; Gaining/losing subs
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ashigaru"]
                      :credits 9}})
    (play-from-hand state :corp "Ashigaru" "HQ")
    (let [ashigaru (get-ice state :hq 0)]
      (rez state :corp ashigaru)
      (is (zero? (count (:subroutines (refresh ashigaru)))))
      (draw state :corp 1)
      (is (= 1 (count (:subroutines (refresh ashigaru)))))
      (draw state :corp 1)
      (is (= 2 (count (:subroutines (refresh ashigaru)))))
      (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (core/fake-checkpoint state)
      (is (zero? (count (:subroutines (refresh ashigaru))))))))

(deftest ashigaru-sub-is-etr
  ;; Sub is ETR
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ashigaru" "Hedge Fund"]
                      :credits 9}})
    (play-from-hand state :corp "Ashigaru" "HQ")
    (let [ashigaru (get-ice state :hq 0)]
      (rez state :corp ashigaru)
      (is (= 1 (count (:subroutines (refresh ashigaru)))))
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (fire-subs state ashigaru)
      (is (nil? (:run @state)) "Sub is ETR"))))

(deftest asteroid-belt
  ;; Asteroid Belt - Space ice rez cost reduced by 3 credits per advancement
  (do-game
    (new-game {:corp {:deck ["Asteroid Belt"]
                      :credits 10}})
    (play-from-hand state :corp "Asteroid Belt" "HQ")
    (let [ab (get-ice state :hq 0)]
      (advance state ab 2)
      (is (= 8 (:credit (get-corp))))
      (is (= 2 (get-counters (refresh ab) :advancement)))
      (rez state :corp (refresh ab))
      (is (= 5 (:credit (get-corp))) "Paid 3 credits to rez; 2 advancments on Asteroid Belt"))))

(deftest attini
  (do-game
    (new-game {:corp {:hand ["Attini"]
                      :credits 10}
               :runner {:hand ["Sure Gamble" "Caldera"]
                        :credits 10}})
    (play-from-hand state :corp "Attini" "Archives")
    (take-credits state :corp)
    (let [att (get-ice state :archives 0)]
      (play-from-hand state :runner "Caldera")
      (run-on state "Archives")
      (rez state :corp (refresh att))
      (run-continue state)
      (fire-subs state att)
      (is (changed? [(:credit (get-runner)) -2]
            (click-prompt state :runner "Pay 2 [Credits]"))
          "Runner lost 2 credits")
      (is (changed? [(count (:hand (get-runner))) 0]
            (click-prompt state :runner "Take 1 net damage")
            (click-prompt state :runner "Caldera"))
          "Runner prevented 1 net damage")
      (is (changed? [(count (:hand (get-runner))) -1]
            (click-prompt state :runner "Take 1 net damage"))
          "Runner got 1 damage"))))

(deftest attini-threat-ability
  (do-game
    (new-game {:corp {:hand ["Attini"]
                      :score-area  ["Obokata Protocol"]
                      :credits 10}
               :runner {:hand [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Attini" "HQ")
    (take-credits state :corp)
    (let [att (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp (refresh att))
      (run-continue state)
      (is (changed? [(count (:hand (get-runner))) -3]
            (fire-subs state att))
          "Runner took 3 damage and couldn't choose to spend credits"))))

(deftest attini-threat-ability-vs-hush
  (do-game
    (new-game {:corp {:hand ["Attini"]
                      :score-area  ["Obokata Protocol"]
                      :credits 10}
               :runner {:hand ["Hush" (qty "Sure Gamble" 3)]
                        :credits 7}})
    (play-from-hand state :corp "Attini" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Hush")
    (click-card state :runner "Attini")
    (let [att (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp (refresh att))
      (run-continue state)
      (is (changed? [(count (:hand (get-runner))) 0]
            (fire-subs state (refresh att))
            (dotimes [_ 3]
              (is (changed? [(:credit (get-runner)) -2]
                    (click-prompt state :runner "Pay 2 [Credits]"))
                  "Paid 2 to not take a net")))
          "Took 0 net (paid it off)"))))

(deftest attini-threat-vs-prana-condenser
  (do-game
    (new-game {:corp {:hand ["Attini" "Prāna Condenser" "Obokata Protocol"]
                      :credits 10}})
    (play-from-hand state :corp "Prāna Condenser" "New remote")
    (play-from-hand state :corp "Attini" "HQ")
    (play-and-score state "Obokata Protocol")
    (let [att (get-ice state :hq 0)
          pc (get-content state :remote1 0)]
      (rez state :corp pc)
      (take-credits state :corp)
      (run-on state :hq)
      (rez state :corp att)
      (run-continue state :encounter-ice)
      (fire-subs state (refresh att))
      (dotimes [_ 3]
        (is (changed? [(:credit (get-corp)) 3]
              (click-prompt state :corp "Prāna Condenser"))
            "prevented 1 net with prana")))))

(deftest attini-threat-ability-cannot-spend-credits
  (do-game
    (new-game {:corp {:hand ["Attini" "Obokata Protocol"]
                      :credits 10}
               :runner {:hand [(qty "Sure Gamble" 3) "Caldera"]}})
    (play-and-score state "Obokata Protocol")
    (play-from-hand state :corp "Attini" "HQ")
    (take-credits state :corp)
    (let [att (get-ice state :hq 0)]
      (play-from-hand state :runner "Caldera")
      (run-on state "HQ")
      (rez state :corp att)
      (run-continue state)
      (is (changed?
            [(count (:hand (get-runner))) -3]
            (fire-subs state (refresh att))
            (is (no-prompt? state :runner) "Not prompted :)"))
          "Runner took 3 damage and couldn't prevent any of them by spending credits"))))

(deftest authenticator-encounter-decline-to-take-tag
  (do-game
    (new-game{:corp {:hand ["Authenticator"]}})
    (play-from-hand state :corp "Authenticator" "HQ")
    (take-credits state :corp)
    (let [ath (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp ath)
      (run-continue state)
      (is (= 0 (count-tags state)))
      (click-prompt state :runner "No")
      (is (= 0 (count-tags state)) "Authenticator did not give a tag")
      (is (= :encounter-ice (:phase (:run @state))) "Authenticator has not been bypassed"))))

(deftest authenticator-encounter-take-tag-to-bypass
  (do-game
    (new-game{:corp {:hand ["Authenticator"]}})
    (play-from-hand state :corp "Authenticator" "HQ")
    (take-credits state :corp)
    (let [ath (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp ath)
      (run-continue state)
      (is (= 0 (count-tags state)))
      (click-prompt state :runner "Yes")
      (is (= 1 (count-tags state)))
      (is (= :movement (:phase (:run @state))) "Authenticator has been bypassed"))))

(deftest authenticator-encounter-jesminder-fizzles
  (do-game
    (new-game {:corp {:hand ["Authenticator"]}
               :runner {:id "Jesminder Sareen: Girl Behind the Curtain"
                        :hand ["Dorm Computer"]}})
    (play-from-hand state :corp "Authenticator" "HQ")
    (take-credits state :corp)
    (let [ath (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp ath)
      (run-continue state)
      (is (= :encounter-ice (:phase (:run @state))) "Authenticator has been encountered")
      (is (not (= "Take 1 tag to bypass?" (:msg (prompt-map :runner))))
          "No prompt to bypass authenticator because we are tag-immune"))))

(deftest authenticator-encounter-qianju-fizzles
  (do-game
    (new-game {:corp {:hand ["Authenticator"]}
               :runner {:hand ["Qianju PT"]}})
    (play-from-hand state :corp "Authenticator" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Qianju PT")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (:runner-phase-12 @state) "Runner in Step 1.2")
    (let [pt (get-hardware state 0)
          ath (get-ice state :hq 0)]
      (card-ability state :runner pt 0)
      (end-phase-12 state :runner)
      (is (= 3 (:click (get-runner))) "Spent 1 click on Qianju PT")
      (run-on state :hq)
      (rez state :corp ath)
      (run-continue state)
      (is (= :encounter-ice (:phase (:run @state))) "Authenticator has been encountered")
      (is (not (= "Take 1 tag to bypass?" (:msg (prompt-map :runner))))
          "No prompt to bypass authenticator because we are tag-immune")
      (fire-subs state ath)
      (take-credits state :runner)
      (take-credits state :corp)
      (end-phase-12 state :runner)
      (run-on state :hq)
      (run-continue state)
      (is (= 0 (count-tags state)))
      (click-prompt state :runner "Yes")
      (is (= 1 (count-tags state)))
      (is (= :movement (:phase (:run @state))) "Authenticator has been bypassed"))))

(deftest authenticator-encounter-dorm-computer-fizzles
  (do-game
    (new-game {:corp {:hand ["Authenticator"]}
               :runner {:hand ["Dorm Computer"]}})
    (play-from-hand state :corp "Authenticator" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Dorm Computer")
    (let [dorm (get-hardware state 0)
          ath (get-ice state :hq 0)]
      (card-ability state :runner dorm 0)
      (click-prompt state :runner "HQ")
      (run-continue state)
      (is (= :approach-ice (:phase (get-run))) "Run is in approach phase")
      (rez state :corp ath)
      (run-continue state)
      (is (= :encounter-ice (:phase (:run @state))) "Authenticator has been encountered")
      (is (not (= "Take 1 tag to bypass?" (:msg (prompt-map :runner))))
          "No prompt to bypass authenticator because we are tag-immune"))))

(deftest bailiff-gain-credit-when-broken
  (do-game
    (new-game {:corp {:hand ["Bailiff"]}
               :runner {:hand ["Corroder"]}})
    (play-from-hand state :corp "Bailiff" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (let [blf (get-ice state :hq 0)
          cor (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp blf)
      (run-continue state)
      (is (changed? [(:credit (get-corp)) +1]
            (card-ability state :runner cor 0)
            (click-prompt state :runner "End the run")
            (is (last-log-contains? state "Corp uses Bailiff to gain 1 [Credits]")
            "Correct message"))
          "Gained 1c from subroutines being broken"))))

(deftest bailiff-interaction-with-hippo
  (do-game
    (new-game {:corp {:hand ["Bailiff"]}
               :runner {:hand ["Corroder" "Hippo"]}})
    (play-from-hand state :corp "Bailiff" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Hippo")
    (let [blf (get-ice state :hq 0)
          cor (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp blf)
      (run-continue state)
      (is (changed? [(:credit (get-corp)) 0]
            (auto-pump-and-break state cor)
            (click-prompt state :runner "Yes"))
          "Never gained money from bailiff"))))

(deftest bailiff-interaction-with-hippo-sub-boost-with-cleaver
  (do-game
    (new-game {:corp {:hand ["Bailiff" "Sub Boost"]}
               :runner {:hand ["Cleaver" "Hippo"]
                        :credits 6}})
    (play-from-hand state :corp "Bailiff" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (play-from-hand state :corp "Sub Boost")
    (click-card state :corp (refresh (get-ice state :hq 0)))
    (take-credits state :corp)
    (play-from-hand state :runner "Cleaver")
    (play-from-hand state :runner "Hippo")
    (let [cor (get-program state 0)]
      (run-on state "HQ")
      (run-continue state)
      (is (changed? [(:credit (get-corp)) +0]
            (auto-pump-and-break state cor)
            (click-prompt state :runner "Yes"))
          "Gained 0 credits from bailiff + sub boost being broken with cleaver + hippo"))))

(deftest bailiff-interaction-with-hippo-sub-boost-with-corroder
  (do-game
    (new-game {:corp {:hand ["Bailiff" "Sub Boost"]}
               :runner {:hand ["Corroder" "Hippo"]
                        :credits 6}})
    (play-from-hand state :corp "Bailiff" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (play-from-hand state :corp "Sub Boost")
    (click-card state :corp (refresh (get-ice state :hq 0)))
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Hippo")
    (let [cor (get-program state 0)]
      (run-on state "HQ")
      (run-continue state)
      (is (changed? [(:credit (get-corp)) +1]
            (auto-pump-and-break state (refresh cor))
            (click-prompt state :runner "Yes"))
          "Gained 1 credit from bailiff + sub boost being broken with corroder + hippo"))))

(deftest bailiff-sub-boost-auto-break
  (do-game
    (new-game {:corp {:hand["Bailiff" "Sub Boost"]}
               :runner {:hand ["Corroder"]}})
    (play-from-hand state :corp "Bailiff" "HQ")
    (let [blf (get-ice state :hq 0)]
      (rez state :corp blf)
      (play-from-hand state :corp "Sub Boost")
      (click-card state :corp (refresh blf))
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (let [cor (get-program state 0)]
        (run-on state "HQ")
        (run-continue state)
        (is (changed? [(:credit (get-corp)) +2]
              (auto-pump-and-break state cor))
            "Gained 2c from the runner breaking")
        (is (last-n-log-contains? state 2 "Corp uses Bailiff to gain 1 [Credits]"))
        (is (last-n-log-contains? state 3 "Corp uses Bailiff to gain 1 [Credits]"))))))

(deftest ballista
  ;; Ballista
  (do-game
    (new-game {:corp {:hand ["Ballista" "Hedge Fund" "Ice Wall"]}
               :runner {:hand ["Datasucker"]}})
    (play-from-hand state :corp "Ballista" "HQ")
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :corp)
    (play-from-hand state :runner "Datasucker")
    (let [ball (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp (refresh ball))
      (run-continue state)
      (fire-subs state ball)
      (is (= ["Trash a program" "End the run"] (prompt-buttons :corp)) "Corp should have 2 options")
      (click-prompt state :corp "End the run")
      (is (not (:run @state)) "Run ended")
      (run-on state "HQ")
      (run-continue state)
      (fire-subs state ball)
      (is (= ["Trash a program" "End the run"] (prompt-buttons :corp)) "Corp should have 2 options")
      (click-prompt state :corp "Trash a program")
      (click-card state :corp "Datasucker")
      (is (nil? (get-program state 0)) "Datasucker is trashed")
      (is (:run @state) "Run continues")
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "No action")
      (run-on state "HQ")
      (run-continue state)
      (fire-subs state ball)
      (is (= ["Do nothing" "End the run"] (prompt-buttons :corp)) "Corp should have 2 options, but it's clear the first does nothing")
      (click-prompt state :corp "End the run")
      (is (not (:run @state)) "Run ended"))))

(deftest bandwidth
  ;; Bandwidth - Give the Runner 1 tag; remove 1 tag if the run is successful
  (do-game
    (new-game {:corp {:deck ["Bandwidth"]}})
    (play-from-hand state :corp "Bandwidth" "Archives")
    (let [bw (get-ice state :archives 0)]
      (take-credits state :corp)
      (run-on state "Archives")
      (rez state :corp bw)
      (run-continue state)
      (card-subroutine state :corp bw 0)
      (is (= 1 (count-tags state)) "Runner took 1 tag")
      (run-continue state)
      (run-continue state)
      (is (zero? (count-tags state)) "Run successful; Runner lost 1 tag")
      (run-on state "Archives")
      (run-continue state)
      (card-subroutine state :corp bw 0)
      (is (= 1 (count-tags state)) "Runner took 1 tag")
      (run-continue state :movement)
      (run-jack-out state)
      (is (= 1 (count-tags state)) "Run unsuccessful; Runner kept 1 tag"))))

(deftest bathynomus
  ;; Crick - Strength boost when protecting Archives; installs a card from Archives
  (do-game
    (new-game {:corp {:hand [(qty "Bathynomus" 2)]}
               :runner {:hand [(qty "Sure Gamble" 4)]}})
    (play-from-hand state :corp "Bathynomus" "HQ")
    (play-from-hand state :corp "Bathynomus" "Archives")
    (take-credits state :corp)
    (let [ba1 (get-ice state :hq 0)
          ba2 (get-ice state :archives 0)]
      (rez state :corp ba1)
      (rez state :corp ba2)
      (is (= 1 (get-strength (refresh ba1))) "Normal strength over HQ")
      (is (= 4 (get-strength (refresh ba2))) "+3 strength over Archives")
      (run-on state "Archives")
      (run-continue state)
      (card-subroutine state :corp ba2 0)
      (is (= 3 (count (:discard (get-runner)))) "Runner suffered 2 net damage"))))


(deftest biawak-discount
  (do-game
    (new-game {:corp {:score-area ["Project Atlas"]
                      :credits 5
                      :hand ["Biawak"]}})
    (play-from-hand state :corp "Biawak" "HQ")
    (rez state :corp (get-ice state :hq 0) {:expect-rez false})
    (is (changed? [(:credit (get-corp)) -4]
          (click-prompts state :corp "Biawak" "Project Atlas"))
        "Paid 4 + atlas to rez")
    (is (rezzed? (get-ice state :hq 0)) "No rezzed")))

(deftest biawak-subs-tests
  ;; trash program or etr
  (do-game
    (subroutine-test "Biawak" 0 nil {:rig ["Fermenter"] :disable true})
    (click-prompt state :corp "Trash a program")
    (click-card state :corp "Fermenter")
    (is (= 1 (count (:discard (get-runner)))) (str "Trashed prog")))
  ;; trash hardware or etr
  (do-game
    (subroutine-test "Biawak" 1 nil {:rig ["Kati Jones"] :disable true})
    (click-prompt state :corp "Trash a resource")
    (click-card state :corp "Kati Jones")
    (is (= 1 (count (:discard (get-runner)))) (str "Trashed kati")))
  ;; etr
  (do-game (etr-sub "Biawak" 2)))

(deftest biawak-fire-all-subs
  (do-game
    (run-and-encounter-ice-test "Biawak" nil {:rig ["Fermenter" "Daily Casts"]})
    (fire-subs state (get-ice state :hq 0))
    (click-prompts state :corp "Trash a program" "Fermenter" "Trash a resource" "Daily Casts")
    (is (not (:run @state)) "Run ended")
    (is (no-prompt? state :runner))))

(deftest blockchain-face-up-transactions
  ;; Face up transactions
  (do-game
    (new-game {:corp {:deck ["Blockchain" (qty "Beanstalk Royalties" 5)]
                      :credits 7}})
    (core/gain state :corp :click 5)
    (play-from-hand state :corp "Blockchain" "HQ")
    (let [bc (get-ice state :hq 0)]
      (rez state :corp bc)
      (is (= 2 (count (:subroutines (refresh bc)))) "No subroutines gained because no Transactions are in Archives")
      (play-from-hand state :corp "Beanstalk Royalties")
      (is (= 2 (count (:subroutines (refresh bc)))) "No subroutines gained because only 1 Transaction is in Archives")
      (play-from-hand state :corp "Beanstalk Royalties")
      (is (= 3 (count (:subroutines (refresh bc)))) "1 subroutine gained because 2 Transactions are in Archives")
      (play-from-hand state :corp "Beanstalk Royalties")
      (is (= 3 (count (:subroutines (refresh bc)))) "1 subroutine gained because 3 Transactions are in Archives")
      (play-from-hand state :corp "Beanstalk Royalties")
      (is (= 4 (count (:subroutines (refresh bc)))) "2 subroutines gained because 4 Transactions are in Archives")
      (is (= 12 (:credit (get-corp))) "Corp has 12 credits from four Beanstalks")
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (let [credits (:credit (get-corp))]
        (card-subroutine state :corp bc 0)
        (is (= (inc credits) (:credit (get-corp))) "Corp gained 1 credit from Blockchain")
        (is (= 4 (:credit (get-runner))) "Runner lost 1 credit from Blockchain")))))

(deftest blockchain-face-down-transactions
  ;; Face down transactions
  (do-game
    (new-game {:corp {:hand ["Blockchain" (qty "Beanstalk Royalties" 2)]
                      :discard [(qty "Beanstalk Royalties" 3)]
                      :credits 7}})
    (core/gain state :corp :click 5)
    (play-from-hand state :corp "Blockchain" "HQ")
    (let [bc (get-ice state :hq 0)]
      (rez state :corp bc)
      (is (= 2 (count (:subroutines (refresh bc))))
          "No subroutines gained because no face up Transactions are in Archives")
      (play-from-hand state :corp "Beanstalk Royalties")
      (is (= 2 (count (:subroutines (refresh bc))))
          "No subroutines gained because 1 face up Transactions is in Archives")
      (play-from-hand state :corp "Beanstalk Royalties")
      (is (= 3 (count (:subroutines (refresh bc))))
          "1 subroutine gained because 2 face up Transactions are in Archives")
      (is (= 5 (count (:discard (get-corp)))) "5 cards in discard pile"))))

(deftest blockchain-preemptive-action-interaction
  ;; Preemptive Action interaction
  (do-game
    (new-game {:corp {:hand ["Blockchain" "Preemptive Action" (qty "Beanstalk Royalties" 4)]
                      :credits 7}})
    (core/gain state :corp :click 5)
    (play-from-hand state :corp "Blockchain" "HQ")
    (let [bc (get-ice state :hq 0)]
      (rez state :corp bc)
      (is (= 2 (count (:subroutines (refresh bc)))) "No subroutines gained because no Transactions are in Archives")
      (play-from-hand state :corp "Beanstalk Royalties")
      (is (= 2 (count (:subroutines (refresh bc)))) "No subroutines gained because only 1 Transaction is in Archives")
      (play-from-hand state :corp "Beanstalk Royalties")
      (is (= 3 (count (:subroutines (refresh bc)))) "1 subroutine gained because 2 Transactions are in Archives")
      (play-from-hand state :corp "Beanstalk Royalties")
      (is (= 3 (count (:subroutines (refresh bc)))) "1 subroutine gained because 3 Transactions are in Archives")
      (play-from-hand state :corp "Beanstalk Royalties")
      (is (= 4 (count (:subroutines (refresh bc)))) "2 subroutines gained because 4 Transactions are in Archives")
      (is (= 12 (:credit (get-corp))) "Corp has 12 credits from four Beanstalks")
      (play-from-hand state :corp "Preemptive Action")
      (click-card state :corp (first (:discard (get-corp))))
      (click-card state :corp (second (:discard (get-corp))))
      (click-card state :corp (last (:discard (get-corp))))
      (is (= 1 (count (:discard (get-corp)))))
      (is (= 1 (count (:rfg (get-corp)))))
      (is (= 2 (count (:subroutines (refresh bc)))) "No subroutines gained because only 1 Transaction is in Archives"))))

(deftest bloom
  ;; Bloom
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Bloom" "Enigma" "Ice Wall"]
                      :credits 10}})
    (play-from-hand state :corp "Enigma" "HQ")
    (play-from-hand state :corp "Bloom" "HQ")
    (take-credits state :corp)
    (let [bloom (get-ice state :hq 1)]
      (rez state :corp bloom)
      (run-on state :hq)
      (run-continue state)
      (card-subroutine state :corp bloom 1)
      (click-card state :corp "Ice Wall")
      (is (= "Ice Wall" (:title (get-ice state :hq 1))) "Ice Wall is now installed at position 1")
      (is (= 3 (get-in @state [:run :position])) "Runner has been moved back to accomodate"))))

(deftest bloop
  ;; Bloop
  (do-game
    (new-game {:corp {:hand ["Bloop" "Echo"]}})
    (play-from-hand state :corp "Bloop" "HQ")
    (play-from-hand state :corp "Echo" "R&D")
    (rez state :corp (get-ice state :hq 0) {:expect-rez false})
    (is (not (rezzed? (get-ice state :hq 0))) "did not rez bloop, couldn't pay the cost")
    (is (no-prompt? state :corp))
    (rez state :corp (get-ice state :rd 0))
    (rez state :corp (get-ice state :hq 0) {:expect-rez false})
    (click-card state :corp (get-ice state :rd 0))
    (is (not (rezzed? (get-ice state :rd 0))) "derezzed echo")
    (is (rezzed? (get-ice state :hq 0)) "rezzed bloop")))

(deftest border-control
  ;; Border Control
  (do-game
    (new-game {:corp {:hand ["Border Control" "Ice Wall"]
                      :credits 10}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Border Control" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [bc (get-ice state :hq 1)]
      (rez state :corp bc))
    (run-continue state)
    (let [bc (get-ice state :hq 1)
          credits (:credit (get-corp))]
      (card-subroutine state :corp bc 0)
      (is (= (+ credits 2) (:credit (get-corp))))
      (card-ability state :corp bc 0)
      (is (nil? (refresh bc)))
      (is (nil? (get-run))))))

(deftest boto
  (do-game
    (new-game {:corp {:hand ["Boto" "Vanity Project" "Hedge Fund"]
                      :credits 10}
               :runner {:hand [(qty "Sure Gamble" 2)]}})
    (play-from-hand state :corp "Boto" "HQ")
    (take-credits state :corp)
    (let [boto (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp boto)
      (run-continue state)
      (is (changed? [(count (:hand (get-runner))) -2
                     (count (:discard (get-runner))) 2]
                    (card-subroutine state :corp (refresh boto) 0))
          "Runner suffered 2 net damage")
      (is (changed? [(count (:hand (get-corp))) 0]
                    (card-subroutine state :corp (refresh boto) 1)
                    (click-prompt state :corp "No"))
          "Corp declined to discard 1 card from HQ")
      (is (:run @state) "Run still in progress")
      (is (changed? [(count (:hand (get-corp))) -1
                     (count (:discard (get-corp))) 1]
                    (card-subroutine state :corp (refresh boto) 2)
                    (click-prompt state :corp "Yes")
                    (click-card state :corp "Hedge Fund"))
          "Corp discarded 1 card from HQ")
      (is (not (:run @state)) "Run ended")
      (take-credits state :runner)
      (is (changed? [(get-strength (refresh boto)) 2]
                    (play-and-score state "Vanity Project"))
          "Boto gained 2 strength"))))

(deftest brainstorm-subroutine-gain-loss-ability
  ;; Subroutine gain/loss ability
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Brainstorm"]
                      :credits 9}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["Sure Gamble"]}})
    (play-from-hand state :corp "Brainstorm" "HQ")
    (let [bs (get-ice state :hq 0)]
      (rez state :corp bs)
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (is (= 1 (count (:subroutines (refresh bs)))) "1 card in hand, no existing subs")
      (draw state :runner 1)
      (core/redirect-run state :corp "HQ" :approach-ice)
      (run-continue state)
      (run-continue state)
      (is (= 3 (count (:subroutines (refresh bs)))) "2 cards in hand, 1 existing sub")
      (draw state :runner 1)
      (core/redirect-run state :corp "HQ" :approach-ice)
      (run-continue state)
      (run-continue state)
      (is (= 6 (count (:subroutines (refresh bs)))) "3 cards in hand, 3 existing subs")
      (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (is (= 1 (count (:hand (get-runner)))) "Runner now only has 1 card in hand")
      (core/redirect-run state :corp "HQ" :approach-ice)
      (run-continue state)
      (run-continue state)
      (is (= 7 (count (:subroutines (refresh bs)))) "1 card in hand, 6 existing subs"))))

(deftest brainstorm-subroutines-not-going-away-until-end-of-run
  ;; Subroutines not going away until end of run
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Brainstorm"]
                      :credits 9}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["Sure Gamble"]}})
    (play-from-hand state :corp "Brainstorm" "R&D")
    (take-credits state :corp)
    (run-on state :rd)
    (let [bs (get-ice state :rd 0)]
      (rez state :corp bs)
      (run-continue state)
      (is (= 1 (count (:subroutines (refresh bs)))))
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "No action")
      (is (zero? (count (:subroutines (refresh bs))))))))

(deftest bran-1-0
  ;; Bran 1.0
  ;; Brân 1.0
  (do-game
    (new-game {:corp {:hand ["Brân 1.0" "Mausolus"]}})
    (play-from-hand state :corp "Brân 1.0" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [bran (get-ice state :hq 0)]
      (rez state :corp bran)
      (run-continue state)
      (card-subroutine state :corp bran 0)
      (waiting? state :runner)
      (is (changed? [(:credit (get-corp)) 0]
            (click-card state :corp "Mausolus"))
          "Mausolus installed for free")
      (is (= 2 (count (get-ice state :hq))) "2 pieces of ice protecting HQ")
      (is (= 2 (:position (get-run))) "Runner position moved along Bran position")
      (card-subroutine state :corp (get-ice state :hq 1) 1)
      (is (not (:run @state)) "Run ended"))))

(deftest bran-1-0-install-ice-log-messages-display-correctly
  ;; Install ice log messages display correctly
  (do-game
    (new-game {:corp {:hand ["Brân 1.0"]
                      :discard ["Mausolus" "Ice Wall"]}})
    (play-from-hand state :corp "Brân 1.0" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [bran (get-ice state :hq 0)
          unrezzed-msg "pays 0 [Credits] to use Brân 1.0 to install ice from Archives protecting HQ"
          rezzed-msg "pays 0 [Credits] to use Brân 1.0 to install Ice Wall from Archives protecting HQ"
          declined-msg "Corp declines to use Brân 1.0 to install a card."]
      (rez state :corp bran)
      (run-continue state)
      (card-subroutine state :corp bran 0)
      (click-card state :corp "Mausolus")
      (is (last-log-contains? state unrezzed-msg) "Mausolus is face down and should not be revealed in the log")
      (card-subroutine state :corp bran 1)
      (run-empty-server state :archives)
      (run-on state :hq)
      (run-continue state)
      (card-subroutine state :corp bran 0)
      (click-card state :corp "Ice Wall")
      (is (last-log-contains? state rezzed-msg) "Ice Wall is face up and should be revealed in the log")
      (card-subroutine state :corp bran 1)
      (run-on state :hq)
      (run-continue state)
      (card-subroutine state :corp bran 0)
      (click-prompt state :corp "Done")
      (is (last-log-contains? state declined-msg) "Log should indicate the corp declined to install a card"))))

(deftest bumi-1-0-full-test
  (do-game
    (subroutine-test "Bumi 1.0" 0 nil {:rig ["Rezeki"]})
    (click-card state :corp "Rezeki")
    (is (= 1 (count (:discard (get-runner)))))
    (is (no-prompt? state :corp)))
  (do-game
    (subroutine-test "Bumi 1.0" 1 {:runner {:hand ["Rezeki"]}})
    (is (= 1 (count (:discard (get-runner)))))
    (is (no-prompt? state :corp)))
  (do-game
    (new-game {:runner {:hand ["Ika"]}
               :corp {:hand ["Bumi 1.0"]}})
    (play-from-hand state :corp "Bumi 1.0" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Ika")
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 0))
    (click-card state :corp "Ika")
    (is-discard? state :runner ["Ika"])))

(deftest bullfrog
  ;; Bullfrog - Win psi to move to outermost position of another server and continue run there
  (do-game
    (new-game {:corp {:deck ["Bullfrog" (qty "Pup" 2)]}})
    (play-from-hand state :corp "Bullfrog" "HQ")
    (play-from-hand state :corp "Pup" "R&D")
    (play-from-hand state :corp "Pup" "R&D")
    (take-credits state :corp)
    (run-on state :hq)
    (let [frog (get-ice state :hq 0)]
      (rez state :corp frog)
      (run-continue state)
      (card-subroutine state :corp frog 0)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (click-prompt state :corp "R&D")
      (run-continue state)
      (is (= :rd (-> (get-run) :server first)) "Run redirected to R&D")
      (is (= 2 (:position (get-run))) "Passed Bullfrog")
      (is (= "Bullfrog" (:title (get-ice state :rd 2))) "Bullfrog at outermost position of R&D"))))

(deftest cell-portal-run-on-centrals
  ;; Run on centrals
  (do-game
    (new-game {:corp {:deck ["Cell Portal" (qty "Paper Wall" 2)]}})
    (core/gain state :corp :credit 5)
    (play-from-hand state :corp "Cell Portal" "HQ")
    (play-from-hand state :corp "Paper Wall" "HQ")
    (play-from-hand state :corp "Paper Wall" "HQ")
    (take-credits state :corp)
    (let [cp (get-ice state :hq 0)]
      (rez state :corp cp)
      (run-on state :hq)
      (run-continue-until state :encounter-ice cp)
      (is (= 1 (get-in @state [:run :position])))
      (card-subroutine state :corp cp 0)
      (is (= 3 (get-in @state [:run :position])) "Run back at outermost position")
      (click-prompt state :runner "No")
      (is (not (rezzed? (refresh cp))) "Cell Portal derezzed"))))

(deftest capacitor
  (do-game
    (new-game {:corp {:hand ["Capacitor"]}})
    (play-from-hand state :corp "Capacitor" "HQ")
    (take-credits state :corp)
    (let [cap (get-ice state :hq 0)]
      (rez state :corp cap)
      (is (changed? [(get-strength (refresh cap)) 2]
                    (gain-tags state :runner 2))
          "Capacitor gained 2 strength")
      (run-on state :hq)
      (run-continue state)
      (is (changed? [(:credit (get-corp)) 2]
                    (card-subroutine state :corp cap 0))
          "Corp gained 1 credit per Runner tag")
      (card-subroutine state :corp cap 1)
      (is (empty? (:run @state)) "Run has ended"))))

(deftest cell-portal-run-on-servers
  ;; Run on servers
  (do-game
    (new-game {:corp {:deck ["Cell Portal" (qty "Paper Wall" 2)]}})
    (core/gain state :corp :credit 5)
    (play-from-hand state :corp "Cell Portal" "New remote")
    (play-from-hand state :corp "Paper Wall" "Server 1")
    (play-from-hand state :corp "Paper Wall" "Server 1")
    (take-credits state :corp)
    (let [cp (get-ice state :remote1 0)]
      (rez state :corp cp)
      (run-on state "Server 1")
      (run-continue-until state :encounter-ice cp)
      (is (= 1 (get-in @state [:run :position])))
      (card-subroutine state :corp cp 0)
      (is (= 3 (get-in @state [:run :position])) "Run back at outermost position")
      (click-prompt state :runner "No")
      (is (not (rezzed? (refresh cp))) "Cell Portal derezzed"))))

(deftest cell-portal-jack-out
  ;; Jack out
  (do-game
    (new-game {:corp {:deck ["Cell Portal" (qty "Paper Wall" 2)]}})
    (core/gain state :corp :credit 5)
    (play-from-hand state :corp "Cell Portal" "HQ")
    (play-from-hand state :corp "Paper Wall" "HQ")
    (play-from-hand state :corp "Paper Wall" "HQ")
    (take-credits state :corp)
    (let [cp (get-ice state :hq 0)]
      (rez state :corp cp)
      (run-on state :hq)
      (run-continue-until state :encounter-ice cp)
      (is (= 1 (get-in @state [:run :position])))
      (card-subroutine state :corp cp 0)
      (is (= 3 (get-in @state [:run :position])) "Run back at outermost position")
      (click-prompt state :runner "Yes")
      (is (not (rezzed? (refresh cp))) "Cell Portal derezzed")
      (is (empty? (:run @state)) "Run has ended"))))

(deftest checkpoint-deals-damage-on-successful-run
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Checkpoint" "Hedge Fund"]}
               :runner {:hand [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Checkpoint" "HQ")
    (let [chckpnt (get-ice state :hq 0)]
      (take-credits state :corp)
      (run-on state "HQ")
      (rez state :corp chckpnt)
      (run-continue state)
      (fire-subs state chckpnt)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 5 (:base (prompt-map :corp))) "Trace is base 5")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (run-continue state :movement)
      (run-jack-out state)
      (is (= 0 (count (:discard (get-runner)))) "Runner suffered no meat damage")
      (run-on state "HQ")
      (run-continue state)
      (fire-subs state chckpnt)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (run-continue state)
      (run-continue state)
      (is (prompt-map :runner) "Still have access prompt")
      (is (= 3 (count (:discard (get-runner)))) "Runner suffered 3 meat damage")
      (click-prompt state :runner "No action")
      (is (not (:run @state)) "Run is finished"))))

(deftest chimera
  ;; Chimera - Gains chosen subtype
  (before-each
    [state (new-game {:corp {:deck ["Chimera"]
                             :credits 10}})
     _ (play-from-hand state :corp "Chimera" "HQ")
     ch (get-ice state :hq 0)]
    (testing "Gains a subtype when rezzed"
      (letfn [(chimera-test [ice-type]
                (do-game state
                  (is (not (has-subtype? (refresh ch) ice-type)) (str "Chimera does not have " ice-type))
                  (rez state :corp ch)
                  (click-prompt state :corp ice-type)
                  (is (has-subtype? (refresh ch) ice-type) (str "Chimera has " ice-type))
                  (take-credits state :corp)
                  (is (not (has-subtype? (refresh ch) ice-type)) (str "Chimera does not have " ice-type))))]
        (doall (map chimera-test ["Barrier" "Code Gate" "Sentry"]))))
    (testing "Can only choose Barrier, Code Gate, or Sentry"
      (do-game state
        (rez state :corp (refresh ch))
        (is (= ["Barrier" "Code Gate" "Sentry"] (prompt-buttons :corp)))))
    (testing "derezzes at the end of the turn"
      (do-game state
        (rez state :corp (refresh ch))
        (click-prompt state :corp "Barrier")
        (take-credits state :corp)
        (is (not (rezzed? (refresh ch))))
        (rez state :corp (refresh ch))
        (click-prompt state :corp "Barrier")
        (take-credits state :runner)
        (is (not (rezzed? (refresh ch))))))))

(deftest chiyashi-auto-trash
  (do-game
    (new-game {:corp {:hand [(qty "Chiyashi" 2)] :credits 30}
               :runner {:hand ["Crypsis" "Corroder" "Hippo"] :deck [(qty "Sure Gamble" 50)] :credits 50}})
    (play-from-hand state :corp "Chiyashi" "HQ")
    (play-from-hand state :corp "Chiyashi" "R&D")
    (rez state :corp (get-ice state :hq 0))
    (rez state :corp (get-ice state :rd 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Crypsis")
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Hippo")
    (core/gain state :runner :click 100)
    (let [corroder (get-program state 1)
          crypsis (get-program state 0)]
      ;; the ice is trashed before the 'trash 2' text on chiyashi
      ;; is active, so it does not fire for the last subroutine
      (run-on state "HQ")
      (run-continue state)
      (is (changed? [(count (:discard (get-runner))) +4]
            (auto-pump-and-break state corroder)
            (core/process-action "continue" state :corp nil)
            (click-prompt state :runner "Yes")
            (run-jack-out state))
          "milled 4 with Chiyashi (2 + 2, ice trashed before third mill)")
      (run-on state "R&D")
      (run-continue state)
      (is (changed? [(count (:discard (get-runner))) +7]
            (auto-pump-and-break state crypsis)
            (core/process-action "continue" state :corp nil)
            (run-jack-out state))
          "milled 6 with Chiyashi, and trashed crypsis")
      (run-on state "R&D")
      (run-continue state)
      (is (changed? [(count (:discard (get-runner))) 0]
            (auto-pump-and-break state corroder)
            (core/process-action "continue" state :corp nil)
            (run-jack-out state))
          "milled 0 with Chiyashi, no AI is installed"))))

(deftest chrysalis
  ;; Chrysalis
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Chrysalis"]}
               :runner {:hand [(qty "Sure Gamble" 2)]}})
    (play-from-hand state :corp "Chrysalis" "HQ")
    (let [chrysalis (get-ice state :hq 0)]
      (take-credits state :corp)
      (run-on state "HQ")
      (rez state :corp chrysalis)
      (run-continue state)
      (card-subroutine state :corp chrysalis 0)
      (is (= 2 (count (:discard (get-runner)))) "Runner suffered 2 net damage"))))

(deftest chrysalis-access-test
  ;; Access test
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Chrysalis"]}
               :runner {:hand [(qty "Sure Gamble" 2)]}})
    (take-credits state :corp)
    (run-empty-server state :hq)
    (is (= "Chrysalis" (:title (core/get-current-ice state))) "Encountering Chrysalis on access")
    (fire-subs state (core/get-current-ice state))
    (is (= 2 (count (:discard (get-runner)))) "Runner suffered 2 net damage")))

(deftest chum-2-strength
  ;; +2 strength
  (do-game
    (new-game {:corp {:deck ["Chum" (qty "Enigma" 2) "Ice Wall"]}
               :runner {:deck ["Corroder"]}})
    (core/gain state :corp :click 1 :credit 6)
    (play-from-hand state :corp "Enigma" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Enigma" "HQ")
    (play-from-hand state :corp "Chum" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (let [chum (get-ice state :hq 3)
          icewall (get-ice state :hq 1)
          enigma (get-ice state :hq 0)
          corroder (get-program state 0)]
      (rez state :corp chum)
      (rez state :corp icewall)
      (rez state :corp enigma)
      (run-on state :hq)
      (run-continue state)
      (is (= 4 (get-strength (refresh chum))) "Chum is at 4 strength")
      (card-subroutine state :corp (refresh chum) 0)
      (is (= 4 (get-strength (refresh chum))) "Chum stays at 4 strength")
      (is (= 1 (get-strength (refresh icewall))) "Ice Wall still at 1 strength")
      (run-continue state)
      (is (= 1 (get-strength (refresh icewall))) "Ice Wall still at 1 strength while passing unrezzed ice")
      (run-continue-until state :encounter-ice icewall)
      (is (= 3 (get-strength (refresh icewall))) "Ice Wall now at 3 strength")
      (is (= 2 (get-strength (refresh enigma))) "Enigma stays at 2 strength before encounter")
      (auto-pump-and-break state corroder)
      (core/process-action "continue" state :corp nil)
      (run-continue-until state :encounter-ice enigma)
      (is (= 2 (get-strength (refresh enigma))) "Enigma stays at 2 strength during encounter")
      (run-continue state :movement)
      (run-jack-out state))))

(deftest chum-net-damage-from-passing-without-breaking
  ;; Net damage from passing without breaking
  (do-game
    (new-game {:corp {:deck ["Chum" "Pachinko"]}})
    (play-from-hand state :corp "Pachinko" "HQ")
    (play-from-hand state :corp "Chum" "HQ")
    (take-credits state :corp)
    (let [chum (get-ice state :hq 1)
          pachinko (get-ice state :hq 0)]
      (rez state :corp chum)
      (rez state :corp pachinko)
      (run-on state :hq)
      (run-continue state)
      (card-subroutine state :corp (refresh chum) 0)
      (run-continue-until state :encounter-ice pachinko)
      (is (changed? [(count (:hand (get-runner))) -3]
            (run-continue state))
          "3 Damage from passing an unbroken ice"))))

(deftest chum-net-damage-from-ice-ending-the-run
  ;; Net damage from ice ending the run
  (do-game
    (new-game {:corp {:deck ["Chum" "Ice Wall"]}
               :runner {:deck ["Corroder" (qty "Sure Gamble" 4)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Chum" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (let [chum (get-ice state :hq 1)
          icewall (get-ice state :hq 0)
          corroder (get-program state 0)]
      (run-on state :hq)
      (rez state :corp chum)
      (run-continue state)
      (card-subroutine state :corp (refresh chum) 0)
      (run-continue-until state :approach-ice icewall)
      (rez state :corp icewall)
      (run-continue state)
      (auto-pump-and-break state corroder)
      (is (changed? [(count (:hand (get-runner))) 0]
            (core/process-action "continue" state :corp nil))
          "No Damage from Ice Wall ending the run"))))

(deftest cloud-eater
  (do-game
    (new-game {:corp {:hand ["Cloud Eater"]
               :credits 10}
               :runner {:hand [(qty "Sure Gamble" 3) "Smartware Distributor"]}})
    (play-from-hand state :corp "Cloud Eater" "HQ")
    (take-credits state :corp)
    (let [ce (get-ice state :hq 0)]
      (play-from-hand state :runner "Smartware Distributor")
      (run-on state :hq)
      (rez state :corp ce)
      (run-continue state)
      (is (changed? [(count (:discard (get-runner))) 1
                     (count (get-resource state)) -1]
                    (card-subroutine state :corp (refresh ce) 0)
                    (click-card state :corp "Smartware Distributor"))
          "Smartware Distributor got trashed")
      (is (changed? [(count-tags state) 2]
                    (card-subroutine state :corp (refresh ce) 1))
          "Runner got 2 tags")
      (is (changed? [(count (:hand (get-runner))) -3
                     (count (:discard (get-runner))) 3]
                    (card-subroutine state :corp (refresh ce) 2))
          "Runner suffered 3 net damage")
      (run-continue-until state :movement)
      (is (= 2 (count (prompt-buttons :runner))) "Runner doesn't have the option to suffer net damage")
      (is (changed? [(count-tags state) 2]
                    (click-prompt state :runner "Take 2 tags"))
          "Runner got 2 tags")
      (is (last-n-log-contains? state 1 "Cloud Eater to force the Runner to take 2 tag") "Correctly logs choice")
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (run-continue-until state :encounter-ice)
      (is (no-prompt? state :runner) "Runner has no Cloud Eater prompt if it was rezzed since the previous turn"))))

(deftest congratulations
  ;; Congratulations!
  (do-game
    (new-game {:corp {:deck ["Congratulations!"]}})
    (play-from-hand state :corp "Congratulations!" "HQ")
    (take-credits state :corp)
    (let [congrats (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp congrats)
      (is (= 6 (:credit (get-corp))))
      (is (= 5 (:credit (get-runner))))
      (run-continue state)
      (card-subroutine state :corp congrats 0)
      (is (= 8 (:credit (get-corp))))
      (is (= 6 (:credit (get-runner))))
      (run-continue state)
      (is (= 9 (:credit (get-corp))) "+1 Credit for passing Congratulations!"))))

(deftest cortex-lock
  ;; Cortex Lock - Do net damage equal to Runner's unused memory
  (do-game
    (new-game {:corp {:deck ["Cortex Lock"]}
               :runner {:deck [(qty "Corroder" 2) (qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Cortex Lock" "HQ")
    (take-credits state :corp)
    (let [cort (get-ice state :hq 0)]
      (play-from-hand state :runner "Corroder")
      (is (= 3 (core/available-mu state)))
      (run-on state "HQ")
      (rez state :corp cort)
      (run-continue state)
      (card-subroutine state :corp cort 0)
      (is (= 3 (count (:discard (get-runner)))) "Runner suffered 3 net damage"))))

(deftest crick
  ;; Crick - Strength boost when protecting Archives; installs a card from Archives
  (do-game
    (new-game {:corp {:hand [(qty "Crick" 2)]
                      :discard ["Ice Wall"]}})
    (play-from-hand state :corp "Crick" "HQ")
    (play-from-hand state :corp "Crick" "Archives")
    (take-credits state :corp)
    (let [cr1 (get-ice state :hq 0)
          cr2 (get-ice state :archives 0)]
      (rez state :corp cr1)
      (rez state :corp cr2)
      (is (= 3 (get-strength (refresh cr1))) "Normal strength over HQ")
      (is (= 6 (get-strength (refresh cr2))) "+3 strength over Archives")
      (run-on state "Archives")
      (run-continue state)
      (card-subroutine state :corp cr2 0)
      (click-card state :corp "Ice Wall")
      (click-prompt state :corp "HQ")
      (is (= 3 (:credit (get-corp))) "Paid 1 credit to install as 2nd piece of ice over HQ"))))

(deftest curtain-wall
  ;; Curtain Wall - Strength boost when outermost piece of ice
  (do-game
    (new-game {:corp {:deck ["Curtain Wall" "Paper Wall"]}})
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "Curtain Wall" "HQ")
    (let [curt (get-ice state :hq 0)]
      (rez state :corp curt)
      (is (= 10 (get-strength (refresh curt)))
          "Curtain Wall has +4 strength as outermost piece of ice")
      (play-from-hand state :corp "Paper Wall" "HQ")
      (let [paper (get-ice state :hq 1)]
        (rez state :corp paper)
        (is (= 6 (get-strength (refresh curt))) "Curtain Wall back to default 6 strength")))))

(deftest data-hound
  ;; Data Hound - Full test
  (do-game
    (new-game {:corp {:deck ["Data Hound"]}
               :runner {:deck ["Sure Gamble" "Desperado"
                               "Corroder" "Patron"]
                        :hand ["Sure Gamble"]}})
    (play-from-hand state :corp "Data Hound" "HQ")
    (take-credits state :corp)
    (let [dh (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp dh)
      (run-continue state)
      (card-subroutine state :corp dh 0)
      (click-prompt state :corp "2")
      (click-prompt state :runner "0")
      ;; trash 1 card and rearrange the other 3
      (click-prompt state :corp "Desperado")
      (is (= 1 (count (:discard (get-runner)))))
      (click-prompt state :corp "Sure Gamble")
      (click-prompt state :corp "Corroder")
      (click-prompt state :corp "Patron")
      ;; try starting over
      (click-prompt state :corp "Start over")
      (click-prompt state :corp "Patron")
      (click-prompt state :corp "Corroder")
      (click-prompt state :corp "Sure Gamble") ;this is the top card on stack
      (click-prompt state :corp "Done")
      (is (= "Sure Gamble" (:title (first (:deck (get-runner))))))
      (is (= "Corroder" (:title (second (:deck (get-runner))))))
      (is (= "Patron" (:title (second (rest (:deck (get-runner)))))))
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state "HQ")
      (run-continue state)
      (card-subroutine state :corp dh 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "1")
      ;; trash the only card automatically
      (is (= 2 (count (:discard (get-runner)))))
      (is (= "Corroder" (:title (first (:deck (get-runner)))))))))

(deftest data-loop-encounter-ability-issue-4744
  ;; Encounter ability. Issue #4744
  (testing "Enough cards in hand"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Data Loop"]
                        :credits 10}
                 :runner {:deck ["Account Siphon"]
                          :hand ["Sure Gamble" "Easy Mark"]}})
      (play-from-hand state :corp "Data Loop" "HQ")
      (take-credits state :corp)
      (is (= "Account Siphon" (:title (first (:deck (get-runner))))))
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (= "Choose 2 cards in the grip to add to the top of the stack (second card targeted will be topmost)"
             (:msg (prompt-map :runner)))
          "Runner is prompted")
      (click-card state :runner "Easy Mark")
      (click-card state :runner "Sure Gamble")
      (is (= "Sure Gamble" (:title (first (:deck (get-runner))))))
      (is (= "Easy Mark" (:title (second (:deck (get-runner))))))))
  (testing "1 card in hand"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Data Loop"]
                        :credits 10}
                 :runner {:deck ["Account Siphon"]
                          :hand ["Sure Gamble"]}})
      (play-from-hand state :corp "Data Loop" "HQ")
      (take-credits state :corp)
      (is (= "Account Siphon" (:title (first (:deck (get-runner))))))
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (= "Choose 1 card in the grip to add to the top of the stack (second card targeted will be topmost)"
             (:msg (prompt-map :runner)))
          "Runner is prompted")
      (click-card state :runner "Sure Gamble")
      (is (no-prompt? state :runner) "Runner only selects 1 card")
      (is (= "Sure Gamble" (:title (first (:deck (get-runner))))))))
  (testing "Empty hand"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Data Loop"]
                        :credits 10}
                 :runner {:deck ["Account Siphon"]
                          :hand ["Sure Gamble"]}})
      (play-from-hand state :corp "Data Loop" "HQ")
      (take-credits state :corp)
      (is (= "Account Siphon" (:title (first (:deck (get-runner))))))
      (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :discard)
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (no-prompt? state :runner) "Runner doesn't have a prompt"))))

(deftest data-mine
  ;; Data Mine - do one net and trash
  (do-game
    (new-game {:corp {:deck ["Data Mine"]}})
    (play-from-hand state :corp "Data Mine" "New remote")
    (take-credits state :corp)
    (let [dm (get-ice state :remote1 0)]
      (run-on state "Server 1")
      (rez state :corp dm)
      (run-continue state)
      (card-subroutine state :corp dm 0)
      (is (= 1 (count (:discard (get-runner)))) "Runner suffered 1 net damage"))))

(deftest data-ward-3-credits-on-encounter-keeps-open-prompt-issue-4965
  ;; 3 credits on encounter keeps open prompt. Issue #4965
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Data Ward"]}})
    (play-from-hand state :corp "Data Ward" "HQ")
    (take-credits state :corp)
    (let [dw (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp dw)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -3]
            (click-prompt state :runner "Pay 3 [Credits]"))
          "Runner pays 3 credits on Data Ward encounter")
      (is (no-prompt? state :runner) "Runner doesn't have a prompt"))))

(deftest data-ward-runner-takes-1-tag-on-encounter
  ;; Runner takes 1 tag on encounter
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Data Ward"]}})
    (play-from-hand state :corp "Data Ward" "HQ")
    (take-credits state :corp)
    (let [dw (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp dw)
      (run-continue state)
      (is (changed? [(count-tags state) 1]
            (click-prompt state :runner "Take 1 tag"))
          "Runner takes 1 tag on Data Ward encounter")
      (is (no-prompt? state :runner) "Runner doesn't have a prompt"))))

(deftest data-ward-data-ward-ends-run-only-if-runner-is-tagged
  ;; Data Ward ends run only if runner is tagged
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Data Ward"]}
               :runner {:credis 30}})
    (play-from-hand state :corp "Data Ward" "HQ")
    (take-credits state :corp)
    (let [dw (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp dw)
      (run-continue state)
      (click-prompt state :runner "Take 1 tag")
      (fire-subs state (refresh dw))
      (is (not (:run @state)) "Run ended")
      (remove-tag state :runner)
      (run-on state "HQ")
      (run-continue state)
      (click-prompt state :runner "Pay 3 [Credits]")
      (fire-subs state (refresh dw))
      (is (:run @state) "Run still ongoing"))))

(deftest datapike
  ;; Datapike - Runner pays 2 credits or end the run
  (do-game
    (new-game {:corp {:deck ["Datapike"]}})
    (play-from-hand state :corp "Datapike" "HQ")
    (let [dp (get-ice state :hq 0)]
      (rez state :corp dp)
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (is (= 5 (:credit (get-runner))) "Runner starts with 5 credits")
      (card-subroutine state :corp dp 0)
      (is (= 3 (:credit (get-runner))) "Runner spent 2 credits")
      (is (some? (:run @state)) "Run is continuing")
      (card-subroutine state :corp dp 1)
      (is (nil? (:run @state)) "Run has ended"))))

(deftest datapike-cannot-pay
  ;; Datapike - Runner cannot pay 2 credits
  (do-game
    (new-game {:corp {:deck ["Datapike"]}
               :runner {:hand ["Professional Contacts"]}})
    (play-from-hand state :corp "Datapike" "HQ")
    (let [dp (get-ice state :hq 0)]
      (rez state :corp dp)
      (take-credits state :corp)
      (play-from-hand state :runner "Professional Contacts")
      (run-on state "HQ")
      (run-continue state)
      (card-subroutine state :corp dp 0)
      (is (nil? (:run @state)) "Run has ended"))))

(deftest descent
  (do-game
    (new-game {:corp {:hand ["Descent"]
                      :deck ["Hedge Fund" "Project Atlas"]
                      :discard ["Ikawah Project"]}})
    (play-from-hand state :corp "Descent" "HQ")
    (take-credits state :corp)
    (let [hm (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp hm)
      (run-continue state)
      (fire-subs state hm)
      (is (not (:run @state)) "Run has ended")
      (take-credits state :runner)
      (is (changed? [(count (:hand (get-corp))) 2]
                    (click-prompt state :corp "Yes")
                    (is (nil? (refresh (get-ice state :hq 0)))))
          "Hangman returned to HQ + corp mandatory drew")
      (expend state :corp (find-card "Descent" (:hand (get-corp))))
      (is (changed? [(count (:hand (get-corp))) -1
                     (count (:discard (get-corp))) -1
                     (count (:deck (get-corp))) 2]
                    (click-card state :corp "Ikawah Project")
                    (click-card state :corp "Project Atlas"))
          "The 2 agendas are shuffled into R&D and Hangman is expended"))))

(deftest diviner
  ;; Diviner
  (do-game
    (new-game {:corp {:hand ["Diviner"]}
               :runner {:hand ["Corroder"]}})
    (play-from-hand state :corp "Diviner" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [diviner (get-ice state :hq 0)]
      (rez state :corp diviner)
      (run-continue state)
      (is (changed? [(count (:discard (get-runner))) 1]
            (fire-subs state diviner))
          "1 card is trashed")
      (is (get-run) "Corroder has an even cost, so run continues")))
  (testing "Basic Test with Odd Cost Trash"
    (do-game
      (new-game {:corp {:hand ["Diviner"]}
                 :runner {:hand ["Sure Gamble"]}})
      (play-from-hand state :corp "Diviner" "HQ")
      (take-credits state :corp)
      (run-on state :hq)
      (let [diviner (get-ice state :hq 0)]
        (rez state :corp diviner)
        (run-continue state)
        (fire-subs state diviner)
        (is (not (get-run)) "Sure Gamble has an odd cost, so run ends")))))

(deftest doomscroll-full-test
  ;; give the runner a tag
  (do-game
    (subroutine-test "Doomscroll" 0 {:runner {:hand 3}})
    (is (= 1 (count-tags state)) "Took 1 tag"))
  ;; Do 1 net damage
  (do-game
    (subroutine-test "Doomscroll" 1 {:runner {:hand 10 :tags 0}})
    (is (= 1 (count (:discard (get-runner)))) "Took 1 damage"))
  (do-game
    (subroutine-test "Doomscroll" 1 {:runner {:hand 10 :tags 1}})
    (is (= 1 (count (:discard (get-runner)))) "Took 1 damage"))
  ;; Do 2 net if 2 tag
  (do-game
    (subroutine-test "Doomscroll" 2 {:runner {:hand 10 :tags 1}})
    (is (= 0 (count (:discard (get-runner)))) "Took 0 damage"))
  (do-game
    (subroutine-test "Doomscroll" 2 {:runner {:hand 10 :tags 2}})
    (is (= 2 (count (:discard (get-runner)))) "Took 2 damage")))

(deftest draco
  ;; Dracō - Pay credits when rezzed to increase strength; trace to give 1 tag and end the run
  (do-game
    (new-game {:corp {:deck ["Dracō"]}})
    (play-from-hand state :corp "Dracō" "HQ")
    (take-credits state :corp)
    (let [drac (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp drac)
      (click-prompt state :corp "4")
      (run-continue state)
      (is (= 4 (get-counters (refresh drac) :power)) "Dracō has 4 power counters")
      (is (= 4 (get-strength (refresh drac))) "Dracō is 4 strength")
      (card-subroutine state :corp drac 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (count-tags state)) "Runner took 1 tag")
      (is (nil? (:run @state)) "Run was ended"))))

(deftest drafter-subroutine-1-add-1-card-from-archives-to-hq
  ;; Subroutine 1: Add 1 card from Archives to HQ
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Drafter"]
                      :discard ["Wotan"]}})
    (play-from-hand state :corp "Drafter" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [drafter (get-ice state :hq 0)]
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-subroutine state :corp drafter 0)
      (is (= :select (prompt-type :corp)))
      (click-card state :corp "Wotan")
      (is (find-card "Wotan" (:hand (get-corp))) "Wotan is now in HQ"))))

(deftest drafter-subroutine-2-install-1-card
  ;; Subroutine 2: Install 1 card
  (testing "from Archives"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Drafter" "Fairchild"]
                        :discard ["Wotan"]}})
      (play-from-hand state :corp "Drafter" "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [drafter (get-ice state :hq 0)]
        (rez state :corp (get-ice state :hq 0))
        (run-continue state)
        (card-subroutine state :corp drafter 1)
        (is (= :select (prompt-type :corp)))
        (click-card state :corp "Wotan")
          (is (changed? [(:credit (get-corp)) 0]
                (click-prompt state :corp "HQ"))
              "Costs no credits to install a second ice on HQ")
          (is (= "Wotan" (:title (get-ice state :hq 1)))
              "Wotan is now installed in the outermost position protecting HQ"))))
    (testing "from HQ"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Drafter" "Fairchild"]
                          :discard ["Wotan"]}})
        (play-from-hand state :corp "Drafter" "HQ")
        (take-credits state :corp)
        (run-on state "HQ")
        (let [drafter (get-ice state :hq 0)]
          (rez state :corp (get-ice state :hq 0))
          (run-continue state)
          (card-subroutine state :corp drafter 1)
          (is (= :select (prompt-type :corp)))
          (click-card state :corp "Fairchild")
          (is (changed? [(:credit (get-corp)) 0]
                (click-prompt state :corp "HQ"))
              "Costs no credits to install a second ice on HQ")
          (is (= "Fairchild" (:title (get-ice state :hq 1)))
              "Fairchild is now installed in the outermost position protecting HQ")))))

(deftest echo
  (do-game
    (new-game {:corp {:deck [(qty "Echo" 3)] :credits 15}})
    (play-from-hand state :corp "Echo" "HQ")
    (play-from-hand state :corp "Echo" "HQ")
    (play-from-hand state :corp "Echo" "HQ")
    (let [e1 (get-ice state :hq 0)
          e2 (get-ice state :hq 1)
          e3 (get-ice state :hq 2)]
      (rez state :corp e1)
      (is (= 1 (get-counters (refresh e1) :power)) "Placed 1 counter on rezzed Echo")
      (is (= 0 (get-counters (refresh e2) :power)) "Placed 0 counter on unrezzed Echo")
      (rez state :corp e2)
      (is (= 2 (get-counters (refresh e1) :power)) "Placed 2 counters on rezzed Echo")
      (is (= 1 (get-counters (refresh e2) :power)) "Placed 1 counter on rezzed Echo")
      (is (= 0 (get-counters (refresh e3) :power)) "Placed 0 counter on unrezzed Echo")
      (rez state :corp e3)
      (is (= 3 (get-counters (refresh e1) :power)) "Placed 3 counters on rezzed Echo")
      (is (= 2 (get-counters (refresh e2) :power)) "Placed 2 counter on rezzed Echo")
      (is (= 1 (get-counters (refresh e3) :power)) "Placed 1 counter on unrezzed Echo")
      (is (= 3 (count (:subroutines (refresh e1)))) "Should have 3 subroutine"))))

(deftest empiricist-subs-test
  (do-game
    (subroutine-test "Empiricist" 0 {:corp {:hand ["Restructure"] :deck ["IPO" "Hedge Fund"]}})
    (is (= (+ 1 1) (count (:hand (get-corp)))) (str "drew 1 card"))
    (click-card state :corp "Restructure")
    (is (= "Restructure" (get-in @state [:corp :deck 0 :title])) "Restructure ontop"))
  (do-game (does-damage-sub "Empiricist" 1 1)
           (is (= 1 (count-tags state)) "tagged"))
  (do-game (does-damage-sub "Empiricist" 2 2)))

(deftest endless-eula-runner-side-ability
  ;; Runner side ability
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Endless EULA"]}
               :runner {:credits 10}})
    (play-from-hand state :corp "Endless EULA" "HQ")
    (take-credits state :corp)
    (let [eula (get-ice state :hq 0)
          credits (:credit (get-runner))]
      (rez state :corp eula)
      (run-on state "HQ")
      (run-continue state)
      (card-side-ability state :runner eula 0)
      (is (= (- credits 6) (:credit (get-runner))) "Runner should lose 6 credits"))))

(deftest endless-eula-testing-interaction-with-subs-not-resolving-mass-driver
  ;; Testing interaction with subs not resolving (Mass-Driver)
  (do-game
    (new-game {:corp {:deck ["Enigma" "Endless EULA"]
                      :credits 20}
               :runner {:deck ["Mass-Driver"]
                        :credits 20}})
    (play-from-hand state :corp "Endless EULA" "HQ")
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Mass-Driver")
    (let [eula (get-ice state :hq 0)
          enigma (get-ice state :hq 1)
          mass-driver (get-program state 0)]
      (run-on state :hq)
      (rez state :corp enigma)
      (run-continue state)
      (card-ability state :runner mass-driver 1)
      (card-ability state :runner mass-driver 0)
      (click-prompt state :runner "Force the Runner to lose [Click]")
      (click-prompt state :runner "End the run")
      (run-continue-until state :approach-ice eula)
      (rez state :corp eula)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -3]
            (card-side-ability state :runner eula 0))
          "Runner should only lose 3 credits"))))

(deftest engram-flush
  ;; Engram Flush
  (do-game
    (new-game {:corp {:deck ["Engram Flush"]}
               :runner {:hand ["Daily Casts" "Sure Gamble" "Dirty Laundry" "Political Operative" "Corroder"]}})
    (play-from-hand state :corp "Engram Flush" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [ef (get-ice state :hq 0)]
      (rez state :corp ef)
      (run-continue state)
      (click-prompt state :corp "Program")
      (card-subroutine state :corp ef 0)
      (is (= 0 (count (:discard (get-runner)))) "Heap is empty")
      (click-card state :corp "Corroder")
      (is (not (find-card "Corroder" (:hand (get-runner)))) "Corroder got trashed")
      (is (= 1 (count (:discard (get-runner)))) "Corroder in heap")
      (card-subroutine state :corp ef 0)
      (is (no-prompt? state :corp) "No prompt because no more fitting cards in grip"))))

(deftest enigma
  ;; Enigma - Force Runner to lose 1 click if able
  (do-game
    (new-game {:corp {:deck ["Enigma"]}})
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (let [enig (get-ice state :hq 0)]
      (run-on state "HQ")
      (is (= 3 (:click (get-runner))))
      (rez state :corp enig)
      (run-continue state)
      (card-subroutine state :corp enig 0)
      (is (= 2 (:click (get-runner))) "Runner lost 1 click"))))

(deftest envelope
  ;; Envelope - do 1 net damage, end the run
  (do-game
    (new-game {:corp {:deck ["Envelope"]}})
    (play-from-hand state :corp "Envelope" "HQ")
    (take-credits state :corp)
    (let [envl (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp envl)
      (run-continue state)
      (is (zero? (count (:discard (get-runner)))) "No discarded cards")
      (card-subroutine state :corp envl 0)
      (is (= 1 (count (:discard (get-runner)))) "1 card in discard pile")
      (is (:run @state) "Run still ongoing")
      (card-subroutine state :corp envl 1)
      (is (not (:run @state)) "Run ended"))))

(deftest envelopment
  ;; Envelopment
  (do-game
   (new-game {:corp {:hand ["Envelopment"] :credits 10}})
   (play-from-hand state :corp "Envelopment" "HQ")
   (let [env (get-ice state :hq 0)]
     (letfn [(subs-test [env n]
               ;; n power counters, n+1 subs, advance game state by a turn
               (is (= n (get-counters (refresh env) :power)) (str "Envelopment has "n" power counters"))
               (is (= (inc (get-counters (refresh env) :power)) (count (:subroutines (refresh env))))
                   "one more sub than power counters")
               (take-credits state :corp)
               (take-credits state :runner))]
       (rez state :corp env)
       ;; starts with 4 counters
       (subs-test env 4)
       (subs-test env 3)
       (subs-test env 2)
       (subs-test env 1)
       (subs-test env 0)
       (take-credits state :corp)
       (run-on state :hq)
       (run-continue state)
       (fire-subs state (refresh env))
       (is (= 1 (count (:discard (get-corp)))) "Envelopment was trashed")))))

(deftest envelopment-etr-does-not-trash
  (do-game
   (new-game {:corp {:hand ["Envelopment"] :credits 10}})
   (play-from-hand state :corp "Envelopment" "HQ")
   (let [env (get-ice state :hq 0)
         n 4]
     (rez state :corp env)
     (is (= n (get-counters (refresh env) :power)) (str "Envelopment has "n" power counters"))
     (is (= (inc (get-counters (refresh env) :power)) (count (:subroutines (refresh env))))
         "one more sub than power counters")
     (take-credits state :corp)
     (run-on state :hq)
     (run-continue state)
     (fire-subs state (refresh env)))
   (is (empty? (:discard (get-corp))))
   (is (not (:run @state)) "Run ended")))

(deftest envelopment-sub-indexed-correctly-7201
  ;; Envelopment should only have one trash subroutine at index 0 after running out of counters
  (do-game
    (new-game {:corp {:hand ["Envelopment"] :credits 10}})
    (play-from-hand state :corp "Envelopment" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (dotimes [_ 4]
      (take-credits state :runner)
      (take-credits state :corp))
    (is (= 1 (count (:subroutines (get-ice state :hq 0)))))
    (is (= 0 (:index (first (:subroutines (get-ice state :hq 0))))))))

(deftest excalibur
  ;; Excalibur - Prevent Runner from making another run this turn
  (do-game
    (new-game {:corp {:deck ["Excalibur"]}
               :runner {:deck ["Stimhack"]}})
    (play-from-hand state :corp "Excalibur" "HQ")
    (take-credits state :corp)
    (let [excal (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp excal)
      (run-continue state)
      (card-subroutine state :corp excal 0)
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state "R&D" {:wait-at-initiation true})
      (is (not (:run @state)) "No run initiated")
      (is (= 3 (:click (get-runner))))
      (play-from-hand state :runner "Stimhack")
      (is (not (:run @state)) "No run initiated")
      (is (= 3 (:click (get-runner))))
      (is (empty? (:discard (get-runner))) "Card not played from Grip")
      ; Check cannot run flag is cleared on next turn #2474
      (take-credits state :runner)
      (is (= :corp (:active-player @state)) "Corp turn")
      (core/gain state :runner :click 1)
      (run-on state "HQ")
      (is (:run @state) "Run initiated ok"))))

(deftest f2p
  ;; F2P
  (do-game
    (new-game {:corp {:deck ["F2P"]}
               :runner {:deck ["Inti" "Scrubber"]}})
    (play-from-hand state :corp "F2P" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Inti")
    (play-from-hand state :runner "Scrubber")
    (is (zero? (count (:hand (get-runner)))) "Runner's hand is empty")
    (run-on state "HQ")
    (let [f2p (get-ice state :hq 0)]
      (rez state :corp (refresh f2p))
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -2]
            (card-side-ability state :runner f2p 0)
            (click-prompt state :runner "Add an installed Runner card to the grip"))
          "Pay 2c to break sub")
      (card-subroutine state :corp (refresh f2p) 0)
      (is (changed? [(count (:hand (get-runner))) 1]
            (click-card state :corp "Inti"))
          "Bounce Inti to hand")
      (card-subroutine state :corp (refresh f2p) 0)
      (is (changed? [(count (:hand (get-runner))) 1]
            (click-card state :corp "Scrubber"))
          "Bounce Scrubber to hand")
      (card-subroutine state :corp (refresh f2p) 0)
      (is (no-prompt? state :corp) "F2P doesn't fire if no installed cards"))))

(deftest fairchild-1-0
  ;; Fairchild 1.0
  (do-game
    (new-game {:corp {:deck ["Fairchild 1.0"]}
               :runner {:deck ["Sacrificial Construct", "Clone Chip"]}})
    (play-from-hand state :corp "Fairchild 1.0" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Sacrificial Construct")
    (play-from-hand state :runner "Clone Chip")
    (let [fairchild (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp fairchild)
      (run-continue state)
      (card-subroutine state :corp fairchild 0)
      (click-prompt state :runner "Trash an installed card")
      (is (= "Choose an installed card to trash" (:msg (prompt-map :runner))))
      (click-card state :runner "Sacrificial Construct")
      (is (empty? (get-resource state)) "Sac Con trashed")
      (card-subroutine state :corp fairchild 1)
      (click-prompt state :runner "Trash an installed card")
      (is (= "Choose an installed card to trash" (:msg (prompt-map :runner))))
      (click-card state :runner "Clone Chip")
      (is (empty? (get-hardware state)) "Sac Con trashed"))))

(deftest fairchild-1-0-runner-cannoy-pay
  ;; Fairchild 1.0 - runner has no credits nor cards installed
  (do-game
    (new-game {:corp {:deck ["Fairchild 1.0"]}
               :runner {:credits 1}})
    (play-from-hand state :corp "Fairchild 1.0" "HQ")
    (take-credits state :corp)
    (let [fc1 (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp fc1)
      (run-continue state)
      (card-subroutine state :corp fc1 0)
      (is (= 1 (count (:choices (prompt-map :runner)))) "Only 1 choice in prompt")
      (is (changed? [(:credit (get-runner)) -1]
            (click-prompt state :runner "Pay 1 [Credits]"))
          "Paid 1c for subroutine")
      (card-subroutine state :corp fc1 1)
      (is (no-prompt? state :runner) "No prompt because we cannot pay!")
      (is (no-prompt? state :runner) "no lingering prompt"))))

(deftest fairchild-2-0
  ;; Fairchild 2.0
  (do-game
    (new-game {:corp {:deck ["Fairchild 2.0"]}
               :runner {:deck ["Sacrificial Construct", "Clone Chip", "Sure Gamble"]}})
    (play-from-hand state :corp "Fairchild 2.0" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Sacrificial Construct")
    (play-from-hand state :runner "Clone Chip")
    (let [fairchild (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp fairchild)
      (run-continue state)
      (card-subroutine state :corp fairchild 0)
      (click-prompt state :runner "Trash an installed card")
      (is (= "Choose an installed card to trash" (:msg (prompt-map :runner))))
      (click-card state :runner "Sacrificial Construct")
      (is (empty? (get-resource state)) "Sac Con trashed")
      (card-subroutine state :corp fairchild 1)
      (click-prompt state :runner "Trash an installed card")
      (is (= "Choose an installed card to trash" (:msg (prompt-map :runner))))
      (click-card state :runner "Clone Chip")
      (is (empty? (get-hardware state)) "Sac Con trashed")
      (card-subroutine state :corp fairchild 2)
      (is (= 1 (:brain-damage (get-runner))) "Runner took 1 core damage"))))

(deftest fairchild-3-0
  ;; Fairchild 3.0
  (do-game
    (new-game {:corp {:deck ["Fairchild 3.0"]}
               :runner {:deck ["Sacrificial Construct", "Clone Chip", "Sure Gamble"]}})
    (play-from-hand state :corp "Fairchild 3.0" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Sacrificial Construct")
    (play-from-hand state :runner "Clone Chip")
    (let [fairchild (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp fairchild)
      (run-continue state)
      (card-subroutine state :corp fairchild 0)
      (click-prompt state :runner "Trash an installed card")
      (is (= "Choose an installed card to trash" (:msg (prompt-map :runner))))
      (click-card state :runner "Sacrificial Construct")
      (is (empty? (get-resource state)) "Sac Con trashed")
      (card-subroutine state :corp fairchild 1)
      (click-prompt state :runner "Trash an installed card")
      (is (= "Choose an installed card to trash" (:msg (prompt-map :runner))))
      (click-card state :runner "Clone Chip")
      (is (empty? (get-hardware state)) "Sac Con trashed")
      (card-subroutine state :corp fairchild 2)
      (click-prompt state :corp "End the run")
      (is (not (:run @state)) "Run is ended"))))

(deftest fenris
  ;; Fenris - Illicit ice give Corp 1 bad publicity when rezzed
  (do-game
    (new-game {:corp {:deck ["Fenris"]}})
    (play-from-hand state :corp "Fenris" "HQ")
    (take-credits state :corp)
    (let [fen (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp fen)
      (run-continue state)
      (is (= 1 (count-bad-pub state)) "Gained 1 bad pub")
      (card-subroutine state :corp fen 0)
      (is (= 1 (:brain-damage (get-runner))) "Runner took 1 core damage")
      (is (= 1 (count (:discard (get-runner)))))
      (is (= 4 (hand-size :runner))))))

(deftest flare
  ;; Flare - Trash 1 program, do 2 unpreventable meat damage, and end the run
  (do-game
    (new-game {:corp {:deck ["Flare"]}
               :runner {:deck ["Plascrete Carapace" "Clone Chip" (qty "Cache" 3)]}})
    (play-from-hand state :corp "Flare" "HQ")
    (core/gain state :corp :credit 2)
    (take-credits state :corp)
    (play-from-hand state :runner "Plascrete Carapace")
    (play-from-hand state :runner "Clone Chip")
    (let [flare (get-ice state :hq 0)
          cc (get-hardware state 1)]
      (run-on state :hq)
      (rez state :corp flare)
      (run-continue state)
      (card-subroutine state :corp flare 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp cc)
      (is (= 1 (count (get-hardware state))) "Clone Chip trashed")
      (is (no-prompt? state :runner) "Plascrete didn't try preventing meat damage")
      (is (= 1 (count (:hand (get-runner)))))
      (is (= 3 (count (:discard (get-runner)))) "Clone Chip plus 2 cards lost from damage in discard")
      (is (not (:run @state)) "Run ended"))))

(deftest flyswatter-purge
  (do-game
    (new-game {:corp {:hand ["Flyswatter"]}
               :runner {:hand ["Lamprey"]}})
    (play-from-hand state :corp "Flyswatter" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Lamprey")
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 0))
    (is (= "Lamprey" (get-in @state [:runner :discard 0 :title])) "Trashed due to purge")))

(deftest flyswatter-etr-sub (do-game (etr-sub "Flyswatter" 0)))

(deftest formicary-verifies-basic-functionality
  ;; Verifies basic functionality
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 2) (qty "Formicary" 3)]}
               :runner {:deck [(qty "First Responders" 6)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Formicary" "Archives")
    (play-from-hand state :corp "Formicary" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "First Responders")
    (let [responders (get-resource state 0)]
      (run-on state "HQ")
      (run-continue state)
      (is (zero? (get-in @state [:run :position])) "Now approaching server")
      (run-continue state)
      (click-prompt state :corp "Formicary")
      (is (= "Rez and move Formicary protecting Archives at position 0 to protect the approached server?" (:msg (prompt-map :corp))))
      (click-prompt state :corp "Yes") ; Move Formicary
      (is (= 2 (count (get-in @state [:corp :servers :hq :ices]))) "2 pieces of ice protecting HQ")
      (is (= 1 (get-in @state [:run :position])) "Now encountering Formicary")
      (card-subroutine state :corp (get-ice state :hq 0) 0)
      (click-prompt state :runner "Suffer 2 net damage") ; take 2 net
      (is (= 2 (count (:discard (get-runner)))) "Did 2 net damage")
      (run-continue-until state :success)
      (click-prompt state :corp "No")
      (click-prompt state :runner "No action")
      (let [cards-in-hand (count (:hand (get-runner)))]
        (card-ability state :runner responders 0)
        (is (= (inc cards-in-hand) (count (:hand (get-runner)))) "First Responders was able to trigger"))
      (run-on state "Archives")
      (run-continue state)
      (click-prompt state :corp "Yes") ; Move Formicary
      (is (= 1 (get-in @state [:run :position])) "Now approaching Formicary")
      (card-subroutine state :corp (get-ice state :archives 0) 0)
      (click-prompt state :runner "End the run") ; ETR
      (is (not (get-in @state [:run])) "Formicary ended the run"))))

(deftest formicary-verifies-that-formicary-can-be-moved-to-the-innermost-positon-of-its-own-server
  ;; Verifies that Formicary can be moved to the innermost positon of its own server
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Formicary"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Formicary" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (run-continue-until state :movement) ; pass the first ice
    (run-continue-until state :movement) ; pass the second ice
    (is (zero? (get-in @state [:run :position])) "Now approaching server")
    (run-continue state)
    (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall is the innermost piece of ice before swap")
    (is (= "Formicary" (:title (get-ice state :hq 1))) "Formicary is the outermost piece of ice before swap")
    (click-prompt state :corp "Yes") ; Move Formicary
    (is (= 1 (get-in @state [:run :position])) "Now approaching the innermost piece of ice")
    (is (= "Formicary" (:title (get-ice state :hq 0))) "Formicary is the innermost piece of ice after swap")
    (is (= "Ice Wall" (:title (get-ice state :hq 1))) "Ice Wall is the outermost piece of ice after swap")))

(deftest formicary-no-prompt-if-unable-to-rez
  ;; No prompt if unable to rez
  (do-game
    (new-game {:corp {:deck ["Formicary"]}})
    (play-from-hand state :corp "Formicary" "HQ")
    (take-credits state :corp)
    (core/lose-credits state :corp nil 7)
    (run-on state "R&D")
    (run-continue state)
    (is (= "Formicary" (:title (get-ice state :hq 0))) "Formicary is on HQ")
    (is (zero? (:credit (get-corp))) "Corp does not have enough credits to rez Formicary")
    (is (not (get-run)) "The run has ended without prompting for Formicary")))

(deftest formicary-autoresolve-test
  ;; Autoresolve test
  (do-game
    (new-game {:corp {:deck ["Formicary"]}})
    (play-from-hand state :corp "Formicary" "HQ")
    (take-credits state :corp)
    (let [form (get-ice state :hq 0)]
      ;; Never resolve
      (card-ability state :corp form 0)
      (click-prompt state :corp "Never")
      (run-on state "R&D")
      (run-continue state)
      (is (= "Formicary" (:title (get-ice state :hq 0))) "Formicary is on HQ")
      (is (not (get-run)) "The run has ended without prompting for Formicary")
      ;; Always - not enough credits to rez
      (card-ability state :corp form 0)
      (click-prompt state :corp "Always")
      (core/lose-credits state :corp nil 7)
      (is (zero? (:credit (get-corp))) "Corp does not have enough credits to rez Formicary")
      (run-on state "R&D")
      (run-continue state)
      (is (= "Formicary" (:title (get-ice state :hq 0))) "Formicary is on HQ")
      (is (not (get-run)) "The run has ended without prompting for Formicary")
      ;; Always - enough credits to rez
      (core/gain-credits state :corp nil 2)
      (run-on state "R&D")
      (run-continue state)
      (is (= "Formicary" (:title (get-ice state :rd 0))) "Formicary is on R&D")
      (is (= 1 (get-in @state [:run :position])) "Now approaching Formicary")
      (is (get-run) "The run is still in progress"))))

(deftest formicary-autoresolve-qol-test
  ;; checks that if autoresolve is set to never, the prompt will be silent
  ;; this means you can prevent the runner from inferring formicary exists even with 2-3 of them
  ;; on the field
  (do-game
    (new-game {:corp {:deck [(qty "Formicary" 2) "Manegarm Skunkworks"]}})
    (play-from-hand state :corp "Formicary" "HQ")
    (play-from-hand state :corp "Formicary" "Archives")
    (play-from-hand state :corp "Manegarm Skunkworks" "HQ")
    (take-credits state :corp)
    (let [f1 (get-ice state :hq 0)
          f2 (get-ice state :archives 0)
          skunk (get-content state :hq 0)]
      ;; Never resolve
      (card-ability state :corp f1 0)
      (click-prompt state :corp "Never")
      (card-ability state :corp f2 0)
      (click-prompt state :corp "Never")
      (run-on state "R&D")
      (run-continue state)
      (is (= "Formicary" (:title (get-ice state :hq 0))) "Formicary is on HQ")
      (is (not (get-run)) "The run has ended without prompting for Formicary")
      (run-on state "HQ")
      (rez state :corp skunk)
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "Spend [Click][Click]")
      (click-prompt state :runner "No action"))))

(deftest formicary-with-trojan-doesnt-reset-mu-when-moving
  ;; Rezzing and moving Formicary with trojan doesn't make the trojan mu-cost disappear
  (do-game
    (new-game {:corp {:deck ["Formicary"]}
               :runner {:deck ["Saci"]}})
    (play-from-hand state :corp "Formicary" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Saci")
    (click-card state :runner (get-ice state :rd 0))
    (run-on state "HQ")
    (is (zero? (get-in @state [:run :position])) "Now approaching server")
    (run-continue state)
    (is (changed? [(core/available-mu state) 0]
          (click-prompt state :corp "Yes"))
        "Available MU should not change")))

(deftest formicary-tatu-swap
  ;; swapping Formicary in from Tatu-Bola ability should register Formicary ability
  (do-game
    (new-game {:corp {:hand ["Tatu-Bola" "Formicary"]}})
    (play-from-hand state :corp "Tatu-Bola" "R&D")
    (take-credits state :corp)
    (run-on state "R&D")
    (rez state :corp (get-ice state :rd 0))
    (run-continue state)
    (run-continue state :movement)
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "Formicary")
    (run-continue state)
    (is (= "Rez and move Formicary protecting R&D at position 0 to protect the approached server?" (:msg (prompt-map :corp))))))

(deftest formicary-project-yagi-uda-swap
  ;; swapping Formicary in via Project Yagi-Uda should register Formicary ability
  (do-game
    (new-game {:corp {:hand ["Ice Wall" "Formicary" "Project Yagi-Uda"]}})
    (core/gain state :corp :click 10 :credit 10)
    (play-from-hand state :corp "Project Yagi-Uda" "New remote")
    (let [pyu (get-content state :remote1 0)]
      (advance state pyu 4)
      (score state :corp (refresh pyu)))
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (run-on state "R&D")
    (card-ability state :corp (get-scored state :corp 0) 0)
    (click-card state :corp (get-ice state :rd 0))
    (click-card state :corp (find-card "Formicary" (:hand (get-corp))))
    (click-prompt state :runner "No")
    (run-continue state)
    (run-continue state)
    (is (= "Rez and move Formicary protecting R&D at position 0 to protect the approached server?" (:msg (prompt-map :corp))))))

(deftest formicary-tenma-line-swap
  ;; swapping Formicary via Tenma Line should keep Formicary ability
  (do-game
    (new-game {:corp {:hand ["Ice Wall" "Formicary" "Tenma Line"]}})
    (core/gain state :corp :click 10 :credit 10)
    (play-from-hand state :corp "Tenma Line" "New remote")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (play-from-hand state :corp "Formicary" "HQ")
    (let [iw (get-ice state :rd 0)
          fc (get-ice state :hq 0)
          tl (get-content state :remote1 0)]
      (rez state :corp tl)
      (card-ability state :corp (refresh tl) 0)
      (click-card state :corp iw)
      (click-card state :corp fc))
    (take-credits state :corp)
    (run-on state "Archives")
    (run-continue state)
    (is (= "Rez and move Formicary protecting R&D at position 0 to protect the approached server?" (:msg (prompt-map :corp))))))

(deftest formicary-ddos-functionality
  ;; formicary can be rezzed and then moved to the outermost position of a server if it was
  ;; not already the outermost ice of the server it lives on
  (do-game
    (new-game {:corp {:hand ["Formicary" "Vanilla"]}
               :runner {:hand ["DDoS"]}})
    (play-from-hand state :corp "Formicary" "HQ")
    (play-from-hand state :corp "Vanilla" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "DDoS")
    (card-ability state :runner (get-resource state 0) 0)
    (run-on state :rd)
    (run-continue state)
    (is (= "Rez and move Formicary protecting HQ at position 0 to protect the approached server?" (:msg (prompt-map :corp))))
    (click-prompt state :corp "Yes")
    (let [fc (get-ice state :rd 0)]
      (is (rezzed? fc) "formicary rezzed outermost on R&D"))))

(deftest free-lunch-basic-behavior
  ;; Basic behavior
  (do-game
    (new-game {:corp {:deck ["Free Lunch"]}})
    (play-from-hand state :corp "Free Lunch" "HQ")
    (take-credits state :corp)
    (let [fl (get-ice state :hq 0)]
      (rez state :corp fl)
      (run-on state "HQ")
      (run-continue state)
      (card-subroutine state :corp fl 0)
      (is (= 1 (get-counters (refresh fl) :power)) "Free Lunch has 1 power counter")
      (card-subroutine state :corp fl 0)
      (is (= 2 (get-counters (refresh fl) :power)) "Free Lunch has 2 power counters")
      (is (= 5 (:credit (get-runner))))
      (card-ability state :corp (refresh fl) 0)
      (is (= 1 (get-counters (refresh fl) :power)) "Free Lunch has 1 power counter")
      (is (= 4 (:credit (get-runner))) "Runner lost 1 credit"))))

(deftest free-lunch-derez-re-rez
  ;; Derez/re-rez
  (do-game
    (new-game {:corp {:hand ["Free Lunch"]
                      :credits 20}})
    (play-from-hand state :corp "Free Lunch" "HQ")
    (take-credits state :corp)
    (let [fl (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp fl)
      (run-continue state)
      (fire-subs state (refresh fl))
      (is (= 2 (get-counters (refresh fl) :power)) "Free Lunch has 2 power counters")
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (derez state :corp (refresh fl))
      (rez state :corp fl)
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (fire-subs state (refresh fl))
      (is (= 4 (get-counters (refresh fl) :power)) "Free Lunch has 4 power counters"))))

(deftest funhouse
  ;; Funhouse
  (do-game
    (new-game {:corp {:hand ["Funhouse"]}})
    (play-from-hand state :corp "Funhouse" "HQ")
    (take-credits state :corp)
    (let [tt (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp tt)
      (run-continue state)
      (click-prompt state :runner "End the run")
      (is (not (:run @state)) "Run ended")
      (run-on state "HQ")
      (run-continue state)
      (is (= 0 (count-tags state)))
      (click-prompt state :runner "Take 1 tag")
      (is (= 1 (count-tags state)))
      (fire-subs state tt)
      (click-prompt state :runner "Take 1 tag")
      (is (= 2 (count-tags state)))
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state "HQ")
      (run-continue state)
      (click-prompt state :runner "Take 1 tag")
      (fire-subs state tt)
      (is (= 5 (:credit (get-runner))))
      (click-prompt state :runner "Pay 4 [Credits]")
      (is (= 1 (:credit (get-runner))))
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state "HQ")
      (run-continue state)
      (click-prompt state :runner "Take 1 tag")
      (is (changed? [(count-tags state) 1]
            (fire-subs state tt)
            (is (= 1 (:credit (get-runner)))))
          "Skipped prompt if no option to pay"))))

(deftest funhouse-vs-jesminder
  (do-game
    (new-game {:corp {:hand ["Funhouse"]}
               :runner {:id "Jesminder Sareen: Girl Behind the Curtain"}})
    (play-from-hand state :corp "Funhouse" "HQ")
    (take-credits state :corp)
    (let [tt (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp tt)
      (run-continue state)
      (is (= ["End the run"] (prompt-buttons :runner)) "Only option should be 'End the run'")
      (click-prompt state :runner "End the run"))))

(deftest funhouse-vs-dorm-computer
  (do-game
    (new-game {:corp {:hand ["Funhouse"]}
               :runner {:hand ["Dorm Computer"]}})
    (play-from-hand state :corp "Funhouse" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Dorm Computer")
    (let [dorm (get-hardware state 0)
          tt (get-ice state :hq 0)]
      (card-ability state :runner dorm 0)
      (click-prompt state :runner "HQ")
      (run-continue state)
      (is (= :approach-ice (:phase (get-run))) "Run is in approach phase")
      (rez state :corp tt)
      (run-continue state)
      (is (= :encounter-ice (:phase (:run @state))) "Funhouse has been encountered")
      (is (= ["End the run"] (prompt-buttons :runner)) "Only option should be 'End the run'")
      (click-prompt state :runner "End the run"))))

(deftest funhouse-vs-qianju-pt
  (do-game
    (new-game {:corp {:hand ["Funhouse"]}
               :runner {:hand ["Qianju PT"]}})
    (play-from-hand state :corp "Funhouse" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Qianju PT")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (:runner-phase-12 @state) "Runner in Step 1.2")
    (let [pt (get-hardware state 0)
          tt (get-ice state :hq 0)]
      (card-ability state :runner pt 0)
      (end-phase-12 state :runner)
      (is (= 3 (:click (get-runner))) "Spent 1 click on Qianju PT")
      (run-on state :hq)
      (rez state :corp tt)
      (run-continue state)
      (is (= :encounter-ice (:phase (:run @state))) "Funhouse has been encountered")
      (is (= ["End the run"] (prompt-buttons :runner)) "Only option should be 'End the run'")
      (click-prompt state :runner "End the run"))))

(deftest gatekeeper-gatekeeper
  ;; Gatekeeper:
  (testing "basic tests"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 10)]
                        :hand ["Gatekeeper" "Posted Bounty" "Hostile Takeover"]
                        :discard [(qty "Ice Wall" 2) "Hostile Takeover"]}})
      ;; Actual test
      (play-from-hand state :corp "Gatekeeper" "New remote")
      (take-credits state :corp)
      (let [gate (get-ice state :remote1 0)
            hand (-> (get-corp) :hand count)
            deck (-> (get-corp) :deck count)
            num-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))
            hostile (find-card "Hostile Takeover" (:hand (get-corp)))]
        (run-on state "Server 1")
        (rez state :corp gate)
        (run-continue state)
        (is (= 6 (get-strength (refresh gate))))
        (card-subroutine state :corp gate 0)
        (click-prompt state :corp "3")
        (is (= (+ 3 hand) (-> (get-corp) :hand count)) "Corp should draw 3 cards")
        (click-card state :corp hostile)
        (click-card state :corp (find-card "Hostile Takeover" (:discard (get-corp))))
        (click-card state :corp (find-card "Posted Bounty" (:hand (get-corp))))
        (is (= deck (-> (get-corp) :deck count)) "R&D should have same number of cards as start")
        (is (= (inc num-shuffles) (count (core/turn-events state :corp :corp-shuffle-deck)))
            "Corp should shuffle R&D")
        (is (in-deck? (core/find-latest state hostile)) "Hostile Takeover should be in deck now")
        (card-subroutine state :corp gate 1)
        (is (not (:run @state)) "Gatekeeper subroutine should end the run")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (zero? (get-strength (refresh gate))) "Gatekeeper strength should be reset"))))
  (testing "log showing origin of cards"
    (testing "with cards chosen only from HQ"
      (do-game
        (new-game {:corp {:deck [(qty "Ice Wall" 10)]
                          :hand ["Gatekeeper"
                                 "Project Vitruvius"
                                 "NEXT Wave 2"
                                 "Accelerated Beta Test"]}})
        (play-from-hand state :corp "Gatekeeper" "HQ")
        (take-credits state :corp)
        (let [hand    (:hand (get-corp))
              gate    (get-ice state :hq 0)
              log-str (str "Corp uses Gatekeeper to reveal.+"
                           " from HQ, and shuffle them into R&D")]
          (run-on state "HQ")
          (rez state :corp gate)
          (run-continue state)
          (card-subroutine state :corp gate 0)
          (click-prompt state :corp "0")
          (click-card state :corp (find-card "Accelerated Beta Test" hand))
          (click-card state :corp (find-card "Project Vitruvius" hand))
          (click-card state :corp (find-card "NEXT Wave 2" hand))
          (is (last-log-contains? state log-str)))))
    (testing "with cards chosen only from Archives"
      (do-game
        (new-game {:corp {:deck [(qty "Ice Wall" 10)]
                          :hand ["Gatekeeper"]
                          :discard ["Efficiency Committee"
                                    "Hyperloop Extension"
                                    "Bifrost Array"]}})
        (play-from-hand state :corp "Gatekeeper" "HQ")
        (take-credits state :corp)
        (let [discard    (:discard (get-corp))
              gate    (get-ice state :hq 0)
              log-str (str "Corp uses Gatekeeper to reveal.+"
                           " from Archives, and shuffle them into R&D")]
          (run-on state "HQ")
          (rez state :corp gate)
          (run-continue state)
          (card-subroutine state :corp gate 0)
          (click-prompt state :corp "0")
          (click-card state :corp (find-card "Efficiency Committee" discard))
          (click-card state :corp (find-card "Hyperloop Extension" discard))
          (click-card state :corp (find-card "Bifrost Array" discard))
          (is (last-log-contains? state log-str)))))
    (testing "with cards chosen both from HQ and R&D"
      (do-game
        (new-game {:corp {:deck [(qty "Ice Wall" 10)]
                          :hand ["Gatekeeper"
                                 "Project Vitruvius"
                                 "NEXT Wave 2"
                                 "Accelerated Beta Test"]
                          :discard ["Efficiency Committee"
                                    "Hyperloop Extension"]}})
        (play-from-hand state :corp "Gatekeeper" "HQ")
        (take-credits state :corp)
        (let [hand    (:hand (get-corp))
              discard (:discard (get-corp))
              gate    (get-ice state :hq 0)
              log-str (str "Corp uses Gatekeeper to reveal.+ from HQ "
                           "and .+ from Archives, and shuffle them into R&D")]
          (run-on state "HQ")
          (rez state :corp gate)
          (run-continue state)
          (card-subroutine state :corp gate 0)
          (click-prompt state :corp "0")
          (click-card state :corp (find-card "Accelerated Beta Test" hand))
          (click-card state :corp (find-card "Project Vitruvius" hand))
          (click-card state :corp (find-card "Efficiency Committee" discard))
          (is (last-log-contains? state log-str)))))))

(deftest gatekeeper-makes-runner-wait
  (do-game
    (subroutine-test "Gatekeeper" 0 {:corp {:deck 5}})
    (is (waiting? state :runner) "Runner is waiting")
    (click-prompt state :corp "3")
    (is (waiting? state :runner) "Runner is still waiting")
    (click-prompt state :corp "Done")
    (is (not (waiting? state :runner)) "Runner is done waiting")))

(deftest gemini
  ;; Gemini - Successfully trace to do 1 net damage; do 1 net damage if trace strength is 5 or more regardless of success
  (do-game
    (new-game {:corp {:deck ["Gemini" (qty "Hedge Fund" 2)]}
               :runner {:deck [(qty "Sure Gamble" 3) (qty "Dirty Laundry" 2)]}})
    (play-from-hand state :corp "Gemini" "HQ")
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :corp)
    (let [gem (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp gem)
      (run-continue state)
      (card-subroutine state :corp gem 0)
      (click-prompt state :corp "3") ; boost to trace strength 5
      (click-prompt state :runner "0")
      (is (= 2 (count (:discard (get-runner)))) "Did 2 net damage")
      (card-subroutine state :corp gem 0)
      (click-prompt state :corp "3") ; boost to trace strength 5
      (click-prompt state :runner "5") ; match trace
      (is (= 3 (count (:discard (get-runner)))) "Did only 1 net damage for having trace strength 5 or more"))))

(deftest gemini-interaction-with-chronos-protocol-and-kicker
  ;; Interaction with Chronos Protocol and kicker
  (do-game
    (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                      :deck ["Gemini" (qty "Hedge Fund" 2)]}
               :runner {:deck ["Sure Gamble" (qty "Dirty Laundry" 2)]}})
    (play-from-hand state :corp "Gemini" "HQ")
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :corp)
    (let [gem (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp gem)
      (run-continue state)
      (card-subroutine state :corp gem 0)
      (click-prompt state :corp "3") ; boost to trace strength 5
      (click-prompt state :runner "0")
      (click-prompt state :corp "Yes")
      (click-card state :corp "Sure Gamble")
      (is (= 2 (count (:discard (get-runner)))) "Did 2 net damage"))))

(deftest gold-farmer-subroutine-test
  ;; Subroutine test
  (do-game
    (new-game {:corp {:hand ["Gold Farmer"]}})
    (play-from-hand state :corp "Gold Farmer" "HQ")
    (take-credits state :corp)
    (let [gf (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp gf)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -3]
            (card-subroutine state :corp gf 0)
            (click-prompt state :runner "Pay 3 [Credits]"))
          "Paid 3c for subroutine"))))

(deftest gold-farmer-lose-credit-for-breaking
  ;; Lose credit for breaking
  (do-game
    (new-game {:corp {:hand ["Gold Farmer"]}
               :runner {:hand ["Corroder"]
                        :credits 100}})
    (play-from-hand state :corp "Gold Farmer" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (let [gf (get-ice state :hq 0)
          cor (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp gf)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -2]
            (card-ability state :runner cor 0)
            (click-prompt state :runner "End the run unless the Runner pays 3 [Credits]")
            (click-prompt state :runner "Done")
            (is (last-log-contains? state "Corp uses Gold Farmer to force the runner to lose 1 [Credits] for breaking printed subs")
            "Correct message"))
          "Paid 1c + 1c for breaking"))))

(deftest gold-farmer-message-on-auto-pump-and-break
  ;; Message on auto-pump-and-break
  (do-game
    (new-game {:corp {:hand ["Gold Farmer"]}
               :runner {:hand ["Corroder"]
                        :credits 100}})
    (play-from-hand state :corp "Gold Farmer" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (let [gf (get-ice state :hq 0)
          cor (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp gf)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -4]
            (auto-pump-and-break state cor)
            (is (last-n-log-contains? state 2 "Corp uses Gold Farmer to force the runner to lose 1 [Credits] for breaking printed subs")
                "Correct messages")
            (is (last-n-log-contains? state 3 "Corp uses Gold Farmer to force the runner to lose 1 [Credits] for breaking printed subs")))
          "Paid 2c + 2c for breaking"))))

(deftest gold-farmer-interaction-with-paperclip
  ;; Interaction with Paperclip
  (do-game
    (new-game {:corp {:hand ["Gold Farmer"]}
               :runner {:hand ["Paperclip"]
                        :credits 100}})
    (play-from-hand state :corp "Gold Farmer" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Paperclip")
    (let [gf (get-ice state :hq 0)
          pc (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp gf)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -4]
            (auto-pump-and-break state pc))
          "Paid 2c + 2c for breaking"))))

(deftest gold-farmer-hippo-interaction-with-corroder
  ;; Hippo interaction with Corroder
  (do-game
    (new-game {:corp {:hand ["Gold Farmer"]}
               :runner {:hand ["Corroder" "Hippo"]
                        :credits 100}})
    (play-from-hand state :corp "Gold Farmer" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Hippo")
    (let [gf (get-ice state :hq 0)
          cor (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp gf)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -3]
            (auto-pump-and-break state cor)
            (click-prompt state :runner "Yes"))
          "Only got taxed once by Gold Farmer"))))

(deftest gold-farmer-hippo-interaction-with-laamb
  ;; Hippo interaction with Laamb
  (do-game
    (new-game {:corp {:hand ["Gold Farmer"]}
               :runner {:hand ["Laamb" "Hippo"]
                        :credits 100}})
    (play-from-hand state :corp "Gold Farmer" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Laamb")
    (play-from-hand state :runner "Hippo")
    (let [gf (get-ice state :hq 0)
          lam (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp gf)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -2]
            (auto-pump-and-break state lam)
            (click-prompt state :runner "Yes"))
          "Never got taxed by Gold Farmer"))))

(deftest gold-farmer-hippo-interaction-with-paperclip
  ;; Hippo interaction with Paperclip
  (do-game
    (new-game {:corp {:hand ["Gold Farmer"]}
               :runner {:hand ["Paperclip" "Hippo"]
                        :credits 100}})
    (play-from-hand state :corp "Gold Farmer" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Paperclip")
    (play-from-hand state :runner "Hippo")
    (let [gf (get-ice state :hq 0)
          pc (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp gf)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -2]
            (auto-pump-and-break state pc)
            (click-prompt state :runner "Yes"))
          "Never got taxed by Gold Farmer"))))

(deftest gold-farmer-gold-farmer-does-not-trigger-when-breaking-with-grappling-hook-4975
  ;; Gold Farmer does not trigger when breaking with Grappling Hook #4975
  (do-game
    (new-game {:corp {:hand ["Gold Farmer"]}
               :runner {:hand ["Grappling Hook"]
                        :credits 100}})
    (play-from-hand state :corp "Gold Farmer" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Grappling Hook")
    (let [gf (get-ice state :hq 0)
          gh (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp gf)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -1]
            (card-ability state :runner gh 0)
            (click-prompt state :runner "End the run unless the Runner pays 3 [Credits]"))
          "Get taxed 1c for breaking with Grappling Hook"))))

(deftest gyri-labyrinth
  ;; Gyri Labyrinth - reduce runner handsize by 2 until beginning of corp's next turn
  (do-game
    (new-game {:corp {:hand ["Gyri Labyrinth"]}})
    (play-from-hand state :corp "Gyri Labyrinth" "HQ")
    (take-credits state :corp)
    (let [gyri (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp gyri)
      (run-continue state)
      (is (= 5 (hand-size :runner)) "Runner starts with handsize of 5")
      (card-subroutine state :corp gyri 0)
      (is (= 3 (hand-size :runner)) "Runner handsize reduced to 3")
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (is (= 5 (hand-size :runner)) "Runner handsize returns to 5"))))

(deftest hafrun
  ;; Hafrún
  (do-game
    (new-game {:corp {:hand ["Hafrún" (qty "Ice Wall" 2) "Hedge Fund"]
                      :credits 10}
               :runner {:hand ["Buzzsaw" "Cleaver"]
                        :credits 50}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (play-from-hand state :corp "Hafrún" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Buzzsaw")
    (play-from-hand state :runner "Cleaver")
    (let [iw1 (get-ice state :hq 0)
          iw2 (get-ice state :rd 0)
          hafrun (get-ice state :hq 1)
          buzz (get-program state 0)
          clea (get-program state 1)]
      (run-on state "HQ")
      (rez state :corp hafrun)
      (click-prompt state :corp "Yes")
      (is (= :select (prompt-type :corp)))
      (click-card state :corp "Hedge Fund")
      (click-card state :corp clea)
      (is (has-icon? state (refresh clea) "H") "Cleaver has an icon")
      (run-continue state)
      (card-ability state :runner (refresh clea) 0)
      (is (no-prompt? state :runner) "Cleaver cannot be used")
      (card-ability state :runner (refresh buzz) 0)
      (click-prompt state :runner "End the run")
      (run-continue state)
      (run-continue state)
      (rez state :corp iw1)
      (run-continue state)
      (card-ability state :runner (refresh clea) 0)
      (is (no-prompt? state :runner) "Cleaver still cannot be used")
      (fire-subs state (refresh iw1))
      (is (no-icons? state (refresh clea)))
      (run-on state "R&D")
      (rez state :corp iw2)
      (run-continue state)
      (card-ability state :runner (refresh clea) 0)
      (click-prompt state :runner "End the run"))))

(deftest hafrun-wrong-server
  (do-game
    (new-game {:corp {:hand ["Hafrún" "Hedge Fund"]}})
    (play-from-hand state :corp "Hafrún" "HQ")
    (take-credits state :corp)
    (run-on state "R&D")
    (rez state :corp (get-ice state :hq 0))
    (is (no-prompt? state :corp) "Hafrún not active outside attacked server")))

(deftest hafrun-outside-run
  (do-game
    (new-game {:corp {:hand ["Hafrún" "Hedge Fund"]}})
    (play-from-hand state :corp "Hafrún" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (is (no-prompt? state :corp) "Hafrún not active outside run")))

(deftest hafrun-cant-afford
  (do-game
    (new-game {:corp {:hand ["Hafrún"]}})
    (play-from-hand state :corp "Hafrún" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (rez state :corp (get-ice state :hq 0))
    (is (no-prompt? state :corp) "Hafrún not active if Corp can't pay the cost")))

(deftest hagen-trashing-only-non-fracter-non-decoder-non-killer-cards
  ;; Trashing only non-fracter non-decoder non-killer cards.
  (do-game
    (new-game {:corp {:deck ["Hagen"]}
               :runner {:deck ["Inti" "Gordian Blade" "Pipeline" "Misdirection"]}})
    (play-from-hand state :corp "Hagen" "HQ")
    (take-credits state :corp)
    (let [hag (get-ice state :hq 0)]
      (core/gain state :corp :click 100)
      (play-from-hand state :runner "Inti")
      (play-from-hand state :runner "Gordian Blade")
      (play-from-hand state :runner "Pipeline")
      (play-from-hand state :runner "Misdirection")
      (run-on state "HQ")
      (rez state :corp hag)
      (run-continue state)
      (card-subroutine state :corp hag 0)
      (click-card state :corp "Inti") ; shouldn't trash
      (is (empty? (:discard (get-runner))) "Can't target fracter")
      (click-card state :corp "Gordian Blade") ; shouldn't trash
      (is (empty? (:discard (get-runner))) "Can't target decoder")
      (click-card state :corp "Pipeline") ; shouldn't trash
      (is (empty? (:discard (get-runner))) "Can't target killer")
      (click-card state :corp "Misdirection") ; should trash
      (is (= 1 (count (:discard (get-runner)))) "Misdirection trashed"))))

(deftest hagen-strength-decrease-with-installed-icebreakers
  ;; Strength decrease with installed icebreakers
  (do-game
    (new-game {:corp {:deck ["Hagen"]}
               :runner {:deck ["Inti" "Gordian Blade" "Pipeline" "Misdirection"]}})
    (play-from-hand state :corp "Hagen" "HQ")
    (take-credits state :corp)
    (let [hag (get-ice state :hq 0)]
      (core/gain state :runner :click 100)
      (core/gain state :runner :credit 100)
      (run-on state "HQ")
      (rez state :corp hag)
      (run-continue state)
      (is (= 6 (get-strength (refresh hag))) "Hagen is at base strength of 6.")
      (run-continue state :movement)
      (run-jack-out state)
      (play-from-hand state :runner "Inti")
      (run-on state "HQ")
      (is (= 5 (get-strength (refresh hag))) "Inti lowered strength to 5.")
      (run-continue state)
      (run-continue state :movement)
      (run-jack-out state)
      (play-from-hand state :runner "Gordian Blade")
      (run-on state "HQ")
      (is (= 4 (get-strength (refresh hag))) "Gordian Blade lowered strength to 4.")
      (run-continue state)
      (run-continue state :movement)
      (run-jack-out state)
      (play-from-hand state :runner "Pipeline")
      (run-on state "HQ")
      (is (= 3 (get-strength (refresh hag))) "Pipeline lowered strength to 3.")
      (run-continue state)
      (run-continue state :movement)
      (run-jack-out state)
      (play-from-hand state :runner "Misdirection")
      (run-on state "HQ")
      (is (= 3 (get-strength (refresh hag))) "Misdirection didn't lower strength."))))

(deftest hakarl-1-0-happy-path
  (do-game
    (new-game {:corp {:hand ["Hákarl 1.0" "Rashida Jaheem" "Eli 1.0" "Eli 1.0"]
                      :credit 20}
               :runner {:hand ["Sure Gamble"]}})
    (core/gain state :corp :click 4)
    (play-from-hand state :corp "Hákarl 1.0" "HQ")
    (play-from-hand state :corp "Eli 1.0" "New remote")
    (play-from-hand state :corp "Rashida Jaheem" "Server 1")
    (take-credits state :corp)
    (let [rash (get-content state :remote1 0)
          hakarl (get-ice state :hq 0)
          eli (get-ice state :remote1 0)]
      (rez state :corp rash)
      (run-on state "HQ")
      (is (rezzed? (refresh rash)) "Rashida is rezzed")
      (rez state :corp hakarl)
      (is (not (no-prompt? state :corp)))
      (click-card state :corp rash)
      (is (no-prompt? state :corp))
      (is (not (rezzed? (refresh rash))) "Rashida was derezzed")
      (run-continue state)
      (card-side-ability state :runner hakarl 0)
      (is (no-prompt? state :runner) "No prompt to break hakarl")
      (fire-subs state (refresh hakarl))
      (is (nil? (:run @state)))
      (is (= 1 (:brain-damage (get-runner))) "Runner took 1 core damage")
      (run-on state "Server 1")
      ;; effect lasts all turn
      (rez state :corp eli)
      (run-continue state)
      (card-side-ability state :runner eli 0)
      (is (no-prompt? state :runner) "no prompt to break eli")
      (fire-subs state (refresh eli))
      (take-credits state :runner)
      (take-credits state :corp)
      ;; check effect gone after turn end
      (run-on state "HQ")
      (run-continue state)
      (card-side-ability state :runner hakarl 0)
      (is (not (no-prompt? state :runner)) "Runner prompted to break hakarl")
      (click-prompt state :runner "Do 1 core damage")
      (click-prompt state :runner "End the run")
      (is (= 1 (:click (get-runner))) "Runner spent clicks breaking hakarl")
      (is (not (nil? (:run @state))))
      (fire-subs state (refresh hakarl))
      (is (= 1 (:brain-damage (get-runner))) "Runner did not take any extra core damage")
      (is (not (nil? (:run @state)))))))

(deftest hakarl-1-0-wrong-server
  (do-game
    (new-game {:corp {:hand ["Hákarl 1.0" "Rashida Jaheem" "Eli 1.0" "Eli 1.0"]
                      :credit 20}
               :runner {:hand ["Sure Gamble"]}})
    (core/gain state :corp :click 4)
    (play-from-hand state :corp "Hákarl 1.0" "HQ")
    (play-from-hand state :corp "Eli 1.0" "New remote")
    (play-from-hand state :corp "Rashida Jaheem" "Server 1")
    (take-credits state :corp)
    (let [rash (get-content state :remote1 0)
          hakarl (get-ice state :hq 0)]
      (rez state :corp rash)
      (run-on state "R&D")
      (is (rezzed? (refresh rash)) "Rashida is rezzed")
      (rez state :corp hakarl)
      (is (no-prompt? state :corp)))))

(deftest hakarl-1-0-no-targets
  (do-game
    (new-game {:corp {:hand ["Hákarl 1.0" "Rashida Jaheem" "Eli 1.0" "Eli 1.0"]
                      :credit 20}
               :runner {:hand ["Sure Gamble"]}})
    (core/gain state :corp :click 4)
    (play-from-hand state :corp "Hákarl 1.0" "HQ")
    (play-from-hand state :corp "Eli 1.0" "New remote")
    (play-from-hand state :corp "Rashida Jaheem" "Server 1")
    (take-credits state :corp)
    (let [hakarl (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp hakarl)
      (is (no-prompt? state :corp)))))

(deftest hakarl-1-0-outside-run
  (do-game
    (new-game {:corp {:hand ["Hákarl 1.0" "Rashida Jaheem" "Eli 1.0" "Eli 1.0"]
                      :credit 20}
               :runner {:hand ["Sure Gamble"]}})
    (core/gain state :corp :click 4)
    (play-from-hand state :corp "Hákarl 1.0" "HQ")
    (play-from-hand state :corp "Eli 1.0" "New remote")
    (play-from-hand state :corp "Rashida Jaheem" "Server 1")
    (take-credits state :corp)
    (let [rash (get-content state :remote1 0)
          hakarl (get-ice state :hq 0)]
      (rez state :corp rash)
      (is (rezzed? (refresh rash)) "Rashida is rezzed")
      (rez state :corp hakarl)
      (is (no-prompt? state :corp)))))

(deftest hailstorm-happy-path
  ;; Happy Path
  (do-game
    (new-game {:corp {:deck ["Hailstorm" "Hedge Fund"]}
               :runner {:deck ["Sure Gamble"]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hailstorm" "HQ")
    (take-credits state :corp)
    (let [hs (get-ice state :hq 0)]
      (rez state :corp hs)
      (play-from-hand state :runner "Sure Gamble")
      (run-on state "HQ")
      (run-continue state)
      (card-subroutine state :corp hs 0)
      (click-prompt state :corp "Sure Gamble")
      (card-subroutine state :corp hs 1)
      (is (nil? (:run @state)))
      (is (= ["Sure Gamble"] (->> (get-runner) :rfg (map :title))) "Sure Gamble should be rfg'd"))))

(deftest hailstorm-heap-locked-test
  ;; Heap Locked Test
  (do-game
    (new-game {:corp {:deck ["Hailstorm" "Hedge Fund" "Blacklist"]}
               :runner {:deck ["Sure Gamble"]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hailstorm" "HQ")
    (play-from-hand state :corp "Blacklist" "New remote")
    (take-credits state :corp)
    (let [hs (get-ice state :hq 0)]
      (rez state :corp hs)
      (rez state :corp (refresh (get-content state :remote1 0)))
      (play-from-hand state :runner "Sure Gamble")
      (run-on state "HQ")
      (run-continue state)
      (card-subroutine state :corp hs 0)
      (is (no-prompt? state :corp) "RFG prompt did not come up")
      (card-subroutine state :corp hs 1)
      (is (nil? (:run @state)))
      (is (= ["Sure Gamble"] (->> (get-runner) :discard (map :title))) "Sure Gamble should be in heap"))))

(deftest hammer
  (do-game
    (new-game {:corp {:hand ["Hammer"]}
               :runner {:hand ["Smartware Distributor" "Simulchip" "Aumakua"]}})
    (play-from-hand state :corp "Hammer" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :click 1)
    (play-from-hand state :runner "Smartware Distributor")
    (play-from-hand state :runner "Simulchip")
    (play-from-hand state :runner "Aumakua")
    (let [ham (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp ham)
      (run-continue state)
      (is (changed? [(count-tags state) 1]
                    (card-subroutine state :corp ham 0))
          "Runner got 1 tag")
      (is (changed? [(count (get-hardware state)) -1
                     (count (:discard (get-runner))) 1]
                    (card-subroutine state :corp ham 1)
                    (click-card state :corp (get-hardware state 0)))
          "Simulchip got trashed")
      (is (changed? [(count (get-program state)) -1
                     (count (:discard (get-runner))) 1]
                    (card-subroutine state :corp ham 2)
                    (click-card state :corp (get-program state 0)))
          "Aumakua got trashed")
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state "HQ")
      (run-continue state)
      (is (changed? [(count (get-resource state)) -1
                     (count (:discard (get-runner))) 1]
                    (card-subroutine state :corp ham 1)
                    (click-card state :corp (get-resource state 0)))
          "Smartware Distributor got trashed"))))

(deftest hammer-restrictions
  (do-game
    (new-game {:corp {:hand ["Hammer"] :credits 10}
               :runner {:hand ["Carmen" "Botulus" "Cookbook"] :credits 15}})
    (play-from-hand state :corp "Hammer" "HQ")
    (take-credits state :corp)
    (rez state :corp (get-ice state :hq 0))
    (play-from-hand state :runner "Carmen")
    (play-from-hand state :runner "Cookbook")
    (play-from-hand state :runner "Botulus")
    (click-card state :runner "Hammer")
    (run-on state :hq)
    (run-continue-until state :encounter-ice)
    (card-ability state :runner (first (:hosted (get-ice state :hq 0))) 0)
    (click-prompt state :runner "Done")
    (card-ability state :runner (get-program state 0) 1)
    (card-ability state :runner (get-program state 0) 0)
    (click-prompts state :runner "Choose a resource or piece of hardware to trash" "Done")
    (is (no-prompt? state :runner))
    (card-ability state :runner (first (:hosted (get-ice state :hq 0))) 0)
    (click-prompt state :runner "Give the Runner 1 tag")
    (is (no-prompt? state :runner) "Can no longer break stuff")))

(deftest hammer-restrictions-while-hushed
  (do-game
    (new-game {:corp {:hand ["Hammer"] :credits 10}
               :runner {:hand ["Botulus" "Cookbook" "Hush"] :credits 15}})
    (play-from-hand state :corp "Hammer" "HQ")
    (take-credits state :corp)
    (rez state :corp (get-ice state :hq 0))
    (play-cards state :runner "Cookbook" ["Botulus" "Hammer"] ["Hush" "Hammer"])
    (run-on state :hq)
    (run-continue-until state :encounter-ice)
    (card-ability state :runner (first (:hosted (get-ice state :hq 0))) 0)
    (click-prompts state :runner "Choose a resource or piece of hardware to trash" "Give the Runner 1 tag")
    (is (no-prompt? state :runner) "No more virus counters")))

(deftest harvester
  ;; Harvester - draw 3, then discard
  (do-game
    (new-game {:corp {:deck ["Harvester"]}
               :runner {:deck [(qty "Sure Gamble" 10)]
                        :hand ["The Class Act" "Sure Gamble" "Sure Gamble" "Sure Gamble" "Sure Gamble"]}})
    (play-from-hand state :corp "Harvester" "HQ")
    (let [harv (get-ice state :hq 0)]
      (rez state :corp harv)
      (take-credits state :corp)
      (play-from-hand state :runner "The Class Act")
      (run-on state "HQ")
      (run-continue state)
      (is (= 4 (count (:hand (get-runner)))) "Runner has 4 cards in hand")
      (card-subroutine state :corp harv 0)
      (is (= "The Class Act" (-> (prompt-map :runner) :card :title)) "The Class Act prompt showing")
      (is (= 2 (count (:prompt (get-runner)))) "Harvester prompt not open yet")
      (click-card state :runner (last (:set-aside (get-runner))))
      (is (= 7 (count (:hand (get-runner)))) "Runner bottomed Class Act draw")
      (is (= "Harvester" (-> (prompt-map :runner) :card :title)) "Harvester prompt showing")
      (click-card state :runner (last (:hand (get-runner))))
      (click-card state :runner (first (:hand (get-runner))))
      (is (= 5 (count (:hand (get-runner)))) "Harvester discarded some cards")
      (is (no-prompt? state :runner) "No more prompts for the Runner")
      (is (no-prompt? state :corp) "No more prompts for the Corp"))))

(deftest harvester-actually-is-a-discard
  (do-game
    (subroutine-test "Harvester" 0 {:runner {:id "Magdalene Keino-Chemutai: Cryptarchitect"
                                             :hand [(qty "Sure Gamble" 4) "Rezeki"]
                                             :deck ["Strike Fund" "Ika" "Steelskin Scarring"]}})
    (is (= 8 (count (:hand (get-runner)))) "Drew 3")
    (click-prompts state :runner "Strike Fund" "Rezeki" "Steelskin Scarring")
    (is (= 3 (count (:discard (get-runner)))) "Discarded 3")
    (click-prompt state :runner "Rezeki")
    (is (= "Rezeki" (:title (get-program state 0))) "Rezeki installed from discard")
    (is (no-prompt? state :runner) "No prompts for strike fund/steelskin")))

(deftest herald
  ;; Herald
  (do-game
    (new-game {:corp {:deck ["Herald" "Project Beale"]}})
    (play-from-hand state :corp "Herald" "HQ")
    (play-from-hand state :corp "Project Beale" "New remote")
    (let [herald (get-ice state :hq 0)
          beale (get-content state :remote1 0)]
      (rez state :corp herald)
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (is (= 4 (:credit (get-corp))))
      (card-subroutine state :corp herald 0)
      (is (= 6 (:credit (get-corp))))
      (card-subroutine state :corp herald 1)
      (click-prompt state :corp "2")
      (click-card state :corp beale)
      (is (= 4 (:credit (get-corp))) "Paid 2 credits through Herald second sub")
      (is (= 2 (get-counters (refresh beale) :advancement)) "Herald placed 2 advancement tokens"))))

(deftest herald-access-test
  ;; Access test
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Herald" "Project Beale"]}})
    (play-from-hand state :corp "Project Beale" "New remote")
    (let [beale (get-content state :remote1 0)]
      (take-credits state :corp)
      (run-empty-server state :hq)
      (is (= 7 (:credit (get-corp))))
      (is (= "Herald" (:title (core/get-current-ice state))) "Encountering Herald on access")
      (fire-subs state (core/get-current-ice state))
      (is (= 9 (:credit (get-corp))))
      (click-prompt state :corp "2")
      (click-card state :corp beale)
      (is (= 7 (:credit (get-corp))) "Paid 2 credits through Herald second sub")
      (is (= 2 (get-counters (refresh beale) :advancement)) "Herald placed 2 advancement tokens"))))

(deftest herald-partial-break
  ;; Partial break
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Herald" "Project Beale"]}
               :runner {:hand ["Unity"]}})
    (play-from-hand state :corp "Project Beale" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Unity")
    (let [unity (get-program state 0)]
      (run-empty-server state :hq)
      (is (= 7 (:credit (get-corp))))
      (is (= "Herald" (:title (core/get-current-ice state))) "Encountering Herald on access")
      (is (= 3 (count (:abilities (refresh unity)))) "Has auto break abilities")
      (card-ability state :runner unity 0)
      (click-prompt state :runner "Pay up to 2 [Credits] to place up to 2 advancement counters")
      (fire-subs state (core/get-current-ice state))
      (is (= 9 (:credit (get-corp))))
      (is (not= "How many advancement tokens?" (:msg (prompt-map :corp))) "Second subroutine did not fire"))))

(deftest hive
  ;; Hive - 5x ETR. Lose an ETR for each agenda point in corp's score area
  (do-game
    (new-game {:corp {:hand ["Hive" "Hostile Takeover" "Rebranding Team" "Government Takeover"]
                      :credits 50}})
    (core/gain state :corp :click 20)
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (play-from-hand state :corp "Rebranding Team" "New remote")
    (play-from-hand state :corp "Government Takeover" "New remote")
    (play-from-hand state :corp "Hive" "HQ")
    (let [ht (get-content state :remote1 0)
          rt (get-content state :remote2 0)
          gt (get-content state :remote3 0)
          hive (get-ice state :hq 0)]
      (rez state :corp hive)
      (is (= 5 (count (:subroutines (refresh hive)))) "Starts with 5 subs")
      (score-agenda state :corp ht)
      (is (= 1 (:agenda-point (get-corp))) "Hostile Takeover scored for 1 agenda point")
      (is (= 4 (count (:subroutines (refresh hive)))) "Loses one sub for scoring Hostile Takeover")
      (score-agenda state :corp rt)
      (is (= 3 (:agenda-point (get-corp))) "Rebranding Team scored for 2 agenda points")
      (is (= 2 (count (:subroutines (refresh hive)))) "Loses two more subs")
      (score-agenda state :corp gt)
      (is (= 9 (:agenda-point (get-corp))) "Government Takeover scored for 6 agenda points")
      (is (= 0 (count (:subroutines (refresh hive)))) "Hive has lost all subs"))))

(deftest holmegaard
  ;; Holmegaard - Stop Runner from accessing cards if win trace
  (do-game
    (new-game {:corp {:deck ["Holmegaard" "Hostile Takeover"]}
               :runner {:deck ["Cache" "Inti"]}})
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "Holmegaard" "HQ")
    (let [holm (get-ice state :hq 0)]
      (rez state :corp holm)
      (take-credits state :corp)
      (play-from-hand state :runner "Inti")
      (play-from-hand state :runner "Cache")
      (run-on state "HQ")
      (run-continue state)
      (card-subroutine state :corp holm 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (card-subroutine state :corp holm 1)
      (click-card state :corp "Cache")
      (is (empty? (:discard (get-runner))) "Can't target non-icebreaker program")
      (click-card state :corp "Inti")
      (is (= 1 (count (:discard (get-runner)))) "Inti trashed")
      (run-continue state)
      (run-continue state)
      ;; Prompt for "you cannot access any card this run"
      (click-prompt state :runner "OK")
      (is (not (accessing state "Hostile Takeover"))))))

(deftest howler
  ;; Howler
  (before-each
    [state (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                             :hand ["Howler" "Eli 1.0" "Hedge Fund"]
                             :discard ["Ichi 1.0"]}})
     _ (do (play-from-hand state :corp "Howler" "HQ")
           (take-credits state :corp))]
    (testing "Choosing from HQ"
      (do-game state
        (run-on state :hq)
        (let [howler (get-ice state :hq 0)]
          (rez state :corp howler)
          (run-continue state)
          (fire-subs state (refresh howler))
          (click-card state :corp "Eli 1.0")
          (is (find-card "Eli 1.0" (get-ice state :hq))))
        (run-continue-until state :encounter-ice (get-ice state :hq 0))
        (run-continue-until state :success)
        (click-prompt state :runner "No action")
        (is (not (rezzed? (get-ice state :hq 0))))
        (is (find-card "Howler" (:discard (get-corp))))))
    (testing "Choosing from Archives"
      (do-game state
        (run-on state :hq)
        (let [howler (get-ice state :hq 0)]
          (rez state :corp howler)
          (run-continue state)
          (fire-subs state (refresh howler))
          (click-card state :corp "Ichi 1.0")
          (is (find-card "Ichi 1.0" (get-ice state :hq))))
        (run-continue-until state :encounter-ice (get-ice state :hq 0))
        (run-continue-until state :success)
        (click-prompt state :runner "No action")
        (is (not (rezzed? (get-ice state :hq 0))))
        (is (find-card "Howler" (:discard (get-corp))))))))

(deftest howler-runner-can-jack-out
  (do-game
    (new-game {:corp {:hand ["Howler" "Eli 1.0"]}})
    (play-from-hand state :corp "Howler" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 0))
    (run-continue state :encounter-ice)
    (fire-subs state (get-ice state :hq 0))
    (click-card state :corp "Eli 1.0")
    (is (find-card "Eli 1.0" (get-ice state :hq)))
    (run-continue state :movement)
    (run-jack-out state)
    (is (no-prompt? state :runner))
    (is (not (:run @state)))
    (is (find-card "Howler" (:discard (get-corp))))))

(deftest howler-runner-can-break-ice
  (do-game
    (new-game {:corp {:hand ["Howler" "Eli 1.0"]}
               :runner {:hand ["Corroder"]
                        :credits 10}})
    (play-from-hand state :corp "Howler" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 0))
    (run-continue state :encounter-ice)
    (fire-subs state (get-ice state :hq 0))
    (click-card state :corp "Eli 1.0")
    (is (find-card "Eli 1.0" (get-ice state :hq)))
    (is (rezzed? (get-ice state :hq 0)) "Eli is rezzed")
    (run-continue state :movement)
    (run-continue-until state :encounter-ice)
    (is (= "Eli 1.0" (:title (core/get-current-ice state))) "The runner should be encountering Eli")
    (auto-pump-and-break state (get-program state 0))))

(deftest hydra
  ;; Hydra - do an effect Runner is tagged, otherwise give Runner 1 tag
  (do-game
    (new-game {:corp {:deck ["Hydra"]}})
    (play-from-hand state :corp "Hydra" "HQ")
    (take-credits state :corp)
    (core/gain state :corp :credit 10)
    (run-on state :hq)
    (let [hydra (get-ice state :hq 0)
          corp-creds (:credit (get-corp))]
      (rez state :corp hydra)
      (run-continue state)
      (is (= (- corp-creds 10) (:credit (get-corp))) "Cost 10 credits to rez Hydra")
      (is (not (is-tagged? state)) "Runner is not tagged approaching Hydra")
      (testing "Hydra subroutines give tags if Runner is not tagged"
        (doseq [n (range 3)]
          (card-subroutine state :corp hydra n)
          (is (= 1 (count-tags state)) (str "Hydra sub " (inc n) " gave Runner 1 tag"))
          (core/lose-tags state :runner (core/make-eid state) 1)))
      (testing "Hydra subroutines do their effect if the Runner is tagged"
        ;; Gain 1 tag to turn on main effect of subroutines
        (gain-tags state :runner 1)
        (is (is-tagged? state) "Runner is tagged")
        (is (= 3 (count (:hand (get-runner)))) "3 cards in Runner grip before Hydra damage")
        (card-subroutine state :corp hydra 0)
        (is (zero? (count (:hand (get-runner)))) "Hydra sub 1 did 3 damage when Runner is tagged")
        (card-subroutine state :corp hydra 1)
        (is (= (- corp-creds 5) (:credit (get-corp))) "Hydra sub 2 gave 5 credits to Corp when Runner is tagged")
        (is (:run @state) "Still a run going on before resolving last subroutine")
        (card-subroutine state :corp hydra 2)
        (is (not (:run @state)) "Hydra sub 3 ended the run when Runner is tagged")))))

(deftest ice-wall
  ;; Ice Wall
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall"]}})
    (play-from-hand state :corp "Ice Wall" "New remote")
    (let [iw (get-ice state :remote1 0)]
      (rez state :corp iw)
      (advance state iw 1)
      (is (= 2 (core/get-strength (refresh iw))))
      (take-credits state :corp)
      (run-on state :remote1)
      (run-continue state)
      (card-subroutine state :corp iw 0)
      (is (nil? (:run @state))))))

(deftest inazuma-cannot-jack-out-after-encounter-of-next-piece-of-ice
  ;; Cannot jack out after encounter of next piece of ice
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Inazuma" "Ice Wall" "Cortex Lock"]
                      :credits 30}
               :runner {:hand [(qty "Sure Gamble" 5)]
                        :credits 20}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Cortex Lock" "HQ")
    (play-from-hand state :corp "Inazuma" "HQ")
    (take-credits state :corp)
    (let [inazuma (get-ice state :hq 2)
          cl (get-ice state :hq 1)]
      (run-on state "HQ")
      (rez state :corp inazuma)
      (run-continue state)
      (fire-subs state (refresh inazuma))
      (run-continue-until state :movement)
      (run-jack-out state)
      (is (:run @state) "Runner cannot jack out")
      (run-continue state :approach-ice)
      (rez state :corp cl)
      (run-continue state)
      (fire-subs state cl)
      (run-continue-until state :movement)
      (run-jack-out state)
      (is (not (:run @state)) "Runner jacked out"))))

(deftest inazuma-cannot-break-subroutines-of-next-piece-of-ice
  ;; Cannot break subroutines of next piece of ice
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Inazuma" "Ice Wall" "Cortex Lock"]
                      :credits 30}
               :runner {:hand [(qty "Sure Gamble" 5) "Bukhgalter" "Corroder"]
                        :credits 20}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Cortex Lock" "HQ")
    (play-from-hand state :corp "Inazuma" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Bukhgalter")
    (play-from-hand state :runner "Corroder")
    (let [inazuma (get-ice state :hq 2)
          cortex-lock (get-ice state :hq 1)
          ice-wall (get-ice state :hq 0)
          bukhgalter (get-program state 0)
          corroder (get-program state 1)]
      (run-on state "HQ")
      (rez state :corp inazuma)
      (run-continue state)
      (fire-subs state (refresh inazuma))
      (run-continue-until state :approach-ice cortex-lock)
      (rez state :corp cortex-lock)
      (run-continue state)
      ;; Inazuma subs prevented break on next piece of ice
      (card-ability state :runner bukhgalter "Break 1 Sentry subroutine")
      (is (no-prompt? state :runner) "Bukhgalter can't break so no prompt")
      (is (changed? [(count (:hand (get-runner))) -2]
            (fire-subs state (refresh cortex-lock)))
          "2 net damage from Cortex Lock")
      ;; Next piece of ice is fine to break again
      (run-continue-until state :approach-ice ice-wall)
      (rez state :corp ice-wall)
      (run-continue state)
      (card-ability state :runner corroder 0)
      (click-prompt state :runner "End the run")
      (is (empty? (remove :broken (:subroutines (refresh ice-wall)))) "All subroutines broken"))))

(deftest information-overload
  ;; Information Overload
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Information Overload"]
                      :credits 6}})
    (play-from-hand state :corp "Information Overload" "HQ")
    (take-credits state :corp)
    (let [io (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp io)
      (is (zero? (count (:subroutines (refresh io)))))
      (run-continue state)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (count (:subroutines (refresh io)))))
      (gain-tags state :runner 1)
      (is (= 2 (count (:subroutines (refresh io)))))
      (core/lose-tags state :runner (core/make-eid state) 2)
      (core/fake-checkpoint state)
      (is (zero? (count (:subroutines (refresh io))))))))

(deftest interrupt-0
  ;; Interrupt 0
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Interrupt 0" "Battlement"]
                      :credits 20}
               :runner {:hand ["Corroder"]
                        :credits 10}})
    (play-from-hand state :corp "Battlement" "HQ")
    (play-from-hand state :corp "Interrupt 0" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (let [corroder (get-program state 0)
          battlement (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 1))
      (run-continue state)
      (card-subroutine state :corp (get-ice state :hq 1) 0)
      (run-continue-until state :approach-ice battlement)
      (rez state :corp battlement)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -1]
            (card-ability state :runner corroder 1))
          "Runner loses 1 credit only for boosting strength")
      (is (changed? [(:credit (get-runner)) -2]
            (card-ability state :runner corroder 0)
            (click-prompt state :runner "End the run")
            (click-prompt state :runner "Done"))
          "Runner should lose 2 credits, 1 for Interrupt 0, 1 for base ability")
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state "HQ")
      (run-continue-until state :encounter-ice battlement)
      (is (changed? [(:credit (get-runner)) -1]
            (card-ability state :runner corroder 0)
            (click-prompt state :runner "End the run")
            (click-prompt state :runner "Done"))
          "Runner should lose 1 for base ability as Interrupt 0 sub ends at end of run")
      (run-continue state :movement)
      (run-jack-out state))))

(deftest iq
  ;; IQ - Rez cost and strength equal to cards in HQ
  (do-game
    (new-game {:corp {:deck [(qty "IQ" 3) (qty "Hedge Fund" 3)]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "IQ" "R&D")
    (let [iq1 (get-ice state :rd 0)]
      (rez state :corp iq1)
      (is (and (= 4 (count (:hand (get-corp))))
               (= 4 (get-strength (refresh iq1)))
               (= 5 (:credit (get-corp)))) "4 cards in HQ: paid 4 to rez, has 4 strength")
      (play-from-hand state :corp "IQ" "HQ")
      (let [iq2 (get-ice state :hq 0)]
        (rez state :corp iq2)
        (is (and (= 3 (count (:hand (get-corp))))
                 (= 3 (get-strength (refresh iq1)))
                 (= 3 (get-strength (refresh iq2)))
                 (= 2 (:credit (get-corp)))) "3 cards in HQ: paid 3 to rez, both have 3 strength")))))

(deftest ireress
  ;; Information Overload
  ;; TODO: This is a bad test cuz losing bp isn't consistent
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ireress" "Hostile Takeover"]}})
    (play-from-hand state :corp "Ireress" "HQ")
    (let [irs (get-ice state :hq 0)]
      (rez state :corp irs)
      (is (zero? (count (:subroutines (refresh irs)))))
      (play-and-score state "Hostile Takeover")
      (is (= 1 (count (:subroutines (refresh irs))))))))

(deftest it-s-a-trap
  ;; It's a Trap! - 2 net dmg on expose, self-trash and make Runner trash installed card
  (do-game
    (new-game {:corp {:deck ["It's a Trap!"]}
               :runner {:deck [(qty "Cache" 3) (qty "Infiltration" 2)]}})
    (play-from-hand state :corp "It's a Trap!" "Archives")
    (let [iat (get-ice state :archives 0)]
      (take-credits state :corp)
      (play-from-hand state :runner "Infiltration")
      (click-prompt state :runner "Expose a card")
      (click-card state :runner iat)
      (is (= 3 (count (:discard (get-runner)))) "Did 2 net damage on expose")
      (play-from-hand state :runner "Cache")
      (run-on state "Archives")
      (rez state :corp iat)
      (run-continue state)
      (card-subroutine state :corp (refresh iat) 0)
      (click-card state :runner (get-program state 0))
      (is (= 4 (count (:discard (get-runner)))) "Cache trashed")
      (is (= 1 (count (:discard (get-corp)))) "It's a Trap trashed"))))

(deftest ivik
  (do-game
    (new-game {:corp {:hand [(qty "Mind Game" 2) "Ivik"] :credits 50}})
    (play-from-hand state :corp "Ivik" "New remote")
    (play-from-hand state :corp "Mind Game" "New remote")
    (play-from-hand state :corp "Mind Game" "New remote")
    (rez state :corp (get-ice state :remote2 0))
    (rez state :corp (get-ice state :remote3 0))
    (is (changed? [(:credit (get-corp)) -5]
          (rez state :corp (get-ice state :remote1 0)))
        "7 - 2 = 5 credits")))

(deftest jaguarundi-no-threat
  (do-game
    (new-game {:corp {:hand ["City Works Project" "Jaguarundi"]}})
    (play-and-score state "City Works Project")
    (play-from-hand state :corp "Jaguarundi" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 0))
    (run-continue state :encounter-ice)
    (is (no-prompt? state :runner) "no jaguarundi prompt")
    (fire-subs state (get-ice state :hq 0))
    (is (= 1 (count-tags state)))
    (is (= 1 (:brain-damage (get-runner))))))

(deftest jaguarundi-jesminder-interaction
  (do-game
    (new-game {:corp {:hand ["Jaguarundi"]}
               :runner {:id "Jesminder Sareen: Girl Behind the Curtain"}})
    (play-from-hand state :corp "Jaguarundi" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 0))
    (run-continue state :encounter-ice)
    (is (changed? [(:brain-damage (get-runner)) 0]
          (fire-subs state (get-ice state :hq 0)))
        "Got no core damage")))

(deftest jaguarundi-threat-ability
  (do-game
    (new-game {:corp {:hand ["City Works Project" "Hostile Takeover" "Jaguarundi"]}})
    (core/gain state :corp :click 10)
    (core/gain state :corp :credit 10)
    (play-and-score state "City Works Project")
    (play-and-score state "Hostile Takeover")
    (is (= 2 (count (:scored (get-corp)))))
    (play-from-hand state :corp "Jaguarundi" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 0))
    (run-continue state :encounter-ice)
    (is (changed? [(:click (get-runner)) -1]
          (click-prompt state :runner "Spend [Click]"))
        "spent a click on jag")
    (is (no-prompt? state :runner) "no jaguarundi prompt")))

(deftest jua-encounter-effect-prevent-runner-from-installing-cards-for-the-rest-of-the-turn
  ;; Encounter effect - Prevent Runner from installing cards for the rest of the turn
  (do-game
    (new-game {:corp {:deck ["Jua"]}
               :runner {:deck ["Desperado" "Sure Gamble"]}})
    (play-from-hand state :corp "Jua" "HQ")
    (take-credits state :corp)
    (let [jua (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp jua)
      (run-continue state)
      (run-continue state)
      (run-continue state)
      (is (= 2 (count (:hand (get-runner)))) "Runner starts with 2 cards in hand")
      (play-from-hand state :runner "Desperado")
      (is (= 2 (count (:hand (get-runner)))) "No cards installed")
      (play-from-hand state :runner "Sure Gamble")
      (is (= 1 (count (:hand (get-runner)))) "Can play events")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 1 (count (:hand (get-runner)))) "Runner starts with 1 cards in hand")
      (play-from-hand state :runner "Desperado")
      (is (zero? (count (:hand (get-runner)))) "Card installed"))))

(deftest jua-subroutine-effect-choose-2-runner-cards-runner-moves-one-to-the-stack
  ;; Subroutine effect - Choose 2 runner cards, runner moves one to the stack
  (do-game
    (new-game {:corp {:deck ["Jua"]}
               :runner {:deck ["Desperado" "Gordian Blade"]}})
    (play-from-hand state :corp "Jua" "HQ")
    (take-credits state :corp)
    (let [jua (get-ice state :hq 0)]
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Desperado")
      (run-on state "HQ")
      (rez state :corp jua)
      (run-continue state)
      (card-subroutine state :corp (refresh jua) 0)
      (is (no-prompt? state :corp) "Can't fire for 1 installed card")
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Gordian Blade")
      (run-on state "HQ")
      (run-continue state)
      (card-subroutine state :corp (refresh jua) 0)
      (click-card state :corp "Gordian Blade")
      (click-card state :corp "Desperado")
      (click-card state :runner "Gordian Blade")
      (is (nil? (get-program state 0)) "Card is uninstalled")
      (is (= 1 (count (:deck (get-runner)))) "Runner puts card in deck"))))

(deftest jua-should-only-lock-installing-for-runner-not-for-both-sides
  ;; Should only lock installing for Runner, not for both sides
  (do-game
    (new-game {:corp {:id "Mti Mwekundu: Life Improved"
                      :deck ["Jua" "Kakugo"]}
               :runner {:hand ["Sure Gamble"]
                        :discard ["Paperclip"]}})
    (play-from-hand state :corp "Jua" "HQ")
    (let [jua (get-ice state :hq 0)]
      (take-credits state :corp)
      (run-on state "HQ")
      (rez state :corp jua)
      (run-continue state)
      (is (= :encounter-ice (:phase (:run @state))) "Jua encounter effect happens")
      (run-continue state)
      (is (zero? (get-in @state [:run :position])) "Initial position approaching server")
      (run-continue state)
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Kakugo" (:hand (get-corp))))
      (click-prompt state :runner "No")
      (is (= 1 (get-in @state [:run :position])) "Now approaching Kakugo")
      (is (= "Kakugo" (:title (get-ice state :hq 0))) "Kakugo was installed")
      (is (empty? (:hand (get-corp))) "Kakugo removed from HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state :encounter-ice)
      (is (no-prompt? state :runner) "Runner can't install Paperclip because of Jua encounter ability")
      (run-continue state :movement)
      (is (= 2 (-> (get-runner) :discard count)) "Runner should take 1 net damage from Kakugo"))))

(deftest kakugo-ability-continues-to-work-when-ice-is-swapped
  ;; ability continues to work when ice is swapped
  (do-game
    (new-game {:corp {:deck ["Kakugo" "Ice Wall"]}})
    (play-from-hand state :corp "Kakugo" "R&D")
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp)
    (let [kakugo (get-ice state :rd 0)
          ice-wall (get-ice state :archives 0)]
      (run-on state "R&D")
      (rez state :corp kakugo)
      (run-continue-until state :movement)
      (run-jack-out state)
      (is (= 2 (count (:hand (get-runner)))) "Runner took damage before swap")
      (core/swap-ice state :corp (refresh kakugo) (refresh ice-wall))
      (core/fake-checkpoint state)
      (run-on state "Archives")
      (run-continue-until state :movement)
      (run-jack-out state)
      (is (= 1 (count (:hand (get-runner)))) "Runner took damage after swap"))))

(deftest kakugo-after-wrassling-kakugo-should-still-do-damage-despite-temporary-card-change
  ;; After wrassling, Kakugo should still do damage despite temporary card change
  (do-game
    (new-game {:corp {:deck ["Kakugo"]}
               :runner {:deck ["Engolo" (qty "Sure Gamble" 2)]}})
    (play-from-hand state :corp "Kakugo" "R&D")
    (take-credits state :corp) ;; This also ends the corps turn.
    (play-from-hand state :runner "Sure Gamble") ;; Needed to ensure that we can pay for Kakugo + ability
    (play-from-hand state :runner "Engolo")
    (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
    (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card before run")
    (let [kakugo (get-ice state :rd 0)]
      (run-on state "R&D")
      (rez state :corp kakugo)
      (run-continue state)
      (click-prompt state :runner "Yes")
      (is (has-subtype? (refresh kakugo) "Code Gate") "Kakugo was made into a code gate")
      (run-continue state)
      (is (empty? (:hand (get-runner))) "Runner took damage passing kakugo"))))

(deftest kamali-1-0
  ;; Kamali 1.0
  (do-game
    (new-game {:corp {:deck ["Kamali 1.0"]}
               :runner {:deck ["Astrolabe" "Decoy" "Cache"]}})
    (play-from-hand state :corp "Kamali 1.0" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Astrolabe")
    (play-from-hand state :runner "Decoy")
    (play-from-hand state :runner "Cache")
    (let [kamali (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp kamali)
      (run-continue state)
      (card-subroutine state :corp kamali 0)
      (is (zero? (:brain-damage (get-runner))) "Runner starts with 0 core damage")
      (click-prompt state :runner "Take 1 core damage")
      (is (= 1 (:brain-damage (get-runner))) "Runner took 1 core damage")
      (card-subroutine state :corp kamali 1)
      (is (empty? (:discard (get-runner))) "Runner starts with no discarded cards")
      (click-prompt state :runner "Trash 1 installed piece of hardware")
      (click-card state :runner (get-hardware state 0))
      (is (empty? (get-hardware state)) "Astrolabe trashed")
      (is (= 1 (count (:discard (get-runner)))) "Runner trashed 1 card")
      (card-subroutine state :corp kamali 2)
      (is (= 1 (count (:discard (get-runner)))) "Runner starts with 1 discarded card")
      (click-prompt state :runner "Trash 1 installed program")
      (click-card state :runner (get-program state 0))
      (is (empty? (get-program state)) "Cache trashed")
      (is (= 2 (count (:discard (get-runner)))) "Runner trashed 1 card"))))

(deftest kamali-1-0-runner-has-no-installed-cards
  ;; Kamali 1.0
  (do-game
    (new-game {:corp {:deck ["Kamali 1.0"]}
               :runner {:hand [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Kamali 1.0" "HQ")
    (take-credits state :corp)
    (let [kamali (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp kamali)
      (run-continue state)
      (dotimes [i 3]
        (card-subroutine state :corp kamali i)
        (is (= 1 (count (:choices (prompt-map :runner)))) "Only 1 choice in prompt")
        (click-prompt state :runner "Take 1 core damage")))))

(deftest kamali-1-0-fire-all-subs
  (do-game
    (new-game {:corp {:hand ["Kamali 1.0"]}})
    (play-from-hand state :corp "Kamali 1.0" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 0))
    (run-continue state :encounter-ice)
    (fire-subs state (get-ice state :hq 0))
    (dotimes [_ 3]
      (click-prompt state :runner "Take 1 core damage"))
    (is (no-prompt? state :runner) "No lingering prompt (runner)")
    (is (no-prompt? state :runner) "No lingering prompt (corp)")))

(deftest karuna
  (do-game
    (new-game {:corp {:hand ["Karunā"]}
               :runner {:hand [(qty "Sure Gamble" 3) (qty "Easy Mark" 3)]}})
    (play-from-hand state :corp "Karunā" "HQ")
    (take-credits state :corp)
    (let [kar (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp kar)
      (run-continue state)
      (is (= 0 (count (:discard (get-runner)))) "Heap Empty")
      (fire-subs state kar)
      (is (= 2 (count (:discard (get-runner)))) "2 cards trashed")
      (click-prompt state :runner "Yes")
      (is (nil? (:run @state)) "Runner jacked out")
      (is (= 2 (count (:discard (get-runner)))) "2 cards trashed, 2nd sub didn't fire")
      (run-on state "HQ")
      (run-continue state)
      (fire-subs state kar)
      (is (= 4 (count (:discard (get-runner)))) "4 cards trashed")
      (click-prompt state :runner "No")
      (is (= 6 (count (:discard (get-runner)))) "6 cards trashed")
      (is (not (nil? (:run @state))) "Run Continues"))))

(deftest kitsune-corp-choices-card-for-runner-to-access
  ;; Corp choices card for Runner to access
  (do-game
    (new-game {:corp {:hand ["Kitsune" "Snare!"]}})
    (play-from-hand state :corp "Kitsune" "R&D")
    (take-credits state :corp)
    (run-on state "R&D")
    (let [kitsune (get-ice state :rd 0)]
      (rez state :corp kitsune)
      (run-continue state)
      (card-subroutine state :corp kitsune 0)
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Snare!" (:hand (get-corp))))
      ;; Runner access Snare! corp has prompt
      (is (waiting? state :runner)
          "Runner has prompt to wait for Corp to use Snare!")
      (click-prompt state :corp "Yes")
      (click-prompt state :runner "No action")
      (is (= "Kitsune" (-> (get-corp) :discard first :title)) "Kitsune was trashed after use"))))

(deftest kessleroid-subs-test
  (do-game (etr-sub "Kessleroid" 0))
  (do-game (etr-sub "Kessleroid" 1)))

(deftest kitsune-trash-after-use
  ;; Trash after use
  (do-game
    (new-game {:corp {:hand ["Kitsune" "Snare!" "Hostile Takeover"]}})
    (play-from-hand state :corp "Kitsune" "R&D")
    (take-credits state :corp)
    (run-on state "R&D")
    (let [kitsune (get-ice state :rd 0)]
      (rez state :corp kitsune)
      (run-continue state)
      (card-subroutine state :corp kitsune 0)
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Snare!" (:hand (get-corp))))
      ;; Runner access Snare! corp has prompt
      (is (waiting? state :runner)
          "Runner has prompt to wait for Corp to use Snare!")
      (click-prompt state :corp "Yes")
      (click-prompt state :runner "No action")
      (is (= "Kitsune" (-> (get-corp) :discard first :title)) "Kitsune was trashed after use"))))

(deftest klevetnik
  ;; Klevetnik
  (do-game
    (new-game {:corp {:hand ["Klevetnik"]}
               :runner {:hand [(qty "No Free Lunch" 2) "Keiko"]}})
    (play-from-hand state :corp "Klevetnik" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Keiko")
    (play-from-hand state :runner "No Free Lunch")
    (play-from-hand state :runner "No Free Lunch")
    (let [klev (get-ice state :hq 0)
          nfl1 (get-resource state 0)
          nfl2 (get-resource state 1)]
      (run-on state "HQ")
      (rez state :corp klev)
      (click-prompt state :corp "Yes")
      (click-card state :corp (get-hardware state 0))
      (is (= :select (prompt-type :corp)) "Hardware cannot be selected")
      (is (changed? [(:credit (get-runner)) 2]
            (click-card state :corp nfl1))
          "Runner gained 2 Credits thanks to Klevetnik's on-rez ability")
      (is (not (:playable (first (:abilities (refresh nfl1))))) "NFL abilities are not playable")
      (is (changed? [(:credit (get-runner)) 3]
            (card-ability state :runner (refresh nfl2) 0))
          "Other No Free Lunch was not blanked")
      (run-continue state)
      (card-subroutine state :corp klev 0)
      (is (not (:run @state)) "The run should have ended")
      (take-credits state :runner)
      (is (not (:playable (first (:abilities (refresh nfl1))))) "NFL abilities are still not playable")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (changed? [(:credit (get-runner)) 3]
            (card-ability state :runner (refresh nfl1) 0))
          "No Free Lunch unblanked"))))

(deftest klevetnik-wrong-server
  (do-game
    (new-game {:corp {:hand ["Klevetnik"]}
               :runner {:hand ["No Free Lunch"]}})
    (play-from-hand state :corp "Klevetnik" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "No Free Lunch")
    (run-on state :rd)
    (rez state :corp (get-ice state :hq 0))
    (is (no-prompt? state :corp) "Klevetnik not active outside attacked server")))

(deftest klevetnik-outside-run
  (do-game
    (new-game {:corp {:hand ["Klevetnik"]}
               :runner {:hand ["No Free Lunch"]}})
    (play-from-hand state :corp "Klevetnik" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "No Free Lunch")
    (rez state :corp (get-ice state :hq 0))
    (is (no-prompt? state :corp) "Klevetnik not active outside run")))

(deftest klevetnik-on-the-corp-turn
  ;; Klevetnik - effect should last until the end of the Corp next turn
  (do-game
    (new-game {:corp {:hand ["Klevetnik" "An Offer You Can't Refuse"]}
               :runner {:hand ["No Free Lunch"]}})
    (play-from-hand state :corp "Klevetnik" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "No Free Lunch")
    (let [klev (get-ice state :hq 0)
          nfl (get-resource state 0)]
      (take-credits state :runner)
      (play-from-hand state :corp "An Offer You Can't Refuse")
      (click-prompt state :corp "HQ")
      (click-prompt state :runner "Yes")
      (run-continue state)
      (rez state :corp klev)
      (click-prompt state :corp "Yes")
      (is (changed? [(:credit (get-runner)) 2]
            (click-card state :corp nfl))
          "Runner gained 2 Credits thanks to Klevetnik's on-rez ability")
      (is (not (:playable (first (:abilities (refresh nfl))))) "NFL abilities are not playable")
      (run-continue state)
      (card-subroutine state :corp klev 0)
      (take-credits state :corp) ;; End of the Corp current turn
      (is (not (:playable (first (:abilities (refresh nfl))))) "NFL abilities are still not playable")
      (take-credits state :runner)
      (take-credits state :corp) ;; End of the Corp next turn
      (is (changed? [(:credit (get-runner)) 3]
            (card-ability state :runner (refresh nfl) 0))
          "No Free Lunch unblanked"))))

(deftest komainu-subroutine-gain-loss-ability
  ;; Subroutine gain/loss ability
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Komainu"]}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["Sure Gamble"]}})
    (play-from-hand state :corp "Komainu" "HQ")
    (take-credits state :corp)
    (let [ko (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp ko)
      (run-continue state)
      (is (= 1 (count (:subroutines (refresh ko)))) "1 card in hand, no existing subs")
      (draw state :runner 1)
      (core/redirect-run state :corp "HQ" :approach-ice)
      (run-continue state)
      (run-continue state)
      (is (= 3 (count (:subroutines (refresh ko)))) "2 cards in hand, 1 existing sub")
      (draw state :runner 1)
      (core/redirect-run state :corp "HQ" :approach-ice)
      (run-continue state)
      (run-continue state)
      (is (= 6 (count (:subroutines (refresh ko)))) "3 cards in hand, 3 existing subs")
      (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (core/redirect-run state :corp "HQ" :approach-ice)
      (run-continue state)
      (run-continue state)
      (is (= 7 (count (:subroutines (refresh ko)))) "1 card in hand, 6 existing subs"))))

(deftest komainu-subroutines-not-going-away-until-end-of-run
  ;; Subroutines not going away until end of run
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Komainu"]}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["Sure Gamble"]}})
    (play-from-hand state :corp "Komainu" "R&D")
    (take-credits state :corp)
    (run-on state :rd)
    (let [ko (get-ice state :rd 0)]
      (rez state :corp ko)
      (run-continue state)
      (is (= 1 (count (:subroutines (refresh ko)))))
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "No action")
      (is (zero? (count (:subroutines (refresh ko))))))))

(deftest konjin-return-to-encountering-konjin-after-forced-encounter
  ;; Return to encountering Konjin after forced encounter
  (do-game
    (new-game {:corp {:hand ["Ice Wall" "Konjin"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Konjin" "R&D")
    (take-credits state :corp)
    (let [konjin (get-ice state :rd 0)
          iw (get-ice state :hq 0)]
      (rez state :corp konjin)
      (rez state :corp iw)
      (run-on state :rd)
      (run-continue state)
      (is (= (refresh konjin) (core/get-current-ice state)) "The runner should be encountering Konjin")
      (is (= "Choose an amount to spend for Konjin" (:msg (prompt-map :corp))) "Psi Game")
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (= "Choose a piece of ice" (:msg (prompt-map :corp))) "Prompt to choose Ice")
      (click-card state :corp iw)
      (is (= (refresh iw) (core/get-current-ice state)) "The runner should be encountering Ice Wall")
      (run-continue state :encounter-ice)
      (is (= (refresh konjin) (core/get-current-ice state)) "The runner should be back to encountering Konjin"))))

(deftest konjin-end-run-completely-if-forced-encounter-ends-the-run
  ;; End run completely if forced encounter ends the run
  (do-game
    (new-game {:corp {:hand ["Ice Wall" "Konjin"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Konjin" "R&D")
    (take-credits state :corp)
    (let [konjin (get-ice state :rd 0)
          iw (get-ice state :hq 0)]
      (rez state :corp konjin)
      (rez state :corp iw)
      (run-on state :rd)
      (run-continue state)
      (is (= (refresh konjin) (core/get-current-ice state)) "The runner should be encountering Konjin")
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (click-card state :corp iw)
      (is (= (refresh iw) (core/get-current-ice state)) "The runner should be encountering Ice Wall")
      (fire-subs state (refresh iw))
      (is (not (:run @state)) "The run should have ended"))))

(deftest konjin-target-ice-gets-trashed
  ;; Return to encountering Konjin after forced encounter
  (do-game
    (new-game {:corp {:hand ["Ice Wall" "Konjin"]}
               :runner {:hand ["Arruaceiras Crew"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Konjin" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Arruaceiras Crew")
    (let [konjin (get-ice state :rd 0)
          iw (get-ice state :hq 0)]
      (rez state :corp konjin)
      (rez state :corp iw)
      (run-on state :rd)
      (run-continue state)
      (is (= (refresh konjin) (core/get-current-ice state)) "The runner should be encountering Konjin")
      (is (= "Choose an amount to spend for Konjin" (:msg (prompt-map :corp))) "Psi Game")
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (= "Choose a piece of ice" (:msg (prompt-map :corp))) "Prompt to choose Ice")
      (click-card state :corp iw)
      (is (= (refresh iw) (core/get-current-ice state)) "The runner should be encountering Ice Wall")
      (card-ability state :runner (get-resource state 0) 0)
      (card-ability state :runner (get-resource state 0) 1)
      (is (= "Ice Wall" (-> (get-corp) :discard first :title)) "Trashed ice wall")
      (run-continue state :encounter-ice)
      (is (= (refresh konjin) (core/get-current-ice state)) "The runner should be back to encountering Konjin")
      (is (no-prompt? state :runner) "No repeat psi prompt")
      (is (no-prompt? state :corp) "No repeat psi prompt"))))

(deftest lamplighter-subs-test
  (do-game
    (subroutine-test "Lamplighter" 0)
    (click-prompt state :runner "Take 1 tag")
    (is (= 1 (count-tags state))))
  (do-game
    (subroutine-test "Lamplighter" 0)
    (click-prompt state :runner "Pay 3 [Credits]")
    (is (= 2 (:credit (get-runner))))
    (is (= 0 (count-tags state))))
  (do-game
    (subroutine-test "Lamplighter" 1 nil)
    (is (:run @state) "Run not ended"))
  (do-game
    (subroutine-test "Lamplighter" 1 {:runner {:tags 1}})
    (is (not (:run @state)) "Run ended")))

(deftest lamplighter-steal-trash
  (do-game
    (subroutine-test "Lamplighter" 0 {:corp {:hand ["Hostile Takeover"]}})
    (click-prompt state :runner "Take 1 tag")
    (run-continue-until state :success)
    (click-prompt state :runner "Steal")
    (is (= 1 (count (:discard (get-corp)))) "Trashed pickpocket")))

(deftest lamplighter-score-trash
  (do-game
    (new-game {:corp {:hand ["Lamplighter" "Project Atlas"]}})
    (play-from-hand state :corp "Lamplighter" "New remote")
    (rez state :corp (get-ice state :remote1 0))
    (play-from-hand state :corp "Project Atlas" "Server 1")
    (score-agenda state :corp (get-content state :remote1 0))
    (is (= 1 (count (:discard (get-corp)))) "Trashed pickpocket")))

(deftest lockdown
  ;; Lockdown - Prevent Runner from drawing cards for the rest of the turn
  (do-game
    (new-game {:corp {:deck ["Lockdown"]}
               :runner {:deck [(qty "Sure Gamble" 3)]
                        :hand [(qty "Diesel" 2)]}})
    (play-from-hand state :corp "Lockdown" "R&D")
    (take-credits state :corp)
    (let [lock (get-ice state :rd 0)]
      (run-on state "R&D")
      (rez state :corp lock)
      (run-continue state)
      (card-subroutine state :corp lock 0)
      (run-continue state)
      (run-continue state)
      (play-from-hand state :runner "Diesel")
      (is (= 1 (count (:hand (get-runner)))) "No cards drawn")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Diesel")
      (is (= 3 (count (:hand (get-runner))))
          "New turn ends prevention; remaining 3 cards drawn from Stack"))))

(deftest logjam
  (do-game
    (new-game {:corp {:hand ["Logjam" "Slash and Burn Agriculture"]
                      :discard ["NGO Front" "Hedge Fund"]
                      :credits 10}})
    (play-from-hand state :corp "Logjam" "HQ")
    (take-credits state :corp)
    (run-empty-server state :archives)
    (take-credits state :runner)
    (let [lj (get-ice state :hq 0)]
      (expend state :corp (first (:hand (get-corp))))
      (click-card state :corp lj)
      (take-credits state :corp)
      (run-on state "HQ")
      (rez state :corp lj)
      (is (= 6 (get-strength (refresh lj))) "Logjam got +1 strength for each hosted advancement counter")
      (run-continue state)
      (is (changed? [(:credit (get-corp)) 2]
                    (fire-subs state (refresh lj)))
          "Corp gained 2 credits")
      (is (not (:run @state)) "Run has ended"))))

(deftest loki-runner-does-not-shuffle-cards
  ;; Runner does not shuffle cards
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Loki" "Karunā"]
                      :credits 100}
               :runner {:hand [(qty "Sure Gamble" 3) (qty "Easy Mark" 3)]}})
    (play-from-hand state :corp "Karunā" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (play-from-hand state :corp "Loki" "R&D")
    (rez state :corp (get-ice state :rd 0))
    (take-credits state :corp)
    (run-on state "R&D")
    (run-continue state)
    (click-card state :corp "Karunā")
    (is (last-log-contains? state "Corp uses Loki to choose Karunā protecting HQ at position 0")
        "The message correctly prints")
    (is (= ["Do 2 net damage. The Runner may jack out"
            "Do 2 net damage"
            "End the run unless the Runner shuffles the grip into the stack"]
           (map :label (:subroutines (get-ice state :rd 0))))
        "Loki gains all of Karunā's subroutines")
    (is (= #{"AP" "Sentry" "Bioroid"} (into #{} (:subtypes (get-ice state :rd 0)))))
    (is (= 0 (count (:discard (get-runner)))) "Heap Empty")
    (fire-subs state (get-ice state :rd 0))
    (is (= 2 (count (:discard (get-runner)))) "2 cards trashed")
    (click-prompt state :runner "No")
    (is (= 4 (count (:discard (get-runner)))) "4 cards trashed")
    (click-prompt state :runner "End the run")
    (is (= 2 (count (:hand (get-runner)))) "Runner has 2 cards in hand")
    (is (not (:run @state)) "Run is ended")
    (is (= ["End the run unless the Runner shuffles the grip into the stack"]
           (map :label (:subroutines (get-ice state :rd 0))))
        "Loki's subroutines revert to printed after the run ends")
    (is (= ["Bioroid"] (:subtypes (get-ice state :rd 0))))))

(deftest loki-runner-does-shuffle-cards
  ;; Runner does shuffle cards
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Loki"]
                      :credits 100}
               :runner {:hand [(qty "Sure Gamble" 3) (qty "Easy Mark" 3)]}})
    (play-from-hand state :corp "Loki" "R&D")
    (rez state :corp (get-ice state :rd 0))
    (take-credits state :corp)
    (run-on state "R&D")
    (run-continue state)
    (fire-subs state (get-ice state :rd 0))
    (click-prompt state :runner "Shuffle the grip into the stack")
    (is (= 0 (count (:hand (get-runner)))) "Runner has 0 cards in hand")
    (is (= 6 (count (:deck (get-runner)))) "Runner has 6 cards in stack")
    (is (:run @state) "Run is still live")))

(deftest loki-runner-can-shuffle-cards-with-zero-in-hand
  ;; Runner can shuffle cards with zero in hand
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Loki"]
                      :credits 100}
               :runner {:hand []
                        :deck [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Loki" "R&D")
    (rez state :corp (get-ice state :rd 0))
    (take-credits state :corp)
    (run-on state "R&D")
    (run-continue state)
    (fire-subs state (get-ice state :rd 0))
    (click-prompt state :runner "Shuffle the grip into the stack")
    (is (= 0 (count (:hand (get-runner)))) "Runner has 0 cards in hand")
    (is (= 3 (count (:deck (get-runner)))) "Runner has 3 card in stack")
    (is (:run @state) "Run is still live")))

(deftest loki-runner-cannot-shuffle-cards
  ;; Runner cannot shuffle cards
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Loki"]
                      :credits 100}
               :runner {:hand []
                        :deck ["Sure Gamble"]}})
    (play-from-hand state :corp "Loki" "R&D")
    (rez state :corp (get-ice state :rd 0))
    (take-credits state :corp)
    (run-on state "R&D")
    (run-continue state)
    (fire-subs state (get-ice state :rd 0))
    (click-prompt state :runner "End the run")
    (is (= 0 (count (:hand (get-runner)))) "Runner has 0 cards in hand")
    (is (= 1 (count (:deck (get-runner)))) "Runner has 1 card in stack")
    (is (not (:run @state)) "Run is ended")))

(deftest loki-vs-kamali-1-0-fire-all-subs
  (doseq [loki-opt ["End the run" "Shuffle the grip into the stack"]]
    (do-game
      (new-game {:corp {:hand ["Loki" "Kamali 1.0"] :credits 20}
                 :runner {:hand [(qty "Sure Gamble" 4)]}})
      (play-from-hand state :corp "Kamali 1.0" "HQ")
      (play-from-hand state :corp "Loki" "HQ")
      (take-credits state :corp)
      (run-on state :hq)
      (rez state :corp (get-ice state :hq 0))
      (rez state :corp (get-ice state :hq 1))
      (run-continue state :encounter-ice)
      (click-card state :corp "Kamali 1.0")
      (fire-subs state (get-ice state :hq 1))
      (dotimes [_ 3]
        (click-prompt state :runner "Take 1 core damage"))
      (click-prompt state :runner loki-opt)
      (is (no-prompt? state :runner) "No lingering prompt (runner)")
      (is (no-prompt? state :runner) "No lingering prompt (corp)"))))

(deftest loot-box
  (do-game
    (fire-all-subs-test "Loot Box"
                        {:runner {:deck ["Dirty Laundry" "Datasucker" "Liberated Account"]}})
    (is (changed? [(:credit (get-runner)) -2]
          (click-prompt state :runner "Pay 2 [Credits]"))
        "Paid 2 credits to not ETR")
    (is (changed? [(:credit (get-corp)) 6]
          (click-prompt state :corp "Liberated Account")
          (is (find-card "Liberated Account" (:hand (get-runner))))
          (is (not (find-card "Liberated Account" (:deck (get-runner))))))
        "Gained 6 credits from Liberated Account")
    (is (= "Loot Box" (-> (get-corp) :discard first :title)) "Loot Box trashed")))

(deftest loot-box-empty-stack
  ;; Loot Box
  (do-game
    (subroutine-test "Loot Box" 1 {:runner {:deck 0}})
    (is (last-log-contains? state "uses Loot Box to trash itself") "Loot box trashed itself")))

(deftest lotus-field
  ;; Lotus Field strength cannot be lowered
  (do-game
    (new-game {:corp {:deck ["Lotus Field" "Lag Time"]}
               :runner {:deck ["Ice Carver" "Parasite"]}})
    (play-from-hand state :corp "Lotus Field" "Archives")
    (take-credits state :corp 2)
    (let [lotus (get-ice state :archives 0)]
      (rez state :corp lotus)
      (play-from-hand state :runner "Ice Carver")
      (run-on state "Archives")
      (run-continue state)
      (is (= 4 (get-strength (refresh lotus))) "Lotus Field strength unchanged")
      (run-continue state :movement)
      (run-jack-out state)
      (play-from-hand state :runner "Parasite")
      (click-card state :runner lotus)
      (is (= 1 (count (:hosted (refresh lotus)))) "Parasite hosted on Lotus Field")
      (take-credits state :runner 1)
      (take-credits state :corp)
      (is (= 1 (core/get-virus-counters state (first (:hosted (refresh lotus)))))
          "Parasite has 1 virus counter")
      (is (= 4 (get-strength (refresh lotus))) "Lotus Field strength unchanged")
      (take-credits state :runner)
      (play-from-hand state :corp "Lag Time")
      (is (= 5 (get-strength (refresh lotus))) "Lotus Field strength increased")
      (take-credits state :corp 2)
      (is (= 5 (get-strength (refresh lotus))) "Lotus Field strength increased"))))

(deftest lycian-multi-munition
  (do-game
    (new-game {:corp {:hand ["Lycian Multi-Munition"]}
               :runner {:hand ["Marjanah"]}})
    (play-from-hand state :corp "Lycian Multi-Munition" "HQ")
    (take-credits state :corp)
    (let [lmm (get-ice state :hq 0)]
      (play-from-hand state :runner "Marjanah")
      (run-on state "HQ")
      (rez state :corp lmm)
      (is (= 3 (count (:choices (prompt-map :corp)))) "Corp cannot select Done right away")
      (click-prompt state :corp "Code Gate")
      (is (= 3 (count (:choices (prompt-map :corp)))) "Done is added to the choices list")
      (click-prompt state :corp "Sentry")
      (is (= 2 (count (:choices (prompt-map :corp)))))
      (click-prompt state :corp "Barrier")
      (is (= 1 (count (:choices (prompt-map :corp)))))
      (click-prompt state :corp "Done")
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -1
                     (:click (get-runner)) -1]
                    (card-subroutine state :corp lmm 0))
          "Runner lost 1 credit and 1 click")
      (is (changed? [(count (get-program state)) -1
                     (count (:discard (get-runner))) 1]
                    (card-subroutine state :corp lmm 1)
                    (click-card state :corp (get-program state 0)))
          "Runner program trashed")
      (is (changed? [(:credit (get-corp)) 1]
                    (card-subroutine state :corp lmm 2)
                    (is (not (:run @state)) "Run has ended"))
          "Corp gained 1 credit")
      (take-credits state :runner)
      (is (not (rezzed? (refresh lmm))) "Lycian Multi-Munition was derezzed at the end of turn"))))

(deftest macrophage-happy-path
  ;; Happy Path
  (do-game
    (new-game {:corp {:deck ["Macrophage"]}
               :runner {:deck ["Cache"]}})
    (play-from-hand state :corp "Macrophage" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (let [mp (get-ice state :hq 0)
          cache (get-program state 0)]
      (rez state :corp mp)
      (run-on state "HQ")
      (run-continue state)
      (is (= 3 (core/get-virus-counters state (refresh cache))))
      (card-subroutine state :corp mp 0)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 4 (:base (prompt-map :corp))) "Trace is base 4")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (zero? (core/get-virus-counters state (refresh cache))))
      (is (= 0 (count (:discard (get-runner)))) "0 cards in heap")
      (card-subroutine state :corp mp 1)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 3 (:base (prompt-map :corp))) "Trace is base 3")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp cache)
      (is (= ["Cache"] (->> (get-runner) :discard (map :title))) "Cache in heap")
      (card-subroutine state :corp mp 2)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 2 (:base (prompt-map :corp))) "Trace is base 2")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-prompt state :corp "Cache")
      (is (not (= ["Cache"] (->> (get-runner) :discard (map :title)))) "Cache not in heap after RFG sub")
      (is (= ["Cache"] (->> (get-runner) :rfg (map :title))) "Cache should be rfg'd")
      (card-subroutine state :corp mp 3)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 1 (:base (prompt-map :corp))) "Trace is base 1")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (nil? (:run @state))))))

(deftest macrophage-heap-locked-test
  ;; Heap Locked Test
  (do-game
    (new-game {:corp {:deck ["Macrophage" "Blacklist"]}
               :runner {:deck ["Cache"]}})
    (play-from-hand state :corp "Macrophage" "HQ")
    (play-from-hand state :corp "Blacklist" "New remote")
    (rez state :corp (refresh (get-content state :remote1 0)))
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (let [mp (get-ice state :hq 0)
          cache (get-program state 0)]
      (rez state :corp mp)
      (run-on state "HQ")
      (run-continue state)
      (is (= 3 (core/get-virus-counters state (refresh cache))))
      (card-subroutine state :corp mp 0)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 4 (:base (prompt-map :corp))) "Trace is base 4")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (zero? (core/get-virus-counters state (refresh cache))))
      (is (= 0 (count (:discard (get-runner)))) "0 cards in heap")
      (card-subroutine state :corp mp 1)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 3 (:base (prompt-map :corp))) "Trace is base 3")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp cache)
      (is (= 1 (count (:discard (get-runner)))) "1 card in heap")
      (card-subroutine state :corp mp 2)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 2 (:base (prompt-map :corp))) "Trace is base 2")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (no-prompt? state :corp) "RFG prompt did not come up")
      (is (= ["Cache"] (->> (get-runner) :discard (map :title))) "Cache in heap after RFG sub")
      (is (not (= ["Cache"] (->> (get-runner) :rfg (map :title)))) "Cache should not be rfg'd")
      (card-subroutine state :corp mp 3)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 1 (:base (prompt-map :corp))) "Trace is base 1")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (nil? (:run @state))))))

(deftest magnet-faceup-ice
  ;; Faceup ice
  (do-game
    (new-game {:corp {:deck ["Magnet" "Enigma"]}
               :runner {:deck ["Parasite"]}})
    (play-from-hand state :corp "Magnet" "HQ")
    (play-from-hand state :corp "Enigma" "R&D")
    (rez state :corp (get-ice state :rd 0))
    (take-credits state :corp)
    (let [m (get-ice state :hq 0)
          e (get-ice state :rd 0)]
      (play-from-hand state :runner "Parasite")
      (click-card state :runner (refresh e))
      (is (= 1 (count (:hosted (refresh e)))) "Parasite hosted on Enigma")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (click-card state :corp (first (:hosted (get-ice state :rd 0))))
      (run-continue state)
      (is (empty? (:hosted (refresh e))) "Enigma not hosting Parasite")
      (is (= 1 (count (:hosted (refresh m)))) "Parasite hosted on Magnet")
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (is (zero? (core/get-virus-counters state (first (:hosted (refresh m)))))
          "Parasite does not gain a virus counter"))))

(deftest magnet-facedown-ice
  ;; Facedown ice
  (do-game
    (new-game {:corp {:deck ["Magnet" "Enigma"]}
               :runner {:deck ["Trypano"]}})
    (play-from-hand state :corp "Magnet" "HQ")
    (play-from-hand state :corp "Enigma" "R&D")
    (take-credits state :corp)
    (let [m (get-ice state :hq 0)
          e (get-ice state :rd 0)]
      (play-from-hand state :runner "Trypano")
      (click-card state :runner (refresh e))
      (is (= 1 (count (:hosted (refresh e)))) "Trypano hosted on Enigma")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (click-card state :corp (first (:hosted (get-ice state :rd 0))))
      (is (empty? (:hosted (refresh e))) "Enigma not hosting Trypano")
      (is (= 1 (count (:hosted (refresh m)))) "Trypano hosted on Magnet")
      (run-continue state)
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (is (no-prompt? state :runner) "No Trypano prompt")
      (is (zero? (core/get-virus-counters state (first (:hosted (refresh m)))))
          "Trypano does not gain a virus counter"))))

(deftest magnet-derezzed-ice
  ;; Derezzed ice
  (do-game
    (new-game {:corp {:deck ["Magnet" "Enigma"]}
               :runner {:deck [(qty "Parasite" 2)]}})
    (play-from-hand state :corp "Magnet" "HQ")
    (play-from-hand state :corp "Enigma" "R&D")
    (rez state :corp (get-ice state :rd 0))
    (take-credits state :corp)
    (let [m (get-ice state :hq 0)
          e (get-ice state :rd 0)]
      (play-from-hand state :runner "Parasite")
      (click-card state :runner (refresh e))
      (is (= 1 (count (:hosted (refresh e)))) "Parasite hosted on Enigma")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (click-card state :corp (first (:hosted (get-ice state :rd 0))))
      (is (empty? (:hosted (refresh e))) "Enigma not hosting Parasite")
      (is (= 1 (count (:hosted (refresh m)))) "Parasite hosted on Magnet")
      (run-continue state)
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (is (zero? (core/get-virus-counters state (first (:hosted (refresh m)))))
          "Parasite does not gain a virus counter")
      (take-credits state :runner)
      (derez state :corp (refresh m))
      (take-credits state :corp)
      (is (= 1 (core/get-virus-counters state (first (:hosted (refresh m)))))
          "Parasite gains a virus counter on derezzed Magnet")
      (play-from-hand state :runner "Parasite")
      (click-card state :runner (refresh e))
      (is (= 1 (count (:hosted (refresh e)))) "Parasite hosted on Enigma")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (click-card state :corp (first (:hosted (get-ice state :rd 0))))
      (is (empty? (:hosted (refresh e))) "Enigma not hosting Parasite")
      (is (= 2 (count (:hosted (refresh m)))) "Parasites hosted on Magnet")
      (run-continue state)
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 1 (core/get-virus-counters state (first (:hosted (refresh m)))))
          "First parasite stays at 1 virus counter on rezzed Magnet")
      (is (zero? (core/get-virus-counters state (second (:hosted (refresh m)))))
          "Second parasite does not gain a virus counter on derezzed Magnet")
      (take-credits state :runner)
      (derez state :corp (refresh m))
      (take-credits state :corp)
      (is (= 2 (core/get-virus-counters state (first (:hosted (refresh m)))))
          "First parasite gains a virus counter on derezzed Magnet")
      (is (= 1 (core/get-virus-counters state (second (:hosted (refresh m)))))
          "Second parasite gains a virus counter on rezzed Magnet"))))

(deftest magnet-should-not-reset-program-mu
  ;; Rezzing Magnet should not affect MU cost of hosted programs
  (do-game
    (new-game {:corp {:deck ["Magnet"]}
               :runner {:deck ["Saci"]}})
    (play-from-hand state :corp "Magnet" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Saci")
    (click-card state :runner (get-ice state :rd 0))
    (is (changed? [(core/available-mu state) 0]
          (rez state :corp (get-ice state :rd 0)))
        "Available MU should not change")
    (is (changed? [(core/available-mu state) 0]
          (derez state :corp (get-ice state :rd 0)))
        "Available MU should not change")))

(deftest mamba
  (do-game
    (fire-all-subs-test "Mamba" {:runner {:hand 5}})
    (is (= 1 (count (:discard (get-runner)))) "Took 1 net damage")
    (click-prompt state :corp "0 [Credits]")
    (click-prompt state :runner "1 [Credits]")
    (is (not (waiting? state :runner)) "No leftover prompt")
    (let [mamba (get-ice state :hq 0)]
      (is (= 1 (get-counters mamba :power)) "Gained 1 power counter")
      (card-ability state :corp mamba 0)
      (is (= 0 (get-counters (refresh mamba) :power)) "Spent 1 power counter")
      (is (= 2 (count (:discard (get-runner)))) "Took 1 more net damage"))))

(deftest marker
  ;; Marker
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Marker" "Ice Wall"]
                      :credits 100}})
    (play-from-hand state :corp "Ice Wall" "New remote")
    (play-from-hand state :corp "Marker" "Server 1")
    (take-credits state :corp)
    (let [iw (get-ice state :remote1 0)
          mark (get-ice state :remote1 1)]
      (rez state :corp mark)
      (rez state :corp iw)
      (run-on state "Server 1")
      (run-continue state)
      (card-subroutine state :corp mark 0)
      (is (last-log-contains? state "Marker to give next encountered ice")
          "The message correctly prints")
      (run-continue-until state :approach-ice iw)
      (is (= 1 (count (:subroutines (refresh iw)))) "Ice Wall starts with 1 subroutine")
      (run-continue state)
      (is (= 2 (count (:subroutines (refresh iw)))) "Marker correctly gives Ice Wall an additional subroutine")
      (run-continue state :movement)
      (run-jack-out state)
      (is (= 1 (count (:subroutines (refresh iw)))) "Ice Wall's subroutines reset after the run ends"))))

(deftest maskirovka
  (do-game
   (new-game {:corp {:hand ["Maskirovka"]}})
   (play-from-hand state :corp "Maskirovka" "HQ")
   (take-credits state :corp)
   (run-on state :hq)
   (let [money (get-ice state :hq 0)]
     (rez state :corp (refresh money))
     (run-continue state)
     (is (changed? [(:credit (get-corp)) 2]
           (fire-subs state (refresh money))
           (is (not (:run @state)) "Run ended"))
         "gained 2c from Maskirovka"))))

(deftest masvingo
  ;; Masvingo
  (do-game
    (new-game {:corp {:deck ["Masvingo"]}})
    (play-from-hand state :corp "Masvingo" "HQ")
    (let [mas (get-ice state :hq 0)]
      (is (zero? (get-counters (refresh mas) :advancement)) "Should install with 0 counter")
      (rez state :corp (refresh mas))
      (is (= 1 (get-counters (refresh mas) :advancement)) "Should rez with 1 counter")
      (is (= 1 (count (:subroutines (refresh mas)))) "Should gain 1 subroutine")
      (advance state (refresh mas) 1)
      (is (= 2 (count (:subroutines (refresh mas)))) "Should gain 1 subroutine")
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (card-subroutine state :corp mas 0)
      (is (not (:run @state)) "Run is ended"))))

(deftest mausolus
  ;; Mausolus - 3 adv tokens change the subroutines
  (do-game
    (new-game {:corp {:deck ["Mausolus"]}
               :runner {:deck [(qty "NetChip" 5)]}})
    (play-from-hand state :corp "Mausolus" "HQ")
    (let [mau (get-ice state :hq 0)]
      (rez state :corp mau)
      (take-credits state :corp)
      (run-on state "HQ")
      (is (= 3 (:credit (get-corp))) "corp starts encounter with 3 crs")
      (is (zero? (count (:discard (get-runner)))) "runner starts encounter with no cards in heap")
      (is (zero? (count-tags state)) "runner starts encounter with 0 tags")
      (run-continue state)
      (card-subroutine state :corp mau 0)
      (is (= 4 (:credit (get-corp))) "corp gains 1 cr from mausolus")
      (card-subroutine state :corp mau 1)
      (is (= 1 (count (:discard (get-runner)))) "corp does 1 net damage")
      (card-subroutine state :corp mau 2)
      (is (= 1 (count-tags state)) "corp gives 1 tag")
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (advance state mau 3)
      (take-credits state :corp)
      (run-on state "HQ")
      (is (= 1 (:credit (get-corp))) "corp starts encounter with 1 crs")
      (is (= 1 (count (:discard (get-runner)))) "runner starts encounter with 1 card in heap")
      (is (= 1 (count-tags state)) "runner starts encounter with 1 tags")
      (run-continue state)
      (card-subroutine state :corp mau 0)
      (is (= 4 (:credit (get-corp))) "corp gains 3 cr")
      (card-subroutine state :corp mau 1)
      (is (= 4 (count (:discard (get-runner)))) "corp does 3 net damage")
      (card-subroutine state :corp mau 2)
      (is (= 2 (count-tags state)) "corp gives 1 tag")
      (is (not (:run @state)) "Run is ended")
      (is (:unsuccessful-run (:register (:runner @state))) "Run was unsuccessful"))))

(deftest meridian-etr
  ;; ETR
  (do-game
    (new-game {:corp {:deck ["Meridian"]}})
    (play-from-hand state :corp "Meridian" "HQ")
    (take-credits state :corp)
    (let [mer (get-ice state :hq 0)]
      (rez state :corp (refresh mer))
      (run-on state "HQ")
      (run-continue state)
      (card-subroutine state :corp (refresh mer) 0)
      (click-prompt state :runner "Corp gains 4 [Credits] and end the run")
      (is (not (:run @state)) "Run is ended")
      (is (empty? (:scored (get-runner))) "Not in runner score area")
      (is (= 1 (count (get-ice state :hq))) "ice still installed"))))

(deftest meridian-score-as-1-point-agenda
  ;; Score as -1 point agenda
  (do-game
    (new-game {:corp {:deck ["Meridian"]}})
    (play-from-hand state :corp "Meridian" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [mer (get-ice state :hq 0)]
      (rez state :corp (refresh mer))
      (run-continue state)
      (card-subroutine state :corp (refresh mer) 0)
      (click-prompt state :runner "Add Meridian to score area")
      (is (= 1 (count (:scored (get-runner)))) "In runner score area")
      (is (= -1 (:agenda-point (get-runner))) "Worth -1 agenda points")
      (is (empty? (get-ice state :hq)) "ice uninstalled")
      (run-continue state)
      (is (not (:run @state)) "Run has ended"))))

(deftest meru-mati
  (do-game
    (new-game {:corp {:deck [(qty "Meru Mati" 2)]}})
    (play-from-hand state :corp "Meru Mati" "HQ")
    (play-from-hand state :corp "Meru Mati" "R&D")
    (rez state :corp (get-ice state :hq 0))
    (rez state :corp (get-ice state :rd 0))
    (is (= 4 (get-strength (get-ice state :hq 0))) "HQ Meru Mati at 4 strength")
    (is (= 1 (get-strength (get-ice state :rd 0))) "R&D at 0 strength")))

(deftest mestnichestvo
  (do-game
    (new-game {:corp {:hand ["Mestnichestvo"] :credits 10}})
    (play-from-hand state :corp "Mestnichestvo" "HQ")
    (let [mes (get-ice state :hq 0)]
      (click-advance state :corp (refresh mes))
      (take-credits state :corp)
      (run-on state :hq)
      (rez state :corp (refresh mes))
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -3]
            (is (= 1 (get-counters (refresh mes) :advancement)) "Starts with 1 counter")
            (click-prompt state :corp "Yes")
            (is (= 0 (get-counters (refresh mes) :advancement)) "Spend a counter for the ability"))
          "Runner lost 3 from encounter effect")
      (is (changed? [(:credit (get-runner)) -2]
            (fire-subs state (refresh mes)))
          "Runner loses remaining credits from subroutines"))
    (is (not (:run @state)) "Run ended")))

(deftest metamorph-with-two-installed-ice
  ;; with two installed ice
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Metamorph" "Ice Wall" "Vanilla"]
                      :credits 20}})
    (play-from-hand state :corp "Metamorph" "Archives")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Vanilla" "R&D")
    (take-credits state :corp)
    (run-on state "Archives")
    (rez state :corp (get-ice state :archives 0))
    (run-continue state)
    (fire-subs state (get-ice state :archives 0))
    (is (= ["Swap 2 pieces of ice"] (prompt-buttons :corp)) "Only ice option")
    (click-prompt state :corp "Swap 2 pieces of ice")
    (click-card state :corp "Ice Wall")
    (click-card state :corp "Vanilla")
    (is (= "Vanilla" (:title (get-ice state :hq 0))))
    (is (= "Ice Wall" (:title (get-ice state :rd 0))))))

(deftest metamorph-with-two-installed-non-ice
  ;; with two installed non-ice
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Metamorph" "Allele Repression" "Hostile Takeover"]
                      :credits 20}})
    (play-from-hand state :corp "Metamorph" "Archives")
    (play-from-hand state :corp "Allele Repression" "New remote")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (take-credits state :corp)
    (run-on state "Archives")
    (rez state :corp (get-ice state :archives 0))
    (run-continue state)
    (fire-subs state (get-ice state :archives 0))
    (is (= ["Swap 2 non-ice"] (prompt-buttons :corp)) "Only non-ice option")
    (click-prompt state :corp "Swap 2 non-ice")
    (click-card state :corp "Allele Repression")
    (click-card state :corp "Hostile Takeover")
    (is (= "Hostile Takeover" (:title (get-content state :remote1 0))))
    (is (= "Allele Repression" (:title (get-content state :remote2 0))))))

(deftest metamorph-with-two-installed-non-ice-both-options
  ;; with two installed non-ice both options are avilable
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Metamorph"
                             "Ice Wall" "Vanilla"
                             "Allele Repression" "Hostile Takeover"]
                      :credits 20}})
    (core/gain state :corp :click 10)
    (play-from-hand state :corp "Metamorph" "Archives")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Vanilla" "R&D")
    (play-from-hand state :corp "Allele Repression" "New remote")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (take-credits state :corp)
    (run-on state "Archives")
    (rez state :corp (get-ice state :archives 0))
    (run-continue state)
    (fire-subs state (get-ice state :archives 0))
    (is (= ["Swap 2 pieces of ice" "Swap 2 non-ice"] (prompt-buttons :corp)) "Both options available")))

(deftest mic
  ;; M.I.C.
  (do-game
    (new-game {:corp {:hand ["M.I.C."]}})
    (play-from-hand state :corp "M.I.C." "HQ")
    (take-credits state :corp)
    (let [mic (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp mic)
      (run-continue state)
      (is (changed? [(:click (get-runner)) -2]
            (fire-subs state mic))
          "Runner lost 2 clicks")
      (is (nil? (:run @state)) "M.I.C. ended the run")
      (core/gain state :runner :click 1)
      (run-on state "HQ")
      (run-continue state)
      (is (changed? [(:click (get-runner)) -1]
            (card-ability state :corp mic 0)
            (click-prompt state :runner "Spend [Click]"))
          "Runner spent 1 click to pay M.I.C. ability")
      (is (:run @state) "Run is still going")
      (is (= :movement (:phase (:run @state))) "Encounter with M.I.C. is over")
      (is (= 1 (count (:discard (get-corp)))) "M.I.C. got trashed"))))

(deftest mind-game-server-redirection
  ;; Server redirection
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Mind Game"]}
               :runner {:deck ["Easy Mark"]
                        :hand ["Sure Gamble"]}})
    (play-from-hand state :corp "Mind Game" "HQ")
    (take-credits state :corp)
    (let [mindgame (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp mindgame)
      (run-continue state)
      (card-subroutine state :corp mindgame 0))
    (click-prompt state :corp "1 [Credits]")
    (click-prompt state :runner "0 [Credits]")
    (is (= ["Archives" "R&D"] (prompt-buttons :corp)) "Corp cannot choose server Runner is on")
    (click-prompt state :corp "Archives")
    (click-prompt state :runner "No")
    (is (= [:archives] (get-in @state [:run :server])) "Runner now running on Archives")))

(deftest mind-game-jack-out-additional-cost
  ;; Jack out additional cost
  (testing "and can't pay"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Mind Game"]}
                 :runner {:deck ["Easy Mark"]
                          :hand ["Sure Gamble"]}})
      (play-from-hand state :corp "Mind Game" "HQ")
      (take-credits state :corp)
      (let [mindgame (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp mindgame)
        (run-continue state)
        (card-subroutine state :corp mindgame 0))
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (click-prompt state :corp "Archives")
      (click-prompt state :runner "Yes")
      (is (get-run) "Run hasn't ended because runner can't pay cost")))
  (testing "and can pay"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Mind Game"]}
                 :runner {:deck ["Easy Mark"]
                          :hand ["Sure Gamble" "Corroder"]}})
      (play-from-hand state :corp "Mind Game" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (let [mindgame (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp mindgame)
        (run-continue state)
        (card-subroutine state :corp mindgame 0))
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (click-prompt state :corp "Archives")
      (click-prompt state :runner "Yes")
      (click-card state :runner "Corroder")
      (is (= "Corroder" (-> (get-runner) :deck last :title)) "Corroder is on the bottom of the deck")
      (is (not (get-run)) "Run has ended"))))

(deftest mind-game-server-redirection-with-correct-state-and-non-program-card-to-pay-issue-4847
  ;; Server redirection with correct state and non-program card to pay. Issue 4847
  (do-game
    (new-game {:corp {:hand ["Mind Game" "Ice Wall"]}
               :runner {:hand ["Daily Casts"]}})
    (play-from-hand state :corp "Mind Game" "HQ")
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Daily Casts")
    (let [mindgame (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp mindgame)
      (run-continue state)
      (card-subroutine state :corp mindgame 0)
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= ["Archives" "R&D"] (prompt-buttons :corp)) "Corp cannot choose server Runner is on")
      (click-prompt state :corp "Archives")
      (is (= [:archives] (get-in @state [:run :server])) "Runner now running on Archives")
      (click-prompt state :runner "No")
      (is (= :approach-ice (:phase (:run @state))) "Runner is in correct state")
      (run-continue-until state :movement)
      (run-jack-out state)
      (click-card state :runner "Daily Casts")
      (is (= "Daily Casts" (-> (get-runner) :deck last :title)) "Daily Casts is on the bottom of the deck")
      (is (not (get-run)) "Run has ended"))))

(deftest mind-game-redirection-works-correctly-5047
  ;; Redirection works correctly. #5047
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Mind Game" "Ice Wall"]}
               :runner {:deck ["Easy Mark"]
                        :hand ["Sure Gamble"]}})
    (play-from-hand state :corp "Ice Wall" "Archives")
    (play-from-hand state :corp "Mind Game" "HQ")
    (take-credits state :corp)
    (let [mindgame (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp mindgame)
      (run-continue state)
      (card-subroutine state :corp mindgame 0))
    (click-prompt state :corp "1 [Credits]")
    (click-prompt state :runner "0 [Credits]")
    (is (= ["Archives" "R&D"] (prompt-buttons :corp)) "Corp cannot choose server Runner is on")
    (click-prompt state :corp "Archives")
    (is (= [:archives] (get-in @state [:run :server])) "Runner now running on Archives")
    (click-prompt state :runner "No")
    (rez state :corp (get-ice state :archives 0))
    (run-continue state)
    (is (last-log-contains? state "Runner encounters Ice Wall"))))

(deftest mind-game-redirection-does-not-occur-if-mind-game-would-not-be-passed
  ;; Redirection does not occur if Mind Game would not be passed
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Mind Game" "Konjin"]}
               :runner {:deck ["Easy Mark"]
                        :hand ["Sure Gamble"]}})
    (play-from-hand state :corp "Mind Game" "HQ")
    (play-from-hand state :corp "Konjin" "R&D")
    (take-credits state :corp)
    (let [mindgame (get-ice state :hq 0)
          konjin (get-ice state :rd 0)]
      (run-on state :rd)
      (rez state :corp mindgame)
      (rez state :corp konjin)
      (run-continue state)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (click-card state :corp mindgame)
      (card-subroutine state :corp mindgame 0)
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (click-prompt state :corp "Archives")
      (is (= :rd (-> @state :run :server first)) "Run not redirected")
      (click-prompt state :runner "No")
      (is (= (refresh mindgame) (core/get-current-ice state)) "Still encountering Mind Game")
      (run-continue-until state :movement)
      (run-jack-out state)
      (is (get-run) "Jack out cost still applied"))))

(deftest minelayer
  ;; Minelayer - Install a piece of ice in outermost position of Minelayer's server at no cost
  (do-game
    (new-game {:corp {:deck ["Minelayer" "Fire Wall"]}})
    (play-from-hand state :corp "Minelayer" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 0))
    (is (= 6 (:credit (get-corp))))
    (run-continue state)
    (card-subroutine state :corp (get-ice state :hq 0) 0)
    (click-card state :corp (find-card "Fire Wall" (:hand (get-corp))))
    (is (= 2 (count (get-in @state [:corp :servers :hq :ices]))) "2 pieces of ice protecting HQ")
    (is (= 6 (:credit (get-corp))) "Didn't pay 1 credit to install as second piece of ice")))

(deftest miraju-breaking-sub-redirects-run
  ;; Breaking sub redirects run
  (do-game
    (new-game {:corp {:hand ["Mirāju" "Ice Wall"]}
               :runner {:hand ["Force of Nature"]
                        :credits 10}})
    (play-from-hand state :corp "Mirāju" "HQ")
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Force of Nature")
    (run-on state "HQ")
    (let [miraju (get-ice state :hq 0)
          iw (get-ice state :archives 0)]
      (rez state :corp miraju)
      (rez state :corp iw)
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "Draw 1 card, then shuffle 1 card from HQ into R&D")
      (run-continue state)
      (is (= [:archives] (:server (get-run))) "Run is redirected to Archives")
      (click-prompt state :runner "No")
      (is (not (rezzed? (refresh miraju))) "Mirāju is derezzed")
      (run-continue state)
      (is (= :encounter-ice (:phase (:run @state))))
      (is (= (refresh iw) (core/get-current-ice state))))))

(deftest miraju-runner-can-jack-out-after-redirect
  ;; Runner can jack out after redirect
  (do-game
    (new-game {:corp {:hand ["Mirāju" "Ice Wall"]}
               :runner {:hand ["Force of Nature"]
                        :credits 10}})
    (play-from-hand state :corp "Mirāju" "HQ")
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Force of Nature")
    (run-on state "HQ")
    (let [miraju (get-ice state :hq 0)
          iw (get-ice state :archives 0)]
      (rez state :corp miraju)
      (rez state :corp iw)
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "Draw 1 card, then shuffle 1 card from HQ into R&D")
      (run-continue state)
      (is (= [:archives] (:server (get-run))) "Run is redirected to Archives")
      (click-prompt state :runner "Yes")
      (is (not (rezzed? (refresh miraju))) "Mirāju is derezzed")
      (is (empty? (:run @state)) "Runner has jacked out"))))

(deftest miraju-breaking-sub-in-forced-encounter-does-not-redirect-run
  ;; Breaking sub in forced encounter does not redirect run
  (do-game
    (new-game {:corp {:hand ["Mirāju" "Konjin"]}
               :runner {:hand ["Force of Nature"]
                        :credits 10}})
    (play-from-hand state :corp "Mirāju" "HQ")
    (play-from-hand state :corp "Konjin" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Force of Nature")
    (let [miraju (get-ice state :hq 0)
          konjin (get-ice state :rd 0)]
      (run-on state "R&D")
      (rez state :corp miraju)
      (rez state :corp konjin)
      (run-continue state)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (click-card state :corp miraju)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "Draw 1 card, then shuffle 1 card from HQ into R&D")
      (run-continue state)
      (click-prompt state :runner "No") ;; Runner still prompted to jack out
      (is (= (refresh konjin) (core/get-current-ice state)))
      (is (= [:rd] (:server (get-run))) "Run not redirected since Mirāju wasn't passed")
      (is (not (rezzed? (refresh miraju))) "Mirāju is derezzed"))))

(deftest miraju-loop-issue-4958
  (do-game
    (new-game {:corp {:hand [(qty "Mirāju" 2)] :credits 50}
               :runner {:hand ["Buzzsaw"] :credits 50}})
    (play-from-hand state :corp "Mirāju" "HQ")
    (play-from-hand state :corp "Mirāju" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Buzzsaw")
    (let [hq-ice (get-ice state :hq 0)
          arc-ice (get-ice state :archives 0)
          buzz (get-program state 0)
          sub "Draw 1 card, then shuffle 1 card from HQ into R&D"]
      (rez state :corp hq-ice)
      (run-on state "HQ")
      (run-continue state :encounter-ice)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner sub)
      (run-continue state)
      (is (= [:archives] (:server (get-run))) "Run is redirected to Archives")
      (click-prompt state :runner "No")
      (is (not (rezzed? (refresh hq-ice))) "HQ Ice derezzed")
      (dotimes [n 10]
        (is (= 1 (:position (get-in @state [:run]))) "Outside of miraju")
        (rez state :corp (refresh arc-ice))
        (run-continue state :encounter-ice)
        (card-ability state :runner (get-program state 0) 0)
        (click-prompt state :runner sub)
        (run-continue state)
        (is (= [:archives] (:server (get-run))) "Still on archives")
        (click-prompt state :runner "No")
        (is (not (rezzed? (refresh arc-ice))) "Miraju on archives was derezzed"))
      (run-continue-until state :success))))

(deftest mlinzi-each-side-of-each-subroutine
  ;; Each side of each subroutine
  (do-game
    (new-game {:corp {:deck ["Mlinzi"]}
               :runner {:hand [(qty "Sure Gamble" 10)]
                        :deck [(qty "Sure Gamble" 10)]}})
    (play-from-hand state :corp "Mlinzi" "HQ")
    (take-credits state :corp)
    (let [ml (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp ml)
      (run-continue state)
      ;; Subroutine 1
      (card-subroutine state :corp (refresh ml) 0)
      (is (= 10 (count (:hand (get-runner)))) "Runner has 10 cards in hand")
      (is (= ["Take 1 net damage" "Trash 2 cards from the top of your deck"] (prompt-buttons :runner)))
      (click-prompt state :runner "Take 1 net damage")
      (is (= 1 (count (:discard (get-runner)))) "Runner trashes 1 card in hand")
      (is (= 9 (count (:hand (get-runner)))) "Runner trashes 1 card in hand")
      (card-subroutine state :corp (refresh ml) 0)
      (click-prompt state :runner "Trash 2 cards from the top of your deck")
      (is (= 3 (count (:discard (get-runner)))) "Runner trashes 2 cards in deck")
      (is (= 8 (count (:deck (get-runner)))) "Runner trashes 2 cards in deck")
      ;; Subroutine 2
      (card-subroutine state :corp (refresh ml) 1)
      (is (= ["Take 2 net damage" "Trash 3 cards from the top of your deck"] (prompt-buttons :runner)))
      (click-prompt state :runner "Take 2 net damage")
      (is (= 5 (count (:discard (get-runner)))) "Runner trashes 2 cards in hand")
      (is (= 7 (count (:hand (get-runner)))) "Runner trashes 2 cards in hand")
      (card-subroutine state :corp (refresh ml) 1)
      (click-prompt state :runner "Trash 3 cards from the top of your deck")
      (is (= 8 (count (:discard (get-runner)))) "Runner trashes 3 cards in deck")
      (is (= 5 (count (:deck (get-runner)))) "Runner trashes 3 cards in deck")
      ;; Subroutine 3
      (card-subroutine state :corp (refresh ml) 2)
      (is (= ["Take 3 net damage" "Trash 4 cards from the top of your deck"] (prompt-buttons :runner)))
      (click-prompt state :runner "Take 3 net damage")
      (is (= 11 (count (:discard (get-runner)))) "Runner trashes 3 cards in hand")
      (is (= 4 (count (:hand (get-runner)))) "Runner trashes 3 cards in hand")
      (card-subroutine state :corp (refresh ml) 2)
      (click-prompt state :runner "Trash 4 cards from the top of your deck")
      (is (= 15 (count (:discard (get-runner)))) "Runner trashes 4 cards in deck")
      (is (= 1 (count (:deck (get-runner)))) "Runner trashes 4 cards in deck"))))

(deftest mlinzi-not-enough-cards-in-hand
  ;; Not enough cards in hand
  (do-game
    (new-game {:corp {:deck ["Mlinzi"]}
               :runner {:hand ["Sure Gamble"]
                        :deck [(qty "Sure Gamble" 10)]}})
    (play-from-hand state :corp "Mlinzi" "HQ")
    (take-credits state :corp)
    (let [ml (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp ml)
      (run-continue state)
      ;; Subroutine 3, 3 net or 4 from stack
      (is (= 1 (count (:hand (get-runner)))) "Runner only has 1 card in hand")
      (card-subroutine state :corp (refresh ml) 2)
      (is (= ["Take 3 net damage" "Trash 4 cards from the top of your deck"] (prompt-buttons :runner))
          "Taking net damage is allowed to be chosen when there aren't enough cards in hand"))))

(deftest mlinzi-not-enough-cards-in-deck
  ;; Not enough cards in deck
  (do-game
    (new-game {:corp {:deck ["Mlinzi"]}
               :runner {:deck ["Sure Gamble"]
                        :hand [(qty "Sure Gamble" 10)]}})
    (play-from-hand state :corp "Mlinzi" "HQ")
    (take-credits state :corp)
    (let [ml (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp ml)
      (run-continue state)
      ;; Subroutine 3, 3 net or 4 from stack
      (is (= 1 (count (:deck (get-runner)))) "Runner only has 1 card in deck")
      (card-subroutine state :corp (refresh ml) 2)
      (is (= ["Take 3 net damage"] (prompt-buttons :runner))
          "Trashing cards isn't allowed to be chosen when there aren't enough cards in the deck"))))

(deftest mother-goddess
  ;; Mother Goddess - Gains other ice subtypes
  (do-game
    (new-game {:corp {:deck ["Mother Goddess" "NEXT Bronze"]}})
    (core/gain state :corp :credit 1)
    (play-from-hand state :corp "Mother Goddess" "HQ")
    (play-from-hand state :corp "NEXT Bronze" "R&D")
    (let [mg (get-ice state :hq 0)
          nb (get-ice state :rd 0)]
      (rez state :corp mg)
      (is (has-subtype? (refresh mg) "Mythic") "Mother Goddess has Mythic")
      (is (not (has-subtype? (refresh mg) "Code Gate")) "Mother Goddess does not have Code Gate")
      (is (not (has-subtype? (refresh mg) "NEXT")) "Mother Goddess does not have NEXT")
      (rez state :corp nb)
      (is (has-subtype? (refresh mg) "Mythic") "Mother Goddess has Mythic")
      (is (has-subtype? (refresh mg) "Code Gate") "Mother Goddess has Code Gate")
      (is (has-subtype? (refresh mg) "NEXT") "Mother Goddess has NEXT"))))

(deftest mycoweb-code-gate-test
  (do-game
    (subroutine-test "Mycoweb" 0 {:corp {:discard ["Enigma"]}})
    (click-card state :corp "Enigma")
    (click-prompt state :corp "HQ")
    (card-subroutine state :corp (get-ice state :hq 0) 1)
    (click-card state :corp "Enigma")
    (is (rezzed? (get-ice state :hq 1)) "Enigma rezzed")
    (card-subroutine state :corp (get-ice state :hq 0) 3)
    (click-card state :corp "Enigma")
    (click-prompt state :corp "End the run")
    (is (not (:run @state)) "Run ended by enigma")))

(deftest mycoweb-sentry-test
  (do-game
    (subroutine-test "Mycoweb" 0 {:corp {:discard ["Guard"]}})
    (click-card state :corp "Guard")
    (click-prompt state :corp "HQ")
    (card-subroutine state :corp (get-ice state :hq 0) 1)
    (click-card state :corp "Guard")
    (is (rezzed? (get-ice state :hq 1)) "Guard rezzed")
    (card-subroutine state :corp (get-ice state :hq 0) 2)
    (click-card state :corp "Guard")
    (click-prompt state :corp "End the run")
    (is (not (:run @state)) "Run ended by Guard")))

(deftest mycoweb-sentry-self-test
  ;; Mycoweb - can fire 3rd sub if turned into sentry
  (do-game
    (new-game {:corp {:deck ["Mycoweb" "Ice Wall"] :credits 100}
               :runner {:deck ["Chromatophores"]}})
    (play-from-hand state :corp "Mycoweb" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Chromatophores")
    (click-card state :runner "Mycoweb")
    (let [mycoweb (get-ice state :hq 0)
          icewall (get-ice state :rd 0)]
      (run-on state "HQ")
      (rez state :corp mycoweb)
      (run-continue state)
      (card-subroutine state :corp mycoweb 2)
      (click-card state :corp "Mycoweb")
      (is (changed? [(:credit (get-corp)) 0]
                    (click-prompt state :corp "Rez an ice, paying 2 less"))
          "able to fire another sub on same Mycoweb")
      (click-card state :corp icewall)
      (is (rezzed? (refresh icewall))))))

(deftest n-pot-full-subs-test
  (do-game (etr-sub "N-Pot" 0))
  ;; threat 2 - etr
  (do-game (subroutine-test "N-Pot" 1 nil {:threat 1})
           (is (:run @state) "Run not ended"))
  (do-game (subroutine-test "N-Pot" 1 nil {:threat 2})
           (is (not (:run @state)) "Run ended"))
  ;; threat 4 - etr
  (do-game (subroutine-test "N-Pot" 2 nil {:threat 3})
           (is (:run @state) "Run not ended"))
  (do-game (subroutine-test "N-Pot" 2 nil {:threat 4})
           (is (not (:run @state)) "Run ended")))

(deftest negotiator-subroutines-fire-correctly
  ;; Subroutines fire correctly.
  (do-game
    (new-game {:corp {:deck ["Negotiator"]}
               :runner {:deck ["Pelangi"]}})
    (play-from-hand state :corp "Negotiator" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Pelangi")
    (let [negotiator (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp negotiator)
      (run-continue state)
      ;; corp has 3 credits after clicking for 2 and rezzing for 3
      (is (= 3 (:credit (get-corp) "Corp starts with 3 credits.")))
      (card-subroutine state :corp negotiator 0)
      (is (= 5 (:credit (get-corp))) "Corp gained two credits from firing first sub.")
      (card-subroutine state :corp negotiator 1)
      (click-card state :corp "Pelangi")
      (is (= 0 (count (get-program state))) "Pelangi is trashed from firing second sub."))))

(deftest negotiator-runner-can-break-subs-with-credits
  ;; Runner can break subs with credits.
  (do-game
    (new-game {:corp {:deck ["Negotiator"]}})
    (play-from-hand state :corp "Negotiator" "HQ")
    (take-credits state :corp)
    (let [negotiator (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp negotiator)
      (run-continue state)
      (is (= 5 (:credit (get-runner))) "Runner starts with 5 credits.")
      (card-side-ability state :runner negotiator 0)
      (click-prompt state :runner "Gain 2 [Credits]")
      (click-prompt state :runner "Trash a program")
      (is (= 1 (:credit (get-runner))) "Runner has 1 credit after breaking 2 subs.")
      (run-continue state)
      (is (= [:hq] (:server (get-run)))))))

(deftest news-hound-rezzes-with-the-etr-sub
  ;; Rezzes with the ETR sub
  (do-game
    (new-game {:corp {:deck [(qty "Project Atlas" 5)]
                      :hand [(qty "Scarcity of Resources" 2) "News Hound"]}
               :runner {:hand ["Employee Strike"]}})
    (play-from-hand state :corp "Scarcity of Resources")
    (play-from-hand state :corp "News Hound" "HQ")
    (let [news (get-ice state :hq 0)]
      (rez state :corp news)
      (is (= 2 (count (:subroutines (refresh news)))) "News Hound gains a sub on corp current"))))

(deftest news-hound-loses-and-gains-etr-sub-properly
  ;; Loses and gains ETR sub properly
  (do-game
    (new-game {:corp {:deck [(qty "Project Atlas" 5)]
                      :hand [(qty "Scarcity of Resources" 2) "News Hound"]}
               :runner {:hand ["Employee Strike"]}})
    (play-from-hand state :corp "News Hound" "HQ")
    (let [news (get-ice state :hq 0)]
      (rez state :corp news)
      (is (= 1 (count (:subroutines (refresh news)))) "News Hound starts with 1 sub")
      (play-from-hand state :corp "Scarcity of Resources")
      (is (= 2 (count (:subroutines (refresh news)))) "News Hound gains a sub on corp current")
      (play-from-hand state :corp "Scarcity of Resources")
      (is (= 2 (count (:subroutines (refresh news)))) "News Hound doesn't gain 2 subs on second current played")
      (take-credits state :corp)
      (run-empty-server state :rd)
      (click-prompt state :runner "Steal")
      (is (= 1 (count (:subroutines (refresh news)))) "News Hound loses a sub on steal")
      (play-from-hand state :runner "Employee Strike")
      (is (= 2 (count (:subroutines (refresh news)))) "News Hound gains a sub on runner current")
      (take-credits state :runner)
      (play-and-score state "Project Atlas")
      (is (= 1 (count (:subroutines (refresh news)))) "News Hound loses a sub on score"))))

(deftest next-bronze
  ;; NEXT Bronze - Add 1 strength for every rezzed NEXT ice
  (do-game
    (new-game {:corp {:deck [(qty "NEXT Bronze" 2) "NEXT Silver"]}})
    (core/gain state :corp :credit 2)
    (play-from-hand state :corp "NEXT Bronze" "HQ")
    (play-from-hand state :corp "NEXT Bronze" "R&D")
    (play-from-hand state :corp "NEXT Silver" "Archives")
    (let [nb1 (get-ice state :hq 0)
          nb2 (get-ice state :rd 0)
          ns1 (get-ice state :archives 0)]
      (rez state :corp nb1)
      (is (= 1 (get-strength (refresh nb1)))
          "NEXT Bronze at 1 strength: 1 rezzed NEXT ice")
      (rez state :corp nb2)
      (is (= 2 (get-strength (refresh nb1)))
          "NEXT Bronze at 2 strength: 2 rezzed NEXT ice")
      (is (= 2 (get-strength (refresh nb2)))
          "NEXT Bronze at 2 strength: 2 rezzed NEXT ice")
      (rez state :corp ns1)
      (is (= 3 (get-strength (refresh nb1)))
          "NEXT Bronze at 3 strength: 3 rezzed NEXT ice")
      (is (= 3 (get-strength (refresh nb2)))
          "NEXT Bronze at 3 strength: 3 rezzed NEXT ice"))))

(deftest next-diamond-base-rez-cost
  ;; Base rez cost
  (do-game
    (new-game {:corp {:deck ["NEXT Diamond"]}})
    (core/gain state :corp :credit 5)
    (is (= 10 (:credit (get-corp))) "Corp starts with 10 credits")
    (play-from-hand state :corp "NEXT Diamond" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (is (zero? (:credit (get-corp))) "Corp spends 10 credits to rez")))

(deftest next-diamond-lowered-rez-cost
  ;; Lowered rez cost
  (do-game
    (new-game {:corp {:deck ["NEXT Diamond" "NEXT Opal" "NEXT Bronze" "Kakugo"]}})
    (core/gain state :corp :credit 13 :click 1)
    (play-from-hand state :corp "NEXT Diamond" "HQ")
    (play-from-hand state :corp "NEXT Opal" "HQ")
    (play-from-hand state :corp "NEXT Bronze" "R&D")
    (play-from-hand state :corp "Kakugo" "Archives")
    (rez state :corp (get-ice state :hq 1))
    (rez state :corp (get-ice state :archives 0))
    (is (= 9 (:credit (get-corp))) "Corp starts with 9 credits")
    (rez state :corp (get-ice state :hq 0))
    (is (zero? (:credit (get-corp))) "Corp spends 9 credits to rez")))

(deftest next-gold
  ;; NEXT Gold
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["NEXT Gold" "NEXT Bronze"]
                      :credits 100}
               :runner {:hand [(qty "Paperclip" 4) "Clot"]
                        :credits 100}})
    (play-from-hand state :corp "NEXT Gold" "HQ")
    (play-from-hand state :corp "NEXT Bronze" "New remote")
    (let [gold (get-ice state :hq 0)
          bro (get-ice state :remote1 0)]
      (rez state :corp gold)
      (rez state :corp bro)
      (take-credits state :corp)
      (play-from-hand state :runner "Paperclip")
      (play-from-hand state :runner "Clot")
      (is (= 3 (count (:hand (get-runner)))) "Runner starts with 3 cards in hand")
      (is (zero? (count (:discard (get-runner)))) "Runner starts with 0 cards in discard")
      (run-on state :hq)
      (run-continue state)
      (card-subroutine state :corp gold 0)
      (is (= 1 (count (:hand (get-runner)))) "Runner ends with 1 card in hand")
      (is (= 2 (count (:discard (get-runner)))) "Runner ends with 2 cards in discard")
      (card-subroutine state :corp gold 1)
      (click-card state :corp (get-program state 0))
      (click-card state :corp (get-program state 0))
      (is (= 4 (count (:discard (get-runner)))) "Runner ends with 4 cards in discard"))))

(deftest next-opal
  ;; NEXT Opal
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["NEXT Opal" "NEXT Bronze"
                             "Hostile Takeover" "PAD Campaign"
                             "Mumbad Virtual Tour"]
                      :credits 100}})
    (play-from-hand state :corp "NEXT Opal" "HQ")
    (let [no (get-ice state :hq 0)]
      (take-credits state :corp)
      (run-on state "HQ")
      (rez state :corp no)
      (is (= 1 (count (:subroutines (refresh no)))) "Should rez with 1 subroutine")
      (run-continue state)
      (card-subroutine state :corp no 0)
      (click-card state :corp "NEXT Bronze")
      (click-prompt state :corp "New remote")
      (rez state :corp (get-ice state :remote1 0))
      (is (= 2 (count (:subroutines (refresh no))))
          "Should gain additional sub when another NEXT ice is rezzed")
      ; Demonstration that all non-operation types can be installed
      (card-subroutine state :corp no 0)
      (click-card state :corp "Hostile Takeover")
      (click-prompt state :corp "New remote")
      (card-subroutine state :corp no 0)
      (click-card state :corp "PAD Campaign")
      (click-prompt state :corp "New remote")
      (card-subroutine state :corp no 0)
      (click-card state :corp "Mumbad Virtual Tour")
      (click-prompt state :corp "New remote"))))

(deftest next-sapphire
  ;; NEXT Sapphire
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 100)]
                      :hand ["NEXT Bronze" "NEXT Sapphire" (qty "Ice Wall" 2)]
                      :discard (qty "Ice Wall" 5)
                      :credits 10}})
    (play-from-hand state :corp "NEXT Bronze" "HQ")
    (play-from-hand state :corp "NEXT Sapphire" "R&D")
    (let [bronze (get-ice state :hq 0)
          sapphire (get-ice state :rd 0)]
      (rez state :corp sapphire)
      (take-credits state :corp)
      (run-on state "R&D")
      (run-continue state)
      (let [hand (count (:hand (get-corp)))
            deck (count (:deck (get-corp)))]
        (card-subroutine state :corp sapphire 0)
        (is (= 1 (-> (prompt-map :corp) :choices :number)))
        (click-prompt state :corp "1")
        (is (= (inc hand) (count (:hand (get-corp)))) "Corp should draw 1 card from R&D")
        (is (= (dec deck) (count (:deck (get-corp)))) "R&D should lose 1 card"))
      (let [hand (count (:hand (get-corp)))
            trash (count (:discard (get-corp)))]
        (card-subroutine state :corp sapphire 1)
        (click-card state :corp (find-card "Ice Wall" (:discard (get-corp))))
        (is (= (inc hand) (count (:hand (get-corp)))) "Corp should draw 1 card from Archives")
        (is (= (dec trash) (count (:discard (get-corp)))) "Archives should lose 1 card"))
      (let [hand (count (:hand (get-corp)))
            deck (count (:deck (get-corp)))
            num-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))]
        (card-subroutine state :corp sapphire 2)
        (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
        (is (= (dec hand) (count (:hand (get-corp)))) "Corp should add 1 card from HQ to R&D")
        (is (= (inc deck) (count (:deck (get-corp)))) "R&D should gain 1 card")
        (is (= (inc num-shuffles) (count (core/turn-events state :corp :corp-shuffle-deck))) "Corp should shuffle"))
      (rez state :corp bronze)
      (card-subroutine state :corp sapphire 0)
      (is (= 2 (-> (prompt-map :corp) :choices :number)) "2 rezzed NEXT ice increases choice total"))))

(deftest next-sapphire-should-shuffle-even-when-choosing-0
  ;; Should shuffle even when choosing 0
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 100)]
                      :hand ["NEXT Sapphire" "Ice Wall"]}})
    (play-from-hand state :corp "NEXT Sapphire" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [sapphire (get-ice state :hq 0)
          hand (count (:hand (get-corp)))
          deck (count (:deck (get-corp)))
          num-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))]
      (rez state :corp sapphire)
      (run-continue state)
      (card-subroutine state :corp sapphire 2)
      (click-prompt state :corp "Done")
      (is (= hand (count (:hand (get-corp)))) "Nothing selected so HQ shouldn't change")
      (is (= deck (count (:deck (get-corp)))) "Nothing selected so R&D shouldn't change")
      (is (= (inc num-shuffles) (count (core/turn-events state :corp :corp-shuffle-deck)))
          "Corp should shuffle even when selecting nothing"))))

(deftest next-silver
  ;; NEXT Silver
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["NEXT Silver" "NEXT Bronze"]
                      :credits 100}})
    (play-from-hand state :corp "NEXT Silver" "HQ")
    (play-from-hand state :corp "NEXT Bronze" "New remote")
    (let [sil (get-ice state :hq 0)
          bro (get-ice state :remote1 0)]
      (rez state :corp sil)
      (is (= 1 (count (:subroutines (refresh sil)))) "Should rez with 1 subroutine")
      (rez state :corp bro)
      (is (= 2 (count (:subroutines (refresh sil))))
          "Should gain additional sub when another NEXT ice is rezzed")
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (card-subroutine state :corp sil 0)
      (is (not (:run @state)) "Run has ended"))))

(deftest nightdancer
  ;; Nightdancer - Runner loses a click if able, corp gains a click on next turn
  (do-game
    (new-game {:corp {:deck ["Nightdancer"]}})
    (play-from-hand state :corp "Nightdancer" "HQ")
    (take-credits state :corp)
    (let [nd (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp nd)
      (run-continue state)
      (is (= 3 (:click (get-runner))) "Runner starts with 3 clicks")
      (card-subroutine state :corp nd 0)
      (is (= 2 (:click (get-runner))) "Runner lost 1 click")
      (card-subroutine state :corp nd 1)
      (is (= 1 (:click (get-runner))) "Runner lost 1 click")
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (is (= 5 (:click (get-corp))) "Corp has 5 clicks"))))

(deftest oduduwa-encounter-effect
  ;; Encounter effect
  (do-game
    (new-game {:corp {:deck ["Oduduwa" "Enigma"]
                      :credits 10}})
    (play-from-hand state :corp "Oduduwa" "HQ")
    (play-from-hand state :corp "Enigma" "R&D")
    (let [odu (get-ice state :hq 0)
          eni (get-ice state :rd 0)]
      (rez state :corp odu)
      (rez state :corp eni)
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (is (= "Place 1 advancement counter on another ice?" (:msg (prompt-map :corp))) "Corp has correct prompt")
      (click-prompt state :corp "Yes")
      (click-card state :corp (refresh eni))
      (is (= 1 (get-counters (refresh odu) :advancement)))
      (is (= 1 (get-counters (refresh eni) :advancement)))
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (is (= "Place 2 advancement counters on another ice?" (:msg (prompt-map :corp))) "Corp has correct prompt")
      (click-prompt state :corp "Yes")
      (click-card state :corp (refresh eni))
      (is (= 2 (get-counters (refresh odu) :advancement)))
      (is (= 3 (get-counters (refresh eni) :advancement)))
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (is (= "Place 3 advancement counters on another ice?" (:msg (prompt-map :corp)))
          "Corp has correct prompt message")
      (click-prompt state :corp "Yes")
      (click-card state :corp (refresh eni))
      (is (last-log-contains? state "Corp uses Oduduwa to place 3 advancement counters on Enigma")
          "Log is printed correctly")
      (is (= 3 (get-counters (refresh odu) :advancement)))
      (is (= 6 (get-counters (refresh eni) :advancement))))))

(deftest otoroshi
  ;; Otoroshi
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 100)]
                      :hand ["Otoroshi" "Project Junebug"]}
               :runner {:hand ["Sure Gamble"]}})
    (play-from-hand state :corp "Otoroshi" "New remote")
    (play-from-hand state :corp "Project Junebug" "New remote")
    (take-credits state :corp)
    (run-on state :remote1)
    (let [otoroshi (get-ice state :remote1 0)
          junebug (get-content state :remote2 0)
          credits (:credit (get-runner))]
      (is (zero? (get-counters (refresh junebug) :advancement)) "Project Junebug should start with 0 advancement tokens")
      (rez state :corp otoroshi)
      (run-continue state)
      (card-subroutine state :corp otoroshi 0)
      (click-card state :corp junebug)
      (is (= 3 (get-counters (refresh junebug) :advancement)) "Project Junebug should have 3 advancement tokens from Otoroshi subroutine")
      (is (= ["Access a card in Server 2" "Pay 3 [Credits]"] (prompt-buttons :runner)) "Runner should have 2 options")
      (click-prompt state :runner "Pay 3 [Credits]")
      (is (= (- credits 3) (:credit (get-runner))) "Runner should pay 3 credits to Otoroshi subroutine")
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state :remote1)
      (run-continue state)
      (card-subroutine state :corp otoroshi 0)
      (click-card state :corp (refresh junebug))
      (is (= 6 (get-counters (refresh junebug) :advancement)) "Junebug should have 6 advancement tokens from Otoroshi subroutine")
      (is (= ["Access a card in Server 2"] (prompt-buttons :runner)) "Runner should have 1 option")
      (click-prompt state :runner "Access a card in Server 2")
      (click-prompt state :corp "Yes")
      (is (accessing state "Project Junebug"))
      (click-prompt state :runner "No action")
      (is (= :corp (:winner @state)) "Corp has won"))))

(deftest pachinko
  ;;Pachinko
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Pachinko"]}})
    (play-from-hand state :corp "Pachinko" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [pachinko (get-ice state :hq 0)]
      (rez state :corp pachinko)
      (run-continue state)
      (card-subroutine state :corp pachinko 0)
      (card-subroutine state :corp pachinko 1)
      (is (:run @state) "Runner have no tags, run continues"))))

(deftest pachinko-autopump-subtracted-correct-amount-of-credits
  ;; Autopump subtracted correct amount of credits
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Pachinko"]}
               :runner {:hand ["Corroder"]}})
    (play-from-hand state :corp "Pachinko" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (core/gain state :runner :credit 10)
    (run-on state "HQ")
    (let [pachinko (get-ice state :hq 0)
          corroder (get-program state 0)
          runner-credits (:credit (get-runner))]
      (rez state :corp pachinko)
      (run-continue state)
      (auto-pump-and-break state corroder)
      (is (= (- runner-credits 4) (:credit (get-runner))) "Autopump subtracted correct amount of credits"))))

(deftest pachinko-etr-with-tags
  ;; ETR with tags
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Pachinko"]}})
    (play-from-hand state :corp "Pachinko" "HQ")
    (take-credits state :corp)
    (gain-tags state :runner 1)
    (run-on state "HQ")
    (let [pachinko (get-ice state :hq 0)]
      (rez state :corp pachinko)
      (run-continue state)
      (card-subroutine state :corp pachinko 0)
      (is (not (:run @state)) "Run ended"))))

(deftest palisade
  ;; Palisade
  (do-game
    (new-game {:corp {:hand [(qty "Palisade" 2)]}})
    (click-credit state :corp)
    (play-from-hand state :corp "Palisade" "HQ")
    (play-from-hand state :corp "Palisade" "New remote")
    (take-credits state :corp)
    (let [palisade-central (get-ice state :hq 0)
          palisade-remote (get-ice state :remote1 0)]
      (rez state :corp palisade-central)
      (is (= 2 (get-strength (refresh palisade-central))) "Normal Strength")
      (rez state :corp palisade-remote)
      (is (= 4 (get-strength (refresh palisade-remote))) "Boosted Strength on Remote")
      (run-on state "HQ")
      (run-continue state)
      (fire-subs state palisade-central)
      (is (not (:run @state)) "Run ended"))))

(deftest paper-wall
  ;;Paper Wall
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Paper Wall"]}
               :runner {:hand ["Corroder"]}})
    (play-from-hand state :corp "Paper Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (run-on state "HQ")
    (let [paperwall (get-ice state :hq 0)
          corroder (get-program state 0)]
      (rez state :corp paperwall)
      (run-continue state)
      (auto-pump-and-break state corroder)
      (is (nil? (get-ice state :hq 0)) "Paper Wall was trashed"))))

(deftest peeping-tom
  ;;Peeping Tom - Counts # of chosen card type in Runner grip
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Peeping Tom"]}
               :runner {:hand ["Corroder" (qty "Sure Gamble" 4)]}})
    (play-from-hand state :corp "Peeping Tom" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (run-on state "HQ")
    (let [tom (get-ice state :hq 0)]
      (rez state :corp (refresh tom))
      (run-continue state)
      (click-prompt state :corp "Event")
      (is (last-log-contains? state "Sure Gamble, Sure Gamble, Sure Gamble, and Sure Gamble")
          "Revealed Runner grip")
      (is (last-log-contains? state "4") "Correctly counted Events in Runner grip")
      (fire-subs state tom)
      (click-prompt state :runner "Take 1 tag")
      (is (= 1 (count-tags state)) "Tag ability sucessful")
      (click-prompt state :runner "End the run")
      (is (not (:run @state)) "Run ended"))))

(deftest pharos
  ;; Pharos
  (do-game
    (new-game {:corp {:hand ["Pharos" (qty "Hedge Fund" 2)]}})
    (play-from-hand state :corp "Pharos" "HQ")
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :corp)
    (is (= 0 (count-tags state)))
    (let [pha (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp (refresh pha))
      (run-continue state)
      (fire-subs state pha)
      (is (= 1 (count-tags state)))
      (is (not (:run @state)) "Run ended")
      (take-credits state :runner)
      (is (= 0 (get-counters (refresh pha) :advancement)) "Pharos has no adv tokens")
      (is (= 5 (get-strength (refresh pha))) "Pharos starts at 5 strength")
      (dotimes [n 2]
        (advance state pha)
        (is (= (inc n) (get-counters (refresh pha) :advancement)) (str "Pharos has " (inc n) " adv tokens"))
        (is (= 5 (get-strength (refresh pha))) "Pharos stays at 5 strength"))
      (advance state pha)
      (is (= 3 (get-counters (refresh pha) :advancement)) "Pharos has 3 adv tokens")
      (is (= 10 (get-strength (refresh pha))) "Pharos is now at 10 strength"))))

(deftest phonuetria-tag-only-when-passing-itself
  (do-game
    (new-game {:corp {:hand ["Hedge Fund" "Phoneutria" "Vanilla"]}
               :runner {:hand [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Phoneutria" "HQ")
    (play-from-hand state :corp "Vanilla" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 1))
    (rez state :corp (get-ice state :hq 0))
    (is (changed? [(count-tags state) 0]
          (run-continue state :encounter-ice)
          (run-continue state :movement)
          (run-continue state :approach-ice))
        "gained no tag for passing vanilla")
    (run-continue state :encounter-ice)
    (is (changed? [(count-tags state) 1]
          (run-continue state :movement))
        "gained 1 tag for phoneutria")))

(deftest phonuetria-no-tag
  (do-game
    (new-game {:corp {:hand ["Hedge Fund" "Phoneutria"]}
               :runner {:hand [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Phoneutria" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 0))
    (is (changed? [(count-tags state) 0]
          (run-continue state :encounter-ice)
          (run-continue state :movement))
        "gained no tag for passing phoneutria")))

(deftest ping
  ;; Ping
  (do-game
    (new-game {:corp {:hand ["Hedge Fund" (qty "Ping" 2)]}})
    (play-from-hand state :corp "Ping" "HQ")
    (play-from-hand state :corp "Ping" "New remote")
    (take-credits state :corp)
    (let [png1 (get-ice state :hq 0)
          png2 (get-ice state :remote1 0)]
      (is (= 0 (count-tags state)) "Start with no tags")
      (rez state :corp (refresh png2))
      (is (= 0 (count-tags state)) "Didn't take tag on rez outside of run")
      (run-on state "HQ")
      (rez state :corp (refresh png1))
      (is (= 1 (count-tags state)) "Took 1 tag during run")
      (run-continue state)
      (fire-subs state png1)
      (is (not (:run @state)) "Run ended"))))

(deftest ping-no-tag-outside-of-run
  (do-game
    (new-game {:corp {:hand ["Ping"]}})
    (play-cards state :corp ["Ping" "HQ" :rezzed])
    (is (zero? (count-tags state)) "Did not take a tag")))

(deftest piranhas-take-bad-pub
  (do-game
    (new-game {:corp {:hand ["Piranhas"]
                      :deck [(qty "Hedge Fund" 5)]}
               :runner {:hand [(qty "Sure Gamble" 2)]}})
    (play-from-hand state :corp "Piranhas" "HQ")
    (take-credits state :corp)
    (let [pir (get-ice state :hq 0)]
      (run-on state :hq)
      (is (changed? [(count-bad-pub state) 1]
                    (rez state :corp pir))
          "Corp got a bad pub for rezzing Piranhas")
      (run-continue state)
      (is (changed? [(count (:hand (get-corp))) 1]
                    (card-subroutine state :corp (refresh pir) 0)
                    (click-prompt state :corp "Yes"))
          "Corp drew 1 card")
      (is (changed? [(count (:hand (get-runner))) -1
                     (count (:discard (get-runner))) 1]
                    (card-subroutine state :corp (refresh pir) 1))
          "Runner suffered 1 net damage")
      (card-subroutine state :corp (refresh pir) 2)
      (is (:run @state) "Piranhas did not end the run")
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state :hq)
      (run-continue state)
      (fire-subs state (refresh pir))
      (click-prompt state :corp "Yes")
      (is (not (:run @state)) "Run ended"))))

(deftest piranhas-remove-tag
  (do-game
    (new-game {:corp {:hand ["Piranhas"]}})
    (play-from-hand state :corp "Piranhas" "HQ")
    (take-credits state :corp)
    (let [pir (get-ice state :hq 0)]
      (gain-tags state :runner 1)
      (run-on state :hq)
      (is (changed? [(count-tags state) -1]
                    (rez state :corp pir {:expect-rez false})
                    (click-prompt state :corp "Remove 1 tag"))
          "Corp removed 1 tag to rez Piranhas"))))

(deftest pulse
  ;; Pulse
  (do-game
    (new-game {:corp {:hand [(qty "Pulse" 2)] :credits 15}})
    (play-from-hand state :corp "Pulse" "HQ")
    (play-from-hand state :corp "Pulse" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [p1 (get-ice state :hq 1)
          p2 (get-ice state :hq 0)]
      (is (changed? [(:click (get-runner)) -1]
            (rez state :corp (refresh p1)))
          "lost a click to the rez")
      (run-continue state)
      (rez state :corp (refresh p2))
      (is (changed? [(:credit (get-runner)) -2]
            (fire-subs state (refresh p1)))
          "lost 1 credit for 1 rezzed harmonic")
      (is (changed? [(:click (get-runner)) -1]
            (click-prompt state :runner "Spend [Click]"))
          "spent 1 click"))))

(deftest pulse-run-last-click
  ;; Pulse - cannot spend click when running last click
  (do-game
    (new-game {:corp {:hand ["Pulse"]}})
    (play-from-hand state :corp "Pulse" "HQ")
    (take-credits state :corp)
    (dotimes [_ 3]
      (click-credit state :runner))
    (run-on state :hq)
    (let [pulse (get-ice state :hq 0)]
      (rez state :corp pulse)
      (run-continue state)
      (fire-subs state (refresh pulse))
      (is (= ["End the run"] (prompt-buttons :runner))
          "Runner has no click left to spend"))))

(deftest quicksand
  (do-game
    (run-and-encounter-ice-test "Quicksand")
    (let [qs (get-ice state :hq 0)]
      (is (= 1 (get-counters qs :power)) "Gained 1 power counter")
      (fire-subs state qs)
      (is (not (:run @state)) "Quicksand ended the run"))))

(deftest red-tape
  ;; Red Tape
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall" "Red Tape"]
                      :credits 10}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Red Tape" "R&D")
    (let [iw (get-ice state :hq 0)
          red-tape (get-ice state :rd 0)]
      (rez state :corp iw)
      (rez state :corp red-tape)
      (take-credits state :corp)
      (run-on state "R&D")
      (run-continue state)
      (is (changed? [(core/get-strength (refresh iw)) 3]
            (fire-subs state red-tape))
          "Ice Wal should gain strengh"))))

(deftest red-tape-strength-after-rez
  ;; Red Tape
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall" "Red Tape"]
                      :credits 10}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Red Tape" "R&D")
    (let [iw (get-ice state :hq 0)
          red-tape (get-ice state :rd 0)]
      (rez state :corp red-tape)
      (take-credits state :corp)
      (run-on state "R&D")
      (run-continue state)
      (is (changed? [(core/get-strength (refresh iw)) 3]
            (fire-subs state red-tape)
            (rez state :corp iw))
          "Ice Wall should gain strengh"))))


(deftest resistor-strength-based-on-tags
  ;; Strength based on tags
  (do-game
    (new-game {:corp {:deck ["Resistor"]}})
    (play-from-hand state :corp "Resistor" "HQ")
    (let [resistor (get-ice state :hq 0)]
      (rez state :corp resistor)
      (is (zero? (get-strength (refresh resistor))) "No Runner tags; 0 strength")
      (gain-tags state :runner 2)
      (is (= 2 (count-tags state)))
      (is (= 2 (get-strength (refresh resistor))) "2 Runner tags; 2 strength")
      (take-credits state :corp)
      (remove-tag state :runner)
      (is (= 1 (get-strength (refresh resistor))) "Runner removed 1 tag; down to 1 strength"))))

(deftest resistor-subroutine-is-trace-4-etr
  ;; Subroutine is trace 4 etr
  (do-game
    (new-game {:corp {:deck ["Resistor"]}})
    (play-from-hand state :corp "Resistor" "HQ")
    (let [resistor (get-ice state :hq 0)]
      (rez state :corp resistor)
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (fire-subs state resistor)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 4 (:base (prompt-map :corp))) "Trace is base 4")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (not (:run @state)) "Run has been ended"))))

(deftest rime
  ;; Rime
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand [(qty "Rime" 2) (qty "Ice Wall" 2)]}})
    (core/gain state :corp :click 10 :credit 10)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (let [iw1 (get-ice state :hq 0)
          iw2 (get-ice state :rd 0)]
      (rez state :corp iw1)
      (rez state :corp iw2)
      (is (= 1 (core/get-strength (refresh iw1))) "Ice Wall starts with 1 strength")
      (play-from-hand state :corp "Rime" "HQ")
      (is (= 1 (core/get-strength (refresh iw1))) "Rime does nothing until rezzed")
      (rez state :corp (get-ice state :hq 1))
      (is (= 2 (core/get-strength (refresh iw1))) "Rime gives Ice Wall on the same server bonus strength")
      (is (= 1 (core/get-strength (refresh iw2))) "Rime doesn't give ice on other servers bonus strength")
      (core/move state :corp (get-ice state :hq 1) :hand)
      (play-from-hand state :corp "Rime" "R&D")
      (rez state :corp (get-ice state :rd 1))
      (is (= 1 (core/get-strength (refresh iw1))) "Rime no longer gives bonus strength to ice on previous server")
      (is (= 2 (core/get-strength (refresh iw2))) "Rime only gives ice on current server bonus strength"))))

(deftest rsvp-full-test
  (do-game
    (new-game {:corp {:hand ["RSVP" "Forced Connection"]}
               :runner {:credits 50}})
    (play-from-hand state :corp "RSVP" "HQ")
    (take-credits state :corp)
    (let [rsvp (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp rsvp)
      (run-continue state :encounter-ice)
      (card-subroutine state :corp (refresh rsvp) 0)
      (run-continue-until state :success)
      ;; we're still allowed to pay 0 credits for traces and trashes
      (click-prompt state :corp "0")
      (is (= 0 (:choices (prompt-map :runner))))
      (click-prompt state :runner "0")
      (click-prompt state :runner "Pay 0 [Credits] to trash")))) ; trash

(deftest sadaka-sub-1-look-at-the-top-3-cards-of-r-d-arrange-those-or-shuffle-r-d-you-may-draw-1-card
  ;; Sub 1 - Look at the top 3 cards of R&D, arrange those or shuffle R&D. You may draw 1 card
  (do-game
    (new-game {:corp {:deck [(qty "Enigma" 4)]
                      :hand ["Sadaka"]}})
    (play-from-hand state :corp "Sadaka" "Archives")
    (let [sadaka (get-ice state :archives 0)]
      (take-credits state :corp)
      (run-on state "Archives")
      (rez state :corp sadaka)
      (run-continue state)
      (is (zero? (count (:hand (get-corp)))) "Corp starts with empty hand")
      (card-subroutine state :corp (refresh sadaka) 0)
      (click-prompt state :corp "Shuffle R&D")
      (click-prompt state :corp "Yes")
      (is (= 1 (count (:hand (get-corp)))) "Corp draws a card")
      (card-subroutine state :corp (refresh sadaka) 0)
      (click-prompt state :corp "Arrange cards")
      (click-prompt state :corp "Enigma")
      (click-prompt state :corp "Enigma")
      (click-prompt state :corp "Enigma")
      (click-prompt state :corp "Done")
      (click-prompt state :corp "No")
      (is (= 1 (count (:hand (get-corp)))) "Corp doesn't draw a card"))))

(deftest sadaka-sub-2-you-may-trash-1-card-in-hq-if-you-do-trash-1-resource-trash-sadaka
  ;; Sub 2 - You may trash 1 card in HQ. If you do, trash 1 resource. Trash Sadaka.
  (do-game
    (new-game {:corp {:deck [(qty "Sadaka" 2) (qty "Enigma" 3)]}
               :runner {:deck ["Bank Job"]}})
    (play-from-hand state :corp "Sadaka" "Archives")
    (play-from-hand state :corp "Sadaka" "HQ")
    (let [sadaka (get-ice state :archives 0)
          sadakaHQ (get-ice state :hq 0)]
      (take-credits state :corp)
      (play-from-hand state :runner "Bank Job")
      (run-on state "Archives")
      (rez state :corp sadaka)
      (run-continue state)
      (is (= 3 (count (:hand (get-corp)))) "Corp starts with 3 cards in hand")
      (is (zero? (count (:discard (get-corp)))) "Corps starts with 0 cards in archives")
      (card-subroutine state :corp (refresh sadaka) 1)
      (click-prompt state :corp (find-card "Enigma" (:hand (get-corp))))
      (is (= 2 (count (:hand (get-corp)))) "Corp discards 1 card")
      (is (= 1 (count (:discard (get-corp)))) "1 card trashed")
      (click-prompt state :corp "Done")
      (is (= 2 (count (:discard (get-corp)))) "Sadaka trashed")
      (run-jack-out state)
      (run-on state "HQ")
      (rez state :corp sadakaHQ)
      (run-continue state)
      (is (= 2 (count (:hand (get-corp)))) "Corp starts with 2 cards in hand")
      (is (= 2 (count (:discard (get-corp)))) "Corps starts with 2 cards in archives")
      (is (zero? (count (:discard (get-runner)))) "Runner starts with 0 cards in discard")
      (card-subroutine state :corp (refresh sadakaHQ) 1)
      (click-prompt state :corp (find-card "Enigma" (:hand (get-corp))))
      (is (= 1 (count (:hand (get-corp)))) "Corp discards 1 card")
      (is (= 3 (count (:discard (get-corp)))) "1 card trashed")
      (click-card state :corp (get-resource state 0))
      (is (= 1 (count (:discard (get-runner)))) "Runner resource trashed")
      (is (= 4 (count (:discard (get-corp)))) "sadakaHQ trashed"))))

(deftest saisentan-corp-chooses-correctly
  ;; Corp chooses correctly
  (do-game
    (new-game {:corp {:hand ["Saisentan"]}
               :runner {:hand [(qty "Sure Gamble" 6)]}})
    (play-from-hand state :corp "Saisentan" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [sai (get-ice state :hq 0)]
      (rez state :corp sai)
      (run-continue state)
      (click-prompt state :corp "Event")
      (is (zero? (-> (get-runner) :discard count)) "Heap should be empty")
      (card-subroutine state :corp sai 0)
      (is (= 2 (-> (get-runner) :discard count)) "Two cards should be trashed due to correctly guessing"))))

(deftest saisentan-bankhar-interaction
  ;; Corp chooses correctly
  (do-game
    (new-game {:corp {:hand ["Saisentan"]}
               :runner {:hand ["Tsakhia \"Bankhar\" Gantulga" (qty "Sure Gamble" 4)]}})
    (play-from-hand state :corp "Saisentan" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Tsakhia \"Bankhar\" Gantulga")
    (take-credits state :runner)
    (take-credits state :corp)
    (click-prompt state :runner "HQ")
    (run-on state "HQ")
    (let [sai (get-ice state :hq 0)]
      (rez state :corp sai)
      (run-continue state)
      (click-prompt state :corp "Event")
      (is (zero? (-> (get-runner) :discard count)) "Heap should be empty")
      (card-subroutine state :corp sai 0)
      (is (= 2 (-> (get-runner) :discard count)) "Two cards should be trashed due to correctly guessing"))))

(deftest saisentan-corp-chooses-incorrectly
  ;; Corp chooses incorrectly
  (do-game
    (new-game {:corp {:hand ["Saisentan"]}
               :runner {:hand [(qty "Sure Gamble" 6)]}})
    (play-from-hand state :corp "Saisentan" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [sai (get-ice state :hq 0)]
      (rez state :corp sai)
      (run-continue state)
      (click-prompt state :corp "Hardware")
      (is (zero? (-> (get-runner) :discard count)) "Heap should be empty")
      (card-subroutine state :corp sai 0)
      (is (= 1 (-> (get-runner) :discard count)) "Only one card should be trashed due to incorrectly guessing"))))

(deftest saisentan-firing-subs-with-play-unbroken-subroutines
  ;; Firing subs with play-unbroken-subroutines
  (do-game
    (new-game {:corp {:hand ["Saisentan"]}
               :runner {:hand [(qty "Sure Gamble" 9)]}})
    (play-from-hand state :corp "Saisentan" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [sai (get-ice state :hq 0)]
      (rez state :corp sai)
      (run-continue state)
      (click-prompt state :corp "Event")
      (is (changed? [(count (:hand (get-runner))) -6]
            (fire-subs state (refresh sai)))
          "6 damage in total")
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state "HQ")
      (run-continue state)
      (click-prompt state :corp "Hardware")
      (is (changed? [(count (:hand (get-runner))) -3]
            (fire-subs state (refresh sai)))
          "3 damage in total"))))

(deftest saisentan-preventing-damage
  ;; Preventing damage
  (do-game
    (new-game {:corp {:hand ["Saisentan"]}
               :runner {:hand ["Sure Gamble" "Sure Gamble" "Caldera" "Diesel" "Deuces Wild"]}})
    (play-from-hand state :corp "Saisentan" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Caldera")
    (run-on state "HQ")
    (let [sai (get-ice state :hq 0)
          cal (get-resource state 0)]
      (rez state :corp sai)
      (run-continue state)
      (click-prompt state :corp "Event")
      (fire-subs state (refresh sai))
      (is (changed? [(count (:hand (get-runner))) -1]
            (click-prompt state :runner "Pass priority"))
          "Let through first sub damage")
      (is (changed? [(count (:hand (get-runner))) 0]
            (click-prompt state :runner "Caldera"))
          "Prevent special damage")
      (is (changed? [(count (:hand (get-runner))) 0]
            (click-prompt state :runner "Caldera"))
          "Prevent second sub damage")
      (is (changed? [(count (:hand (get-runner))) 0]
            (click-prompt state :runner "Caldera"))
          "Prevent third sub damage")
      (is (no-prompt? state :runner) "No more damage prevention triggers"))))

(deftest salvage-subroutine-gaining-ability
  ;; Subroutine gaining ability
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Salvage"]}})
    (play-from-hand state :corp "Salvage" "HQ")
    (let [salvage (get-ice state :hq 0)]
      (rez state :corp salvage)
      (is (zero? (count (:subroutines (refresh salvage)))) "Salvage starts with 0 subs")
      (advance state salvage 2)
      (is (= 2 (count (:subroutines (refresh salvage)))) "Salvage gains 2 subs"))))

(deftest salvage-subroutine-is-trace-2-gain-a-tag
  ;; Subroutine is trace 2 gain a tag
  (do-game
    (new-game {:corp {:deck ["Salvage"]}})
    (play-from-hand state :corp "Salvage" "HQ")
    (let [salvage (get-ice state :hq 0)]
      (rez state :corp salvage)
      (advance state salvage 1)
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (fire-subs state salvage)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 2 (:base (prompt-map :corp))) "Trace is base 2")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (count-tags state)) "Runner has gained 1 tag"))))

(deftest sand-storm
  ;; Sand Storm should not end the run if protecting an otherwise empty/naked server
  (do-game
    (new-game {:corp {:deck ["Sand Storm" "PAD Campaign"]}})
    (play-from-hand state :corp "Sand Storm" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (run-on state "Server 1")
    (let [sand-storm (get-ice state :remote1 0)]
      (rez state :corp sand-storm)
      (run-continue state)
      (card-subroutine state :corp sand-storm 0)
      (click-prompt state :corp "Server 2")
      (is (= :remote2 (first (get-in @state [:run :server]))) "Is running on server 2"))))

(deftest sandman
  ;; Sandman - add an installed runner card to the grip
  (do-game
    (new-game {:corp {:deck ["Sandman"]}
               :runner {:deck ["Inti" "Scrubber"]}})
    (play-from-hand state :corp "Sandman" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Inti")
    (play-from-hand state :runner "Scrubber")
    (is (zero? (count (:hand (get-runner)))) "Runner's hand is empty")
    (run-on state "HQ")
    (let [sand (get-ice state :hq 0)]
      (rez state :corp (refresh sand))
      (run-continue state)
      (card-subroutine state :corp (refresh sand) 0)
      (click-card state :corp "Inti")
      (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
      (card-subroutine state :corp (refresh sand) 0)
      (click-card state :corp "Scrubber")
      (is (= 2 (count (:hand (get-runner)))) "Runner has 2 cards in hand")
      (card-subroutine state :corp (refresh sand) 0)
      (is (no-prompt? state :corp) "Sandman doesn't fire if no installed cards"))))

(deftest sandstone
  ;; Sandstone - gain virus counter on run reducing strength by 1
  (do-game
    (new-game {:corp {:deck ["Sandstone"]}})
    (play-from-hand state :corp "Sandstone" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :click 10)
    (let [snd (get-ice state :hq 0)]
      (rez state :corp snd)
      (dotimes [i 6]
        (run-on state "HQ")
        (is (= i (get-counters (refresh snd) :virus)) (str "Counter " i " not placed yet"))
        (is (= (- 6 i) (core/get-strength (refresh snd))) "Strength not reduced yet")
        (run-continue state)
        (is (= (inc i) (get-counters (refresh snd) :virus)) (str "Counter " i " placed"))
        (is (= (- 6 (inc i)) (core/get-strength (refresh snd))) "Strength reduced")
        (card-subroutine state :corp (refresh snd) 0)
        (is (not (:run @state)) "Run ended"))
      (is (= 0 (core/get-strength (refresh snd))) "Sandstone strength at 0"))))

(deftest sapper
  ;; Sapper
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Sapper"]}
               :runner {:hand ["Corroder"]}})
    (play-from-hand state :corp "Sapper" "HQ")
    (let [sapper (get-ice state :hq 0)]
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (run-on state "HQ")
      (rez state :corp sapper)
      (run-continue state)
      (card-subroutine state :corp sapper 0)
      (click-card state :corp "Corroder")
      (is (nil? (get-program state 0)) "Corroder is trashed"))))

(deftest sapper-access-test
  ;; Access test
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Sapper"]}
               :runner {:hand ["Corroder"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (run-empty-server state :hq)
    (is (= "Sapper" (:title (core/get-current-ice state))) "Encountering Sapper on access")
    (fire-subs state (core/get-current-ice state))
    (click-card state :corp "Corroder")
    (is (nil? (get-program state 0)) "Corroder is trashed")))

(deftest scatter-field-subs-test
  (do-game (subroutine-test "Scatter Field" 0 {:corp {:hand ["Rashida Jaheem"]}})
           (click-card state :corp "Rashida Jaheem")
           (click-prompt state :corp "New remote")
           (is (= 4 (get-strength (get-ice state :hq 0))) "4 str"))
  (do-game (new-game (etr-sub "Scatter Field" 1))))

(deftest searchlight
  ;; Searchlight - Trace bace equal to advancement counters
  (do-game
    (new-game {:corp {:deck ["Searchlight"]}})
    (play-from-hand state :corp "Searchlight" "HQ")
    (take-credits state :corp)
    (let [searchlight (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp searchlight)
      (run-continue state)
      (card-subroutine state :corp (refresh searchlight) 0)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (zero? (:base (prompt-map :corp))) "Trace is base 0")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (zero? (count-tags state)) "Trace failed with 0 advancements")
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (advance state searchlight 1)
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (card-subroutine state :corp (refresh searchlight) 0)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 1 (:base (prompt-map :corp))) "Trace is now base 1")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (count-tags state)) "Trace succeeds with 1 advancement"))))

(deftest searchlight-x-fn
  (do-game
    (new-game {:corp {:deck ["Searchlight"]}
               :runner {:hand ["Hush"]}})
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "Searchlight" "HQ")
    (let [searchlight (get-ice state :hq 0)]
      (rez state :corp searchlight)
      (advance state (refresh searchlight) 1)
      (take-credits state :corp)
      (play-from-hand state :runner "Hush")
      (click-card state :runner "Searchlight")
      (run-on state "HQ")
      (run-continue state)
      (card-subroutine state :corp (refresh searchlight) 0)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (zero? (:base (prompt-map :corp))) "Trace is base 0")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (zero? (count-tags state)) "Trace failed with 0 advancements")
      (trash state :runner (first (:hosted (get-ice state :hq 0))))
      (card-subroutine state :corp (refresh searchlight) 0)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 1 (:base (prompt-map :corp))) "Trace is now base 1")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (count-tags state)) "Trace succeeds with 1 advancement"))))

(deftest seidr-adaptive-barrier
  ;; Seidr Adaptive Barrier - +1 strength for every ice protecting its server
  (do-game
    (new-game {:corp {:deck ["Seidr Adaptive Barrier" (qty "Ice Wall" 2)]}})
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "Seidr Adaptive Barrier" "HQ")
    (let [sab (get-ice state :hq 0)]
      (rez state :corp sab)
      (is (= 3 (get-strength (refresh sab))) "Seidr gained 1 strength for itself")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (is (= 4 (get-strength (refresh sab))) "+2 strength for 2 pieces of ice")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (is (= 5 (get-strength (refresh sab))) "+3 strength for 3 pieces of ice")
      (core/process-action "move" state :corp {:card (get-ice state :hq 1) :server "Archives"})
      (is (= 4 (get-strength (refresh sab))) "+2 strength for 2 pieces of ice"))))

(deftest self-adapting-code-wall
  ;; Self-Adapting Code Wall
  (do-game
    (new-game {:corp {:deck ["Self-Adapting Code Wall" "Lag Time"]}
               :runner {:deck ["Ice Carver" "Parasite"]}})
    (play-from-hand state :corp "Self-Adapting Code Wall" "Archives")
    (take-credits state :corp 2)
    (let [sacw (get-ice state :archives 0)]
      (play-from-hand state :runner "Ice Carver")
      (run-on state "Archives")
      (rez state :corp sacw)
      (run-continue state)
      (is (= 1 (get-strength (refresh sacw))) "Self-Adapting Code Wall strength unchanged")
      (run-continue state :movement)
      (run-jack-out state)
      (play-from-hand state :runner "Parasite")
      (click-card state :runner sacw)
      (is (= 1 (count (:hosted (refresh sacw)))) "Parasite hosted on Self-Adapting Code Wall")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 1 (core/get-virus-counters state (first (:hosted (refresh sacw)))))
          "Parasite has 1 virus counter")
      (is (= 1 (get-strength (refresh sacw))) "Self-Adapting Code Wall strength unchanged")
      (take-credits state :runner)
      (play-from-hand state :corp "Lag Time")
      (is (= 2 (get-strength (refresh sacw))) "Self-Adapting Code Wall strength increased")
      (take-credits state :corp)
      (is (= 2 (get-strength (refresh sacw))) "Self-Adapting Code Wall strength increased"))))

(deftest semak-samun-sub-0-etr-unless-runner-suffers-3-net-damage
  (do-game
    (subroutine-test "Semak-samun" 0 {:runner {:hand 4}})
    (click-prompt state :runner "End the run")
    (is (not (:run @state)) "Run ended"))
  (do-game
    (subroutine-test "Semak-samun" 0 {:runner {:hand 4}})
    (click-prompt state :runner "Suffer 3 net damage")
    (is (:run @state) "Run ended")
    (is (and (= 1 (count (:hand (get-runner)))) (= 3 (count (:discard (get-runner))))) "Took 3 net")))

(deftest semak-samun-can-be-broken-by-fracters
  (do-game
    (run-and-encounter-ice-test "Semak-samun" nil {:rig ["Rising Tide"]})
    (auto-pump-and-break state (get-program state 0))
    (is (-> (get-ice state :hq 0) :subroutines first :broken) "Broke sub with a fracter")))

(deftest sensei
  ;; Sensei
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Special Offer" "Snowflake" "Sensei"]
                      :credits 100}})
    (play-from-hand state :corp "Special Offer" "HQ")
    (play-from-hand state :corp "Snowflake" "HQ")
    (play-from-hand state :corp "Sensei" "HQ")
    (take-credits state :corp)
    (let [offer (get-ice state :hq 0)
          snow (get-ice state :hq 1)
          sensei (get-ice state :hq 2)]
      (run-on state :hq)
      (rez state :corp sensei)
      (run-continue state)
      (fire-subs state sensei)
      (is (= 1 (count (:subroutines (refresh offer)))) "Special Offer starts with 1 sub")
      (run-continue-until state :approach-ice snow)
      (rez state :corp snow)
      (run-continue state)
      (is (= 2 (count (:subroutines (refresh snow)))) "Snowflake gains 1 sub from Sensei")
      (run-continue-until state :approach-ice offer)
      (rez state :corp offer)
      (run-continue state)
      (is (= 2 (count (:subroutines (refresh offer)))) "Special Offer gains 1 sub from Sensei")
      (run-continue state :movement)
      (run-jack-out state)
      (is (= 1 (count (:subroutines (refresh offer)))) "Special Offer resets on run-end")
      (is (= 1 (count (:subroutines (refresh snow)))) "Snowflake resets on run-end"))))

(deftest seraph
  (do-game
    (new-game {:corp {:hand ["Seraph"]
                      :credits 10}
               :runner {:hand [(qty "Sure Gamble" 4)]
                        :credits 10}})
    (play-from-hand state :corp "Seraph" "HQ")
    (take-credits state :corp)
    (let [ser (get-ice state :hq 0)]
      (rez state :corp ser)
      (run-on state :hq)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -3]
                    (click-prompt state :runner "Lose 3 [Credits]"))
          "Runner lost 3 credits")
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state :hq)
      (run-continue state)
      (is (changed? [(count-tags state) 1]
                    (click-prompt state :runner "Take 1 tag"))
          "Runner got 1 tag")
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state :hq)
      (run-continue state)
      (is (changed? [(count (:hand (get-runner))) -2
                     (count (:discard (get-runner))) 2]
                    (click-prompt state :runner "Suffer 2 net damage"))
          "Runner suffered 2 net damage")
      (is (changed? [(:credit (get-runner)) -3]
                    (card-subroutine state :corp (refresh ser) 0))
          "Runner lost 3 credits")
      (is (changed? [(count (:hand (get-runner))) -2
                     (count (:discard (get-runner))) 2]
                    (card-subroutine state :corp (refresh ser) 1))
          "Runner suffered 2 net damage")
      (is (changed? [(count-tags state) 1]
                    (card-subroutine state :corp (refresh ser) 2))
          "Runner got 1 tag"))))

(deftest sherlock-1-0-subroutine-1-trace-4-add-an-installed-program-to-the-top-of-the-stack
  ;; Subroutine 1: Trace 4 - add an installed program to the top of the stack
  (do-game
    (new-game {:corp {:deck ["Sherlock 1.0"]}
               :runner {:deck [(qty "Gordian Blade" 3) (qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Sherlock 1.0" "HQ")
    (take-credits state :corp)
    (let [sherlock (get-ice state :hq 0)]
      (play-from-hand state :runner "Gordian Blade")
      (run-on state :hq)
      (rez state :corp sherlock)
      (run-continue state)
      (card-subroutine state :corp sherlock 0)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 4 (:base (prompt-map :corp))) "Trace is base 4")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp (get-program state 0))
      (is (empty? (get-program state)) "Gordian uninstalled")
      (is (= "Gordian Blade" (:title (first (:deck (get-runner))))) "Gordian on top of Stack"))))

(deftest sherlock-1-0-subroutine-2-trace-4-add-an-installed-program-to-the-top-of-the-stack
  ;; Subroutine 2: Trace 4 - add an installed program to the top of the stack
  (do-game
    (new-game {:corp {:deck ["Sherlock 1.0"]}
               :runner {:deck [(qty "Gordian Blade" 3) (qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Sherlock 1.0" "HQ")
    (take-credits state :corp)
    (let [sherlock (get-ice state :hq 0)]
      (play-from-hand state :runner "Gordian Blade")
      (run-on state :hq)
      (rez state :corp sherlock)
      (run-continue state)
      (card-subroutine state :corp sherlock 1)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 4 (:base (prompt-map :corp))) "Trace is base 4")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp (get-program state 0))
      (is (empty? (get-program state)) "Gordian uninstalled")
      (is (= "Gordian Blade" (:title (first (:deck (get-runner))))) "Gordian on top of Stack"))))

(deftest sherlock-2-0-subroutine-1-trace-4-add-an-installed-program-to-the-bottom-of-the-stack
  ;; Subroutine 1: Trace 4 - add an installed program to the bottom of the stack
  (do-game
    (new-game {:corp {:deck [(qty "Sherlock 2.0" 1)]}
               :runner {:deck [(qty "Gordian Blade" 3) (qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Sherlock 2.0" "HQ")
    (take-credits state :corp)
    (let [sherlock (get-ice state :hq 0)]
      (play-from-hand state :runner "Gordian Blade")
      (run-on state :hq)
      (rez state :corp sherlock)
      (run-continue state)
      (card-subroutine state :corp sherlock 0)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 4 (:base (prompt-map :corp))) "Trace is base 4")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp (get-program state 0))
      (is (empty? (get-program state)) "Gordian uninstalled")
      (is (= "Gordian Blade" (:title (last (:deck (get-runner))))) "Gordian on bottom of Stack"))))

(deftest sherlock-2-0-subroutine-2-trace-4-add-an-installed-program-to-the-bottom-of-the-stack
  ;; Subroutine 2: Trace 4 - add an installed program to the bottom of the stack
  (do-game
    (new-game {:corp {:deck [(qty "Sherlock 2.0" 1)]}
               :runner {:deck [(qty "Gordian Blade" 3) (qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Sherlock 2.0" "HQ")
    (take-credits state :corp)
    (let [sherlock (get-ice state :hq 0)]
      (play-from-hand state :runner "Gordian Blade")
      (run-on state :hq)
      (rez state :corp sherlock)
      (run-continue state)
      (card-subroutine state :corp sherlock 1)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 4 (:base (prompt-map :corp))) "Trace is base 4")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp (get-program state 0))
      (is (empty? (get-program state)) "Gordian uninstalled")
      (is (= "Gordian Blade" (:title (last (:deck (get-runner))))) "Gordian on bottom of Stack"))))

(deftest sherlock-2-0-subroutine-3-give-1-tag
  ;; Subroutine 3: Give 1 tag
  (do-game
    (new-game {:corp {:deck [(qty "Sherlock 2.0" 1)]}})
    (play-from-hand state :corp "Sherlock 2.0" "HQ")
    (take-credits state :corp)
    (let [sherlock (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp sherlock)
      (run-continue state)
      (card-subroutine state :corp sherlock 2)
      (is (= 1 (count-tags state)) "Runner gains 1 tag"))))

(deftest shiro-subroutine-1-rearrange-the-top-3-cards-of-the-stack
  ;; Subroutine 1: Rearrange the top 3 cards of the stack
  (do-game
    (new-game {:corp {:deck ["Caprice Nisei" "Quandary" "Jackson Howard"]
                      :hand ["Shiro"]}
               :runner {:deck ["R&D Interface"]}})
    (play-from-hand state :corp "Shiro" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "R&D Interface")
    (let [shiro (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp shiro)
      (run-continue state)
      (card-subroutine state :corp shiro 0)
      (click-prompt state :corp (find-card "Caprice Nisei" (:deck (get-corp))))
      (click-prompt state :corp (find-card "Quandary" (:deck (get-corp))))
      (click-prompt state :corp (find-card "Jackson Howard" (:deck (get-corp))))
      ;; try starting over
      (click-prompt state :corp "Start over")
      (click-prompt state :corp (find-card "Jackson Howard" (:deck (get-corp))))
      (click-prompt state :corp (find-card "Quandary" (:deck (get-corp))))
      (click-prompt state :corp (find-card "Caprice Nisei" (:deck (get-corp)))) ;this is the top card of R&D
      (click-prompt state :corp "Done")
      (is (= "Caprice Nisei" (:title (first (:deck (get-corp))))))
      (is (= "Quandary" (:title (second (:deck (get-corp))))))
      (is (= "Jackson Howard" (:title (second (rest (:deck (get-corp))))))))))

(deftest shiro-subroutine-2-the-runner-accesses-the-top-card-of-r-d-unless-the-corp-pays-1
  ;; Subroutine 2: The runner accesses the top card of R&D unless the Corp pays 1.
  (testing "The Corp chooses to let the runner access"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Shiro"]}
                 :runner {:deck ["R&D Interface"]}})
      (play-from-hand state :corp "Shiro" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "R&D Interface")
      (let [shiro (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp shiro)
        (run-continue state)
        (card-subroutine state :corp shiro 1)
        (click-prompt state :corp "No")
        (is (last-n-log-contains? state 2 "make the Runner breach R&D") "Access is logged")
        (is (= (:cid (first (:deck (get-corp))))
               (:cid (:card (prompt-map :runner)))) "Access the top card of R&D")
        (click-prompt state :runner "No action")
        (is (= (:cid (second (:deck (get-corp))))
               (:cid (:card (prompt-map :runner)))) "Access another card due to R&D Interface"))))
  (testing "The Corp chooses to pay to not let the runner access"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Shiro"]}
                 :runner {:deck ["R&D Interface"]}})
      (play-from-hand state :corp "Shiro" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "R&D Interface")
      (let [shiro (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp shiro)
        (run-continue state)
        (card-subroutine state :corp shiro 1)
        (let [credits (:credit (get-corp))]
          (click-prompt state :corp "Yes")
          (is (last-log-contains? state "pays 1 [Credits]") "Payment is logged")
          (is (last-log-contains? state "keep the Runner from breaching R&D") "Prevention is logged")
          (is (= (dec credits) (:credit (get-corp))) "Corp pays 1 to prevent access"))))))

(deftest shiro-with-mwanza-city-grid-should-access-additional-3-cards
  ;; with Mwanza City Grid, should access additional 3 cards
  (do-game
    (new-game {:corp {:deck ["Shiro" "Mwanza City Grid"
                             (qty "Ice Wall" 10)]}
               :runner {:deck ["R&D Interface"]}})
    (starting-hand state :corp ["Shiro" "Mwanza City Grid"])
    (play-from-hand state :corp "Mwanza City Grid" "R&D")
    (play-from-hand state :corp "Shiro" "R&D")
    (take-credits state :corp)
    (core/gain state :corp :credit 100)
    (play-from-hand state :runner "R&D Interface")
    (let [shiro (get-ice state :rd 0)
          mwanza (get-content state :rd 0)]
      (run-on state :rd)
      (rez state :corp shiro)
      (rez state :corp mwanza)
      (run-continue state)
      (let [credits (:credit (get-corp))]
        (card-subroutine state :corp shiro 1)
        (click-prompt state :corp "No")
        (is (= 4 (core/access-bonus-count state :runner :rd)) "Should access an additional 4 cards")
        (dotimes [_ 5]
          (click-prompt state :runner "No action"))
        (run-continue state :movement)
        (run-jack-out state)
        (is (= (+ credits 10) (:credit (get-corp))) "Corp should only gain money once")))))

(deftest slot-machine
  ;; Slot Machine
  (do-game
    (new-game {:corp {:hand ["Slot Machine" "Ice Wall"]}
               :runner {:deck [(qty "Sure Gamble" 10)]}})
    (play-from-hand state :corp "Ice Wall" "R&D")
    (play-from-hand state :corp "Slot Machine" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [sm (get-ice state :hq 0)]
      (rez state :corp sm))
    (let [sm (get-ice state :hq 0)
          iw (get-ice state :rd 0)
          corp-credits (:credit (get-corp))
          runner-credits (:credit (get-runner))
          cid (:cid (first (:deck (get-runner))))]
      (run-continue state)
      (is (not= cid (:cid (first (:deck (get-runner))))))
      (card-subroutine state :corp sm 0)
      (is (= (- runner-credits 3) (:credit (get-runner))))
      (card-subroutine state :corp sm 1)
      (is (= (+ corp-credits 3) (:credit (get-corp))))
      (card-subroutine state :corp sm 2)
      (is (zero? (get-counters (refresh iw) :advancement)))
      (click-card state :corp iw)
      (is (= 3 (get-counters (refresh iw) :advancement))))))

(deftest slot-machine-should-properly-log-the-card-titles-when-revealed
  ;; Should properly log the card titles when revealed
  (do-game
    (new-game {:corp {:hand ["Slot Machine" "Ice Wall"]}
               :runner {:deck [(qty "Sure Gamble" 10)]}})
    (play-from-hand state :corp "Slot Machine" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [sm (get-ice state :hq 0)]
      (rez state :corp sm))
    (run-continue state)
    (is (last-log-contains? state "Corp uses Slot Machine to put the top card of the stack to the bottom, then reveal Sure Gamble \\(Event\\), Sure Gamble \\(Event\\), and Sure Gamble \\(Event\\) from the top of the stack.") "3 top cards revelaed")))

(deftest slot-machine-subroutines
  ;; Subroutines
  (testing "Subroutine 2 should only fire when there are at least 2 cards in deck"
    (testing "Only 1 card in deck"
      (do-game
        (new-game {:corp {:hand ["Slot Machine" "Ice Wall"]}
                   :runner {:deck ["Sure Gamble"]
                            :hand ["Sure Gamble"]}})
        (play-from-hand state :corp "Slot Machine" "HQ")
        (take-credits state :corp)
        (run-on state :hq)
        (let [sm (get-ice state :hq 0)]
          (rez state :corp sm)
          (run-continue state)
          (is (changed? [(:credit (get-corp)) 0]
                (card-subroutine state :corp sm 1))
              "Corp does not gain any credits when runner has 1 or less cards in deck"))))
    (testing "2 same cards in deck"
      (do-game
        (new-game {:corp {:hand ["Slot Machine" "Ice Wall"]}
                   :runner {:deck [(qty "Sure Gamble" 2)]
                            :hand ["Sure Gamble"]}})
        (play-from-hand state :corp "Slot Machine" "HQ")
        (take-credits state :corp)
        (run-on state :hq)
        (let [sm (get-ice state :hq 0)]
          (rez state :corp sm)
          (run-continue state)
          (is (changed? [(:credit (get-corp)) 3]
                (card-subroutine state :corp sm 1))
              "Corp gains credits when runner has 2 same cards in deck"))))
    (testing "2 different cards in deck. Issue #4884"
      (do-game
        (new-game {:corp {:hand ["Slot Machine" "Ice Wall"]}
                   :runner {:deck ["Harbinger" "Aesop's Pawnshop"]
                            :hand ["Sure Gamble"]}})
        (play-from-hand state :corp "Slot Machine" "HQ")
        (take-credits state :corp)
        (run-on state :hq)
        (let [sm (get-ice state :hq 0)]
          (rez state :corp sm)
          (run-continue state)
          (is (changed? [(:credit (get-corp)) 0]
                (card-subroutine state :corp sm 1))
              "Corp does not gain any credits when runner has 2 cards with different types in deck"))))
    (testing "Enough cards in deck"
      (do-game
        (new-game {:corp {:hand ["Slot Machine" "Ice Wall"]}
                   :runner {:deck [(qty "Sure Gamble" 10)]}})
        (play-from-hand state :corp "Slot Machine" "HQ")
        (take-credits state :corp)
        (run-on state :hq)
        (let [sm (get-ice state :hq 0)]
          (rez state :corp sm)
          (run-continue state)
          (is (changed? [(:credit (get-corp)) 3]
                (card-subroutine state :corp sm 1))
              "Corp does not gain any credits when runner has 1 or less cards in deck")))))
  (testing "Subroutine 3 should only fire when there are at least 2 cards in deck"
    (testing "Only 1 card in deck"
      (do-game
        (new-game {:corp {:hand ["Slot Machine" "Ice Wall"]}
                   :runner {:deck ["Sure Gamble"]
                            :hand ["Sure Gamble"]}})
        (play-from-hand state :corp "Slot Machine" "HQ")
        (take-credits state :corp)
        (run-on state :hq)
        (let [sm (get-ice state :hq 0)]
          (rez state :corp sm)
          (run-continue state)
          (card-subroutine state :corp sm 2)
          (is (no-prompt? state :corp) "No target prompt as effect didn't happen"))))
    (testing "Enough cards in deck"
      (do-game
        (new-game {:corp {:hand ["Slot Machine" "Ice Wall"]}
                   :runner {:deck [(qty "Sure Gamble" 10)]}})
        (play-from-hand state :corp "Ice Wall" "R&D")
        (play-from-hand state :corp "Slot Machine" "HQ")
        (take-credits state :corp)
        (run-on state :hq)
        (let [iw (get-ice state :rd 0)
              sm (get-ice state :hq 0)]
          (rez state :corp sm)
          (run-continue state)
          (is (changed? [(get-counters (refresh iw) :advancement) 3]
                (card-subroutine state :corp sm 2)
                (click-card state :corp iw))
              "Corp does not gain any credits when runner has 1 or less cards in deck"))))))

(deftest snowflake
  ;; Snowflake - Win a psi game to end the run
  (do-game
    (new-game {:corp {:deck ["Snowflake"]}})
    (play-from-hand state :corp "Snowflake" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [sf (get-ice state :hq 0)]
      (rez state :corp sf)
      (run-continue state)
      (card-subroutine state :corp sf 0)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (:run @state) "Runner won psi, run continues")
      (card-subroutine state :corp sf 0)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (not (:run @state)) "Run ended"))))

(deftest sorocaban-blade
  (do-game
    (new-game {:corp {:hand ["Sorocaban Blade"]}
               :runner {:hand ["Smartware Distributor" "Marjanah" "Simulchip"]}})
    (play-from-hand state :corp "Sorocaban Blade" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Smartware Distributor")
    (play-from-hand state :runner "Marjanah")
    (play-from-hand state :runner "Simulchip")
    (run-on state "HQ")
    (let [sb (get-ice state :hq 0)]
      (rez state :corp sb)
      (run-continue state)
      (fire-subs state sb)
      (is (changed? [(count (:discard (get-runner))) 0]
                    (click-prompt state :corp "Done"))
          "Trashed no resource")
      (is (changed? [(count (:discard (get-runner))) 1]
                    (click-card state :corp (get-hardware state 0)))
          "Trashed 1 hardware")
      (is (no-prompt? state :corp) "No more Sorocaban Blade prompts")
      )))

(deftest special-offer
  ;; Special Offer trashes itself and updates the run position
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Special Offer"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Special Offer" "HQ")
    (take-credits state :corp 1)
    (run-on state "HQ")
    (is (= 2 (:position (get-in @state [:run]))) "Initial position approaching Special Offer")
    (let [special (get-ice state :hq 1)]
      (rez state :corp special)
      (run-continue state)
      (is (= 4 (:credit (get-corp))))
      (card-subroutine state :corp special 0)
      (is (= 9 (:credit (get-corp))) "Special Offer paid 5 credits")
      (is (= 1 (:position (get-in @state [:run])))
          "Run position updated; now approaching Ice Wall"))))

(deftest starlit-knight
  (do-game
    (new-game {:corp {:hand ["Starlit Knight" "Vanity Project"]
                      :credits 20}})
    (play-from-hand state :corp "Starlit Knight" "HQ")
    (take-credits state :corp)
    (let [sk (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp sk)
      (run-continue state)
      (is (changed? [(count-tags state) 2]
            (fire-subs state (refresh sk)))
          "Runner got 2 tags")
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (play-and-score state "Vanity Project")
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (is (= ["Give the Runner 1 tag"
              "Give the Runner 1 tag"
              "End the run"
              "End the run"]
             (map :label (:subroutines (refresh sk)))))
      (is (changed? [(count-tags state) 2]
            (fire-subs state (refresh sk)))
          "Runner got 2 more tags")
      (is (not (:run @state)) "Run ended"))))

(deftest starlit-knight-acme
  (do-game
    (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                      :hand ["Starlit Knight" "Vanity Project"]
                      :credits 20}})
    (play-from-hand state :corp "Starlit Knight" "HQ")
    (let [sk (get-ice state :hq 0)]
      (rez state :corp sk)
      (play-and-score state "Vanity Project")
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (is (= ["Give the Runner 1 tag"
              "Give the Runner 1 tag"
              "End the run"]
             (map :label (:subroutines (refresh sk))))))))

(deftest stavka
  (do-game
   (new-game {:corp {:hand ["Stavka" "Prisec"] :credits 10}
              :runner {:hand ["Rezeki" "Rezeki"]}})
   (play-from-hand state :corp "Stavka" "HQ")
   (play-from-hand state :corp "Prisec" "HQ")
   (take-credits state :corp)
   (play-from-hand state :runner "Rezeki")
   (play-from-hand state :runner "Rezeki")
   (run-on state :hq)
   (let [sta (get-ice state :hq 0)]
     (rez state :corp sta)
     (is (changed? [(get-strength (refresh sta)) 5]
           (click-prompt state :corp "Yes")
           (click-card state :corp "Prisec"))
         "Stavka gains 5str")
     (run-continue state)
     (fire-subs state (refresh sta))
     (click-card state :corp (get-program state 0))
     (click-card state :corp (get-program state 0)))
   (is (= 2 (count (:discard (get-runner)))) "Both rezekis trashed")
   (is (= 1 (count (:discard (get-corp)))) "Prisec trashed")))

(deftest stavka-outside-of-run
  ;; Stavka should not gain strength if rez ability triggered outside of run
  (do-game
   (new-game {:corp {:hand ["Stavka" "Prisec"]}})
   (play-from-hand state :corp "Stavka" "HQ")
   (play-from-hand state :corp "Prisec" "HQ")
   (let [sta (get-ice state :hq 0)]
     (rez state :corp sta)
     (is (changed? [(get-strength (refresh sta)) 0]
           (click-prompt state :corp "Yes")
           (click-card state :corp "Prisec"))
         "Stavka gains no strength outside of a run"))))

(deftest surveyor
  ;; Surveyor ice strength
  (do-game
    (new-game {:corp {:deck [(qty "Surveyor" 1) (qty "Ice Wall" 2)]}})
    (core/gain state :corp :credit 10)
    (core/gain state :runner :credit 10)
    (play-from-hand state :corp "Surveyor" "HQ")
    (let [surv (get-ice state :hq 0)]
      (rez state :corp surv)
      (is (= 2 (get-strength (refresh surv))) "Surveyor has 2 strength for itself")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (is (= 4 (get-strength (refresh surv))) "Surveyor has 4 strength for 2 pieces of ice")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (is (= 6 (get-strength (refresh surv))) "Surveyor has 6 strength for 3 pieces of ice")
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue-until state :encounter-ice surv)
      (card-subroutine state :corp surv 0)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 6 (:base (prompt-map :corp))) "Trace is base 6")
      (click-prompt state :corp "0")
      (click-prompt state :runner "5")
      (is (= 2 (count-tags state)) "Runner took 2 tags from Surveyor Trace 6 with boost 5")
      (card-subroutine state :corp surv 0)
      (is (= :trace (prompt-type :corp)) "Trace is initiated")
      (is (= 6 (:base (prompt-map :corp))) "Trace is base 6")
      (click-prompt state :corp "0")
      (click-prompt state :runner "6")
      (is (= 2 (count-tags state)) "Runner did not take tags from Surveyor Trace 6 with boost 6")
      (core/process-action "move" state :corp {:card (get-ice state :hq 1) :server "Archives"})
      (is (= 4 (get-strength (refresh surv))) "Surveyor has 4 strength for 2 pieces of ice"))))

(deftest surveyor-x-fn-test
  (do-game
    (new-game {:corp {:deck ["Surveyor"]}})
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "Surveyor" "HQ")
    (let [surv (get-ice state :hq 0)]
      (rez state :corp surv)
      (is (= 2 (get-strength (refresh surv))) "Surveyor has 2 strength for itself")
      (core/disable-card state :corp (refresh surv))
      (core/fake-checkpoint state)
      (is (= 0 (get-strength (refresh surv))) "Surveyor has 0 strength while disabled")
      (core/enable-card state :corp (refresh surv))
      (core/fake-checkpoint state)
      (is (= 2 (get-strength (refresh surv))) "Surveyor has 2 strength again"))))

(deftest susanoo-no-mikoto
  ;;Susanoo-no-Mikoto
  (do-game
    (new-game {:corp {:deck ["Susanoo-no-Mikoto" "Cortex Lock" "Anansi"]
                      :credits 20}
               :runner {:deck [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Anansi" "Archives")
    (play-from-hand state :corp "Cortex Lock" "Archives")
    (play-from-hand state :corp "Susanoo-no-Mikoto" "HQ")
    (take-credits state :corp)
    (let [susanoo (get-ice state :hq 0)
          cl (get-ice state :archives 1)]
      (run-on state "HQ")
      (rez state :corp susanoo)
      (run-continue state)
      (fire-subs state susanoo)
      (is (= [:archives] (get-in @state [:run :server])) "Deflected to archives")
      (rez state :corp cl)
      (run-continue-until state :encounter-ice cl)
      (fire-subs state cl)
      (run-continue state :movement)
      (run-jack-out state)
      (is (not (:run @state)) "Runner can jack out"))))

(deftest susanoo-no-mikoto-redirection-does-not-occur-during-a-forced-encounter
  ;; Redirection does not occur during a forced encounter
  (do-game
    (new-game {:corp {:deck ["Susanoo-no-Mikoto" "Konjin" "Cortex Lock"]
                      :credits 20}
               :runner {:deck [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Cortex Lock" "R&D")
    (play-from-hand state :corp "Konjin" "R&D")
    (play-from-hand state :corp "Susanoo-no-Mikoto" "HQ")
    (take-credits state :corp)
    (let [susanoo (get-ice state :hq 0)
          konjin (get-ice state :rd 1)
          cl (get-ice state :rd 0)]
      (run-on state "R&D")
      (rez state :corp susanoo)
      (rez state :corp konjin)
      (run-continue state)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (click-card state :corp susanoo)
      (fire-subs state susanoo)
      (is (= [:rd] (get-in @state [:run :server])) "Run still on R&D")
      (run-continue state :encounter-ice)
      (rez state :corp cl)
      (run-continue-until state :encounter-ice cl)
      (is (not (get-in @state [:run :cannot-jack-out])) "Runner can jack out again"))))

(deftest swarm-variable-subroutines-update
  ;; Variable subroutines update
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Swarm"]
                      :credits 10}})
    (play-from-hand state :corp "Swarm" "HQ")
    (let [swarm (get-ice state :hq 0)]
      (rez state :corp swarm)
      (is (zero? (count (:subroutines (refresh swarm)))) "Swarm starts with 0 subs")
      (advance state swarm 2)
      (is (= 2 (count (:subroutines (refresh swarm)))) "Swarm gains 2 subs"))))

(deftest swarm-subroutine-is-correct-4608
  ;; Subroutine is correct #4608
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Swarm"]
                      :credits 10}
               :runner {:hand ["Corroder"]}})
    (play-from-hand state :corp "Swarm" "HQ")
    (let [swarm (get-ice state :hq 0)]
      (advance state swarm 2)
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (run-on state "HQ")
      (rez state :corp (refresh swarm))
      (run-continue state)
      (fire-subs state (refresh swarm))
      (is (= ["The Corp trashes a program" "Pay 3 [Credits]"] (prompt-buttons :runner)))
      (click-prompt state :runner "The Corp trashes a program")
      (click-card state :corp "Corroder")
      (is (find-card "Corroder" (:discard (get-runner))))
      (is (changed? [(:credit (get-runner)) -3]
            (click-prompt state :runner "Pay 3 [Credits]"))
          "Costs 3"))))

(deftest swordsman-can-t-be-broken-with-ai
  ;; Can't be broken with AI
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Swordsman"]}
               :runner {:hand ["Alpha" "Faerie"]
                        :credits 15}})
    (play-from-hand state :corp "Swordsman" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Alpha")
    (play-from-hand state :runner "Faerie")
    (run-on state "HQ")
    (let [swordsman (get-ice state :hq 0)
          alpha (get-program state 0)
          faerie (get-program state 1)]
      (rez state :corp swordsman)
      (run-continue state)
      (card-ability state :runner alpha "Add 1 strength")
      (card-ability state :runner alpha "Break 1 subroutine")
      (is (no-prompt? state :runner) "Alpha can't break so no prompt")
      (card-ability state :runner faerie "Break 1 Sentry subroutine")
      (is (= "Break a subroutine" (:msg (prompt-map :runner)))
          "Runner has Faerie break prompt"))))

(deftest swordsman-first-subroutine-trashes-ai-programs
  ;; First subroutine trashes AI programs
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Swordsman"]}
               :runner {:hand ["Alpha" "Faerie"]
                        :credits 15}})
    (play-from-hand state :corp "Swordsman" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Alpha")
    (play-from-hand state :runner "Faerie")
    (run-on state "HQ")
    (let [swordsman (get-ice state :hq 0)
          alpha (get-program state 0)
          faerie (get-program state 1)]
      (rez state :corp swordsman)
      (run-continue state)
      (fire-subs state swordsman)
      (is (= :select (prompt-type :corp)) "Swordsman subroutine to choose an AI")
      (click-card state :corp faerie)
      (is (refresh faerie) "Faerie isn't an AI so isn't trashed")
      (click-card state :corp alpha)
      (is (not (refresh alpha)) "Alpha is trashed because it's an AI")
      (is (= "Alpha" (:title (first (:discard (get-runner)))))
          "Alpha is trashed because it's an AI"))))

(deftest swordsman-innate-ability-can-t-be-copied
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Loki" "Swordsman"]
                      :credits 100}
               :runner {:hand ["Alpha"]
                        :credits 100}})
    (play-from-hand state :corp "Loki" "R&D")
    (play-from-hand state :corp "Swordsman" "HQ")
    (rez state :corp (get-ice state :rd 0))
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Alpha")
    (run-on state "R&D")
    (run-continue state :encounter-ice)
    (click-card state :corp (get-ice state :hq 0))
    (let [alpha (get-program state 0)]
      (card-ability state :runner alpha 1)
      (card-ability state :runner alpha 1)
      (card-ability state :runner alpha 0)
      (is (= ["Trash an AI program"
              "Do 1 net damage"
              "End the run unless the Runner shuffles the grip into the stack"
              "Done"]
             (prompt-buttons :runner))))))

(deftest syailendra-wonder-encounter
  (do-game
    (run-and-encounter-ice-test "Syailendra" nil {:counters [[:advancement 3]]})
    (click-card state :corp "Syailendra")
    (is (= 4 (get-counters (get-ice state :hq 0) :advancement)) "Placed 1 more")))

(deftest syailendra-subs-test
  (do-game
    (subroutine-test "Syailendra" 0)
    (click-card state :corp (get-ice state :hq 0))
    (is (= 1 (get-counters (get-ice state :hq 0) :advancement)) "Placed 1"))
  (do-game
    (subroutine-test "Syailendra" 1 {:runner {:credits (inc 2)}})
    (is (= 1 (:credit (get-runner))) "lost 2 credits"))
  (do-game (does-damage-sub "Syailendra" 2 1)))

(deftest tatu-bola
  (do-game
    (new-game {:corp {:hand ["Tatu-Bola" "Guard"]}})
    (play-from-hand state :corp "Tatu-Bola" "Archives")
    (take-credits state :corp)
    (run-on state "Archives")
    (rez state :corp (get-ice state :archives 0))
    (run-continue state)
    (run-continue state :movement)
    (is (changed? [(:credit (get-corp)) 4]
          (click-prompt state :corp "Yes")
          (click-prompt state :corp "Guard"))
        "Corp gained 4 credits")
    (is (= "Tatu-Bola" (-> (get-corp) :hand first :title)))
    (is (= "Guard" (:title (get-ice state :archives 0))))))

(deftest tatu-bola-ice-wall
  (do-game
    (new-game {:corp {:hand ["Tatu-Bola" "Ice Wall"]}})
    (play-from-hand state :corp "Tatu-Bola" "Archives")
    (take-credits state :corp)
    (run-on state "Archives")
    (rez state :corp (get-ice state :archives 0))
    (run-continue state)
    (run-continue state :movement)
    (is (changed? [(:credit (get-corp)) 4]
          (click-prompt state :corp "Yes")
          (click-prompt state :corp "Ice Wall"))
        "Corp gained 4 credits")
    (is (= "Tatu-Bola" (-> (get-corp) :hand first :title)))
    (is (= "Ice Wall" (:title (get-ice state :archives 0))))
    (run-continue-until state :success)
    (take-credits state :runner)
    (click-advance state :corp (get-ice state :archives 0))
    (is (= 1 (get-counters (get-ice state :archives 0) :advancement)) "was able to adv ice wall")))

(deftest tatu-bola-fake-prompt
  (do-game
    (new-game {:corp {:hand ["Tatu-Bola" "Hedge Fund"]}})
    (play-from-hand state :corp "Tatu-Bola" "Archives")
    (take-credits state :corp)
    (run-on state "Archives")
    (rez state :corp (get-ice state :archives 0))
    (run-continue state)
    (run-continue state)
    (is (not (last-log-contains? state "decline to install")))
    (is (changed? [(:credit (get-corp)) 0]
          (click-prompt state :corp "OK")
          (is (last-log-contains? state "decline to install")))
        "Corp gained no credits")))

(deftest tatu-bola-swaps-correct-ice-when-swapped
  (do-game
    (new-game {:corp {:hand ["Tatu-Bola" "Vanilla" "Ice Wall"]}
               :runner {:hand ["Inversificator"]
                        :credits 10
                        :id "Rielle \"Kit\" Peddler: Transhuman"}})
    (play-from-hand state :corp "Tatu-Bola" "HQ")
    (play-from-hand state :corp "Vanilla" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Inversificator")
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 0))
    (run-continue state :encounter-ice)
    (card-ability state :runner (get-program state 0) 0)
    (click-prompt state :runner "End the run")
    (run-continue state :movement)
    (click-prompt state :runner "Yes")
    (click-card state :runner "Vanilla")
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "Ice Wall")
    (is (= "Ice Wall" (:title (get-ice state :rd 0))) "Ice wall on R&D")
    (is (= "Vanilla" (:title (get-ice state :hq 0))) "Vanilla on HQ")
    (is (= ["Tatu-Bola"] (map :title (:hand (get-corp)))) "Tatu bola in HQ")))

(deftest thimblerig-thimblerig-does-not-open-a-prompt-if-it-s-the-only-piece-of-ice
  ;; Thimblerig does not open a prompt if it's the only piece of ice
  (do-game
    (new-game {:corp {:deck ["Thimblerig" "Guard"]}})
    (play-from-hand state :corp "Thimblerig" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (take-credits state :runner)
    (is (not (prompt-map :corp)) "Corp doesn't have a prompt to use Thimblerig")
    (play-from-hand state :corp "Guard" "New remote")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (prompt-map :corp) "Corp has a prompt to use Thimblerig because there are 2 cards")))

(deftest thimblerig-swap-ability-at-the-start-of-turn
  ;; Swap ability at the start of turn
  (do-game
    (new-game {:corp {:deck ["Pup" "Thimblerig"]}})
    (play-from-hand state :corp "Thimblerig" "HQ")
    (play-from-hand state :corp "Pup" "HQ")
    (let [thimble (get-ice state :hq 0)
          pup (get-ice state :hq 1)]
      (rez state :corp thimble)
      (rez state :corp pup)
      (is (= "Thimblerig" (:title (get-ice state :hq 0))) "Thimblerig innermost ice on HQ")
      (is (= "Pup" (:title (get-ice state :hq 1))) "Pup outermost ice on HQ")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= "Swap Thimblerig protecting HQ at position 0 with another ice?" (:msg (prompt-map :corp))))
      (click-prompt state :corp "Yes")
      (click-card state :corp (refresh pup))
      (is (= "Pup" (:title (get-ice state :hq 0))) "Pup innermost ice on HQ after swap")
      (is (= "Thimblerig" (:title (get-ice state :hq 1))) "Thimblerig outermost ice on HQ after swap"))))

(deftest thimblerig-swap-ability-on-runner-pass
  ;; Swap ability on runner pass
  (do-game
    (new-game {:corp {:deck ["Vanilla" "Thimblerig"]}})
    (play-from-hand state :corp "Thimblerig" "HQ")
    (play-from-hand state :corp "Vanilla" "New remote")
    (take-credits state :corp)
    (let [thimble (get-ice state :hq 0)
          vanilla (get-ice state :remote1 0)]
      (run-on state "HQ")
      (rez state :corp thimble)
      (run-continue state)
      (run-continue state)
      (click-prompt state :corp "Yes")
      (is (= "Thimblerig" (:title (get-ice state :hq 0))) "Thimblerig outermost ice on HQ")
      (is (= "Vanilla" (:title (get-ice state :remote1 0))) "Vanilla ice on remote")
      (click-card state :corp vanilla)
      (is (= "Vanilla" (:title (get-ice state :hq 0))) "Vanilla outermost ice on HQ after swap during run")
      (is (= "Thimblerig" (:title (get-ice state :remote1 0))) "Thimblerig ice on remote after swap during run"))))

(deftest thimblerig-swap-doesnt-mash-events
  ;; Swap ability on runner pass
  (do-game
    (new-game {:corp {:deck ["Vanilla" "Thimblerig"]}
               :runner {:hand ["Sure Gamble" "Inversificator"]}})
    (play-from-hand state :corp "Thimblerig" "HQ")
    (play-from-hand state :corp "Vanilla" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Inversificator")
    (let [thimble (get-ice state :hq 0)
          vanilla (get-ice state :remote1 0)
          inv (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp thimble)
      (run-continue state)
      (card-ability state :runner (refresh inv) 0)
      (click-prompt state :runner "End the run")
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-card state :runner (get-ice state :remote1 0))
      (click-prompt state :corp "Yes")
      (is (= "Thimblerig" (:title (get-ice state :remote1 0))) "Thimblerig outermost ice on HQ")
      (is (= "Vanilla" (:title (get-ice state :hq 0))) "Vanilla ice on remote")
      (click-card state :corp (get-ice state :hq 0))
      (is (= "Vanilla" (:title (get-ice state :remote1 0))) "Vanilla outermost ice on HQ after swap during run")
      (is (= "Thimblerig" (:title (get-ice state :hq 0))) "Thimblerig ice on remote after swap during run"))))

(deftest thimblerig-swapping-doesnt-lose-prompts
  ;; Swap ability on runner pass
  (do-game
    (new-game {:corp {:deck [(qty "Thimblerig" 2)]}})
    (play-from-hand state :corp "Thimblerig" "HQ")
    (play-from-hand state :corp "Thimblerig" "R&D")
    (take-credits state :corp)
    (let [thimble1 (get-ice state :hq 0)
          thimble2 (get-ice state :rd 0)]
      (rez state :corp thimble1)
      (rez state :corp thimble2)
      (take-credits state :runner)
      (click-prompt state :corp "Yes")
      (click-card state :corp thimble2)
      (click-prompt state :corp "Yes"))))

(deftest thimblerig-swapping-issues-related-to-trashing-5197
  ;; Swapping issues related to trashing #5197
  (do-game
    (new-game {:corp {:hand ["Drafter" "Border Control" "Vanilla" "Thimblerig"]
                      :credits 100}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Border Control" "R&D")
    (play-from-hand state :corp "Drafter" "R&D")
    (play-from-hand state :corp "Thimblerig" "HQ")
    (play-from-hand state :corp "Vanilla" "HQ")
    (doseq [ice (concat (get-ice state :rd) (get-ice state :hq))]
      (rez state :corp ice))
    (take-credits state :corp)
    (run-on state "R&D")
    (run-continue-until state :encounter-ice (get-ice state :rd 0))
    (card-ability state :corp (get-ice state :rd 0) 0)
    (run-on state "HQ")
    (run-continue-until state :encounter-ice (get-ice state :hq 0)) ;; encounter Thimblerig
    (run-continue state :movement)
    (click-prompt state :corp "Yes")
    (click-card state :corp "Drafter")
    (run-jack-out state)
    (is (= ["Thimblerig"] (map :title (get-ice state :rd))))
    (is (= ["Drafter" "Vanilla"] (map :title (get-ice state :hq))))))

(deftest thimblerig-duplication-bug
  ;; Duplication bug
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 3)]
                      :hand ["Thimblerig" "Ice Wall" "Vanilla" "Hostile Takeover"]
                      :credits 10}
               :runner {:id "Leela Patel: Trained Pragmatist"
                        :hand ["Gordian Blade"]
                        :credits 10}})
    (core/gain state :corp :click 3)
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Thimblerig" "HQ")
    (rez state :corp (get-ice state :hq 2))
    (play-and-score state "Hostile Takeover")
    (click-card state :runner "Vanilla")
    (take-credits state :corp)
    (play-from-hand state :runner "Gordian Blade")
    (run-on state "HQ")
    (run-continue state)
    (card-ability state :runner (get-program state 0) 0)
    (click-prompt state :runner "End the run")
    (run-continue state)
    (click-prompt state :corp "Yes")
    (click-card state :corp "Ice Wall")
    (is (= ["Thimblerig" "Ice Wall"] (map :title (get-ice state :hq))))))

(deftest tithe
  ;; Tithe
  (do-game
    (new-game {:corp {:hand ["Tithe"]}
               :runner {:hand ["Sure Gamble"]}})
    (play-from-hand state :corp "Tithe" "HQ")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))) "Gained 2 credits")
    (rez state :corp (get-ice state :hq 0))
    (is (= 6 (:credit (get-corp))) "Cost 1 to rez")
    (run-on state "HQ")
    (run-continue state)
    (is (= 0 (count (:discard (get-runner)))) "heap empty")
    (fire-subs state (get-ice state :hq 0))
    (is (= 1 (count (:discard (get-runner)))) "Runner took net damage")
    (is (= 7 (:credit (get-corp))) "Gained 1 credit")))

(deftest tithonium
  ;; Tithonium - Forfeit option as rez cost, can have hosted condition counters
  (do-game
    (new-game {:corp {:deck ["Hostile Takeover" "Tithonium" "Patch"]}
               :runner {:deck ["Wasteland"]}})
    (core/gain state :corp :click 10)
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (play-from-hand state :corp "Tithonium" "HQ")
    (let [ht (get-content state :remote1 0)
          ti (get-ice state :hq 0)]
      (score-agenda state :corp ht)
      (is (= 1 (count (:scored (get-corp)))) "Agenda scored")
      (is (= 12 (:credit (get-corp))) "Gained 7 credits")
      (rez state :corp ti {:expect-rez false})
      (click-prompt state :corp "No") ; don't use alternative cost
      (is (= 3 (:credit (get-corp))) "Spent 9 to Rez")
      (derez state :corp (refresh ti))
      (rez state :corp ti {:expect-rez false})
      (click-prompt state :corp "Yes") ; use alternative cost
      (click-card state :corp "Hostile Takeover")
      (is (= 3 (:credit (get-corp))) "Still on 3c")
      (is (zero? (count (:scored (get-corp)))) "Agenda forfeited")
      (is (rezzed? (refresh ti)) "Tithonium is rezzed")
      ;; Can Host Conditions Counters
      (play-from-hand state :corp "Patch")
      (click-card state :corp (refresh ti))
      (is (= 1 (count (:hosted (refresh ti)))) "1 card on Tithonium")
      (take-credits state :corp)
      (derez state :corp (refresh ti))
      (is (= 1 (count (:hosted (refresh ti)))) "1 card on Tithonium")
      (play-from-hand state :runner "Wasteland")
      (let [wast (get-resource state 0)]
        (run-on state "HQ")
        (core/gain state :corp :credit 9)
        (rez state :corp (refresh ti))
        (run-continue state)
        (card-subroutine state :corp ti 2)
        (click-card state :corp (refresh wast))
        (is (= 1 (count (:discard (get-runner)))) "1 card trashed")
        (is (not (:run @state)) "Run ended")
        (run-on state "HQ")
        (run-continue state)
        (card-subroutine state :corp ti 2)
        (is (not (:run @state)) "Run ended")))))

(deftest tithonium-oversight-ai-does-not-prompt-for-alt-cost-2734
  ;; Oversight AI does not prompt for alt cost #2734
  (do-game
    (new-game {:corp {:deck ["Hostile Takeover" "Oversight AI" "Tithonium"]}})
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (play-from-hand state :corp "Tithonium" "R&D")
    (let [ht (get-content state :remote1 0)
          ti (get-ice state :rd 0)]
      (score-agenda state :corp ht)
      (play-from-hand state :corp "Oversight AI")
      (click-card state :corp ti)
      (is (rezzed? (refresh ti)))
      (is (= "Oversight AI" (get-title (first (:hosted (refresh ti)))))
          "Tithonium hosting OAI as a condition"))))

(deftest tithonium-hosted-pawn-is-trashed
  ;; Hosted Pawn is trashed
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand ["Tithonium"]
                      :credits 100}
               :runner {:hand ["Pawn"]}})
    (play-from-hand state :corp "Tithonium" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Pawn")
    (card-ability state :runner (get-program state 0) 0)
    (click-card state :runner "Tithonium")
    (let [ti (get-ice state :rd 0)]
      (rez state :corp ti)
      (is (last-log-contains? state "Corp trashes Pawn hosted on Tithonium"))
      (is (empty? (:hosted (refresh ti)))))))

(deftest tl-dr
  ;; TL;DR
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Enigma" "TL;DR"]
                      :credits 20}})
    (play-from-hand state :corp "Enigma" "HQ")
    (play-from-hand state :corp "TL;DR" "HQ")
    (take-credits state :corp)
    (let [e (get-ice state :hq 0)
          tldr (get-ice state :hq 1)]
      (run-on state :hq)
      (rez state :corp e)
      (rez state :corp tldr)
      (run-continue state)
      (is (= 2 (count (:subroutines (refresh e)))) "Enigma starts with 2 subroutines")
      (fire-subs state tldr)
      (run-continue-until state :encounter-ice e)
      (is (= 4 (count (:subroutines (refresh e)))) "Enigma has 4 subroutines after TLDR doubles them")
      (run-continue state :movement)
      (is (= 2 (count (:subroutines (refresh e)))) "Enigma starts with 2 subroutines"))))

(deftest tmi
  ;; TMI
  (do-game
    (new-game {:corp {:deck ["TMI"]}})
    (play-from-hand state :corp "TMI" "HQ")
    (let [tmi (get-ice state :hq 0)]
      (rez state :corp tmi)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (rezzed? (refresh tmi))))))

(deftest tmi-losing-trace-derezzes-tmi
  ;; Losing trace derezzes TMI
  (do-game
    (new-game {:corp {:deck ["TMI"]}
               :runner {:deck [(qty "Blackmail" 3)]}})
    (play-from-hand state :corp "TMI" "HQ")
    (let [tmi (get-ice state :hq 0)]
      (rez state :corp tmi)
      (click-prompt state :corp "0")
      (click-prompt state :runner "2")
      (is (not (rezzed? (refresh tmi)))))))

(deftest tour-guide-rez-before-other-assets
  ;; Rez before other assets
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Tour Guide" (qty "NGO Front" 3)]
                      :credits 10}})
    (core/gain state :corp :click 10)
    (play-from-hand state :corp "Tour Guide" "HQ")
    (play-from-hand state :corp "NGO Front" "New remote")
    (play-from-hand state :corp "NGO Front" "New remote")
    (play-from-hand state :corp "NGO Front" "New remote")
    (let [tg (get-ice state :hq 0)]
      (rez state :corp tg)
      (is (zero? (count (:subroutines (refresh tg)))) "Tour Guide starts with 0 subs")
      (rez state :corp (get-content state :remote1 0))
      (is (= 1 (count (:subroutines (refresh tg)))) "Tour Guide gains 1 sub on asset rez")
      (rez state :corp (get-content state :remote2 0))
      (rez state :corp (get-content state :remote3 0))
      (is (= 3 (count (:subroutines (refresh tg)))) "Tour Guide has a total of 3 subs"))))

(deftest tour-guide-rez-after-other-assets
  ;; Rez after other assets
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Tour Guide" (qty "NGO Front" 3)]
                      :credits 10}})
    (core/gain state :corp :click 10)
    (play-from-hand state :corp "NGO Front" "New remote")
    (play-from-hand state :corp "NGO Front" "New remote")
    (play-from-hand state :corp "NGO Front" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (rez state :corp (get-content state :remote2 0))
    (rez state :corp (get-content state :remote3 0))
    (play-from-hand state :corp "Tour Guide" "HQ")
    (let [tg (get-ice state :hq 0)]
      (rez state :corp tg)
      (is (= 3 (count (:subroutines (refresh tg)))) "Tour Guide has a total of 3 subs"))))

(deftest tour-guide-trashing-resets-the-number
  ;; trashing resets the number
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Tour Guide" (qty "NGO Front" 3)]
                      :credits 10}})
    (core/gain state :corp :click 10)
    (play-from-hand state :corp "NGO Front" "New remote")
    (play-from-hand state :corp "NGO Front" "New remote")
    (play-from-hand state :corp "NGO Front" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (rez state :corp (get-content state :remote2 0))
    (rez state :corp (get-content state :remote3 0))
    (play-from-hand state :corp "Tour Guide" "HQ")
    (let [tg (get-ice state :hq 0)
          ngo (get-content state :remote2 0)]
      (rez state :corp tg)
      (is (= 3 (count (:subroutines (refresh tg)))) "Tour Guide has a total of 3 subs")
      (click-advance state :corp (refresh ngo))
      (click-advance state :corp (refresh ngo))
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (= 2 (count (:subroutines (refresh tg)))) "Tour Guide has a total of 2 subs")
      (card-ability state :corp (refresh ngo) 0)
      (is (= 1 (count (:subroutines (refresh tg)))) "Tour Guide has a total of 1 subs"))))

(deftest trebuchet-no-stealing-on-successful-trace
  ;; No stealing on successful trace.
  (do-game
    (new-game {:corp {:deck ["Trebuchet" "Project Atlas"]}
               :runner {:deck ["Inti"]}})
    (play-from-hand state :corp "Trebuchet" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Inti")
    (let [treb (get-ice state :hq 0)]
      (run-on state "HQ")
      (is (= 0 (count-bad-pub state)) "No BP before")
      (rez state :corp treb)
      (is (= 1 (count-bad-pub state)) "Gained 1 BP from rez")
      (run-continue state)
      (card-subroutine state :corp treb 0)
      (click-card state :corp "Inti")
      (is (= 1 (count (:discard (get-runner)))) "Inti trashed")
      (card-subroutine state :corp treb 1)
      (is (waiting? state :runner) "Runner waits for Corp to boost first")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "No action"))))

(deftest trebuchet-no-trashing-on-successful-trace
  ;; No trashing on successful trace.
  (do-game
    (new-game {:corp {:deck ["Trebuchet" "PAD Campaign"]}
               :runner {:deck ["Inti"]}})
    (play-from-hand state :corp "Trebuchet" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Inti")
    (let [treb (get-ice state :hq 0)]
      (run-on state "HQ")
      (is (= 0 (count-bad-pub state)) "No BP before")
      (rez state :corp treb)
      (is (= 1 (count-bad-pub state)) "Gained 1 BP from rez")
      (run-continue state)
      (card-subroutine state :corp treb 0)
      (click-card state :corp "Inti")
      (is (= 1 (count (:discard (get-runner)))) "Inti trashed")
      (card-subroutine state :corp treb 1)
      (is (waiting? state :runner) "Runner waits for Corp to boost first")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (run-continue state)
      (run-continue state)
      (is (= 1 (count (:choices (prompt-map :runner)))) "Only one choice in prompt")
      (click-prompt state :runner "No action")
      (is (= 0 (count (:discard (get-corp)))) "PAD Campaign didn't get trashed"))))

(deftest tree-line
  (do-game
    (new-game {:corp {:hand ["Tree Line"]}})
    (play-from-hand state :corp "Tree Line" "HQ")
    (let [tl (get-ice state :hq 0)]
      (rez state :corp tl)
      (is (changed? [(core/get-strength (refresh tl)) 1]
            (advance state tl 1))
          "Tree Line got 1 strength")
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (is (changed? [(:credit (get-corp)) 1]
            (fire-subs state (refresh tl)))
          "Corp got 1 credit")
      (is (not (:run @state)) "Run ended"))))

(deftest tree-line-expend
  (do-game
    (new-game {:corp {:hand ["Tree Line" "Enigma"]}})
    (play-from-hand state :corp "Enigma" "HQ")
    (let [enigma (get-ice state :hq 0)
          tl (first (:hand (get-corp)))]
      (expend state :corp tl)
      (is (changed? [(get-counters (refresh enigma) :advancement) 3]
            (click-card state :corp enigma))
          "Enigma got 3 advancement counters")
      (is (= 4 (:credit (get-corp))) "Expend cost was payed")
      (is (= 1 (count (:discard (get-corp)))) "Tree Line discarded as cost"))))

(deftest tributary
  (do-game
    (new-game {:corp {:hand ["Tributary" "Rime" "Enigma" "Palisade"]
                      :deck [(qty "Hedge Fund" 5)]
                      :credits 20}})
    (play-from-hand state :corp "Tributary" "HQ")
    (play-from-hand state :corp "Rime" "New remote")
    (play-from-hand state :corp "Enigma" "Archives")
    (rez state :corp (get-ice state :hq 0))
    (rez state :corp (get-ice state :archives 0))
    (take-credits state :corp)
    (run-on state "Archives")
    (click-prompt state :corp "Yes")
    (is (= 2 (count (get-in @state [:corp :servers :archives :ices]))) "2 pieces of ice protecting Archives")
    (is (= 2 (:position (get-run))) "Runner position was adjusted")
    (run-continue state)
    (let [enigma (get-ice state :archives 0)
          trib (get-ice state :archives 1)
          rime (get-ice state :remote1 1)]
      (is (changed? [(count (:deck (get-corp))) -1
                      (count (:hand (get-corp))) 0
                      (count (get-in @state [:corp :servers :remote1 :ices])) 1]
                    (card-subroutine state :corp (refresh trib) 0)
                    (click-prompt state :corp "Yes")
                    (click-card state :corp "Palisade")
                    (click-prompt state :corp "Server 1"))
          "Corp drew and installed ice on R&D")
      (is (changed? [(get-strength (refresh enigma)) 2
                     (get-strength (refresh trib)) 2
                     (get-strength (refresh enigma)) 2]
                    (card-subroutine state :corp (refresh trib) 1))
          "All ice got +2 strength"))))

(deftest troll-giving-the-runner-a-choice-on-successful-trace-shouldn-t-make-runner-pay-trace-first-5335
  ;; Giving the runner a choice on successful trace shouldn't make runner pay trace first. #5335
  (do-game
    (new-game {:corp {:deck ["Troll"]}})
    (play-from-hand state :corp "Troll" "HQ")
    (take-credits state :corp)
    (let [troll (get-ice state :hq 0)]
      (rez state :corp troll)
      (run-on state "HQ")
      (run-continue state)
      (is (waiting? state :runner) "Runner waits for Corp to boost first")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-prompt state :runner "End the run")
      (is (not (:run @state)) "Run is ended"))))

(deftest tsurugi
  ;; Tsurugi
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Tsurugi"]
                      :credits 10}})
    (play-from-hand state :corp "Tsurugi" "HQ")
    (take-credits state :corp)
    (let [tsurugi (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp tsurugi)
      (run-continue state)
      (card-subroutine state :corp tsurugi 0)
      (is (not (no-prompt? state :corp)) "Corp is prompted to pay")
      (is (waiting? state :runner)
          "Runner has prompt to wait for Corp to use Tsurugi"))))

(deftest turing-strength-boosted-when-protecting-a-remote-server
  ;; Strength boosted when protecting a remote server
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Turing" 2) "Hedge Fund"]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Turing" "HQ")
    (play-from-hand state :corp "Turing" "New remote")
    (let [t1 (get-ice state :hq 0)
          t2 (get-ice state :remote1 0)]
      (rez state :corp t1)
      (is (= 2 (get-strength (refresh t1)))
          "Turing default 2 strength over a central server")
      (rez state :corp t2)
      (is (= 5 (get-strength (refresh t2)))
          "Turing increased to 5 strength over a remote server"))))

(deftest turing-can-t-be-broken-with-ai
  ;; Can't be broken with AI
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Turing"]}
               :runner {:hand ["Alpha" "Abagnale"]
                        :credits 15}})
    (play-from-hand state :corp "Turing" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Alpha")
    (play-from-hand state :runner "Abagnale")
    (run-on state "HQ")
    (let [turing (get-ice state :hq 0)
          alpha (get-program state 0)
          abagnale (get-program state 1)]
      (rez state :corp turing)
      (run-continue state)
      (card-ability state :runner alpha "Add 1 strength")
      (card-ability state :runner alpha "Break 1 subroutine")
      (is (no-prompt? state :runner) "Alpha can't break so no prompt")
      (card-ability state :runner abagnale "Break 1 Code Gate subroutine")
      (is (= "Break a subroutine" (:msg (prompt-map :runner)))
          "Runner has Abagnale break prompt"))))

(deftest turnpike
  ;; Turnpike
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Turnpike"]}})
    (play-from-hand state :corp "Turnpike" "HQ")
    (take-credits state :corp)
    (is (changed? [(:credit (get-runner)) -1]
          (run-on state "HQ")
          (rez state :corp (get-ice state :hq 0))
          (run-continue state))
        "Runner loses 1 credit to Turnpike")))

(deftest tyr-click-gain-by-bioroid-breaking
  ;; Click gain by bioroid breaking
  (do-game
    (new-game {:corp {:deck ["Týr"]}})
    (play-from-hand state :corp "Týr" "HQ")
    (core/gain state :corp :credit 10)
    (take-credits state :corp)
    (let [tyr (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp tyr)
      (run-continue state)
      (is (= 3 (:click (get-runner))) "Runner starts with 3 clicks")
      (card-side-ability state :runner tyr 0)
      (click-prompt state :runner "Do 2 core damage")
      (click-prompt state :runner "Trash an installed Runner card. Gain 3 [Credits]")
      (click-prompt state :runner "End the run")
      (is (= 0 (:click (get-runner))) "Runner has no clicks left")
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (is (= 6 (:click (get-corp))) "Corp has 6 clicks"))))

(deftest tyrant
  ;; Tyrant
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Tyrant"]
                      :credits 10}})
    (play-from-hand state :corp "Tyrant" "HQ")
    (let [tyrant (get-ice state :hq 0)]
      (rez state :corp tyrant)
      (is (zero? (count (:subroutines (refresh tyrant)))) "Tyrant starts with 0 subs")
      (advance state tyrant 2)
      (is (= 2 (count (:subroutines (refresh tyrant)))) "Tyrant gains 2 subs"))))

(deftest unsmiling-tsarevna
  ;; Unsmiling Tsarevna
  (do-game
    (new-game {:corp {:hand [(qty "Unsmiling Tsarevna" 2)]
                      :deck [(qty "Hedge Fund" 5)]
                      :credits 20}
               :runner {:hand ["Carmen" (qty "Sure Gamble" 2)]
                        :credits 20}})
    (play-from-hand state :corp "Unsmiling Tsarevna" "R&D")
    (play-from-hand state :corp "Unsmiling Tsarevna" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Carmen")
    (let [ut-rd (get-ice state :rd 0)
          ut-hq (get-ice state :hq 0)
          carm (get-program state 0)]
      (run-on state :hq)
      (rez state :corp ut-hq)
      (is (changed? [(:credit (get-runner)) 2]
            (click-prompt state :corp "Yes"))
          "Runner gained 2 Credits")
      (run-continue state)
      (card-ability state :runner carm 0)
      (click-prompt state :runner "Do 2 net damage")
      (is (no-prompt? state :runner) "Cannot break more than 1 sub")
      (fire-subs state (refresh ut-hq))
      (is (= 2 (count (:hand (get-runner)))) "Runner took no net damage")
      (is (= 1 (count-tags state)) "Runner took 1 tag")
      (is (changed? [(count (:hand (get-corp))) 2]
            (click-prompt state :corp "Yes"))
          "Corp drew 2 cards")
      (run-continue state)
      (run-jack-out state)
      (run-on state :rd)
      (rez state :corp ut-rd)
      (is (changed? [(:credit (get-runner)) 0]
            (click-prompt state :corp "No"))
          "Runner gained no Credits")
      (run-continue state)
      (card-ability state :runner carm 0)
      (click-prompt state :runner "Do 2 net damage")
      (click-prompt state :runner "Give the Runner 1 tag")
      (click-prompt state :runner "Done"))))

(deftest unsmiling-with-extra-sub
  (doseq [order [["End the run" "Give the Runner 1 tag"]
                 ["Give the Runner 1 tag" "End the run"]]]
    (do-game
      (new-game {:corp {:hand ["Marker" "Unsmiling Tsarevna"]}
                 :runner {:hand ["Carmen"] :credits 20}})
      (play-from-hand state :corp "Unsmiling Tsarevna" "HQ")
      (play-from-hand state :corp "Marker" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Carmen")
      (run-on state :hq)
      (rez state :corp (get-ice state :hq 1))
      (run-continue-until state :encounter-ice)
      (fire-subs state (get-ice state :hq 1))
      (rez state :corp (get-ice state :hq 0))
      (click-prompt state :corp "Yes")
      (run-continue-until state :encounter-ice)
      (is (= 4 (count (:subroutines (get-ice state :hq 0)))))
      (card-ability state :runner (get-program state 0) 0)
      (doseq [i order]
        (click-prompt state :runner i))
      (is (no-prompt? state :runner) "Broke 2 subs, no more choices offered"))))

(deftest unsmiling-tsarevna-wrong-server
  (do-game
    (new-game {:corp {:hand ["Unsmiling Tsarevna"]}})
    (play-from-hand state :corp "Unsmiling Tsarevna" "HQ")
    (take-credits state :corp)
    (run-on state :rd)
    (rez state :corp (get-ice state :hq 0))
    (is (no-prompt? state :corp) "Unsmiling Tsarevna not active outside attacked server")))

(deftest unsmiling-tsarevna-outside-run
  (do-game
    (new-game {:corp {:hand ["Unsmiling Tsarevna"]}})
    (play-from-hand state :corp "Unsmiling Tsarevna" "HQ")
    (take-credits state :corp)
    (rez state :corp (get-ice state :hq 0))
    (is (no-prompt? state :corp) "Unsmiling Tsarevna not active outside run")))

(deftest unsmiling-tsarevna-multiple-encounters
  ;; Unsmiling Tsarevna - multiple encounters during the same run are still limited on subrouting breaking
  (do-game
    (new-game {:corp {:hand ["Unsmiling Tsarevna" "Vanilla" "Mumbad City Grid"]
                      :deck [(qty "Hedge Fund" 5)]
                      :credits 20}
               :runner {:hand ["Carmen" (qty "Sure Gamble" 2)]
                        :credits 20}})
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "Unsmiling Tsarevna" "HQ")
    (play-from-hand state :corp "Mumbad City Grid" "HQ")
    (rez state :corp (get-content state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Carmen")
    (let [ut (get-ice state :hq 1)
          carm (get-program state 0)]
      (run-on state :hq)
      (rez state :corp ut)
      (is (changed? [(:credit (get-runner)) 2]
            (click-prompt state :corp "Yes"))
          "Runner gained 2 Credits")
      (run-continue state)
      (card-ability state :runner carm 0)
      (click-prompt state :runner "Do 2 net damage")
      (is (no-prompt? state :runner) "Cannot break more than 1 sub")
      (fire-subs state (refresh ut))
      (is (= 2 (count (:hand (get-runner)))) "Runner took no net damage")
      (is (= 1 (count-tags state)) "Runner took 1 tag")
      (is (changed? [(count (:hand (get-corp))) 2]
            (click-prompt state :corp "Yes"))
          "Corp drew 2 cards")
      (run-continue state)
      ;; Swap Unsmiling Tsarevna with the innermost ice
      (click-card state :corp (get-ice state :hq 0))
      (run-continue state)
      (run-continue state)
      (card-ability state :runner carm 0)
      (click-prompt state :runner "Do 2 net damage")
      (is (no-prompt? state :runner) "Cannot break more than 1 sub"))))

(deftest unsmiling-tsarevna-auto-break
  ;; Unsmiling Tsarevna - limit auto break when rez ability fired
  (do-game
    (new-game {:corp {:hand [(qty "Unsmiling Tsarevna" 1)]
                      :deck [(qty "Hedge Fund" 5)]
                      :credits 20}
               :runner {:hand ["Carmen"]
                        :credits 20}})
    (play-from-hand state :corp "Unsmiling Tsarevna" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Carmen")
    (let [ut-hq (get-ice state :hq 0)
          carm (get-program state 0)]
      (run-on state :hq)
      (rez state :corp ut-hq)
      (click-prompt state :corp "Yes")
      (run-continue state)
      (is (empty? (filter #(:dynamic %) (:abilities (refresh carm)))) "No auto break dynamic ability"))))

(deftest valentao-no-choice-without-tags
  (do-game
    (new-game {:corp {:hand ["Valentão"]}})
    (play-from-hand state :corp "Valentão" "HQ")
    (is (changed? [(count-bad-pub state) 1]
          (rez state :corp (get-ice state :hq 0)))
        "Corp got 1 bad publicity")))

(deftest valentao-spend-tag
  (do-game
    (new-game {:corp {:hand [(qty "Valentão" 2)]
                      :credits 20}
               :runner {:hand ["Paparazzi"]}})
    (play-from-hand state :corp "Valentão" "HQ")
    (play-from-hand state :corp "Valentão" "R&D")
    (gain-tags state :runner 1)
    (is (changed? [(count-tags state) -1]
          (rez state :corp (get-ice state :hq 0) {:expect-rez false})
          (click-prompt state :corp "Remove 1 tag"))
        "Removed 1 tag")
    (is (zero? (count-bad-pub state)) "Got no bad publicity")
    (take-credits state :corp)
    (play-from-hand state :runner "Paparazzi")
    (is (changed? [(count-bad-pub state) 1]
          (rez state :corp (get-ice state :rd 0)))
        "Corp got 1 bad publicity even if runner is considered tagged")))

(deftest valentao-choosing-bad-pub-with-tags
  (do-game
    (new-game {:corp {:hand ["Valentão"]}})
    (play-from-hand state :corp "Valentão" "HQ")
    (gain-tags state :runner 1)
    (is (changed? [(count-bad-pub state) 1
                   (count-tags state) 0]
          (rez state :corp (get-ice state :hq 0) {:expect-rez false})
          (click-prompt state :corp "Gain 1 bad publicity"))
        "Corp gained 1 bad publicity")))

(deftest valentao-subs
  (do-game
    (new-game {:corp {:hand ["Valentão"]
                      :credits 10}})
    (play-from-hand state :corp "Valentão" "HQ")
    (take-credits state :corp)
    (let [val (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp val)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -2
             (:credit (get-corp)) 2]
            (fire-subs state (refresh val)))
          "Runner lost 2 credits")
      (is (not (:run @state)) "Run ended")
      (core/gain state :runner :credit 10)
      (run-on state :hq)
      (run-continue state)
      (fire-subs state (refresh val))
      (is (:run @state) "Run didn't end"))))

(deftest vampyronassa
  ;; Vampyronassa
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 3)]
                      :hand ["Vampyronassa"]}})
    (play-from-hand state :corp "Vampyronassa" "HQ")
    (take-credits state :corp)
    (let [vam (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp vam)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -2]
            (card-subroutine state :corp vam 0))
          "Runner loses 2 credits")
      (is (changed? [(:credit (get-corp)) 2]
            (card-subroutine state :corp vam 1))
          "Corp gains 2 credits")
      (is (changed? [(count (:hand (get-runner))) -2]
            (card-subroutine state :corp vam 2))
          "Runner suffers 2 net damage")
      (is (changed? [(count (:hand (get-corp))) 2]
            (card-subroutine state :corp vam 3)
            (click-prompt state :corp "2"))
          "Corp draws 2 cards"))))

(deftest vampyronassa-draw-no-cards
  ;; Vampyronassa - choose to skip drawing cards
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 3)]
                      :hand ["Vampyronassa"]}})
    (play-from-hand state :corp "Vampyronassa" "HQ")
    (take-credits state :corp)
    (let [vam (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp vam)
      (run-continue state)
      (is (changed? [(count (:hand (get-corp))) 0]
            (card-subroutine state :corp vam 3)
            (click-prompt state :corp "0"))
          "Corp draws no cards"))))


(deftest vasilisa
  ;; Vasilisa
  (do-game
    (new-game {:corp {:hand ["Vasilisa" "NGO Front"]}})
    (play-from-hand state :corp "Vasilisa" "HQ")
    (play-from-hand state :corp "NGO Front" "New remote")
    (take-credits state :corp)
    (run-on state :hq)
    (let [vas (get-ice state :hq 0)
          ngo (get-content state :remote1 0)]
      (rez state :corp vas)
      (run-continue state)
      (is (changed? [(:credit (get-corp)) -1]
            (click-card state :corp (refresh ngo))
            (is (= 1 (get-counters (refresh ngo) :advancement)) "ngo has a counter"))
          "paid 1c to place a token on ngo front")
      (card-subroutine state :corp (refresh vas) 0)
      (is (= 1 (count-tags state)) "Runner took 1 tag"))))

(deftest virtual-service-agent
  (do-game
    (new-game {:corp {:hand ["Virtual Service Agent"]}
               :runner {:hand ["Nanuq" "Buzzsaw"]
                        :credits 20}})
    (play-from-hand state :corp "Virtual Service Agent" "HQ")
    (take-credits state :corp)
    (let [vsa (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp vsa)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -1]
            (fire-subs state (refresh vsa)))
          "Runner lost 1 credit")
      (is (changed? [(count-tags state) 1]
            (run-continue state :movement))
          "Runner got 1 tag from not breaking any sub")
      (run-jack-out state)
      (play-from-hand state :runner "Nanuq")
      (run-on state :hq)
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "Make the Runner lose 1 [Credits]")
      (is (changed? [(count-tags state) 1]
            (run-continue state :movement))
          "Runner got 1 tag")
      (run-jack-out state)
      (play-from-hand state :runner "Buzzsaw")
      (core/gain state :runner :click 1)
      (run-on state :hq)
      (run-continue state)
      (card-ability state :runner (get-program state 1) 0)
      (click-prompt state :runner "Make the Runner lose 1 [Credits]")
      (is (changed? [(count-tags state) 0]
            (run-continue state :movement))
          "Runner got no tag"))))

(deftest virtual-service-agent-regression-other-ice
  (do-game
    (new-game {:corp {:hand ["Vanilla" "Virtual Service Agent"]}})
    (play-from-hand state :corp "Vanilla" "R&D")
    (play-from-hand state :corp "Virtual Service Agent" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (rez state :corp (get-ice state :rd 0))
    (take-credits state :corp)
    (run-on state :rd)
    (run-continue-until state :success)
    (is (zero? (count-tags state)) "Not tagged")))

(deftest waiver
  ;; Waiver - Trash Runner cards in grip with play/install cost <= trace exceed
  (do-game
    (new-game {:corp {:deck ["Waiver"]}
               :runner {:deck ["Corroder" "Dean Lister" "Ubax" "Caldera"]}})
    (play-from-hand state :corp "Waiver" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [waiv (get-ice state :hq 0)]
      (rez state :corp waiv)
      (run-continue state)
      (card-subroutine state :corp waiv 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "3")
      (is (not (find-card "Ubax" (:discard (get-runner)))) "Ubax not trashed")
      (is (not (find-card "Caldera" (:discard (get-runner)))) "Caldera not trashed")
      (is (= 2 (count (:discard (get-runner)))) "2 cards trashed"))))

(deftest wave
  ;; Wave - on rez vs. server, search R&D for an ice. Gain 1c for each rezzed harmonic ice.
  (do-game
    (new-game {:corp {:id "Hyoubu Institute: Absolute Clarity"
                      :hand ["Wave" "Wave"]
                      :deck ["Ice Wall"]
                      :credits 10}})
    (play-from-hand state :corp "Wave" "HQ")
    (play-from-hand state :corp "Wave" "R&D")
    (take-credits state :corp)
    (run-on state "HQ")
    (rez state :corp (get-ice state :rd 0))
    (is (no-prompt? state :corp) "No prompt to search from ice on another server")
    (rez state :corp (get-ice state :hq 0))
    (click-prompt state :corp "Yes")
    (is (changed? [(:credit (get-corp)) 1]
          (click-prompt state :corp "Ice Wall"))
        "gained 1c from Hyoubu Institute because of card been revealed")
    (is (find-card "Ice Wall" (:hand (get-corp))))
    (run-continue state)
    (is (changed? [(:credit (get-corp)) 2]
          (card-subroutine state :corp (get-ice state :hq 0) 0))
        "gained 2c from wave")))

(deftest wave-just-shuffle
  (do-game
    (new-game {:corp {:hand ["Wave" "Wave"]
                      :deck ["Ice Wall"]
                      :credits 10}})
    (play-from-hand state :corp "Wave" "HQ")
    (play-from-hand state :corp "Wave" "R&D")
    (take-credits state :corp)
    (run-on state "HQ")
    (rez state :corp (get-ice state :rd 0))
    (is (no-prompt? state :corp) "No prompt to search from ice on another server")
    (rez state :corp (get-ice state :hq 0))
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "Cancel")
    (is (no-prompt? state :corp))))

(deftest wendigo
  ;; Morph ice gain and lose subtypes from normal advancements and placed advancements
  (do-game
    (new-game {:corp {:deck ["Wendigo" "Shipment from SanSan" "Superior Cyberwalls"]
                      :credits 100}})
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "Superior Cyberwalls" "New remote")
    (let [sc (get-content state :remote1 0)]
      (score-agenda state :corp sc)
      (play-from-hand state :corp "Wendigo" "HQ")
      (let [wend (get-ice state :hq 0)]
        (rez state :corp wend)
        (is (= 4 (get-strength (refresh wend))) "Wendigo at normal 4 strength")
        (advance state (refresh wend) 1)
        (is (has-subtype? (refresh wend) "Barrier") "Wendigo gained Barrier")
        (is (not (has-subtype? (refresh wend) "Code Gate")) "Wendigo lost Code Gate")
        (is (= 5 (get-strength (refresh wend))) "Wendigo boosted to 5 strength by scored Superior Cyberwalls")
        (play-from-hand state :corp "Shipment from SanSan")
        (click-prompt state :corp "1")
        (click-card state :corp wend)
        (is (not (has-subtype? (refresh wend) "Barrier")) "Wendigo lost Barrier")
        (is (has-subtype? (refresh wend) "Code Gate") "Wendigo gained Code Gate")
        (is (= 4 (get-strength (refresh wend))) "Wendigo returned to normal 4 strength")))))

(deftest whirlpool-on-remote
  ;; on remote
  (do-game
    (new-game {:corp {:hand ["Whirlpool" "Ice Wall" "Border Control"]}
               :runner {:deck [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Border Control" "New remote")
    (play-from-hand state :corp "Ice Wall" "Server 1")
    (play-from-hand state :corp "Whirlpool" "Server 1")
    (take-credits state :corp)
    (let [wp (get-ice state :remote1 2)]
      (run-on state :remote1)
      (rez state :corp wp)
      (run-continue state)
      (fire-subs state wp)
      (run-jack-out state)
      (is (:run @state) "Runner cannot jack out")
      (is (nil? (refresh wp)) "Whirlpool is trashed"))))

(deftest whirlpool-on-hq
  ;; on hq
  (do-game
    (new-game {:corp {:hand ["Whirlpool" "Ice Wall" "Border Control"]}
               :runner {:deck [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Border Control" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Whirlpool" "HQ")
    (take-credits state :corp)
    (let [wp (get-ice state :hq 2)]
      (run-on state :hq)
      (rez state :corp wp)
      (run-continue state)
      (fire-subs state wp)
      (run-jack-out state)
      (is (:run @state) "Runner cannot jack out")
      (is (nil? (refresh wp)) "Whirlpool is trashed"))))

(deftest whirlpool-whirlpool-not-trashed-when-broken
  ;; whirlpool not trashed when broken
  (do-game
    (new-game {:corp {:hand ["Whirlpool" "Ice Wall" "Border Control"]}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["Aumakua"]}})
    (play-from-hand state :corp "Border Control" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Whirlpool" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Aumakua")
    (run-empty-server state :archives) ;;gain 1 virus counter
    (let [wp (get-ice state :hq 2)
          au (get-program state 0)]
      (run-on state :hq)
      (rez state :corp wp)
      (run-continue state)
      (card-ability state :runner au 0)
      (click-prompt state :runner "The Runner cannot jack out for the remainder of this run")
      (is (refresh wp) "Whirlpool not trashed"))))

(deftest whitespace
  ;; Whitespace
  (do-game
    (new-game {:corp {:hand ["Whitespace" "Hedge Fund"]}
               :runner {:hand [(qty "Sure Gamble" 2)]}})
    (play-from-hand state :corp "Whitespace" "HQ")
    (take-credits state :corp)
    (let [ws (get-ice state :hq 0)]
      (play-from-hand state :runner "Sure Gamble")
      (click-credit state :runner)
      (run-on state :hq)
      (rez state :corp ws)
      (run-continue state)
      (is (= 10 (:credit (get-runner))) "Starting with 10 credits")
      (fire-subs state ws)
      (is (= 7 (:credit (get-runner))) "Lost 3 credits")
      (is (:run @state) "Runner has sufficient credits")
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "No action")
      (run-on state :hq)
      (run-continue state)
      (is (= 7 (:credit (get-runner))) "Starting with 7 credits")
      (fire-subs state ws)
      (is (= 4 (:credit (get-runner))) "Lost 3 credits")
      (is (not (:run @state)) "Runner has insufficient credits"))))

(deftest winchester
  ;; Winchester
  (do-game
    (new-game {:corp {:deck ["Winchester"]}
               :runner {:hand ["Misdirection" "Astrolabe" "Fan Site"]}})
    (play-from-hand state :corp "Winchester" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Misdirection")
    (play-from-hand state :runner "Astrolabe")
    (play-from-hand state :runner "Fan Site")
    (run-on state :hq)
    (let [win (get-ice state :hq 0)
          misd (get-program state 0)
          astro (get-hardware state 0)
          fs (get-resource state 0)]
      (rez state :corp win)
      (run-continue state)
      (is (= 3 (count (:subroutines (refresh win)))) "Winchester has 3 subroutines on HQ")
      (fire-subs state win)
      (is (= 4 (:base (prompt-map :corp))) "Trace[4] - Trash 1 installed program")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp astro)
      (is (= 0 (count (:discard (get-runner)))) "Could not choose hardware")
      (click-card state :corp fs)
      (is (= 0 (count (:discard (get-runner)))) "Could not choose resource")
      (click-card state :corp misd)
      (is (= 1 (count (:discard (get-runner)))) "Trashed program")
      (is (= 3 (:base (prompt-map :corp))) "Trace[3] - Trash 1 installed hardware")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp fs)
      (is (= 1 (count (:discard (get-runner)))) "Could not choose resource")
      (click-card state :corp astro)
      (is (= 2 (count (:discard (get-runner)))) "Trashed hardware")
      (is (= 3 (:base (prompt-map :corp))) "Trace[3] - End the run")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (not (:run @state)) "Run has been ended"))))

(deftest winchester-2-subs-on-other-servers
  ;; 2 subs on other servers
  (do-game
    (new-game {:corp {:deck ["Winchester"]}})
    (play-from-hand state :corp "Winchester" "R&D")
    (take-credits state :corp)
    (run-on state :rd)
    (let [win (get-ice state :rd 0)]
      (rez state :corp win)
      (run-continue state)
      (is (= 2 (count (:subroutines (refresh win)))) "Winchester has 2 subroutines on R&D"))))

(deftest winchester-2-subs-when-moved-with-thimblerig
  ;; 2 subs when moved with Thimblerig
  (do-game
    (new-game {:corp {:deck ["Winchester" "Thimblerig"]}
               :runner {:deck ["Aumakua"]}})
    (play-from-hand state :corp "Winchester" "R&D")
    (play-from-hand state :corp "Thimblerig" "HQ")
    (take-credits state :corp)
    ;Click 1 - Run R&D, rez Winchester and let it fire
    (run-on state :rd)
    (let [win (get-ice state :rd 0)
          thim (get-ice state :hq 0)]
      (rez state :corp win)
      (run-continue state)
      (is (= 2 (count (:subroutines (refresh win)))) "Winchester has 2 subroutines on R&D")
      (fire-subs state win)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (run-continue state :movement)
      (run-jack-out state)
      ;Click 2 - Install Aumakua
      (play-from-hand state :runner "Aumakua")
      ;Click 3 - Run HQ, rez Thimblerig, break, swap ice
      (run-on state :hq)
      (rez state :corp thim)
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (run-continue state)
      (click-prompt state :corp "Yes")
      (click-card state :corp (refresh win))
      (run-jack-out state)
      ;Click 4 - Run HQ
      (run-on state :hq)
      (run-continue state)
      (is (= 3 (count (:subroutines (get-ice state :hq 0)))) "Winchester has 3 subroutines on HQ"))))

(deftest winchester-2-subs-when-moved-with-tao
  ;; 2 subs when moved with Tāo
  (do-game
    (new-game {:corp {:deck ["Winchester" "Thimblerig" "Merger"]
                      :credits 6}
               :runner {:id "Tāo Salonga: Telepresence Magician"}})
    (play-from-hand state :corp "Winchester" "HQ")
    (play-from-hand state :corp "Thimblerig" "R&D")
    (let [thim (get-ice state :rd 0)
          win (get-ice state :hq 0)]
      (rez state :corp thim)
      (rez state :corp win)
      (play-and-score state "Merger")
      (click-prompt state :runner "Yes")
      (click-card state :runner "Winchester")
      (click-card state :runner "Thimblerig")
      (is (= 2 (count (:subroutines (get-ice state :rd 0))))))))

(deftest woodcutter
  ;; Woodcutter
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Woodcutter"]
                      :credits 10}})
    (play-from-hand state :corp "Woodcutter" "HQ")
    (let [woodcutter (get-ice state :hq 0)]
      (rez state :corp woodcutter)
      (is (zero? (count (:subroutines (refresh woodcutter)))) "Woodcutter starts with 0 subs")
      (advance state woodcutter 2)
      (is (= 2 (count (:subroutines (refresh woodcutter)))) "Woodcutter gains 2 subs"))))

(deftest wormhole
  ;; Wormhole
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall" "Wormhole"]
                      :credits 10}})
    (play-from-hand state :corp "Ice Wall" "R&D")
    (play-from-hand state :corp "Wormhole" "HQ")
    (take-credits state :corp)
    (let [iw (get-ice state :rd 0)
          wormhole (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp wormhole)
      (run-continue state)
      (card-subroutine state :corp wormhole 0)
      (is (:fired (first (:subroutines (refresh wormhole))))
          "Subroutine fires even when there are no viable ice.")
      (is (no-prompt? state :corp) "No choice prompt for the Corp")
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state :hq)
      (rez state :corp iw)
      (run-continue state)
      (fire-subs state wormhole)
      (click-card state :corp iw)
      (click-prompt state :corp "End the run")
      (is (not (:run @state)) "Run has been ended"))))

(deftest wraparound
  ;; Wraparound - Strength boosted when no fracter is installed
  (do-game
    (new-game {:corp {:deck ["Wraparound"]}
               :runner {:deck ["Corroder"]}})
    (play-from-hand state :corp "Wraparound" "HQ")
    (let [wrap (get-ice state :hq 0)]
      (rez state :corp wrap)
      (is (= 7 (get-strength (refresh wrap)))
          "Wraparound +7 strength with no fracter in play")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (is (zero? (get-strength (refresh wrap)))
          "Wraparound 0 strength after Corroder installed"))))

(deftest zed-1.0
  ;; zed 1.0 - only does brain damage if the runner spends a click to break a sub
  (do-game
    (new-game {:corp {:hand ["Zed 1.0"]}})
    (play-from-hand state :corp "Zed 1.0" "HQ")
    (let [zed (get-ice state :hq 0)]
      (rez state :corp zed)
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (card-side-ability state :runner zed 0)
      (click-prompt state :runner "Do 1 core damage")
      (fire-subs state zed)
      (is (= 1 (:brain-damage (get-runner))) "Runner took 1 core damage")))
  (do-game
    (new-game {:corp {:hand ["Zed 1.0"]}})
    (play-from-hand state :corp "Zed 1.0" "HQ")
    (let [zed (get-ice state :hq 0)]
      (rez state :corp zed)
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (fire-subs state zed)
      (is (= 0 (:brain-damage (get-runner))) "Runner took 0 core damage"))))

(deftest zed-2.0
  ;; zed 2.0 - only does brain damage if the runner spends a click to break a sub
  (do-game
    (new-game {:corp {:hand ["Zed 2.0"] :credits 10}})
    (play-from-hand state :corp "Zed 2.0" "HQ")
    (let [zed (get-ice state :hq 0)]
      (rez state :corp zed)
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (card-side-ability state :runner zed 0)
      (click-prompt state :runner "Trash a piece of hardware")
      (click-prompt state :runner "Trash a piece of hardware")
      (fire-subs state zed)
      (is (= 2 (:brain-damage (get-runner))) "Runner took 2 core damage")))
  (do-game
    (new-game {:corp {:hand ["Zed 2.0"] :credits 10}})
    (play-from-hand state :corp "Zed 2.0" "HQ")
    (let [zed (get-ice state :hq 0)]
      (rez state :corp zed)
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (fire-subs state zed)
      (is (= 0 (:brain-damage (get-runner))) "Runner took 0 core damage"))))
