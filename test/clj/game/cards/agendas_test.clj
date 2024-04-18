(ns game.cards.agendas-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.eid :refer [make-eid]]
   [game.test-framework :refer :all]))

(deftest ^{:card-title "15-minutes"}
  fifteen-minutes
  ;; 15 Minutes - check if it works correctly from both sides
  (do-game
    (new-game {:corp {:hand ["15 Minutes"]}})
    (play-from-hand state :corp "15 Minutes" "New remote")
    (take-credits state :corp)
    ;; use 15 minutes to take it away from runner
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (is (= 1 (:agenda-point (get-runner))))
    (is (= 1 (count (:scored (get-runner)))))
    (let [fifm (first (:scored (get-runner)))]
      (is (= 3 (:click (get-corp))))
      (is (= 1 (count (:abilities (refresh fifm)))))
      (card-ability state :corp (refresh fifm) 0)
      (is (zero? (:agenda-point (get-runner))))
      (is (zero? (count (:scored (get-runner))))))
    (is (find-card "15 Minutes" (:deck (get-corp)))))
  (do-game
    (new-game {:corp {:hand ["15 Minutes"]}})
    (play-and-score state "15 Minutes")
    (is (= 1 (:agenda-point (get-corp))))
    (is (= 1 (count (:scored (get-corp)))))
    (let [fifm (first (:scored (get-corp)))]
      (is (= 1 (count (:abilities (refresh fifm)))))
      (card-ability state :corp (refresh fifm) 0)
      (is (zero? (:agenda-point (get-corp))))
      (is (zero? (count (:scored (get-corp))))))
    (is (find-card "15 Minutes" (:deck (get-corp))))))

(deftest above-the-law
  ;; Above the Law
  (do-game
     (new-game {:corp {:hand ["Above the Law"]}
                :runner {:hand ["Armitage Codebusting"]}})
     (take-credits state :corp)
     (play-from-hand state :runner "Armitage Codebusting")
     (take-credits state :runner)
     (play-and-score state "Above the Law")
     (click-card state :corp "Armitage Codebusting")
     (is (find-card "Armitage Codebusting" (:discard (get-runner)))
         "Armitage Codebusting is trashed")))

(deftest accelerated-beta-test
  ;; Accelerated Beta Test
  (do-game
    (new-game {:corp {:deck ["Enigma" (qty "Hedge Fund" 2)]
                      :hand ["Accelerated Beta Test"]}})
    ;; Set up
    (play-and-score state "Accelerated Beta Test")
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "OK")
    (is (= ["Enigma" "Cancel"] (map #(or (:title %) (identity %)) (prompt-buttons :corp))))
    (click-prompt state :corp "Enigma")
    (click-prompt state :corp "HQ")
    (is (no-prompt? state :corp))
    (is (no-prompt? state :runner))
    (is (some? (get-ice state :hq 0)))
    (is (= 2 (count (:discard (get-corp)))))
    (move state :corp (find-card "Accelerated Beta Test" (:scored (get-corp))) :hand)
    (move state :corp (find-card "Hedge Fund" (:discard (get-corp))) :deck)
    (move state :corp (find-card "Hedge Fund" (:discard (get-corp))) :deck)
    (play-and-score state "Accelerated Beta Test")
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "OK")
    (is (= ["Cancel"] (map #(or (:title %) (identity %)) (prompt-buttons :corp))))
    (click-prompt state :corp "Cancel")
    (is (= 2 (count (:discard (get-corp)))))))

(deftest advanced-concept-hopper
  ;; Advanced Concept Hopper
  (do-game
    (new-game {:corp {:deck ["Advanced Concept Hopper" (qty "Hedge Fund" 4)]}})
    (starting-hand state :corp ["Advanced Concept Hopper"])
    (play-and-score state "Advanced Concept Hopper")
    (take-credits state :corp)
    (testing "Corp draws 1 card, only once per turn"
      (let [cards (count (:hand (get-corp)))]
        (is (= cards (count (:hand (get-corp)))) (str "Corp should have " cards " cards in hand"))
        (run-on state :archives)
        (click-prompt state :corp "Draw 1 card")
        (is (= (inc cards) (count (:hand (get-corp)))) (str "Corp should have " (inc cards) " card in hand"))
        (run-continue state)
        (run-on state :archives)
        (is (no-prompt? state :corp) "No prompt as it's once per turn")
        (run-jack-out state)))
    (take-credits state :runner)
    (take-credits state :corp)
    (testing "Corp gains 1 credit, only once per turn"
      (let [credits (:credit (get-corp))]
        (is (= credits (:credit (get-corp))) (str "Corp should have " credits " credits"))
        (run-on state :archives)
        (click-prompt state :corp "Gain 1 [Credits]")
        (is (= (inc credits) (:credit (get-corp))) (str "Corp should have " (inc credits) " credits"))
        (run-continue state)
        (run-on state :archives)
        (is (no-prompt? state :corp) "No prompt as it's once per turn")))))

(deftest ancestral-imager
  ;; Ancestral Imager
  (do-game
    (new-game {:corp {:deck [(qty "Ancestral Imager" 3)]}})
    (play-and-score state "Ancestral Imager")
    (take-credits state :corp)
    (let [grip (count (:hand (get-runner)))]
      (is (= grip (count (:hand (get-runner)))) (str "Runner has " grip " cards in hand"))
      (run-on state :hq)
      (run-jack-out state)
      (is (= (dec grip) (count (:hand (get-runner)))) "Runner took 1 net damage"))))

(deftest ar-enhanced-security
  ;; AR-Enhanced Security
  (do-game
    (new-game {:corp {:deck ["AR-Enhanced Security" (qty "NGO Front" 3)]}})
    (testing "set up"
      (core/gain state :corp :click 10 :credit 10)
      (core/gain state :runner :credit 10)
      (dotimes [_ 3]
        (play-from-hand state :corp "NGO Front" "New remote"))
      (take-credits state :corp))
    (testing "don't take a tag from trashing normally"
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (= 1 (count (:discard (get-corp)))) "trashed")
      (is (zero? (count-tags state)) "Runner took 0 tags")
      (take-credits state :runner)
      (play-and-score state "AR-Enhanced Security")
      (take-credits state :corp))
    (testing "gain a tag from first trash"
      (run-empty-server state "Server 2")
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (= 2 (count (:discard (get-corp)))) "trashed")
      (is (= 1 (count-tags state)) "Runner took 1 tag"))
    (testing "don't gain a tag from second trash"
      (run-empty-server state "Server 3")
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (= 3 (count (:discard (get-corp)))) "trashed")
      (is (= 1 (count-tags state)) "Runner took 0 tags"))))

(deftest architect-deployment-test
  ;; Architect Deployment Test
  (do-game
   (new-game {:corp {:deck [(qty "Architect Deployment Test" 5) "Oaktown Renovation" "Enigma" "Rashida Jaheem"]}})
   (starting-hand state :corp (repeat 5 "Architect Deployment Test"))
   (core/gain state :corp :click 4)
   (play-and-score state "Architect Deployment Test") ;makes a remote 1
   (click-prompt state :corp "OK")
   (click-prompt state :corp "Enigma")
   (is (changed? [(:credit (get-corp)) 0]
         (click-prompt state :corp "New remote")))
   (is (faceup? (get-ice state :remote2 0)) "Enigma was installed and rezzed, both at no cost")
   (play-and-score state "Architect Deployment Test")
   (click-prompt state :corp "OK")
   (click-prompt state :corp "Cancel")
   (is (no-prompt? state :corp) "No more prompts if cancel is clicked")
   (play-and-score state "Architect Deployment Test")
   (click-prompt state :corp "OK")
   (click-prompt state :corp "Rashida Jaheem")
   (is (changed? [(:credit (get-corp)) 0]
         (click-prompt state :corp "Server 2")))
   (is (faceup? (get-content state :remote2 0)) "Rashida Jaheem was installed and rezzed, both at no cost")
   (play-and-score state "Architect Deployment Test")
   (click-prompt state :corp "OK")
   (click-prompt state :corp "Oaktown Renovation")
   (click-prompt state :corp "New remote")
   (is (= "Oaktown Renovation" (:title (get-content state :remote6 0))) "Oaktown Renovation was installed")
   (is (faceup? (get-content state :remote6 0)) "Oaktown Renovation is installed faceup.")
   (play-and-score state "Architect Deployment Test")
   (is (no-prompt? state :corp) "No prompt if R&D is empty")))

(deftest armed-intimidation
  ;; Armed Intimidation
  (do-game
    (new-game {:corp {:deck [(qty "Armed Intimidation" 2)]}
               :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 2)]}})
    (play-and-score state "Armed Intimidation")
    (click-prompt state :runner "Take 2 tags")
    (is (= 2 (count-tags state)) "Runner took 2 tags from Armed Intimidation tag choice")
    (play-and-score state "Armed Intimidation")
    (is (= 5 (count (:hand (get-runner)))) "Runner has 5 cards before Armed Intimidation meat damage")
    (click-prompt state :runner "Suffer 5 meat damage")
    (is (zero? (count (:hand (get-runner)))) "Runner has 0 cards after Armed Intimidation meat damage")))

(deftest armed-intimidation-effects-ordering-1
  ;; Armed Intimidation & Malapert Data Vault - should get order of choice
  (do-game
    (new-game {:corp {:hand ["Armed Intimidation" "Malapert Data Vault"]
                      :deck ["Hedge Fund"]}
               :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 2)]}})
    (play-from-hand state :corp "Malapert Data Vault" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Armed Intimidation" "Server 1")
    (let [ai (get-content state :remote1 1)]
      (score-agenda state :corp (refresh ai))
      (click-prompt state :corp "Armed Intimidation")
      (click-prompt state :runner "Suffer 5 meat damage")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Hedge Fund"))))

(deftest armed-intimidation-effects-ordering-2
  ;; Armed Intimidation & Malapert Data Vault - should get order of choice
  (do-game
    (new-game {:corp {:hand ["Armed Intimidation" "Malapert Data Vault"]
                      :deck ["Hedge Fund"]}
               :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 2)]}})
    (play-from-hand state :corp "Malapert Data Vault" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Armed Intimidation" "Server 1")
    (let [ai (get-content state :remote1 1)]
      (score-agenda state :corp (refresh ai))
      (click-prompt state :corp "Malapert Data Vault")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Hedge Fund")
      (click-prompt state :runner "Suffer 5 meat damage"))))

(deftest armored-servers-should-write-to-the-log
    ;; should write to the log
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Armored Servers"]}})
      (play-and-score state "Armored Servers")
      (let [as-scored (get-scored state :corp 0)]
        (is (= 1 (get-counters (refresh as-scored) :agenda)) "Should start with 1 agenda counters")
        (take-credits state :corp)
        (run-on state "HQ")
        (card-ability state :corp as-scored 0)
        (is (last-log-contains? state "make the Runner trash") "Should write to log"))))

(deftest armored-servers-when-using-an-icebreaker-that-breaks-1-sub-at-a-time
    ;; when using an icebreaker that breaks 1 sub at a time
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Armored Servers" "Ice Wall"]
                        :credits 20}
                 :runner {:hand ["Corroder" "Sure Gamble"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-and-score state "Armored Servers")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (run-on state "HQ")
      (card-ability state :corp (get-scored state :corp 0) 0)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (click-card state :runner "Sure Gamble")
      (is (find-card "Sure Gamble" (:discard (get-runner))) "Sure Gamble is now trashed")))

(deftest armored-servers-when-using-an-icebreaker-that-breaks-more-than-1-sub-at-a-time
    ;; when using an icebreaker that breaks more than 1 sub at a time
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Armored Servers" "Battlement"]
                        :credits 20}
                 :runner {:hand ["Berserker" "Sure Gamble" "Easy Mark"]
                          :credits 20}})
      (play-from-hand state :corp "Battlement" "HQ")
      (play-and-score state "Armored Servers")
      (take-credits state :corp)
      (play-from-hand state :runner "Berserker")
      (run-on state "HQ")
      (card-ability state :corp (get-scored state :corp 0) 0)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "End the run")
      (click-card state :runner "Sure Gamble")
      (click-card state :runner "Easy Mark")
      (is (find-card "Sure Gamble" (:discard (get-runner))) "Sure Gamble is now trashed")
      (is (find-card "Easy Mark" (:discard (get-runner))) "Easy Mark is now trashed")))

(deftest armored-servers-when-jacking-out
    ;; when jacking out
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Armored Servers" "Ice Wall"]}
                 :runner {:hand ["Corroder" "Sure Gamble"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-and-score state "Armored Servers")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (run-on state "HQ")
      (card-ability state :corp (get-scored state :corp 0) 0)
      (rez state :corp (get-ice state :hq 0))
      (run-continue-until state :movement)
      (run-jack-out state)
      (click-card state :runner "Sure Gamble")
      (is (find-card "Sure Gamble" (:discard (get-runner))) "Sure Gamble is now trashed")))

(deftest armored-servers-when-spending-multiple-counters
    ;; when spending multiple counters
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Armored Servers" 2) "Ice Wall"]
                        :credits 20}
                 :runner {:hand ["Corroder" "Sure Gamble" "Easy Mark"]
                          :credits 20}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-and-score state "Armored Servers")
      (play-and-score state "Armored Servers")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (run-on state "HQ")
      (card-ability state :corp (get-scored state :corp 0) 0)
      (card-ability state :corp (get-scored state :corp 1) 0)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (click-card state :runner "Sure Gamble")
      (click-card state :runner "Easy Mark")
      (is (find-card "Sure Gamble" (:discard (get-runner))) "Sure Gamble is now trashed")
      (is (find-card "Easy Mark" (:discard (get-runner))) "Easy Mark is now trashed")))

(deftest astroscript-pilot-program
  ;; AstroScript token placement
  (do-game
    (new-game {:corp {:deck [(qty "AstroScript Pilot Program" 3) (qty "Ice Wall" 2)]}})
    (core/gain state :corp :click 3)
    (letfn [(try-place [from to]
              (card-ability state :corp (refresh from) 0)
              (click-card state :corp (refresh to)))
            (should-not-place [from to msg]
              (try-place from to)
              (click-prompt state :corp "Done")
              (is (= 1 (get-counters (refresh from) :agenda))
                  (str (:title from)" token was not used on " (:title to) msg))
              (is (zero? (get-counters (refresh to) :advancement))
                  (str "Advancement token not placed on " (:title to) msg)))
            (should-place [from to msg]
              (try-place from to)
              (is (zero? (get-counters (refresh from) :agenda))
                  (str (:title from) " token was used on " (:title to) msg))
              (is (= 1 (get-counters (refresh to) :advancement))
                  (str "Advancement token placed on " (:title to) msg)))]
      (play-and-score state "AstroScript Pilot Program")
      (play-from-hand state :corp "AstroScript Pilot Program" "New remote")
      (let [scored-astro (get-scored state :corp 0)
            installed-astro (get-content state :remote2 0)
            hand-astro (find-card "AstroScript Pilot Program" (:hand (get-corp)))]
        (should-not-place scored-astro hand-astro " in hand")
        (should-place scored-astro installed-astro " that is installed")
        (advance state installed-astro 2)
        (score state :corp (refresh installed-astro)))
      (play-from-hand state :corp "Ice Wall" "HQ")
      (let [no-token-astro (get-scored state :corp 0)
            token-astro (get-scored state :corp 1)
            hand-ice-wall (find-card "Ice Wall" (:hand (get-corp)))
            installed-ice-wall (get-ice state :hq 0)]
        (should-not-place token-astro no-token-astro " that is scored")
        (should-not-place token-astro hand-ice-wall " in hand")
        (should-place token-astro installed-ice-wall " that is installed")))))

(deftest artificial-cryptocrash
  ;; Offworld Office
  (do-game
    (new-game {:corp {:hand [(qty "Artificial Cryptocrash" 2)]}
               :runner {:credits 9}})
    (is (changed? [(:credit (get-runner)) -7]
          (play-and-score state "Artificial Cryptocrash"))
        "Runner loses 7 from cryptocrash")
    (is (changed? [(:credit (get-runner)) -2]
          (play-and-score state "Artificial Cryptocrash"))
        "Runner loses (all) 2 from cryptocrash")))

(deftest award-bait
  ;; Award Bait
  (do-game
    (new-game {:corp {:deck [(qty "Award Bait" 2) "Ice Wall"]}})
    (core/move state :corp (find-card "Award Bait" (:hand (get-corp))) :deck)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [iw (get-ice state :hq 0)]
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should start with 0 advancement tokens")
      (play-from-hand state :corp "Award Bait" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :corp "2")
      (click-card state :corp "Ice Wall")
      (click-prompt state :runner "Steal")
      (is (= 2 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 2 advancement tokens")
      (run-empty-server state "R&D")
      (click-prompt state :corp "2")
      (click-card state :corp (refresh iw))
      (click-prompt state :runner "Steal")
      (is (= 4 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 2 advancement tokens"))))

(deftest azef-protocol-happy
  ;; Azef Protocol
  (do-game
   (new-game {:corp {:hand ["Azef Protocol", "PAD Campaign"]}
              :runner {:hand ["Sure Gamble" "Sure Gamble" "Sure Gamble"]}})
   (play-from-hand state :corp "Azef Protocol" "New remote")
   (play-from-hand state :corp "PAD Campaign" "New remote")
   (core/add-prop state :corp (get-content state :remote1 0) :advance-counter 3)
   (score state :corp (get-content state :remote1 0))
   ;; check not scored yet
   (is (= 0 (count (:scored (get-corp)))) "Azef Protocol requires a cost be paid")
   (is (not (no-prompt? state :corp)) "Azef Protocol active")
   (click-card state :corp "PAD Campaign")
   ;; check scored, damage dealt
   (is (no-prompt? state :corp) "Azef Protocol prompt resolved")
   (is (= 2 (count (:discard (get-runner)))) "Did 2 meat damage upon scoring")
   (is (= 1 (count (:discard (get-corp)))) "Trashed PAD Campaign")
   (is (= 1 (count (:scored (get-corp)))) "Azef Protocol completed")))

(deftest azef-protocol-requires-valid-target
  ;; Azef Protocol needs a valid target to pay the cost to score
  (do-game
    (new-game {:corp {:hand ["Azef Protocol", "PAD Campaign"]
                      :credit 100}
               :runner {:hand ["Sure Gamble" "Sure Gamble" "Sure Gamble"]}})
    (play-from-hand state :corp "Azef Protocol" "New remote")
    (let [azef (get-content state :remote1 0)]
      (core/gain state :corp :click 3)
      (advance state (refresh azef) 3)
      (score state :corp (refresh azef))
      (is (= 0 (count (:scored (get-corp)))) "Azef Protocol requires a cost be paid")
      (is (no-prompt? state :corp) "No prompt for Azef Protocol because there are no targets")
      (is (= 0 (count (:discard (get-corp)))) "Did not trashed PAD Campaign")
      (is (= 0 (count (:scored (get-corp)))) "Azef Protocol not scored"))))

(deftest azef-protocol-cant-target-self
  ;; Azef Protocol can't trash itself to pay its cost
  (do-game
   (new-game {:corp {:hand ["Azef Protocol", "PAD Campaign"]}
              :runner {:hand ["Sure Gamble" "Sure Gamble" "Sure Gamble"]}})
   (play-from-hand state :corp "Azef Protocol" "New remote")
   (play-from-hand state :corp "PAD Campaign" "New remote")
   (core/add-prop state :corp (get-content state :remote1 0) :advance-counter 3)
   (score state :corp (get-content state :remote1 0))
   (is (= 0 (count (:scored (get-corp)))) "Azef Protocol requires a cost be paid")
   (is (not (no-prompt? state :corp)) "Azef Protocol prompt active")
   (click-card state :corp (get-content state :remote1 0))
   ;; check not scored
   (is (= 0 (count (:scored (get-corp)))) "Azef Protocol can't target self")
   (is (not (no-prompt? state :corp)) "Azef Protocol still active")
   (is (= 0 (count (:discard (get-runner)))) "No meat damage dealt")
   (is (= 0 (count (:discard (get-corp)))) "No card trashed")
   (is (= 0 (count (:scored (get-corp)))) "Azef Protocol not scored")))

(deftest bacterial-programming-stolen-from-archives-should-not-give-duplicate-accesses
  ;; there used to be an issue where you could randomly access the same cards indefinitely
  (do-game
    (new-game {:corp {:discard ["Hedge Fund" "Bacterial Programming"]
                      :hand ["Bellona"]
                      :deck ["IPO" "NGO Front" "Rashida Jaheem" "Tithe"
                             "Ice Wall" "Fire Wall" "Enigma"]}})
    (take-credits state :corp)
    (run-empty-server state :archives)
    (click-prompt state :runner "Bacterial Programming")
    (click-prompt state :runner "Steal")
    (click-prompt state :corp "Yes")
    (dotimes [_ 7]
      (let [card (first (prompt-titles :corp))]
        (click-prompt state :corp card)))
    (click-prompt state :corp "Done") ; Finished with trashing
    (click-prompt state :corp "Done") ; Finished with move-to-hq (no cards to move)
    (dotimes [_ 7]
      (click-prompt state :runner "Facedown card in Archives")
      (click-prompt state :runner "No action"))
    ;;(click-prompt state :runner "Everything else") - this only shows up if an agenda is in archives
    (is (no-prompt? state :corp) "Bacterial Programming prompts finished")
    (is (not (:run @state)) "No run is active")))

(deftest bacterial-programming-scoring-should-not-cause-a-run-to-exist-for-runner
    ;; Scoring should not cause a run to exist for runner.
    (do-game
      (new-game {:corp {:deck ["Bacterial Programming" "Hedge Fund"]}})
      (starting-hand state :corp ["Bacterial Programming"])
      (play-and-score state "Bacterial Programming")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Done")
      (click-prompt state :corp "Done")
      (click-prompt state :corp (first (:deck (get-corp))))
      (click-prompt state :corp "Done")
      (is (no-prompt? state :corp) "Bacterial Programming prompts finished")
      (is (not (:run @state)) "No run is active")))

(deftest bacterial-programming-removing-all-cards-from-r-d-should-not-freeze-for-runner-nor-give-an-extra-access
    ;; Removing all cards from R&D should not freeze for runner, nor give an extra access.
    (do-game
      (new-game {:corp {:deck [(qty "Bacterial Programming" 8)]
                        :hand ["Ice Wall"]}
                 :options {:start-as :runner}})
      (run-empty-server state :rd)
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "Yes")
      ;; Move all 7 cards to trash
      (dotimes [_ 7]
              ;; Get the first card listed in the prompt choice
              ;; TODO make this function
        (let [card (first (prompt-titles :corp))]
          (click-prompt state :corp card)))
      (click-prompt state :corp "Done") ; Finished with trashing
      (click-prompt state :corp "Done") ; Finished with move-to-hq (no cards to move)
      ;; Run and prompts should be over now
      (is (no-prompt? state :corp) "Bacterial Programming prompts finished")
      (is (no-prompt? state :runner) "Bacterial Programming prompts finished")
      (is (not (:run @state)))))

(deftest bellona
  ;; Bellona
  (do-game
      (new-game {:corp {:deck ["Bellona"]}})
      (play-from-hand state :corp "Bellona" "New remote")
      (let [bell (get-content state :remote1 0)]
        (advance state bell 2)
        (take-credits state :corp)
        (core/lose state :runner :credit 1)
        (run-empty-server state "Server 1")
        (click-prompt state :runner "No action")
        (is (zero? (count (:scored (get-runner)))) "Runner could not steal Bellona")
        (is (= 4 (:credit (get-runner))) "Runner couldn't afford to steal, so no credits spent")
        (take-credits state :runner)
        (advance state bell 3)
        (is (changed? [(:credit (get-corp)) 5]
              (score state :corp (refresh bell)))
            "Got 5 credits from Bellona")
        (is (= 3 (:agenda-point (get-corp))) "Scored Bellona for 3 points"))))

(deftest better-citizen-program
  ;; Better Citizen Program
  (do-game
      (new-game {:corp {:deck ["Better Citizen Program"]}
                 :runner {:deck [(qty "The Maker's Eye" 2)
                                 (qty "Wyrm" 2)]}})
      (play-and-score state "Better Citizen Program")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (is (zero? (count-tags state)) "Runner starts with 0 tags")
      (play-from-hand state :runner "The Maker's Eye")
      (click-prompt state :corp "Yes")
      (is (= 1 (count-tags state)) "Runner takes 1 tag for playing a Run event")
      (play-from-hand state :runner "Wyrm")
      (is (no-prompt? state :corp) "Corp shouldn't get a prompt to use Better Citizen Program")
      (is (= 1 (count-tags state)) "Runner doesn't gain a tag from installing an icebreaker after playing a Run event")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Wyrm")
      (click-prompt state :corp "Yes")
      (is (= 2 (count-tags state)) "Runner gains 1 tag for installing an Icebreaker")
      (play-from-hand state :runner "The Maker's Eye")
      (is (no-prompt? state :corp) "Corp shouldn't get a prompt to use Better Citizen Program")
      (is (= 2 (count-tags state)) "Runner doesn't gain a tag from playing a Run event after installing an Icebreaker")))

(deftest better-citizen-program-should-only-trigger-on-run-events-3619
    ;; Should only trigger on Run events. #3619
    (do-game
      (new-game {:corp {:deck ["Better Citizen Program"]}
                 :runner {:deck ["Mining Accident"]}})
      (play-and-score state "Better Citizen Program")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (play-from-hand state :runner "Mining Accident")
      (click-prompt state :corp "Pay 5 [Credits]")
      (is (no-prompt? state :corp) "Corp shouldn't get a prompt to use Better Citizen Program")
      (is (zero? (count-tags state)) "Runner should not gain a tag from playing a non-Run event")))

(deftest better-citizen-program-shouldn-t-trigger-apex-5175
    ;; Shouldn't trigger Apex #5175
    (do-game
      (new-game {:corp {:deck ["Better Citizen Program"]}
                 :runner {:id "Apex: Invasive Predator"
                          :deck ["Wyrm"]}})
      (play-and-score state "Better Citizen Program")
      (take-credits state :corp)
      (end-phase-12 state :runner)
      (click-card state :runner "Wyrm")
      (is (no-prompt? state :corp) "Corp shouldn't get a prompt to use Better Citizen Program")))

(deftest bifrost-array
  ;; Bifrost Array
  (do-game
    (new-game {:corp {:deck ["Bifrost Array" "Hostile Takeover"]}})
    (play-and-score state "Hostile Takeover")
    (is (= 12 (:credit (get-corp))) "Should gain 7 credits from 5 to 12")
    (is (= 1 (count-bad-pub state)) "Should gain 1 bad publicity")
    (play-and-score state "Bifrost Array")
    (click-prompt state :corp "Yes")
    (click-card state :corp "Hostile Takeover")
    (is (= 19 (:credit (get-corp))) "Should gain 7 credits from 12 to 19")
    (is (= 2 (count-bad-pub state)) "Should gain 1 bad publicity")))

(deftest blood-in-the-water
  (do-game
    (new-game {:corp {:hand ["Blood in the Water"]}
               :runner {:hand [(qty "Sure Gamble" 4)]}})
    (play-from-hand state :corp "Blood in the Water" "New remote")
    (let [blood (get-content state :remote1 0)]
      (core/add-prop state :corp blood :advance-counter 2)
      (score state :corp (refresh blood))
      (is (= 0 (:agenda-point (get-corp))) "Can't score Blood in the Water (X = 4)")
      (damage state :corp :net 1)
      (score state :corp (refresh blood))
      (is (= 0 (:agenda-point (get-corp))) "Can't score Blood in the Water (X = 3)")
      (damage state :corp :net 1)
      (score state :corp (refresh blood))
      (is (= 2 (:agenda-point (get-corp))) "Scored Blood in the Water when runner had 2 cards"))))

(deftest brain-rewiring
  ;; Brain Rewiring
  (do-game
    (new-game {:corp {:deck ["Brain Rewiring"]}})
    (starting-hand state :runner ["Sure Gamble" "Sure Gamble"])
    (play-and-score state "Brain Rewiring")
    (click-prompt state :corp "Yes")
    (is (not (no-prompt? state :runner)) "Runner waiting for Corp resolve Brain Rewiring")
    (click-prompt state :corp "2")
    (is (= 1 (count (:hand (get-runner)))))
    (is (no-prompt? state :runner) "Runner not waiting for Corp resolve Brain Rewiring")
    (is (no-prompt? state :corp) "Corp done resolving Brain Rewiring")))

(deftest braintrust
  ;; Braintrust
  (do-game
    (new-game {:corp {:deck ["Braintrust" "Ichi 1.0"]}})
    (play-from-hand state :corp "Braintrust" "New remote")
    (let [bt (get-content state :remote1 0)]
      (core/add-prop state :corp bt :advance-counter 7)
      (score state :corp (refresh bt))
      (let [scored-bt (get-scored state :corp 0)]
        (is (= 2 (get-counters (refresh scored-bt) :agenda))
            "Scored w/ 4 over-advancements; 2 agenda counters")
        (play-from-hand state :corp "Ichi 1.0" "HQ")
        (rez state :corp (get-ice state :hq 0))
        (is (= 2 (:credit (get-corp))) "2c discount to rez Ichi")))))

(deftest breaking-news
  ;; Breaking News
  (do-game
    (new-game {:corp {:deck [(qty "Breaking News" 3)]}})
    (play-and-score state "Breaking News")
    (is (= 2 (count-tags state)) "Runner receives 2 tags from Breaking News")
    (take-credits state :corp)
    (is (zero? (count-tags state))) "Two tags removed at the end of the turn"))

(deftest broad-daylight-take-bad-pub
    ;; take bad pub
    (do-game
      (new-game {:corp {:deck [(qty "Broad Daylight" 3)]}})
      (is (zero? (count-bad-pub state)) "Corp start with no bad pub")
      (play-and-score state "Broad Daylight")
      (click-prompt state :corp "Yes")
      (is (= 1 (count-bad-pub state)) "Corp gains 1 bad pub")
      (is (= 1 (get-counters (get-scored state :corp 0) :agenda)) "Should gain 1 agenda counter")
      (play-and-score state "Broad Daylight")
      (click-prompt state :corp "No")
      (is (= 1 (count-bad-pub state)) "Corp doesn't gain bad pub")
      (is (= 1 (get-counters (get-scored state :corp 1) :agenda)) "Should gain 1 agenda counter")
      (play-and-score state "Broad Daylight")
      (click-prompt state :corp "Yes")
      (is (= 2 (count-bad-pub state)) "Corp gains 1 bad pub")
      (is (= 2 (get-counters (get-scored state :corp 2) :agenda)) "Should gain 2 agenda counters")))

(deftest broad-daylight-deal-damage
    ;; deal damage
    (do-game
      (new-game {:corp {:deck ["Broad Daylight"]}})
      (core/gain state :corp :bad-publicity 3)
      (play-and-score state "Broad Daylight")
      (click-prompt state :corp "Yes")
      (is (= 4 (count-bad-pub state)) "Corp gains 1 bad pub")
      (is (= 4 (get-counters (get-scored state :corp 0) :agenda)) "Should gain 1 agenda counter")
      (is (empty? (:discard (get-runner))) "Runner has no discarded cards")
      (card-ability state :corp (get-scored state :corp 0) 0)
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 damage")
      (card-ability state :corp (get-scored state :corp 0) 0)
      (is (= 2 (count (:discard (get-runner)))) "Runner didn't take additional damage")))

(deftest broad-daylight-bad-pub-triggers
    ;; bad pub triggers
    (do-game
      (new-game {:corp {:deck ["Broad Daylight" "Broadcast Square"]}})
      (core/gain state :corp :bad-publicity 1)
      (play-from-hand state :corp "Broadcast Square" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (is (= 1 (count-bad-pub state)) "Corp start with one bad pub")
      (play-and-score state "Broad Daylight")
      (click-prompt state :corp "Yes")
      (is (= 1 (count-bad-pub state)) "Doesn't gain additional bad pub yet")
      (click-prompt state :corp "0")  ;; Corp doesn't pump trace, base 3
      (click-prompt state :runner "0")  ;; Runner doesn't pump trace; loses trace
      (is (= 1 (count-bad-pub state)) "Blocks gaining additional bad pub")
      (is (= 1 (get-counters (get-scored state :corp 0) :agenda)) "Should gain 1 agenda counter")))

(deftest broad-daylight-bad-pub-triggers-more-cases
    ;; bad pub triggers - more cases
    (do-game
      (new-game {:corp {:deck ["Broad Daylight" "Broadcast Square"]}})
      (core/gain state :corp :bad-publicity 1)
      (play-from-hand state :corp "Broadcast Square" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (is (= 1 (count-bad-pub state)) "Corp start with one bad pub")
      (play-and-score state "Broad Daylight")
      (click-prompt state :corp "Yes")
      (is (= 1 (count-bad-pub state)) "Doesn't gain additional bad pub yet")
      (click-prompt state :corp "0")  ;; Corp doesn't pump trace, base 3
      (click-prompt state :runner "5")  ;; Runner pumps trace; wins trace
      (is (= 2 (count-bad-pub state)) "Gains additional bad pub")
      (is (= 2 (get-counters (get-scored state :corp 0) :agenda)) "Should gain 2 agenda counter")))

(deftest broad-daylight-interaction-with-titan
    ;; Interaction with Titan
    (do-game
      (new-game {:corp {:id "Titan Transnational: Investing In Your Future"
                        :deck [(qty "Broad Daylight" 3)]}})
      (is (zero? (count-bad-pub state)) "Corp start with no bad pub")
      (play-and-score state "Broad Daylight")
      (click-prompt state :corp "No")
      (is (= 0 (count-bad-pub state)) "Corp gains no bad pub")
      (is (= 1 (get-counters (get-scored state :corp 0) :agenda)) "Should gain 1 agenda counters")
      (play-and-score state "Broad Daylight")
      (click-prompt state :corp "Yes")
      (is (= 1 (count-bad-pub state)) "Corp gains 1 bad pub")
      (is (= 2 (get-counters (get-scored state :corp 1) :agenda)) "Should gain 2 agenda counters")))

(deftest broad-daylight-interaction-with-storgotic-resonator-5194
    ;; interaction with Storgotic Resonator #5194
    (do-game
      (new-game {:corp {:deck ["Broad Daylight" "Storgotic Resonator"]}
                 :runner {:id "Reina Roja: Freedom Fighter"
                          :hand [(qty "Stimhack" 5)]}})
      (play-from-hand state :corp "Storgotic Resonator" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (play-and-score state "Broad Daylight")
      (click-prompt state :corp "Yes")
      (is (= 1 (get-counters (get-scored state :corp 0) :agenda)) "Should gain 1 agenda counter")
      (is (empty? (:discard (get-runner))) "Runner has no discarded cards")
      (card-ability state :corp (get-scored state :corp 0) 0)
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 damage")
      (is (= 1 (get-counters (get-content state :remote1 0) :power)))))

(deftest cfc-excavation-contract
  ;; CFC Excavation Contract
  (dotimes [n 5]
    (do-game
      (new-game {:corp {:deck ["CFC Excavation Contract" (qty "Eli 1.0" n)]}})
      (core/gain state :corp :click 10 :credit 10)
      (is (= 15 (:credit (get-corp))) "Should start with 5 credits")
      (dotimes [_ n]
        (play-from-hand state :corp "Eli 1.0" "New remote")
        (rez state :corp (get-ice state (keyword (str "remote" (dec (:rid @state)))) 0)))
      (let [credit (:credit (get-corp))]
        (play-and-score state "CFC Excavation Contract")
        (is (= (+ credit (* 2 n)) (:credit (get-corp)))
            (str "Should now have with " (+ credit (* 2 n)) " credits"))))))

(deftest character-assassination
  ;; Character Assassination
  (do-game
    (new-game {:corp {:deck ["Character Assassination"]}
               :runner {:deck ["Fall Guy" "Kati Jones"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Kati Jones")
    (play-from-hand state :runner "Fall Guy")
    (take-credits state :runner)
    (play-and-score state "Character Assassination")
    (let [kati (get-resource state 0)]
      (click-card state :corp kati)
      (is (no-prompt? state :runner) "Fall Guy prevention didn't occur")
      (is (= 1 (count (:discard (get-runner)))) "Kati Jones trashed"))))

(deftest chronos-project-happy-path
    ;; Happy Path
    (do-game
      (new-game {:corp {:deck ["Chronos Project"]}})
      (dotimes [_ 3]
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :discard))
      (is (= 3 (count (:discard (get-runner)))) "Runner should have 3 cards in heap")
      (play-and-score state "Chronos Project")
      (is (zero? (count (:discard (get-runner)))) "Runner should have 0 cards in heap")))

(deftest chronos-project-heap-locked-test
    ;; Heap Locked Test
    (do-game
      (new-game {:corp {:deck ["Chronos Project" "Blacklist" "Biotic Labor"]
                        :credits 20}})
      (dotimes [_ 3]
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :discard))
      (is (= 3 (count (:discard (get-runner)))) "Runner should have 3 cards in heap")
      (play-from-hand state :corp "Biotic Labor")
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (play-and-score state "Chronos Project")
      (is (= 3 (count (:discard (get-runner)))) "Runner should have 3 cards in heap")))

(deftest city-works-project
  ;; City Works Project
  (do-game
    (new-game {:corp {:deck ["City Works Project"]}
               :runner {:deck [(qty "Sure Gamble" 4)]}})
    (play-from-hand state :corp "City Works Project" "New remote")
    (let [cwp (get-content state :remote1 0)]
      (click-advance state :corp (refresh cwp))
      (click-advance state :corp (refresh cwp)))
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (is (= 4 (count (:discard (get-runner)))) "Runner paid 4 meat damage")))

(deftest clone-retirement
  ;; Clone Retirement
  (do-game
    (new-game {:corp {:deck [(qty "Clone Retirement" 2) "Hostile Takeover"]}})
    (play-and-score state "Hostile Takeover")
    (is (= 12 (:credit (get-corp))))
    (is (= 1 (count-bad-pub state)))
    (play-and-score state "Clone Retirement")
    (is (zero? (count-bad-pub state)))
    (play-from-hand state :corp "Clone Retirement" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 3")
    (click-prompt state :runner "Steal")
    (is (= 1 (count-bad-pub state)))))

(deftest corporate-sales-team
  ;; Corporate Sales Team
  (do-game
    (new-game {:corp {:deck [(qty "Corporate Sales Team" 2)]}})
    (is (= 5 (:credit (get-corp))))
    (play-and-score state "Corporate Sales Team")
    (is (= 5 (:credit (get-corp))))
    (let [scored-cst (get-scored state :corp 0)]
      (end-turn state :corp)
      (start-turn state :runner)
      (is (= 6 (:credit (get-corp))) "Increments at runner's start of turn")
      (is (= 9 (get-counters (refresh scored-cst) :credit)))
      (end-turn state :runner)
      (start-turn state :corp)
      (is (= 7 (:credit (get-corp))) "Increments at corp's start of turn")
      (is (= 8 (get-counters (refresh scored-cst) :credit))))))

(deftest corporate-war
  ;; Corporate War
  (do-game
    (new-game {:corp {:deck [(qty "Corporate War" 2)]}})
    (is (= 5 (:credit (get-corp))))
    (play-and-score state "Corporate War")
    (is (zero? (:credit (get-corp))) "Lost all credits")
    (core/gain state :corp :credit 7)
    (play-and-score state "Corporate War")
    (is (= 14 (:credit (get-corp))) "Had 7 credits when scoring, gained another 7")))

(deftest crisis-management
  ;; Crisis Management
  (do-game
    (new-game {:corp {:deck ["Crisis Management"]}})
    (play-and-score state "Crisis Management")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 3 (count (:hand (get-runner)))) "No damage done, Runner not tagged")
    (take-credits state :corp)
    (gain-tags state :runner 1)
    (take-credits state :runner)
    (is (= 2 (count (:hand (get-runner)))) "Crisis Management dealt 1 meat damage")))

(deftest cyberdex-sandbox
  ;; Cyberdex Sandbox
  (do-game
      (new-game {:corp {:deck ["Cyberdex Virus Suite" "Cyberdex Sandbox" "Cyberdex Trial"]}})
      (play-and-score state "Cyberdex Sandbox")
      (core/gain state :corp :click 10)
      (is (changed? [(:credit (get-corp)) 4]
            (click-prompt state :corp "Yes")))
      (is (changed? [(:credit (get-corp)) 0]
            (purge state :corp)))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (changed? [(:credit (get-corp)) 4]
            (play-from-hand state :corp "Cyberdex Trial")))
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Cyberdex Virus Suite" "HQ")
      (let [cvs (get-content state :hq 0)]
        (rez state :corp cvs)
        (is (changed? [(:credit (get-corp)) 4]
              (card-ability state :corp cvs 0))))))

(deftest cyberdex-sandbox-only-triggers-on-the-first-purge-each-turn-5174
    ;; Only triggers on the first purge each turn #5174
    (do-game
      (new-game {:corp {:deck ["Cyberdex Virus Suite" "Cyberdex Sandbox" "Cyberdex Trial"]}})
      (core/gain state :corp :click 10)
      (purge state :corp)
      (play-and-score state "Cyberdex Sandbox")
      (is (changed? [(:credit (get-corp)) 0]
            (click-prompt state :corp "Yes")))))

(deftest dedicated-neural-net-corp-chooses-card-to-access-issue-4874
    ;; Corp chooses card to access. Issue #4874
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Dedicated Neural Net" "Government Takeover" "Domestic Sleepers"]}})
      (play-and-score state "Dedicated Neural Net")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "0 [Credits]")
      (click-prompt state :corp "1 [Credits]")
      (click-card state :corp "Domestic Sleepers")
      (click-prompt state :runner "Steal")
      (is (= "Government Takeover" (:title (first (:hand (get-corp))))) "Gov Takeover isn't stolen")
      (is (= "Domestic Sleepers" (:title (first (:scored (get-runner))))) "Domestic Sleepers is stolen")))

(deftest dedicated-neural-net-allows-for-accessing-upgrades-issue-2376
    ;; Allows for accessing upgrades. Issue #2376
    (do-game
      (new-game {:corp {:deck ["Dedicated Neural Net" (qty "Scorched Earth" 2)
                               "Hedge Fund" "Caprice Nisei"]}})
      (play-from-hand state :corp "Caprice Nisei" "HQ")
      (play-and-score state "Dedicated Neural Net")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "0 [Credits]")
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "Card from hand")
      (click-card state :corp "Hedge Fund")
      (is (accessing state "Hedge Fund"))
      (click-prompt state :runner "No action")
      (is (accessing state "Caprice Nisei"))
      (click-prompt state :runner "No action")
      (is (not (:run @state)) "Run completed")))

(deftest dedicated-neural-net-multiaccess-works-properly
    ;; Multiaccess works properly
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Dedicated Neural Net" "Government Takeover" "Domestic Sleepers"]}
                 :runner {:hand ["HQ Interface"]}})
      (play-and-score state "Dedicated Neural Net")
      (take-credits state :corp)
      (play-from-hand state :runner "HQ Interface")
      (run-empty-server state "HQ")
      (click-prompt state :runner "0 [Credits]")
      (click-prompt state :corp "1 [Credits]")
      (click-card state :corp "Domestic Sleepers")
      (click-prompt state :runner "Steal")
      (click-card state :corp "Government Takeover")
      (click-prompt state :runner "Steal")))

(deftest dedicated-neural-net-multiaccess-respects-cards-in-hand
    ;; Multiaccess respects cards in hand
    (do-game
      (new-game {:corp {:hand ["Dedicated Neural Net" "Mwanza City Grid" "Domestic Sleepers" "Hedge Fund" "Ice Wall"]}
                 :runner {:hand ["HQ Interface"]}})
      (play-and-score state "Dedicated Neural Net")
      (play-from-hand state :corp "Mwanza City Grid" "HQ")
      (rez state :corp (get-content state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "HQ Interface")
      (run-empty-server state "HQ")
      (click-prompt state :runner "0 [Credits]")
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "Mwanza City Grid")
      (click-prompt state :runner "No action")
      (click-card state :corp "Hedge Fund")
      (click-prompt state :runner "No action")
      (click-card state :corp "Ice Wall")
      (click-prompt state :runner "No action")
      (click-card state :corp "Domestic Sleepers")
      (click-prompt state :runner "Steal")
      (is (no-prompt? state :runner) "No further prompts for Runner")
      (is (no-prompt? state :corp) "No further prompts for Corp")
      (is (nil? (:run @state)) "Run has ended")))

(deftest dedicated-neural-net-can-access-upgrades-between-cards-in-hand
    ;; Can access upgrades between cards in hand
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Dedicated Neural Net" "Ice Wall" "Enigma" "Caprice Nisei"]}
                 :runner {:hand ["HQ Interface"]}})
      (play-from-hand state :corp "Caprice Nisei" "HQ")
      (play-and-score state "Dedicated Neural Net")
      (take-credits state :corp)
      (play-from-hand state :runner "HQ Interface")
      (run-empty-server state "HQ")
      (click-prompt state :runner "0 [Credits]")
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "Card from hand")
      (click-card state :corp "Enigma")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Unrezzed upgrade")
      (click-prompt state :runner "No action")
      (click-card state :corp "Ice Wall")
      (click-prompt state :runner "No action")))

(deftest degree-mill-basic-behavior
    ;; Basic behavior
    (do-game
      (new-game {:corp {:deck [(qty "Degree Mill" 2)]}
                 :runner {:deck ["Ice Analyzer" "All-nighter" "Hunting Grounds"]}})
      (play-from-hand state :corp "Degree Mill" "New remote")
      (take-credits state :corp)
      (is (zero? (count (:deck (get-runner)))) "Runner starts with empty deck")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (is (zero? (:agenda-point (get-runner))) "Runner stole Degree Mill with no installed cards")
      (play-from-hand state :runner "Ice Analyzer")
      (play-from-hand state :runner "All-nighter")
      (let [ia (get-resource state 0)
            an (get-resource state 1)]
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Pay to steal")
        (click-card state :runner ia)
        (click-card state :runner an)
        (is (= 3 (:agenda-point (get-runner))) "Runner failed to steal Degree Mill")
        (is (empty? (get-in @state [:runner :rig :resource])) "Degree Mill didn't remove installed cards")
        (is (= 2 (count (:deck (get-runner)))) "Degree Mill didn't put cards back in deck"))
      (take-credits state :runner)
      ;; Checking if facedowns work as well
      (play-from-hand state :corp "Degree Mill" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Hunting Grounds")
      (let [hg (get-resource state 0)]
        (run-empty-server state "Server 2")
        (click-prompt state :runner "No action")
        (is (= 3 (:agenda-point (get-runner))) "Runner stole Degree Mill with single card")
        (card-ability state :runner hg 1)
        (is (= 2 (count (get-in (get-runner) [:rig :facedown]))) "Hunting Ground did not install cards facedown")
        (is (empty? (:deck (get-runner))) "Hunting Grounds did not remove cards from deck")
        (let [fd1 (get-runner-facedown state 0)
              fd2 (get-runner-facedown state 1)]
          (run-empty-server state "Server 2")
          (click-prompt state :runner "Pay to steal")
          (click-card state :runner fd1)
          (click-card state :runner fd2)
          (is (= 6 (:agenda-point (get-runner))) "Runner failed to steal Degree Mill with facedown cards")
          (is (empty? (get-in (get-runner)  [:rig :facedown])) "Degree Mill didn't remove facedown cards")
          (is (= 2 (count (:deck (get-runner)))) "Degree Mill didn't put cards back in deck")))))

(deftest degree-mill-multiple-steal-costs
    ;; Multiple steal costs
    (do-game
      (new-game {:corp {:deck [(qty "Degree Mill" 1) (qty "Strongbox" 1)]}
                 :runner {:deck [(qty "Ice Analyzer" 3) (qty "All-nighter" 3)]}})
      (play-from-hand state :corp "Degree Mill" "New remote")
      (play-from-hand state :corp "Strongbox" "Server 1")
      (let [dm (get-content state :remote1 0)
            sb (get-content state :remote1 1)]
        (rez state :corp sb)
        (take-credits state :corp)
        (play-from-hand state :runner "Ice Analyzer")
        (play-from-hand state :runner "All-nighter")
        (run-empty-server state :remote1)
        (click-card state :runner (refresh dm))
        (is (= 1 (:click (get-runner))) "Runner should start with 1 remaining click")
        (is (= 2 (count (get-in @state [:runner :rig :resource]))) "Runner starts with 2 resources")
        (click-prompt state :runner "Pay to steal")
        (click-card state :runner (get-resource state 1))
        (click-card state :runner (get-resource state 0))
        (is (zero? (:click (get-runner))) "Runner should have spent a click")
        (is (empty? (get-resource state)) "Degree Mill removed installed cards")
        (is (not-empty (get-scored state :runner)) "Runner stole an agenda"))))

(deftest director-haas-pet-project
  ;; Director Haas' Pet Project
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Strongbox"
                             "Director Haas' Pet Project"
                             "Adonis Campaign"]
                      :discard ["Eli 1.0"]}})
    (play-and-score state "Director Haas' Pet Project")
    (is (changed? [(:credit (get-corp)) 0]
          (click-prompt state :corp "Yes")
          (click-card state :corp "Adonis Campaign")
          (click-card state :corp "Strongbox")
          (click-card state :corp "Eli 1.0")
          (is (= "Adonis Campaign" (:title (get-content state :remote2 0))))
          (is (= "Strongbox" (:title (get-content state :remote2 1))))
          (is (= "Eli 1.0" (:title (get-ice state :remote2 0)))))
        "Corp spends no credits to install")))

(deftest divested-trust
  ;; Divested Trust
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                        :hand ["Hostile Takeover" "Divested Trust"]}})
      (play-and-score state "Divested Trust")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (is (-> (get-corp) :hand count zero?) "Corp has no cards in hand")
      (is (= 1 (:agenda-point (get-corp))) "Corp should agenda points from Divested Trust")
      (is (= 1 (:agenda-point (get-runner))) "Runner should gain agenda points for stealing Hostile Takeover")
      (click-prompt state :corp "Yes")
      (is (= "Hostile Takeover" (-> (get-corp) :hand first :title)) "Hostile Takeover should be in HQ")
      (is (empty? (-> (get-corp) :scored)))
      (is (zero? (:agenda-point (get-corp))) "Corp should lose points from forfeit agenda")
      (is (zero? (:agenda-point (get-runner))) "Runner should lose agenda points from agenda leaving score area")))

(deftest divested-trust-doesn-t-stop-the-runner-from-winning-issue-4107
    ;; Doesn't stop the runner from winning. Issue #4107
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                        :hand ["Government Takeover" "Hostile Takeover" "Divested Trust"]
                        :credits 20}})
      (play-and-score state "Divested Trust")
      (play-from-hand state :corp "Government Takeover" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 2")
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "No")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (is (no-prompt? state :corp) "Corp doesn't get opportunity to use Divested Trust")
      (is (= :runner (:winner @state)) "Runner should win")
      (is (= "Agenda" (:reason @state)) "Win condition reports points")))

(deftest divested-trust-interaction-with-turntable-issue-4789
    ;; Interaction with Turntable. Issue #4789
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                        :hand ["Government Takeover" "Divested Trust"]
                        :credits 20}
                 :runner {:hand ["Turntable"]}})
      (play-and-score state "Divested Trust")
      (play-from-hand state :corp "Government Takeover" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Turntable")
      (run-empty-server state "Server 2")
      (click-prompt state :runner "Steal")
      (is (= "Swap Government Takeover for an agenda in the Corp's score area?"
             (:msg (prompt-map :runner)))
          "Runner has Turntable prompt")
      (click-prompt state :runner "Yes")
      (click-card state :runner "Divested Trust")
      (click-prompt state :corp "Yes")
      (is (zero? (:agenda-point (get-corp))) "Corp has 0 points")
      (is (zero? (:agenda-point (get-runner))) "Runner has 0 points")
      (is (empty? (:scored (get-corp))) "Corp has no cards scored")
      (is (empty? (:scored (get-runner))) "Runner has no cards scored")
      (is (find-card "Divested Trust" (:rfg (get-corp))) "Divested Trust should be rfg'd")
      (is (find-card "Government Takeover" (:hand (get-corp))) "Gov Takeover should be in HQ")))

(deftest domestic-sleepers-ability-changes-points
    ;; Ability changes points
    (do-game
      (new-game {:corp {:deck ["Domestic Sleepers"]}})
      (play-and-score state "Domestic Sleepers")
      (core/gain state :corp :click 3)
      (let [ds_scored (get-scored state :corp 0)]
        (is (zero? (get-counters (refresh ds_scored) :agenda)) "Should start with 0 agenda counters")
        (is (zero? (:agenda-point (get-corp))) "Should provide 0 agenda points initially")
        (card-ability state :corp ds_scored 0)
        (is (= 1 (get-counters (refresh ds_scored) :agenda)) "Should gain 1 agenda counter")
        (is (= 1 (:agenda-point (get-corp))) "Should provide 1 agenda point after ability use"))))

(deftest domestic-sleepers-interaction-with-mark-yale-issue-2920
    ;; Interaction with Mark Yale (issue #2920)
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Domestic Sleepers" "Mark Yale"]
                        :credits 10}})
      (core/gain state :corp :click 5)
      (play-from-hand state :corp "Mark Yale" "New remote")
      (play-and-score state "Domestic Sleepers")
      (let [sleepers (get-scored state :corp 0)
            yale (get-content state :remote1 0)]
        (card-ability state :corp sleepers 0)
        (rez state :corp yale)
        (card-ability state :corp yale 1)
        (click-card state :corp "Domestic Sleepers")
        (is (zero? (:agenda-point (get-corp))) "Domestic Sleepers is worth 0 points after losing the agenda counter"))))

(deftest elivagar-bifurcation
  (do-game
    (new-game {:corp {:hand ["Élivágar Bifurcation" "Ice Wall"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [iwall (get-ice state :hq 0)]
      (rez state :corp iwall)
      (play-and-score state "Élivágar Bifurcation")
      (click-card state :corp (refresh iwall))
      (is (not (rezzed? (refresh iwall))) "ice wall was derezzed"))))

(deftest elivagar-bifurcation-declined
  ;; Élivágar Bifurcation score effect is optional
  (do-game
    (new-game {:corp {:hand ["Élivágar Bifurcation"]}})
    (play-and-score state "Élivágar Bifurcation")
    (click-prompt state :corp "Done")
    (is (no-prompt? state :corp))))

(deftest eden-fragment
  ;; Test that Eden Fragment ignores the install cost of the first ice
  (do-game
    (new-game {:corp {:deck [(qty "Eden Fragment" 3) (qty "Ice Wall" 3)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-and-score state "Eden Fragment")
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (some? (get-ice state :hq 1)) "Corp has two ice installed on HQ")
    (is (= 6 (:credit (get-corp))) "Corp does not pay for installing the first piece of ice of the turn")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (some? (get-ice state :hq 2)) "Corp has three ice installed on HQ")
    (is (= 4 (:credit (get-corp))) "Corp pays for installing the second piece of ice of the turn")))

(deftest efficiency-committee
  ;; Efficiency Committee
  (do-game
    (new-game {:corp {:deck [(qty "Efficiency Committee" 3) (qty "Shipment from SanSan" 2)
                             "Ice Wall"]}})
    (core/gain state :corp :click 4)
    (play-from-hand state :corp "Efficiency Committee" "New remote")
    (play-from-hand state :corp "Efficiency Committee" "New remote")
    (play-from-hand state :corp "Efficiency Committee" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [ec1 (get-content state :remote1 0)
          ec2 (get-content state :remote2 0)
          ec3 (get-content state :remote3 0)
          iw (get-ice state :hq 0)]
      (score-agenda state :corp ec1)
      (let [ec1_scored (get-scored state :corp 0)]
        (is (= 3 (get-counters (refresh ec1_scored) :agenda)))
        (is (= 2 (:agenda-point (get-corp))))
        ;; use token
        (is (= 3 (:click (get-corp))))
        (card-ability state :corp ec1_scored 0)
        (is (= 4 (:click (get-corp))))
        ;; try to advance Ice Wall
        (advance state iw)
        (is (= 4 (:click (get-corp))))
        (is (zero? (get-counters (refresh iw) :advancement)))
        ;; try to advance Efficiency Committee
        (advance state ec2)
        (is (= 4 (:click (get-corp))))
        (is (zero? (get-counters (refresh ec2) :advancement)))
        ;; advance with Shipment from SanSan
        (play-from-hand state :corp "Shipment from SanSan")
        (click-prompt state :corp "2")
        (click-card state :corp ec2)
        (is (= 2 (get-counters (refresh ec2) :advancement)))
        (play-from-hand state :corp "Shipment from SanSan")
        (click-prompt state :corp "2")
        (click-card state :corp ec2)
        (is (= 4 (get-counters (refresh ec2) :advancement)))
        (score state :corp (refresh ec2))
        (is (= 4 (:agenda-point (get-corp))))
        (take-credits state :corp)
        (take-credits state :runner)
        ;; can advance again
        (advance state iw)
        (is (= 1 (get-counters (refresh iw) :advancement)))
        (advance state ec3)
        (is (= 1 (get-counters (refresh ec3) :advancement)))))))

(deftest elective-upgrade
  ;; Elective Upgrade
  (do-game
    (new-game {:corp {:deck ["Elective Upgrade"]}})
    (play-and-score state "Elective Upgrade")
    (let [eu-scored (get-scored state :corp 0)]
      (is (= 2 (get-counters (refresh eu-scored) :agenda)) "Should start with 2 agenda counters")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 3 (:click (get-corp))) "Should start with 4 clicks")
      (card-ability state :corp eu-scored 0)
      (card-ability state :corp eu-scored 0)
      (is (= 4 (:click (get-corp))) "Should gain 2 clicks, not 3")
      (is (= 1 (get-counters (refresh eu-scored) :agenda)) "Should still have 1 agenda counter"))))

(deftest eminent-domain
  (do-game
    (new-game {:corp {:deck [(qty "Archer" 10)]
                      :hand ["Eminent Domain"]}})
    (play-and-score state "Eminent Domain")
    (is (changed? [(:credit (get-corp)) 0]
          (click-prompt state :corp "Yes")
          (click-prompt state :corp "Archer")
          (click-prompt state :corp "HQ")
          (is (= "Archer" (get-title (get-ice state :hq 0))))
          (is (rezzed? (get-ice state :hq 0))))
        "Eminent Domain allows install and rez from R&D at no cost")))

(deftest eminent-domain-expend-ability
  (do-game
    (new-game {:corp {:hand ["Eminent Domain" "Pharos" "Tithe" "Ice Wall"]}})
    (play-from-hand state :corp "Tithe" "HQ")
    (is (changed? [(:credit (get-corp)) -4]
          (expend state :corp (find-card "Eminent Domain" (:hand (get-corp))))
          (click-card state :corp "Pharos")
          (click-prompt state :corp "HQ")
          (is (= "Pharos" (get-title (get-ice state :hq 1))))
          (is (rezzed? (get-ice state :hq 1))))
        "Eminent Domain expend allows install and rez reducing total cost by 5")))

(deftest encrypted-portals
  ;; Encrypted Portals
  (do-game
    (new-game {:corp {:deck ["Encrypted Portals" "Lotus Field"]}})
    (play-from-hand state :corp "Lotus Field" "HQ")
    (let [lf (get-ice state :hq 0)]
      (rez state :corp lf)
      (is (= 4 (get-strength (refresh lf))) "Should start with base strength of 4")
      (is (zero? (:credit (get-corp))) "Should have 0 credits after rez")
      (play-and-score state "Encrypted Portals")
      (is (= 5 (get-strength (refresh lf))) "Should gain 1 strength from 4 to 5")
      (is (= 1 (:credit (get-corp))) "Should gain 1 credit for rezzed code gate"))))

(deftest escalate-vitriol
  ;; Escalate Vitriol
  (do-game
    (new-game {:corp {:deck ["Escalate Vitriol"]}})
    (core/lose state :corp :credit 5)
    (play-and-score state "Escalate Vitriol")
    (let [ev-scored (get-scored state :corp 0)]
      (dotimes [tag 10]
        (is (zero? (count-tags state)) "Should start with 0 tags")
        (is (zero? (:credit (get-corp))) "Should start with 0 credits")
        (gain-tags state :runner tag)
        (card-ability state :corp ev-scored 0)
        (is (= tag (:credit (get-corp))) (str "Should gain " tag " credits"))
        (take-credits state :corp)
        (take-credits state :runner)
        (core/lose state :corp :credit (:credit (get-corp)))
        (core/lose-tags state :runner (core/make-eid state) tag)))))

(deftest executive-retreat
  ;; Executive Retreat
  (do-game
    (new-game {:corp {:deck ["Executive Retreat" (qty "Hedge Fund" 5)]}})
    (starting-hand state :corp ["Executive Retreat" "Hedge Fund"])
    (is (= 2 (count (:hand (get-corp)))) "Corp should start with 1 card in HQ")
    (play-and-score state "Executive Retreat")
    (is (zero? (count (:hand (get-corp)))) "Corp should have 0 cards in HQ after shuffling HQ back into R&D")
    (let [er-scored (get-scored state :corp 0)]
      (card-ability state :corp er-scored 0)
      (is (= 5 (count (:hand (get-corp)))) "Corp should have 5 cards in hand")
      (is (zero? (get-counters (refresh er-scored) :agenda)) "Executive Retreat should have 0 agenda counters"))))

(deftest executive-retreat-overdraw
    ;; Overdraw
    (do-game
      (new-game {:corp {:deck ["Executive Retreat" (qty "Hedge Fund" 4)]}})
      (starting-hand state :corp ["Executive Retreat" "Hedge Fund"])
      (is (= 2 (count (:hand (get-corp)))) "Corp should start with 1 card in HQ")
      (play-and-score state "Executive Retreat")
      (is (zero? (count (:hand (get-corp)))) "Corp should have 0 cards in HQ after shuffling HQ back into R&D")
      (let [er-scored (get-scored state :corp 0)]
        (card-ability state :corp er-scored 0)
        (is (= 4 (count (:hand (get-corp)))) "Corp should have 5 cards in hand")
        (is (zero? (get-counters (refresh er-scored) :agenda)) "Executive Retreat should have 0 agenda counters")
        (is (= :runner (:winner @state)) "Runner wins")
        (is (= "Decked" (:reason @state)) "Win condition reports decked"))))

(deftest explode-a-palooza
  ;; Explode-a-palooza
  (do-game
      (new-game {:corp {:deck ["Explode-a-palooza"]}})
      (play-from-hand state :corp "Explode-a-palooza" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :corp "Yes")
      (click-prompt state :runner "Steal")
      (is (= 12 (:credit (get-corp))) "Gained 5 credits")))

(deftest explode-a-palooza-interaction-with-the-turning-wheel-issue-1717
    ;; Interaction with The Turning Wheel. Issue #1717.
    (do-game
      (new-game {:corp {:deck [(qty "Explode-a-palooza" 3)]}
                 :runner {:deck ["The Turning Wheel"]}})
      (starting-hand state :corp ["Explode-a-palooza" "Explode-a-palooza"])
      (play-from-hand state :corp "Explode-a-palooza" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "The Turning Wheel")
      (run-empty-server state :remote1)
      (click-prompt state :corp "Yes")
      (click-prompt state :runner "Steal")
      (let [ttw (get-resource state 0)]
        (is (zero? (get-counters (refresh ttw) :power)) "TTW did not gain counters")
        (is (= 1 (count (:scored (get-runner)))) "Runner stole Explodapalooza")
        (is (= 12 (:credit (get-corp))) "Gained 5 credits")
        (run-empty-server state :rd)
        (click-prompt state :corp "Yes")
        (click-prompt state :runner "Steal")
        (is (zero? (get-counters (refresh ttw) :power)) "TTW did not gain counters")
        (is (= 2 (count (:scored (get-runner)))) "Runner stole Explodapalooza")
        (is (= 17 (:credit (get-corp))) "Gained 5 credits"))))

(deftest false-lead
  ;; False Lead
  (do-game
    (new-game {:corp {:deck ["False Lead"]}})
    (play-and-score state "False Lead")
    (is (= 1 (count (:scored (get-corp)))) "Corp should have 1 agenda point")
    (take-credits state :corp)
    (is (= 4 (:click (get-runner))) "Runner should start turn with 4 clicks")
    (card-ability state :corp (get-scored state :corp 0) 0)
    (is (= 2 (:click (get-runner))) "Runner should lose 2 clicks from False Lead")))

(deftest fetal-ai
  ;; Fetal AI
  (do-game
      (new-game {:corp {:deck [(qty "Fetal AI" 3)]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 3) (qty "Quality Time" 3)]}})
      (play-from-hand state :corp "Fetal AI" "New remote")
      (take-credits state :corp 2)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay to steal")
      (is (= 3 (count (:hand (get-runner)))) "Runner took 2 net damage from Fetal AI")
      (is (= 3 (:credit (get-runner))) "Runner paid 2cr to steal Fetal AI")
      (is (= 1 (count (:scored (get-runner)))) "Runner stole Fetal AI")
    (testing "can't afford to steal"
      (do-game
        (new-game {:corp {:deck [(qty "Fetal AI" 3)]}
                   :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 3) (qty "Quality Time" 3)]}})
        (play-from-hand state :corp "Fetal AI" "New remote")
        (take-credits state :corp 2)
        (core/lose state :runner :credit 5)
        (run-empty-server state "Server 1")
        (click-prompt state :runner "No action")
        (is (= 3 (count (:hand (get-runner)))) "Runner took 2 net damage from Fetal AI")
        (is (zero? (count (:scored (get-runner)))) "Runner could not steal Fetal AI")))))

(deftest firmware-updates
  ;; Firmware Updates
  (do-game
    (new-game {:corp {:deck ["Firmware Updates" "Ice Wall"]}})
    (play-and-score state "Firmware Updates")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [fu (get-scored state :corp 0)
          iw (get-ice state :hq 0)]
      (is (= 3 (get-counters (refresh fu) :agenda)) "Firmware Updates should start with 3 agenda counters")
      (rez state :corp iw)
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should start with 0 advancement tokens")
      (card-ability state :corp fu 0)
      (click-card state :corp (refresh iw))
      (is (= 2 (get-counters (refresh fu) :agenda)) "Firmware Updates should now have 2 agenda counters")
      (is (= 1 (get-counters (refresh iw) :advancement)) "Ice Wall should have 1 advancement token"))))

(deftest flower-sermon
  ;; Flower Sermon
  (do-game
      (new-game {:corp {:hand ["Accelerated Beta Test" "Brainstorm" "Chiyashi"
                               "DNA Tracker" "Excalibur" "Fire Wall" "Flower Sermon"]}})
      (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "DNA Tracker" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Excalibur" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Fire Wall" (:hand (get-corp))) :deck)
      (is (= (:title (nth (-> @state :corp :deck) 0)) "Accelerated Beta Test"))
      (is (= (:title (nth (-> @state :corp :deck) 1)) "Brainstorm"))
      (is (= (:title (nth (-> @state :corp :deck) 2)) "Chiyashi"))
      (is (= (:title (nth (-> @state :corp :deck) 3)) "DNA Tracker"))
      (is (= (:title (nth (-> @state :corp :deck) 4)) "Excalibur"))
      (is (= (:title (nth (-> @state :corp :deck) 5)) "Fire Wall"))
      ;; R&D is now from top to bottom: A B C D E F
      (play-and-score state "Flower Sermon")
      (is (= 0 (count (:hand (get-corp)))) "No cards in HQ")
      (let [fs (get-scored state :corp 0)]
        (is (= 5 (get-counters (refresh fs) :agenda)) "5 agenda tokens on Flower Sermon")
        (card-ability state :corp fs 0)
        (is (= 2 (count (:hand (get-corp)))) "Drew 2 cards")
        (click-card state :corp (find-card "Brainstorm" (:hand (get-corp))))
        (is (= "Brainstorm" (:title (first (:deck (get-corp))))) "Brainstorm now on top")
        (is (= 4 (get-counters (refresh fs) :agenda)) "Spent agenda token on Flower Sermon")
        (card-ability state :corp fs 0)
        (is (no-prompt? state :corp) "Can only use once per turn"))))

(deftest flower-sermon-hyoubu-interaction
    ;; Hyoubu interaction
    (do-game
      (new-game {:corp {:id "Hyoubu Institute: Absolute Clarity"
                        :deck [(qty "Hedge Fund" 10)]
                        :hand ["Flower Sermon"]}})
      (play-and-score state "Flower Sermon")
      (let [fs (get-scored state :corp 0)
            corp-credits (:credit (get-corp))]
        (card-ability state :corp fs 0)
        (is (= (+ 1 corp-credits) (:credit (get-corp))) "Gained 1 credit from Hyoubu"))))

(deftest flower-sermon-dbs-interaction
    ;; DBS interaction
    (do-game
      (new-game {:corp {:deck ["Accelerated Beta Test" "Brainstorm" "Chiyashi"
                               "DNA Tracker" "Daily Business Show" "Flower Sermon"]}})
      (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "DNA Tracker" (:hand (get-corp))) :deck)
      (is (= (:title (nth (-> @state :corp :deck) 0)) "Accelerated Beta Test"))
      (is (= (:title (nth (-> @state :corp :deck) 1)) "Brainstorm"))
      (is (= (:title (nth (-> @state :corp :deck) 2)) "Chiyashi"))
      (is (= (:title (nth (-> @state :corp :deck) 3)) "DNA Tracker"))
      ;; R&D is now from top to bottom: A B C D
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (play-and-score state "Flower Sermon")
      (take-credits state :corp)
      (is (= 0 (count (:hand (get-corp)))) "No cards in HQ")
      (let [fs (get-scored state :corp 0)
            dbs (get-content state :remote1 0)]
        (rez state :corp dbs)
        (card-ability state :corp fs 0)
        (is (= (count (:set-aside (get-corp))) 3) "Drew 3 cards with DBS")
        (click-card state :corp (find-card "Chiyashi" (:set-aside (get-corp))))
        (is (= "Chiyashi" (:title (last (:deck (get-corp))))) "Chiyashi at the bottom")
        (click-card state :corp (find-card "Brainstorm" (:hand (get-corp))))
        (is (= "Brainstorm" (:title (first (:deck (get-corp))))) "Brainstorm now on top"))))

(deftest fly-on-the-wall
  ;; Fly on the Wall - give the runner 1 tag
  (do-game
    (new-game {:corp {:deck ["Fly on the Wall"]}})
    (is (zero? (count-tags state)) "Runner starts with no tags")
    (play-and-score state "Fly on the Wall")
    (is (= 1 (count-tags state)) "Runner is tagged")))

(deftest freedom-of-information
  ;; Freedom of Information
  (do-game
    (new-game {:corp {:deck ["Freedom of Information"]}})
    (play-from-hand state :corp "Freedom of Information" "New remote")
    (let [foi (get-content state :remote1 0)]
      (advance state foi 2)
      (score state :corp (refresh foi))
      (is (some? (get-content state :remote1 0))
        "Advancement requirement not yet met, cannot score")
      (gain-tags state :runner 2)
      (score state :corp (refresh foi))
      (is (= 2 (:agenda-point (get-corp))) "Only needed 2 advancements to score"))))

(deftest fujii-asset-retrieval
    (do-game
      (new-game {:corp {:hand [(qty "Fujii Asset Retrieval" 2)]}
                 :runner {:hand [(qty "Sure Gamble" 4)]}})
      (is (changed? [(count (:hand (get-runner))) -2]
            (play-and-score state "Fujii Asset Retrieval"))
          "Runner took 2 damage")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (is (changed? [(count (:hand (get-runner))) -2]
            (click-prompt state :runner "Steal"))
          "Runner took 2 damage")))

(deftest genetic-resequencing
  ;; Genetic Resequencing
  (do-game
    (new-game {:corp {:deck ["Genetic Resequencing" (qty "Braintrust" 2)]}})
    (play-from-hand state :corp "Braintrust" "New remote")
    (play-from-hand state :corp "Braintrust" "New remote")
    (play-from-hand state :corp "Genetic Resequencing" "New remote")
    (let [bt1 (get-content state :remote1 0)
          bt2 (get-content state :remote2 0)
          gr (get-content state :remote3 0)]
      (score-agenda state :corp bt1)
      (let [btscored (get-scored state :corp 0)]
        (is (zero? (get-counters (refresh btscored) :agenda)) "No agenda counters on scored Braintrust")
        (score-agenda state :corp gr)
        (click-card state :corp bt2)
        (is (zero? (get-counters (refresh bt2) :agenda))
            "No agenda counters on installed Braintrust; not a valid target")
        (click-card state :corp btscored)
        (is (= 1 (get-counters (refresh btscored) :agenda))
            "1 agenda counter placed on scored Braintrust")))))

(deftest geothermal-fracking
  ;; Geothermal Fracking
  (do-game
      (new-game {:corp {:deck ["Geothermal Fracking"]}})
      (play-and-score state "Geothermal Fracking")
      (is (= 2 (:click (get-corp))) "Should have 2 clicks left")
      (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
      (is (zero? (count-bad-pub state)) "Should start with 0 bad publicity")
      (let [gf-scored (get-scored state :corp 0)]
        (is (= 2 (get-counters (refresh gf-scored) :agenda)) "Should start with 2 agenda counters")
        (card-ability state :corp gf-scored 0)
        (is (= 1 (:click (get-corp))) "Should have 1 click left")
        (is (= 12 (:credit (get-corp))) "Should gain 7 credits from 5 to 12")
        (is (= 1 (count-bad-pub state)) "Should gain 1 bad publicity"))))

(deftest geothermal-fracking-prevented-bad-publicity-shouldn-t-block-credit-gain
    ;; prevented bad publicity shouldn't block credit gain
    (do-game
      (new-game {:corp {:deck ["Geothermal Fracking" "Broadcast Square"]}})
      (play-and-score state "Geothermal Fracking")
      (is (= 2 (:click (get-corp))) "Should have 2 clicks left")
      (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
      (is (zero? (count-bad-pub state)) "Should start with 0 bad publicity")
      (play-from-hand state :corp "Broadcast Square" "New remote")
      (let [gf-scored (get-scored state :corp 0)
            bs (get-content state :remote2 0)]
        (rez state :corp bs)
        (is (= 2 (get-counters (refresh gf-scored) :agenda)) "Should start with 2 agenda counters")
        (card-ability state :corp gf-scored 0)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (zero? (:click (get-corp))) "Should have 0 click left")
        (is (= 10 (:credit (get-corp))) "Should gain 7 credits from 3 to 10")
        (is (zero? (count-bad-pub state)) "Should gain 0 bad publicity from prevention"))))

(deftest gila-hands-arcology
  ;; Gila Hands Arcology
  (do-game
    (new-game {:corp {:deck ["Gila Hands Arcology"]}})
    (play-and-score state "Gila Hands Arcology")
    (is (= 2 (:click (get-corp))) "Should have 2 clicks left")
    (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
    (core/gain state :corp :click 2)
    (let [gha-scored (get-scored state :corp 0)]
      (card-ability state :corp gha-scored 0)
      (is (= 2 (:click (get-corp))) "Should spend 2 clicks on Gila Hands")
      (is (= 8 (:credit (get-corp))) "Should gain 3 credits from 5 to 8")
      (card-ability state :corp gha-scored 0)
      (is (zero? (:click (get-corp))) "Should spend 2 clicks on Gila Hands")
      (is (= 11 (:credit (get-corp))) "Should gain 3 credits from 8 to 11"))))

(deftest glenn-station
  ;; Glenn Station
  (before-each [state (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                                        :hand ["Glenn Station" "Ice Wall" "Enigma"]}
                                 :runner {:hand ["Political Graffiti"]}})
                _ (play-and-score state "Glenn Station")
                gs-scored (get-scored state :corp 0)]
    (testing "Can host a card"
      (do-game state
        (card-ability state :corp gs-scored 0)
        (click-card state :corp "Ice Wall")
        (is (find-card "Ice Wall" (:hosted (refresh gs-scored))))
        (is (= 1 (count (:hosted (refresh gs-scored)))))))
    (testing "Can't host more than 1 card"
      (do-game state
        (card-ability state :corp gs-scored 0)
        (click-card state :corp "Ice Wall")
        (card-ability state :corp gs-scored 0)
        (is (no-prompt? state :corp))))
    (testing "Requires at least 1 card in hand to host"
      (do-game state
        (starting-hand state :corp [])
        (card-ability state :corp gs-scored 0)
        (is (no-prompt? state :corp))))
    (testing "Can take a hosted card"
      (do-game state
        (card-ability state :corp gs-scored 0)
        (click-card state :corp "Ice Wall")
        (card-ability state :corp gs-scored 1)
        (click-card state :corp "Ice Wall")
        (is (find-card "Ice Wall" (:hand (get-corp))))
        (is (zero? (count (:hosted (refresh gs-scored)))))))
    (testing "Can't take a hosted card if none exist"
      (do-game state
        (card-ability state :corp gs-scored 1)
        (is (no-prompt? state :corp))))
    (testing "Can host a single corp card even if a runner card is hosted"
      (do-game state
        (take-credits state :corp)
        (play-from-hand state :runner "Political Graffiti")
        (run-continue state)
        (click-card state :runner "Glenn Station")
        (is (= 1 (count (:hosted (refresh gs-scored)))))
        (take-credits state :runner)
        (card-ability state :corp (refresh gs-scored) 0)
        (is (= "Choose a card to host" (:msg (get-prompt state :corp))))
        (click-card state :corp "Enigma")
        (is (find-card "Enigma" (:hosted (refresh gs-scored))))))
    (testing "Can't take a card if only a runner card is hosted"
      (do-game state
        (take-credits state :corp)
        (play-from-hand state :runner "Political Graffiti")
        (run-continue state)
        (click-card state :runner "Glenn Station")
        (take-credits state :runner)
        (card-ability state :corp (refresh gs-scored) 1)
        (is (no-prompt? state :corp))))
    (testing "Can take a hosted card even if a runner card is hosted"
      (do-game state
        (take-credits state :corp)
        (play-from-hand state :runner "Political Graffiti")
        (run-continue state)
        (click-card state :runner "Glenn Station")
        (take-credits state :runner)
        (card-ability state :corp (refresh gs-scored) 0)
        (click-card state :corp "Enigma")
        (card-ability state :corp (refresh gs-scored) 1)
        (is (= "Choose a hosted card" (:msg (get-prompt state :corp))))
        (click-card state :corp "Enigma")
        (is (find-card "Enigma" (:hand (get-corp))))))))

(deftest global-food-initiative
  ;; Global Food Initiative
  (do-game
    (new-game {:corp {:deck [(qty "Global Food Initiative" 2)]}})
    (testing "Corp scores"
      (is (zero? (:agenda-point (get-runner))) "Runner should start with 0 agenda points")
      (is (zero? (:agenda-point (get-corp))) "Corp should start with 0 agenda points")
      (play-and-score state "Global Food Initiative")
      (is (= 3 (:agenda-point (get-corp))) "Corp should gain 3 agenda points"))
    (testing "Runner steals"
      (play-from-hand state :corp "Global Food Initiative" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote2)
      (click-prompt state :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Runner should gain 2 agenda points, not 3"))))

(deftest government-contracts
  ;; Government Contracts
  (do-game
    (new-game {:corp {:deck ["Government Contracts"]}})
    (play-and-score state "Government Contracts")
    (is (= 2 (:click (get-corp))))
    (card-ability state :corp (get-scored state :corp 0) 0)
    (is (zero? (:click (get-corp))) "Spent 2 clicks")
    (is (= 9 (:credit (get-corp))) "Gained 4 credits")))

(deftest government-takeover
  ;; Government Takeover
  (do-game
    (new-game {:corp {:deck ["Government Takeover"]}})
    (play-and-score state "Government Takeover")
    (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
    (let [gt-scored (get-scored state :corp 0)]
      (card-ability state :corp gt-scored 0)
      (is (= 8 (:credit (get-corp))) "Should gain 3 credits from 5 to 8"))))

(deftest graft
  ;; Graft
  (letfn [(graft-test [[number-of-picks deck-size]]
            (let [cards ["Ice Wall" "Fire Wall" "Orion"]]
              (do-game
                (new-game {:corp {:deck ["Graft" "Ice Wall"
                                         "Fire Wall" "Orion"]}})
                (starting-hand state :corp ["Graft"])
                (play-and-score state "Graft")
                (dotimes [current-pick number-of-picks]
                  (click-prompt state :corp (find-card (nth cards current-pick) (:deck (get-corp)))))
                (is (= number-of-picks (count (:hand (get-corp)))))
                (is (= deck-size (count (:deck (get-corp))))))))]
    (doall (map graft-test
                [[0 3]
                 [1 2]
                 [2 1]
                 [3 0]]))))

(deftest hades-fragment
  ;; Hades Fragment
  (do-game
    (new-game {:corp {:deck ["Hades Fragment" (qty "Hedge Fund" 2)]}})
    (starting-hand state :corp ["Hades Fragment"])
    (play-and-score state "Hades Fragment")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 1 (count (:hand (get-corp)))) "Corp should have no opportunity to use Hades Shard")
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :discard)
    (take-credits state :corp)
    (take-credits state :runner)
    (let [hf-scored (get-scored state :corp 0)]
      (card-ability state :corp hf-scored 0)
      (click-card state :corp (find-card "Hedge Fund" (:discard (get-corp))))
      (is (= 2 (count (:deck (get-corp)))) "R&D should have 2 cards in it after Hades Fragment use"))))

(deftest helium-3-deposit
  ;; Helium-3 Deposit
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Helium-3 Deposit"
                             "Chief Slee"
                             "Ice Wall"]}})
    (play-from-hand state :corp "Chief Slee" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (let [cs (get-content state :remote1 0)
          iw (get-ice state :hq 0)]
      (is (zero? (get-counters (refresh cs) :power)) "Chief Slee should start with 0 power counters")
      (rez state :corp iw)
      (rez state :corp cs)
      (run-on state "HQ")
      (run-continue state)
      (fire-subs state iw)
      (is (= 1 (get-counters (refresh cs) :power)) "Chief Slee should gain 1 power counter")
      (take-credits state :runner)
      (play-and-score state "Helium-3 Deposit")
      (click-prompt state :corp "2")
      (click-card state :corp cs)
      (is (= 3 (get-counters (refresh cs) :power)) "Chief Slee should gain 2 power counters from 1 to 3"))))

(deftest high-risk-investment
  ;; High-Risk Investment
  (do-game
    (new-game {:corp {:deck ["High-Risk Investment"]}})
    (play-and-score state "High-Risk Investment")
    (let [hri-scored (get-scored state :corp 0)]
      (is (= 1 (get-counters (refresh hri-scored) :agenda)) "Has 1 agenda counter")
      (take-credits state :corp)
      (is (= 7 (:credit (get-corp))))
      (take-credits state :runner)
      (is (= 9 (:credit (get-runner))))
      (card-ability state :corp hri-scored 0)
      (is (= 16 (:credit (get-corp))) "Gained 9 credits")
      (is (= 2 (:click (get-corp))) "Spent 1 click")
      (is (zero? (get-counters (refresh hri-scored) :agenda)) "Spent agenda counter"))))

(deftest hollywood-renovation
  ;; Hollywood Renovation
  (do-game
    (new-game {:corp {:deck ["Hollywood Renovation" "Ice Wall"]}})
    (core/gain state :corp :click 10 :credit 10)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Hollywood Renovation" "New remote")
    (let [hr (get-content state :remote1 0)
          iw (get-ice state :hq 0)]
      (is (zero? (get-counters (refresh hr) :advancement)) "Hollywood Renovation should start with 0 advancement tokens")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should start with 0 advancement tokens")
      (dotimes [_ 5]
        (advance state (refresh hr))
        (click-card state :corp (refresh iw)))
      (is (= 5 (get-counters (refresh hr) :advancement)) "Hollywood Renovation should gain 5 advancement tokens")
      (is (= 5 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 5 advancement tokens")
      (advance state (refresh hr))
      (click-card state :corp (refresh iw))
      (is (= 6 (get-counters (refresh hr) :advancement)) "Hollywood Renovation should gain 1 from 5 to 6 advancement tokens")
      (is (= 7 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 2 from 5 to 7 advancement tokens"))))

(deftest hostile-takeover
  ;; Hostile Takeover
  (do-game
    (new-game {:corp {:deck ["Hostile Takeover"]}})
    (play-and-score state "Hostile Takeover")
    (is (= 12 (:credit (get-corp))) "Gain 7 credits")
    (is (= 1 (count-bad-pub state)) "Take 1 bad publicity")))

(deftest house-of-knives
  ;; House of Knives
  (do-game
    (new-game {:corp {:deck ["House of Knives"]}})
    (play-and-score state "House of Knives")
    (let [hok-scored (get-scored state :corp 0)]
      (is (= 3 (get-counters (refresh hok-scored) :agenda)) "House of Knives should start with 3 counters")
      (take-credits state :corp)
      (run-on state "R&D")
      (is (= :movement (:phase (:run @state))) "In Movement phase before Success")
      (card-ability state :corp hok-scored 0)
      (is (= 1 (count (:discard (get-runner)))) "Runner should pay 1 net damage")
      (run-continue state)
      (run-on state "R&D")
      (is (= :movement (:phase (:run @state))) "In Movement phase before Success")
      (card-ability state :corp hok-scored 0)
      (card-ability state :corp hok-scored 0)
      (is (= 2 (count (:discard (get-runner)))) "Runner should pay 1 net damage"))))

(deftest hybrid-release
    ;; Hybrid Release
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Hybrid Release" (qty "Hansei Review" 2) "PAD Campaign" "Hedge Fund"]
                        :discard ["Obokata Protocol"]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (core/gain state :corp :click 2)
      (play-from-hand state :corp "Hansei Review")
      (click-card state :corp "PAD Campaign")
      (play-from-hand state :corp "Hansei Review")
      (click-card state :corp "Hedge Fund")
      (play-and-score state "Hybrid Release")
      (click-prompt state :corp "Sportsmetal: Go Big or Go Home")
      (click-prompt state :corp "Gain 2 [Credits]")
      (click-card state :corp (find-card "Obokata Protocol" (:discard (get-corp))))
      (is (= "Choose a facedown card in Archives to install" (:msg (prompt-map :corp))) "Cannot select faceup cards in Archives")
      (click-card state :corp (find-card "Hedge Fund" (:discard (get-corp))))
      (is (= "Choose a facedown card in Archives to install" (:msg (prompt-map :corp))) "Cannot install operations")
      (click-card state :corp (find-card "PAD Campaign" (:discard (get-corp))))
      (click-prompt state :corp "New remote")
      (is (no-prompt? state :runner))
      (is (= "PAD Campaign" (:title (get-content state :remote2 0))) "Installed PAD Campaign in remote")))

(deftest hybrid-release-no-prompt-when-no-facedown-card-in-archives
    ;; Hybrid Release - skip prompt when all cards in Archives are faceup
    (do-game
      (new-game {:corp {:hand ["Hybrid Release"]
                        :discard ["Ice Wall"]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (take-credits state :runner)
      (play-and-score state "Hybrid Release")
      (is (no-prompt? state :corp))))

(deftest hyperloop-extension-score
    ;; Score
    (do-game
      (new-game {:corp {:deck ["Hyperloop Extension"]}})
      (play-from-hand state :corp "Hyperloop Extension" "New remote")
      (is (= 5 (:credit (get-corp))) "Corp starts with 5 credits")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 8 (:credit (get-corp))) "Corp gains 3 credits")))

(deftest hyperloop-extension-steal
    ;; Steal
    (do-game
      (new-game {:corp {:deck ["Hyperloop Extension"]}})
      (play-from-hand state :corp "Hyperloop Extension" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
      (click-prompt state :runner "Steal")
      (is (= 10 (:credit (get-corp))) "Corp gains 3 credits")))

(deftest ikawah-project
  ;; Ikawah Project
  (do-game
      (new-game {:corp {:deck ["Ikawah Project"]}})
      (play-from-hand state :corp "Ikawah Project" "New remote")
      (testing "No credits"
        (take-credits state :corp)
        (core/lose state :runner :credit (:credit (get-runner)) :click 3)
        (run-empty-server state :remote1)
        (click-prompt state :runner "No action")
        (is (zero? (:credit (get-runner))) "Runner couldn't afford to steal, so no credits spent")
        (is (zero? (count (:scored (get-runner)))) "Runner could not steal Ikawah Project"))
      (testing "No clicks"
        (take-credits state :runner)
        (take-credits state :corp)
        (core/lose state :runner :credit (:credit (get-runner)) :click 3)
        (run-empty-server state :remote1)
        (click-prompt state :runner "No action")
        (is (zero? (:click (get-runner))) "Runner couldn't afford to steal, so no clicks spent")
        (is (zero? (count (:scored (get-runner)))) "Runner could not steal Ikawah Project"))
      (testing "Enough of both"
        (take-credits state :runner)
        (take-credits state :corp)
        (core/lose state :runner :credit (:credit (get-runner)) :click (:click (get-runner)))
        (core/gain state :runner :credit 5 :click 4)
        (is (= 5 (:credit (get-runner))) "Runner should be reset to 5 credits")
        (is (= 4 (:click (get-runner))) "Runner should be reset to 4 clicks")
        (run-empty-server state :remote1)
        (click-prompt state :runner "Pay to steal")
        (is (= 2 (:click (get-runner))) "Runner should lose 1 click to steal")
        (is (= 3 (:credit (get-runner))) "Runner should lose 2 credits to steal")
        (is (= 3 (:agenda-point (get-runner))))
        (is (= 1 (count (:scored (get-runner)))) "Runner should steal Ikawah Project"))))

(deftest ikawah-project-not-stealing
    ;; do not reveal when the Runner does not steal from R&D
    ;; Not stealing
    (do-game
      (new-game {:corp {:deck [(qty "Ikawah Project" 2)]}})
      (take-credits state :corp)
      (starting-hand state :corp ["Ikawah Project"])
      (run-empty-server state "R&D")
      (click-prompt state :runner "No action")
      (is (not (last-log-contains? state "Ikawah Project")) "Ikawah Project should not be mentioned")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (last-log-contains? state "Ikawah Project") "Ikawah Project should be mentioned")))

(deftest illicit-sales
  ;; Illicit Sales
  (letfn [(illicit-sales-test [[starting-bp answer credits-gained]]
            (testing (str "starting with " starting-bp " and answering " answer " and gaining " credits-gained)
              (do-game
                (new-game {:corp {:deck ["Illicit Sales"]}})
                (let [credits (:credit (get-corp))]
                  (core/gain state :corp :bad-publicity starting-bp)
                  (play-and-score state "Illicit Sales")
                  (click-prompt state :corp answer)
                  (is (= (:credit (get-corp)) (+ credits credits-gained)))))))]
    (run! illicit-sales-test
          [[0 "No" 0]
           [0 "Yes" 3]
           [1 "No" 3]
           [1 "Yes" 6]
           [2 "No" 6]
           [2 "Yes" 9]
           [3 "No" 9]
           [3 "Yes" 12]])))

(deftest improved-protein-source
  ;; Improved Protein Source
  (do-game
    (new-game {:corp {:deck [(qty "Improved Protein Source" 2)]}})
    (is (= 5 (:credit (get-runner))) "Runner starts with 5 credits")
    (play-and-score state "Improved Protein Source")
    (is (= 9 (:credit (get-runner))) "Runner should gain 4 credits from Corp scoring")
    (play-from-hand state :corp "Improved Protein Source" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote2)
    (click-prompt state :runner "Steal")
    (is (= 13 (:credit (get-runner))) "Runner should gain 4 credits from Corp scoring")))

(deftest improved-tracers
  ;; Improved Tracers
  (do-game
    (new-game {:corp {:deck ["Improved Tracers" "News Hound" "Information Overload"]}})
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "News Hound" "HQ")
    (play-from-hand state :corp "Information Overload" "R&D")
    (let [nh (get-ice state :hq 0)
          io (get-ice state :rd 0)]
      (rez state :corp nh)
      (rez state :corp io)
      (is (= 4 (get-strength (refresh nh))) "Should start with base strength of 4")
      (is (= 7 (:credit (get-corp))) "Should have 7 credits after rez")
      (play-and-score state "Improved Tracers")
      (is (= 5 (get-strength (refresh nh))) "Should gain 1 strength from 4 to 5")
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (fire-subs state nh)
      (is (= 1 (:bonus (get-prompt state :corp))) "Should gain 1 bonus trace strength")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (count-tags state)))
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state "HQ")
      (run-continue state)
      (fire-subs state nh)
      (is (= 1 (:bonus (get-prompt state :corp)))
          "Should gain only 1 bonus trace strength regardless of number of runs in a turn")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 2 (count-tags state)))
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state "R&D")
      (run-continue state)
      (fire-subs state io)
      (is (zero? (:bonus (get-prompt state :corp))) "Should gain 0 bonus trace strength, as it's an encounter ability"))))

(deftest jumon
  ;; Jumon
  (do-game
    (new-game {:corp {:deck ["Jumon" "Ice Wall" "Crisium Grid" "Project Atlas"]}})
    (play-and-score state "Jumon")
    (play-from-hand state :corp "Ice Wall" "New remote")
    (play-from-hand state :corp "Project Atlas" "Server 2")
    (end-turn state :corp)
    (let [pa (get-content state :remote2 0)
          iw (get-ice state :remote2 0)]
      (is (zero? (get-counters (refresh pa) :advancement)) "Project Atlas starts with no counters")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall starts with no counters")
      (click-card state :corp iw)
      (click-card state :corp pa)
      (is (= 2 (get-counters (refresh pa) :advancement)) "Project Atlas gains 2 counters")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall doesn't gain any counters")
      (start-turn state :runner)
      (take-credits state :runner)
      (play-from-hand state :corp "Crisium Grid" "Server 2")
      (let [cg (get-content state :remote2 1)]
        (is (zero? (get-counters (refresh cg) :advancement)) "Crisium Grid starts with no counters")
        (end-turn state :corp)
        (click-card state :corp cg)
        (is (= 2 (get-counters (refresh cg) :advancement)) "Crisium Grid gains 2 counters")))))

(deftest labyrinthine-servers
  ;; Labyrinthine Servers
  (do-game
    (new-game {:corp {:deck [(qty "Labyrinthine Servers" 2)]}})
    (play-and-score state "Labyrinthine Servers")
    (play-and-score state "Labyrinthine Servers")
    (take-credits state :corp)
    (let [ls1 (get-scored state :corp 0)
          ls2 (get-scored state :corp 1)]
      (is (= 2 (get-counters (refresh ls1) :power)))
      (is (= 2 (get-counters (refresh ls2) :power)))
      (testing "Don't use token"
        (run-on state "HQ")
        (run-jack-out state)
        (is (:run @state) "Jack out prevent prompt")
        (click-prompt state :corp "Done")
        (is (not (:run @state)) "Corp does not prevent the jack out, run ends"))
      (testing "Use token"
        (run-on state "HQ")
        (run-jack-out state)
        (card-ability state :corp ls1 0)
        (card-ability state :corp ls2 0)
        (card-ability state :corp ls1 0)
        (click-prompt state :corp "Done")
        (is (:run @state) "Jack out prevented, run is still ongoing")
        (is (get-in @state [:run :cannot-jack-out]) "Cannot jack out flag is in effect")
        (run-continue state)
        (is (not (:run @state))))
      (testing "one Labyrinthine is empty but the other still has one token, ensure prompt still occurs"
        (is (zero? (get-counters (refresh ls1) :power)))
        (is (= 1 (get-counters (refresh ls2) :power)))
        (run-on state "HQ")
        (run-jack-out state)
        (is (:run @state))
        (card-ability state :corp ls2 0)
        (click-prompt state :corp "Done")
        (is (get-in @state [:run :cannot-jack-out]))
        (run-continue state)
        (is (not (:run @state))))
      (testing "No more tokens"
        (run-on state "HQ")
        (run-jack-out state)
        (is (not (:run @state)) "No jack out prevent prompt")))))

(deftest kimberlite-no-target
  (do-game
    (new-game {:corp {:hand ["Kimberlite Field" "Rashida Jaheem"]}})
    (play-from-hand state :corp "Rashida Jaheem" "New remote")
    (play-and-score state "Kimberlite Field")
    (is (no-prompt? state :corp))))

(deftest kimberlite-standard-functionality
  ;; Kimberlite Field trashes runner card based on printed costs of each card
  (do-game
    (new-game {:corp {:hand ["Kimberlite Field" "Echo Chamber" "Breaker Bay Grid" "TechnoCo"]}
               :runner {:hand ["Amina" "Paperclip"] :credits 15}})
    (play-from-hand state :corp "Echo Chamber" "New remote")
    (play-from-hand state :corp "Breaker Bay Grid" "Server 1")
    (play-from-hand state :corp "TechnoCo" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Amina")
    (play-from-hand state :runner "Paperclip")
    (take-credits state :runner)
    (rez state :corp (get-content state :remote1 1)) ;; lowers Echo Chamber cost - KF should use printed
    (rez state :corp (get-content state :remote1 0))
    (rez state :corp (get-content state :remote2 0)) ;; increases Amina + Paperclip cost - KF should use printed
    (play-and-score state "Kimberlite Field")
    (click-card state :corp "Echo Chamber")
    (click-card state :corp "Amina")
    (is (= 0 (count (:discard (get-runner)))) "amina not trashed")
    (click-card state :corp "Paperclip")
    (is (= 1 (count (:discard (get-runner)))) "clippy trashed")
    (is (no-prompt? state :corp))))

(deftest kingmaking
  (do-game
    (new-game {:corp {:hand ["Kingmaking"]
                      :deck ["House of Knives" "Project Atlas" "Hedge Fund"]}})
    (is (changed? [(count (:deck (get-corp))) -3
                   (count (:hand (get-corp))) 1
                   (:agenda-point (get-corp)) 3]
                  (play-and-score state "Kingmaking")
                  (click-prompt state :corp "3")
                  (click-card state :corp "Project Atlas")
                  (is (not (no-prompt? state :corp)) "Couldn't choose 2-points agenda")
                  (click-card state :corp "House of Knives"))
        "Corp drew 3 cards (2 of which moved to the score area)")
    (is (zero? (get-counters (get-scored state :runner 1) :agenda)) "House of Knives should have 0 agenda counters")))

(deftest kingmaking-draw-n
  (dotimes [n 4]
    (do-game
      (new-game {:corp {:hand ["Kingmaking" "Hedge Fund"]
                        :deck ["House of Knives" "Project Atlas" "Hedge Fund"]}})
      (is (changed? [(count (:deck (get-corp))) (- n)
                     (count (:hand (get-corp))) (- n 1)]
                    (play-and-score state "Kingmaking")
                    (click-prompt state :corp (str n))
                    (click-prompt state :corp "Done"))
          (str "Corp drew " n " cards")))))


(deftest license-acquisition
  ;; License Acquisition
  (do-game
    (new-game {:corp {:deck [(qty "License Acquisition" 4)
                             "Adonis Campaign" "Eve Campaign"
                             "Strongbox" "Corporate Troubleshooter"]}})
    (testing "Set up"
      (starting-hand state :corp ["License Acquisition" "License Acquisition" "License Acquisition" "License Acquisition"
                                  "Adonis Campaign" "Strongbox"])
      (core/move state :corp (find-card "Eve Campaign" (:deck (get-corp))) :discard)
      (core/move state :corp (find-card "Corporate Troubleshooter" (:deck (get-corp))) :discard)
      (core/gain state :corp :click 4))
    (testing "Asset & HQ"
      (play-and-score state "License Acquisition")
      (click-card state :corp (find-card "Adonis Campaign" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (is (some? (get-content state :remote2 0))))
    (testing "Upgrade & HQ"
      (play-and-score state "License Acquisition")
      (click-card state :corp (find-card "Strongbox" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (is (some? (get-content state :remote4 0))))
    (testing "Asset & Archives"
      (play-and-score state "License Acquisition")
      (click-card state :corp (find-card "Eve Campaign" (:discard (get-corp))))
      (click-prompt state :corp "New remote")
      (is (some? (get-content state :remote6 0))))
    (testing "Upgrade & Archives"
      (play-and-score state "License Acquisition")
      (click-card state :corp (find-card "Corporate Troubleshooter" (:discard (get-corp))))
      (click-prompt state :corp "New remote")
      (is (some? (get-content state :remote8 0))))))

(deftest lightning-laboratory
  (do-game
    (new-game {:corp {:hand ["Lightning Laboratory" "Archer" "Bloop" "Rime"]}})
    (core/gain state :corp :click 1)
    (play-and-score state "Lightning Laboratory")
    (play-from-hand state :corp "Archer" "HQ")
    (play-from-hand state :corp "Bloop" "HQ")
    (play-from-hand state :corp "Rime" "HQ")
    (let [ll (get-scored state :corp 0)
          archer (get-ice state :hq 0)
          bloop (get-ice state :hq 1)
          rime (get-ice state :hq 2)]
      (is (= 1 (get-counters (refresh ll) :agenda)) "Lightning Laboratory should have 1 agenda counter")
      (take-credits state :corp)
      (run-on state :hq)
      (click-prompt state :corp "Yes")
      (click-card state :corp archer)
      (click-card state :corp bloop)
      (is (rezzed? (refresh archer)))
      (is (rezzed? (refresh bloop)))
      (rez state :corp rime)
      (run-continue state)
      (run-continue state)
      (run-jack-out state)
      (is (no-prompt? state :corp) "No Lightning Laboratory prompt at the end of the run")
      (take-credits state :runner)
      (is (not (no-prompt? state :corp)) "Corp has Lightning Laboratory prompt")
      (click-card state :corp archer)
      (click-card state :corp rime)
      (is (not (rezzed? (refresh archer))))
      (is (not (rezzed? (refresh rime))))
      (is (rezzed? (refresh bloop)))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (no-prompt? state :corp) "no repeat prompt"))))

(deftest longevity-serum-basic-behavior
    ;; Basic behavior
    (do-game
      (new-game {:corp {:hand ["Longevity Serum" "Hedge Fund" "IPO" "Afshar"]
                        :discard ["Ice Wall" "Fire Wall" "Hostile Takeover" "Prisec"]}})
      (play-and-score state "Longevity Serum")
      (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp))))
      (click-card state :corp (find-card "IPO" (:hand (get-corp))))
      (is (= 4 (count (:discard (get-corp)))))
      (click-prompt state :corp "Done")
      (is (= 6 (count (:discard (get-corp)))) "Corp trashes two cards from HQ")
      (click-card state :corp "Ice Wall")
      (click-card state :corp "Fire Wall")
      (click-card state :corp "Prisec")
      (is (find-card "Fire Wall" (:deck (get-corp))))
      (is (find-card "Ice Wall" (:deck (get-corp))))
      (is (find-card "Prisec" (:deck (get-corp))))))

(deftest longevity-serum-no-cards-selected
    ;; No cards selected
    (do-game
      (new-game {:corp {:hand ["Longevity Serum" "Hedge Fund" "IPO" "Afshar"]
                        :discard ["Ice Wall" "Fire Wall" "Hostile Takeover" "Prisec"]}})
      (play-and-score state "Longevity Serum")
      (is (= 4 (count (:discard (get-corp)))))
      (click-prompt state :corp "Done")
      (is (= 4 (count (:discard (get-corp)))) "Corp trashes no cards from HQ")
      (click-prompt state :corp "Done")
      (is (= 4 (count (:discard (get-corp)))) "Corp shuffles no cards from discard")))

(deftest longevity-serum-no-cards-trashed-2-shuffled
    ;; No cards trashed, 2 shuffled
    (do-game
      (new-game {:corp {:hand ["Longevity Serum" "Hedge Fund" "IPO" "Afshar"]
                        :discard ["Ice Wall" "Fire Wall" "Hostile Takeover" "Prisec"]}})
      (play-and-score state "Longevity Serum")
      (is (= 4 (count (:discard (get-corp)))))
      (click-prompt state :corp "Done")
      (is (= 4 (count (:discard (get-corp)))) "Corp trashes no cards from HQ")
      (click-card state :corp "Ice Wall")
      (click-card state :corp "Fire Wall")
      (click-prompt state :corp "Done")
      (is (= 2 (count (:discard (get-corp)))) "Corp shuffles 2 cards from discard")))

(deftest longevity-serum-effect-fully-completes-before-runner-abilities-trigger-5992
    ;; Effect fully completes before Runner abilities trigger #5992
    (do-game
      (new-game {:corp {:hand ["Longevity Serum" "Hedge Fund" "IPO" "Afshar" "Enigma"]
                        :discard ["Ice Wall" "Fire Wall" "Hostile Takeover" "Prisec"]}
                 :runner {:id "Tāo Salonga: Telepresence Magician"}})
      (play-from-hand state :corp "Afshar" "HQ")
      (play-from-hand state :corp "Enigma" "R&D")
      (play-and-score state "Longevity Serum")
      (is (prompt-is-type? state :runner :waiting))
      (is (= 4 (count (:discard (get-corp)))))
      (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp))))
      (click-card state :corp (find-card "IPO" (:hand (get-corp))))
      (is (= 6 (count (:discard (get-corp)))) "Corp trashes two cards from HQ")
      (is (prompt-is-type? state :runner :waiting))
      (click-card state :corp "Ice Wall")
      (click-card state :corp "Fire Wall")
      (click-card state :corp "Prisec")
      (is (find-card "Fire Wall" (:deck (get-corp))))
      (is (find-card "Ice Wall" (:deck (get-corp))))
      (is (find-card "Prisec" (:deck (get-corp))))
      (is (prompt-is-type? state :corp :waiting))
      (click-prompt state :runner "No")))

(deftest luminal-transubstantiation
  ;; Luminal Transubstantiation
  (do-game
   (new-game {:corp {:deck ["Luminal Transubstantiation" "Project Vitruvius"]}})
   (play-from-hand state :corp "Luminal Transubstantiation" "New remote")
   (core/add-prop state :corp (get-content state :remote1 0) :advance-counter 3)
   (is (changed? [(:click (get-corp)) 3]
         (score state :corp (get-content state :remote1 0)))
       "Corp gains 3 clicks from Luminal Transubstantiation")
   (is (find-card "Luminal Transubstantiation" (:scored (get-corp))))
   (is (= 1 (count (:scored (get-corp)))))
   (play-from-hand state :corp "Project Vitruvius" "New remote")
   (core/add-prop state :corp (get-content state :remote2 0) :advance-counter 3)
   (score state :corp (get-content state :remote2 0))
   (is (= 1 (count (:scored (get-corp)))) "Cannot be scored because Luminal Transubstantiation")))

(deftest mandatory-seed-replacement
  ;; Mandatory Seed Replacement
  (do-game
    (new-game {:corp {:deck ["Mandatory Seed Replacement"
                             "Ice Wall" "Fire Wall"
                             "Kakugo" "Chum"
                             "RSVP" "Sensei"]
                      :credits 100}})
    (click-draw state :corp)
    (core/gain state :corp :click 10 :credit 10)
    (play-from-hand state :corp "Ice Wall" "Archives")
    (play-from-hand state :corp "Fire Wall" "R&D")
    (play-from-hand state :corp "Kakugo" "HQ")
    (play-from-hand state :corp "Chum" "Archives")
    (play-from-hand state :corp "RSVP" "R&D")
    (play-from-hand state :corp "Sensei" "HQ")
    (let [iw (get-ice state :archives 0)
          fw (get-ice state :rd 0)
          kk (get-ice state :hq 0)
          ch (get-ice state :archives 1)
          rs (get-ice state :rd 1)
          sn (get-ice state :hq 1)]
      (rez state :corp iw)
      (rez state :corp fw)
      (rez state :corp kk)
      (rez state :corp ch)
      (rez state :corp rs)
      (rez state :corp sn)
      (play-and-score state "Mandatory Seed Replacement")
      (click-card state :corp (refresh iw))
      (click-card state :corp (refresh fw))
      (click-card state :corp (refresh kk))
      (click-card state :corp (refresh ch))
      (click-card state :corp (refresh rs))
      (click-card state :corp (refresh sn)))))

(deftest mandatory-upgrades-gain-an-additional-click
    ;; Gain an additional click
    (do-game
      (new-game {:corp {:deck ["Mandatory Upgrades"
                               "Melange Mining Corp."]}})
      (play-and-score state "Mandatory Upgrades")
      (is (= 2 (:agenda-point (get-corp))))
      (play-from-hand state :corp "Melange Mining Corp." "New remote")
      (let [mmc (get-content state :remote2 0)]
        (rez state :corp mmc)
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= 4 (:click (get-corp))))
        (card-ability state :corp mmc 0)
        (is (= 1 (:click (get-corp)))))))

(deftest mandatory-upgrades-lose-additional-click-if-sacrificed
    ;; Lose additional click if sacrificed
    (do-game
      (new-game {:corp {:deck ["Mandatory Upgrades"
                               "Archer"]}})
      (play-and-score state "Mandatory Upgrades")
      (is (= 2 (:agenda-point (get-corp))))
      (play-from-hand state :corp "Archer" "HQ")
      (take-credits state :corp)
      (take-credits state :runner)
      (let [arc (get-ice state :hq 0)
            mu (get-scored state :corp 0)]
        (is (= 4 (:click (get-corp))) "Corp should start turn with 4 clicks")
        (rez state :corp arc {:expect-rez false})
        (click-card state :corp (refresh mu))
        (is (= 3 (:click (get-corp))) "Corp should lose 1 click on agenda sacrifice"))))

(deftest market-research
  ;; Market Research
  (do-game
    (new-game {:corp {:deck [(qty "Market Research" 2)]}})
    (testing "Runner is not tagged"
      (play-and-score state "Market Research")
      (is (= 2 (:agenda-point (get-corp))) "Only 4 advancements: scored for standard 2 points"))
    (testing "Runner is tagged"
      (gain-tags state :runner 1)
      (play-and-score state "Market Research")
      (is (= 5 (:agenda-point (get-corp))) "5 advancements: scored for 3 points"))))

(deftest medical-breakthrough
  ;; Medical Breakthrough
  (do-game
    (new-game {:corp {:deck [(qty "Medical Breakthrough" 3) (qty "Hedge Fund" 3)]}})
    (play-from-hand state :corp "Medical Breakthrough" "New remote")
    (play-from-hand state :corp "Medical Breakthrough" "New remote")
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (let [mb2 (get-content state :remote2 0)]
      (advance state mb2 3)
      (score state :corp (refresh mb2))
      (is (= 2 (:agenda-point (get-corp))) "Only needed 3 advancements to score"))
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Medical Breakthrough" "New remote")
    (let [mb3 (get-content state :remote3 0)]
      (advance state mb3 2)
      (score state :corp (refresh mb3))
      (is (= 4 (:agenda-point (get-corp))) "Only needed 2 advancements to score"))))

(deftest megaprix-qualifier-the-first-scored-megaprix-qualifier-doesn-t-get-a-counter-and-is-worth-1-point
    ;; The first scored Megaprix Qualifier doesn't get a counter, and is worth 1 point
    (do-game
      (new-game {:corp {:hand ["Megaprix Qualifier"]}})
      (play-and-score state "Megaprix Qualifier")
      (let [megaprix-qualifier (first (:scored (get-corp)))]
        (is (zero? (get-counters megaprix-qualifier :agenda)) "Has 0 counters"))
      (is (= 1 (:agenda-point (get-corp))) "Is worth 1 agenda point")))

(deftest megaprix-qualifier-a-second-scored-megaprix-qualifier-gets-a-counter-and-is-worth-2-points
    ;; A second scored Megaprix Qualifier gets a counter, and is worth 2 points
    (do-game
      (new-game {:corp {:hand [(qty "Megaprix Qualifier" 2)]}})
      (play-and-score state "Megaprix Qualifier")
      (play-and-score state "Megaprix Qualifier")
      (let [[first-qualifier second-qualifier] (:scored (get-corp))]
        (is (zero? (get-counters first-qualifier :agenda)) "First has 0 counters")
        (is (= 1 (get-counters second-qualifier :agenda))) "Second has 1 counter")
      (is (= 3 (:agenda-point (get-corp))) "Are worth 3 agenda points in total")))

(deftest megaprix-qualifier-stolen-megaprix-qualifiers-are-only-ever-worth-1-point-and-don-t-get-counters
    ;; Stolen Megaprix Qualifiers are only ever worth 1 point, and don't get counters
    (do-game
      (new-game {:corp {:hand [(qty "Megaprix Qualifier" 2)]}})
      (take-credits state :corp)
      (dotimes [_ 2]
        (run-empty-server state "HQ")
        (click-prompt state :runner "Steal"))
      (let [[first-qualifier second-qualifier] (:scored (get-runner))]
        (is (zero? (get-counters first-qualifier :agenda)) "First has 0 counters")
        (is (zero? (get-counters second-qualifier :agenda)) "Second has 0 counters"))
      (is (= 2 (:agenda-point (get-runner))) "Are worth 2 agenda points in total")))

(deftest megaprix-qualifier-a-megaprix-qualifier-scored-after-the-runner-steals-one-gets-a-counter-and-is-worth-2-points
    ;; A Megaprix Qualifier scored after the runner steals one gets a counter, and is worth 2 points
    (do-game
      (new-game {:corp {:hand [(qty "Megaprix Qualifier" 2)]}})
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (take-credits state :runner)
      (play-and-score state "Megaprix Qualifier")
      (let [first-qualifier (first (:scored (get-runner)))
            second-qualifier (first (:scored (get-corp)))]
        (is (zero? (get-counters first-qualifier :agenda)) "First has 0 counters")
        (is (= 1 (get-counters second-qualifier :agenda)) "Second has 1 counter"))
      (is (= 1 (:agenda-point (get-runner))) "Is worth 1 agenda point to the runner")
      (is (= 2 (:agenda-point (get-corp))) "Is worth 2 agenda points to the corp")))

(deftest megaprix-qualifier-getting-to-7-points-through-a-megaprix-counter-wins-the-game-immediately-5968
    ;; Getting to 7 points through a Megaprix counter wins the game immediately #5968
    (do-game
      (new-game {:corp {:hand [(qty "Megaprix Qualifier" 5)]
                        :deck [(qty "Hedge Fund" 5)]}})
      (core/gain-clicks state :corp 1)
      (play-and-score state "Megaprix Qualifier") ;; 1 point
      (play-and-score state "Megaprix Qualifier") ;; 3 points
      (play-and-score state "Megaprix Qualifier") ;; 5 points
      (play-and-score state "Megaprix Qualifier") ;; 7 points
      (is (= 7 (:agenda-point (get-corp))) "Corp at 7 points")
      (is (= :corp (:winner @state)) "Corp has won")))

(deftest merger
  ;; Merger
  (do-game
    (new-game {:corp {:deck [(qty "Merger" 2)]}})
    (play-and-score state "Merger")
    (is (= 2 (:agenda-point (get-corp))) "Corp should score 2 points")
    (play-from-hand state :corp "Merger" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote2)
    (click-prompt state :runner "Steal")
    (is (= 3 (:agenda-point (get-runner))) "Runner should score 3 points")))

(deftest meteor-mining-when-meteor-mining-is-stolen
    ;; when Meteor Mining is stolen
    (do-game
      (new-game {:corp {:deck ["Meteor Mining"]}})
      (play-from-hand state :corp "Meteor Mining" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Runner should score 2 points")))

(deftest meteor-mining-when-meteor-mining-is-scored
    ;; when Meteor Mining is scored
    (letfn [(meteor-mining-test [[tags num-choices pick creds dmg]]
              (do-game
                (new-game {:corp {:deck ["Meteor Mining"]}
                           :runner {:deck [(qty "Sure Gamble" 7)]}})
                (starting-hand state :runner (repeat 7 "Sure Gamble"))
                (let [credits (:credit (get-corp))
                      grip (count (:hand (get-runner)))]
                  (gain-tags state :runner tags)
                  (play-and-score state "Meteor Mining")
                  (is (= num-choices (count (prompt-buttons :corp))))
                  (click-prompt state :corp pick)
                  (is (= (+ credits creds) (:credit (get-corp)))
                      (str "Corp should have " (+ credits creds) " credits"))
                  (is (= (- grip dmg) (count (:hand (get-runner))))
                      (str "Runner should have " (- grip dmg) " cards in hand")))))]
      (doall (map meteor-mining-test
                  [[0 2 "No action" 0 0]
                   [0 2 "Gain 7 [Credits]" 7 0]
                   [1 2 "No action" 0 0]
                   [1 2 "Gain 7 [Credits]" 7 0]
                   [2 3 "No action" 0 0]
                   [2 3 "Gain 7 [Credits]" 7 0]
                   [2 3 "Do 7 meat damage" 0 7]
                   [3 3 "No action" 0 0]
                   [3 3 "Gain 7 [Credits]" 7 0]
                   [3 3 "Do 7 meat damage" 0 7]]))))

(deftest midnight-3-arcology
  (do-game
    (new-game {:corp {:hand ["Midnight-3 Arcology" (qty "Hedge Fund" 5)]
                      :deck ["NGO Front" "Vanilla" "Chiyashi"]}})
    (is (changed? [(count (:hand (get-corp))) 2]
          (play-and-score state "Midnight-3 Arcology"))
        "drew net 2 when scoring midnight-3 arcology")
    (take-credits state :corp)
    (is (no-prompt? state :corp) "no prompt to discard")
    (is (= 8 (count (:hand (get-corp)))) "8 cards in hand")
    (take-credits state :runner)
    (take-credits state :corp)
    (click-card state :corp "NGO Front")
    (click-card state :corp "Vanilla")
    (click-card state :corp "Chiyashi")
    (is (no-prompt? state :corp) "discards completed")))

(deftest napd-contract
  ;; NAPD Contract
  (do-game
      (new-game {:corp {:deck ["NAPD Contract"]}})
      (play-from-hand state :corp "NAPD Contract" "New remote")
      (let [napd (get-content state :remote1 0)]
        (advance state napd 2)
        (take-credits state :corp)
        (core/lose state :runner :credit 2)
        (run-empty-server state "Server 1")
        (click-prompt state :runner "No action")
        (is (zero? (count (:scored (get-runner)))) "Runner could not steal NAPD Contract")
        (is (= 3 (:credit (get-runner))) "Runner couldn't afford to steal, so no credits spent")
        (take-credits state :runner)
        (core/gain state :corp :bad-publicity 1)
        (advance state napd 2)
        (score state :corp (refresh napd))
        (is (some? (get-content state :remote1 0))
            "Corp can't score with 4 advancements because of BP")
        (advance state napd)
        (score state :corp (refresh napd))
        (is (= 2 (:agenda-point (get-corp))) "Scored NAPD for 2 points after 5 advancements"))))

(deftest napd-contract-scoring-requirement-increases-with-bad-publicity-from-corporate-scandal
    ;; scoring requirement increases with bad publicity from Corporate Scandal
    (do-game
      (new-game {:corp {:deck ["NAPD Contract"]}
                 :runner {:deck ["Corporate Scandal"]}})
      (play-from-hand state :corp "NAPD Contract" "New remote")
      (let [napd (get-content state :remote1 0)]
        (advance state napd 2)
        (take-credits state :corp)
        (play-from-hand state :runner "Corporate Scandal")
        (take-credits state :runner)
        (advance state napd 2)
        (score state :corp (refresh napd))
        (is (some? (get-content state :remote1 0))
            "Corp can't score with 4 advancements because of BP")
        (advance state napd)
        (score state :corp (refresh napd))
        (is (= 2 (:agenda-point (get-corp))) "Scored NAPD for 2 points after 5 advancements"))))

(deftest net-quarantine
  ;; Net Quarantine
  (do-game
    (new-game {:corp {:deck ["Net Quarantine"]}})
    (swap! state assoc-in [:runner :identity :baselink] 1)
    (core/gain state :corp :click 3)
    (play-and-score state "Net Quarantine")
    (let [credits (:credit (get-corp))]
      (is (= credits (:credit (get-corp))) (str "Corp has " credits " credits"))
      (is (= 1 (get-link state)) "Runner has 1 link")
      (trace state 1)
      (click-prompt state :corp "0")
      (is (zero? (:link (get-prompt state :runner))) "Runner has 0 link during first trace")
      (click-prompt state :runner "3")
      (is (= (inc credits) (:credit (get-corp))) "Corp gained a credit from NQ")
      ; second trace of turn - no link reduction
      (trace state 1)
      (click-prompt state :corp "0")
      (is (= 1 (:link (get-prompt state :runner))) "Runner has 1 link during later traces")
      (click-prompt state :runner "2")
      (is (= (+ credits 2) (:credit (get-corp))) "Corp gained a credit from NQ"))))

(deftest new-construction
  ;; New Construction
  (do-game
    (new-game {:corp {:deck ["New Construction" (qty "Commercial Bankers Group" 10)]}})
    (starting-hand state :corp (vec (cons "New Construction" (repeat 10 "Commercial Bankers Group"))))
    (core/gain state :corp :click 10 :credit 10)
    (play-from-hand state :corp "New Construction" "New remote")
    (let [nc (get-content state :remote1 0)]
      (is (zero? (get-counters (refresh nc) :advancement)))
      (dotimes [_ 4]
        (advance state (refresh nc))
        (click-prompt state :corp "Yes")
        (click-card state :corp (find-card "Commercial Bankers Group" (:hand (get-corp)))))
      (is (= 4 (get-counters (refresh nc) :advancement)))
      (is (not= :this-turn (rezzed? (get-content state :remote5 0))))
      (let [credits (:credit (get-corp))]
        (advance state (refresh nc))
        (click-prompt state :corp "Yes")
        (click-card state :corp (find-card "Commercial Bankers Group" (:hand (get-corp))))
        (is (= 5 (get-counters (refresh nc) :advancement)))
        (is (= :this-turn (rezzed? (get-content state :remote6 0))))
        (is (= (dec credits) (:credit (get-corp))))))))

(deftest next-wave-2
  ;; NEXT Wave 2
  (do-game
      (new-game {:corp {:deck [(qty "NEXT Wave 2" 2) "NEXT Bronze"]}})
      (is (zero? (:brain-damage (get-runner))) "Runner should start with 0 core damage")
      (play-from-hand state :corp "NEXT Bronze" "HQ")
      (let [nxbr (get-ice state :hq 0)]
        (rez state :corp nxbr))
      (play-and-score state "NEXT Wave 2")
      (click-prompt state :corp "No")
      (is (zero? (:brain-damage (get-runner))) "Runner should stay at 0 core damage")
      (play-and-score state "NEXT Wave 2")
      (click-prompt state :corp "Yes")
      (is (= 1 (:brain-damage (get-runner))) "Runner should gain 1 core damage")))

(deftest next-wave-2-stealing-doesn-t-do-anything
    ;; Stealing doesn't do anything
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["NEXT Wave 2" "NEXT Bronze"]}})
      (is (zero? (:brain-damage (get-runner))) "Runner should start with 0 core damage")
      (play-from-hand state :corp "NEXT Bronze" "HQ")
      (let [nxbr (get-ice state :hq 0)]
        (rez state :corp nxbr))
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "Steal")
      (is (zero? (:brain-damage (get-runner))) "Runner should still have 0 core damage")))

(deftest nisei-mk-ii
  ;; Nisei MK II - Remove hosted counter to ETR, check this works in 4.3
  (do-game
    (new-game {:corp {:deck ["Nisei MK II"]}})
    (play-and-score state "Nisei MK II")
    (let [scored-nisei (get-scored state :corp 0)]
      (is (= 1 (get-counters (refresh scored-nisei) :agenda)) "Scored Nisei has one counter")
      (take-credits state :corp)
      (run-on state "HQ")
      (is (= :movement (:phase (:run @state))) "In Movement phase before Success")
      (card-ability state :corp (refresh scored-nisei) 0)
      (is (not (:run @state)) "Run ended by using Nisei counter")
      (is (zero? (get-counters (refresh scored-nisei) :agenda)) "Scored Nisei has no counters"))))

(deftest oaktown-renovation
  ;; Oaktown Renovation
  (do-game
    (new-game {:corp {:deck ["Oaktown Renovation" "Shipment from SanSan"]}})
    (core/gain state :corp :click 3)
    (play-from-hand state :corp "Oaktown Renovation" "New remote")
    (let [oak (get-content state :remote1 0)]
      (is (faceup? (refresh oak)) "Oaktown installed face up")
      (advance state oak)
      (is (= 6 (:credit (get-corp))) "Spent 1 credit to advance, gained 2 credits from Oaktown")
      (play-from-hand state :corp "Shipment from SanSan")
      (click-prompt state :corp "2")
      (click-card state :corp oak)
      (is (= 3 (get-counters (refresh oak) :advancement)))
      (is (= 6 (:credit (get-corp))) "No credits gained due to advancements being placed")
      (advance state oak)
      (is (= 7 (:credit (get-corp))) "Spent 1 credit to advance, gained 2 credits from Oaktown")
      (advance state oak)
      (is (= 5 (get-counters (refresh oak) :advancement)))
      (is (= 9 (:credit (get-corp)))
          "Spent 1 credit to advance, gained 3 credits from Oaktown"))))

(deftest obokata-protocol
  ;; Obotaka Protocol
  (do-game
    (new-game {:corp {:id "Jinteki: Personal Evolution"
                      :deck [(qty "Hedge Fund" 5)]
                      :hand ["Obokata Protocol" "Merger" "Hostile Takeover"]}
               :runner {:hand [(qty "Sure Gamble" 6)]}})
    (play-from-hand state :corp "Obokata Protocol" "New remote")
    (take-credits state :corp)
    (run-empty-server state "HQ")
    (click-prompt state :runner "Steal")
    (run-empty-server state "HQ")
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (is (changed? [(count (:discard (get-runner))) 4]
          (click-prompt state :runner "Pay to steal"))
        "Runner paid 4 net damage")
    (is (= :runner (:winner @state)) "Runner wins")
    (is (= "Agenda" (:reason @state)) "Win condition reports agenda points")))

(deftest offworld-office
  ;; Offworld Office
  (do-game
      (new-game {:corp {:hand [(qty "Offworld Office" 2)]}})
      (is (changed? [(:credit (get-corp)) 7]
            (play-and-score state "Offworld Office"))
          "Corp gains 7 credits from Offworld Office")))

(deftest ontological-dependence
  ;; Ontological Dependence
  (do-game
    (new-game {:corp {:hand ["Ontological Dependence"]}})
    (play-from-hand state :corp "Ontological Dependence" "New remote")
    (let [onto (get-content state :remote1 0)]
      (advance state onto 2)
      (score state :corp (refresh onto))
      (is (some? (get-content state :remote1 0))
          "Corp can't score with 2 advancements because of no core damage")
      (damage state :corp :brain 2)
      (is (score state :corp (refresh onto)))
      (is (not (some? (get-content state :remote1 0)))
          "Corp can score with 2 advancements because of 2 core damage"))))

(deftest oracle-thinktank
    (do-game
      (new-game {:corp {:hand [(qty "Oracle Thinktank" 2)]}})
      (play-and-score state "Oracle Thinktank")
      (gain-tags state :runner 1)
      (card-ability state :corp (refresh (first (:scored (get-corp)))) 0)
      (is (= 1 (:agenda-point (get-corp))) "Oracle Thinktank doesn't work from the Corp's score area")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (is (changed? [(count-tags state) 1]
            (click-prompt state :runner "Steal"))
          "Runner took 1 tag")
      (take-credits state :runner)
      (let [ot (first (:scored (get-runner)))]
        (is (= 3 (:click (get-corp))))
        (is (= 1 (count (:abilities (refresh ot)))))
        (is (changed? [(count-tags state) -1]
              (card-ability state :corp (refresh ot) 0))
            "Runner lost 1 tag")
        (is (zero? (:agenda-point (get-runner))))
        (is (zero? (count (:scored (get-runner))))))
      (is (find-card "Oracle Thinktank" (:deck (get-corp))))))

(deftest orbital-superiority
  ;; Orbital Superiority
  (do-game
      (new-game {:corp {:hand [(qty "Orbital Superiority" 2)]}
                 :runner {:hand [(qty "Sure Gamble" 10)]}})
      (is (changed? [(count-tags state) 1]
            (play-and-score state "Orbital Superiority"))
          "Runner takes 1 tag from Orbital Superiority")
      (is (changed? [(count (:hand (get-runner))) -4]
            (play-and-score state "Orbital Superiority"))
          "Runner takes 1 tag from Orbital Superiority")))

(deftest paper-trail
  ;; Paper Trail
  (do-game
    (new-game {:corp {:deck ["Paper Trail"]}
               :runner {:deck ["Aeneas Informant" "Bank Job"
                               "Rosetta 2.0" "Magnum Opus"
                               "Astrolabe"]}})
    (take-credits state :corp)
    (core/gain state :runner :click 10 :credit 10)
    (play-from-hand state :runner "Aeneas Informant")
    (play-from-hand state :runner "Bank Job")
    (play-from-hand state :runner "Rosetta 2.0")
    (play-from-hand state :runner "Magnum Opus")
    (play-from-hand state :runner "Astrolabe")
    (take-credits state :runner)
    (play-and-score state "Paper Trail")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 2 (count (:discard (get-runner)))))
    (is (some? (get-resource state 0)))
    (is (= 1 (count (get-resource state))))
    (is (some? (get-program state 0)))
    (is (some? (get-hardware state 0)))))

(deftest personality-profiles
  ;; Personality Profiles
  (do-game
      (new-game {:corp {:deck ["Personality Profiles"]}
                 :runner {:deck ["Corroder"]
                          :hand ["Self-modifying Code" "Clone Chip" (qty "Patron" 2)]}})
      (play-and-score state "Personality Profiles")
      (take-credits state :corp)
      (play-from-hand state :runner "Self-modifying Code")
      (play-from-hand state :runner "Clone Chip")
      (let [smc (get-program state 0)]
        (card-ability state :runner smc 0)
        (click-prompt state :runner (find-card "Corroder" (:deck (get-runner))))
        (is (= 2 (count (:discard (get-runner))))))
      (let [chip (get-hardware state 0)]
        (card-ability state :runner chip 0)
        (click-card state :runner (find-card "Self-modifying Code" (:discard (get-runner))))
        (is (last-log-contains? state "Patron")
            "Personality Profiles trashed card name is in log")
        (is (= 3 (count (:discard (get-runner))))))))

(deftest personality-profiles-ensure-effects-still-fire-with-an-empty-hand-1840
    ;; Ensure effects still fire with an empty hand, #1840
    (do-game
      (new-game {:corp {:deck ["Personality Profiles"]}
                 :runner {:deck ["Self-modifying Code" "Clone Chip"
                                 "Corroder"]}})
      (starting-hand state :runner ["Self-modifying Code" "Clone Chip"])
      (play-and-score state "Personality Profiles")
      (take-credits state :corp)
      (play-from-hand state :runner "Self-modifying Code")
      (play-from-hand state :runner "Clone Chip")
      (let [smc (get-program state 0)]
        (card-ability state :runner smc 0)
        (click-prompt state :runner (find-card "Corroder" (:deck (get-runner)))))
      (let [cor (get-program state 0)]
        (is (some? cor))
        (is (= (:title cor) "Corroder"))
        (is (= "Self-modifying Code" (:title (first (:discard (get-runner)))))))
      (let [chip (get-hardware state 0)]
        (card-ability state :runner chip 0)
        (click-card state :runner (find-card "Self-modifying Code" (:discard (get-runner)))))
      (let [smc (get-program state 1)]
        (is (some? smc))
        (is (= (:title smc) "Self-modifying Code"))
        (is (= "Clone Chip" (:title (first (:discard (get-runner)))))))))

(deftest philotic-entanglement
  ;; Philotic Entanglement
  (do-game
    (new-game {:corp {:deck ["Philotic Entanglement" (qty "House of Knives" 3)]}
               :runner {:deck [(qty "Sure Gamble" 3) (qty "Cache" 2)]}})
    (play-from-hand state :corp "House of Knives" "New remote")
    (play-from-hand state :corp "House of Knives" "New remote")
    (play-from-hand state :corp "House of Knives" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :runner "Steal")
    (run-empty-server state :remote2)
    (click-prompt state :runner "Steal")
    (run-empty-server state :remote3)
    (click-prompt state :runner "Steal")
    (is (= 3 (count (:scored (get-runner)))))
    (take-credits state :runner)
    (play-and-score state "Philotic Entanglement")
    (is (= 2 (:agenda-point (get-corp))))
    (is (= 3 (count (:discard (get-runner)))) "Dealt 3 net damage upon scoring")))

(deftest post-truth-dividend
  ;; Post-Truth Dividend
  (do-game
    (new-game {:corp {:hand ["Post-Truth Dividend"]
                      :deck ["Hedge Fund"]}})
    (play-and-score state "Post-Truth Dividend")
    (is (changed? [(count (:hand (get-corp))) 1]
          (click-prompt state :corp "Yes"))
        "Drew 1 card")))

(deftest posted-bounty-forfeiting-takes-1-bad-publicity
    ;; Forfeiting takes 1 bad publicity
    (do-game
      (new-game {:corp {:deck ["Posted Bounty"]}})
      (play-and-score state "Posted Bounty")
      (click-prompt state :corp "Yes")
      (is (zero? (:agenda-point (get-corp))) "Forfeiting Posted Bounty nullifies agenda points")
      (is (= 1 (count-bad-pub state)) "Forfeiting takes 1 bad publicity")
      (is (= 1 (count-tags state)) "Runner receives 1 tag forfeiting Posted Bounty")))

(deftest posted-bounty-choosing-not-to-forfeit-scores-normally
    ;; Choosing not to forfeit scores normally
    (do-game
      (new-game {:corp {:deck ["Posted Bounty"]}})
      (play-and-score state "Posted Bounty")
      (click-prompt state :corp "No")
      (is (= 1 (:agenda-point (get-corp))))
      (is (zero? (count-bad-pub state)))
      (is (zero? (count-tags state)))))

(deftest priority-requisition
  ;; Priority Requisition
  (do-game
    (new-game {:corp {:deck ["Priority Requisition" "Archer"]}})
    (play-from-hand state :corp "Archer" "HQ")
    (let [arc (get-ice state :hq 0)]
      (play-and-score state "Priority Requisition")
      (click-card state :corp arc)
      (is (rezzed? (refresh arc))))))

(deftest private-security-force
  ;; Private Security Force
  (do-game
    (new-game {:corp {:deck [(qty "Private Security Force" 10)]}})
    (gain-tags state :runner 1)
    (play-and-score state "Private Security Force")
    (let [psf-scored (get-scored state :corp 0)]
      (card-ability state :corp psf-scored 0)
      (is (= 1 (count (:discard (get-runner)))))
      (take-credits state :runner)
      (dotimes [_ 3]
        (card-ability state :corp psf-scored 0))
      (is (= 3 (count (:discard (get-runner)))))
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest profiteering
  ;; Profiteering
  (do-game
    (new-game {:corp {:deck ["Profiteering"]}})
    (play-and-score state "Profiteering")
    (click-prompt state :corp "3")
    (is (= 1 (:agenda-point (get-corp))))
    (is (= 3 (count-bad-pub state)) "Took 3 bad publicity")
    (is (= 20 (:credit (get-corp))) "Gained 15 credits")))

(deftest project-ares
  ;; Project Ares
  (do-game
    (new-game {:corp {:deck [(qty "Project Ares" 2)]}
               :runner {:deck ["Clone Chip"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Clone Chip")
    (take-credits state :runner)
    (play-and-score state "Project Ares")
    (is (no-prompt? state :runner) "No prompt for Runner if scored with 4 advancement tokens")
    (core/gain state :corp :click 5)
    (play-from-hand state :corp "Project Ares" "New remote")
    (let [ares (get-content state :remote2 0)]
      (advance state ares 6)
      (is (= 6 (get-counters (refresh ares) :advancement)))
      (score state :corp (refresh ares))
      (is (= "Choose 2 installed cards installed cards to trash" (:msg (prompt-map :runner)))
          "Runner has Ares prompt to trash installed cards"))
    (click-card state :runner "Clone Chip")
    (is (no-prompt? state :runner) "Runner must trash 2 cards but only has 1 card in rig, prompt ended")
    (is (= 1 (count (:discard (get-runner)))))
    (is (= 1 (count-bad-pub state)))))

(deftest project-atlas
  ;; Project Atlas
  (do-game
      (new-game {:corp {:deck ["Project Atlas"
                               "Beanstalk Royalties"]}})
      ;; Set up
      (starting-hand state :corp ["Project Atlas"])
      (is (= 1 (count (:hand (get-corp)))) "Corp should have 1 cards in hand")
      (core/gain state :corp :click 10 :credit 10)
      ;; Should gain 1 counter
      (play-from-hand state :corp "Project Atlas" "New remote")
      (let [atlas (get-content state :remote1 0)]
        (advance state atlas 4)
        (is (= 4 (get-counters (refresh atlas) :advancement)) "Atlas should have 4 advancement tokens")
        (score state :corp (refresh atlas)))
      (let [atlas-scored (get-scored state :corp 0)]
        (is (= 1 (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 1 agenda counter")
        (card-ability state :corp atlas-scored 0)
        (click-prompt state :corp (find-card "Beanstalk Royalties" (:deck (get-corp))))
        (is (zero? (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 0 agenda counters")
        (is (= 1 (count (:hand (get-corp)))) "Corp should have 1 cards in hand"))))

(deftest project-atlas-test-with-titan
    ;; test with Titan
    (do-game
      (new-game {:corp {:id "Titan Transnational: Investing In Your Future"
                        :deck [(qty "Project Atlas" 2) "Beanstalk Royalties" "Hedge Fund"]}})
      ;; Set up
      (starting-hand state :corp ["Project Atlas" "Project Atlas"])
      (is (= 2 (count (:hand (get-corp)))) "Corp should have 2 cards in hand")
      (core/gain state :corp :click 10 :credit 10)
      ;; Should gain 1 counter
      (play-from-hand state :corp "Project Atlas" "New remote")
      (let [atlas (get-content state :remote1 0)]
        (advance state atlas 3)
        (is (= 3 (get-counters (refresh atlas) :advancement)) "Atlas should have 3 advancement tokens")
        (score state :corp (refresh atlas)))
      (let [atlas-scored (get-scored state :corp 0)]
        (is (= 1 (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 1 agenda counter")
        (card-ability state :corp atlas-scored 0)
        (click-prompt state :corp (find-card "Beanstalk Royalties" (:deck (get-corp))))
        (is (zero? (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 0 agenda counters")
        (is (= 2 (count (:hand (get-corp)))) "Corp should have 2 card in hand"))
      ;; Should gain 2 counters
      (play-from-hand state :corp "Project Atlas" "New remote")
      (let [atlas (get-content state :remote2 0)]
        (advance state atlas 4)
        (is (= 4 (get-counters (refresh atlas) :advancement)) "Atlas should have 4 advancement tokens")
        (score state :corp (refresh atlas)))
      (let [atlas-scored (get-scored state :corp 1)]
        (is (= 2 (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 2 agenda counter")
        (card-ability state :corp atlas-scored 0)
        (click-prompt state :corp (find-card "Hedge Fund" (:deck (get-corp))))
        (is (= 1 (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 1 agenda counters")
        (is (= 2 (count (:hand (get-corp)))) "Corp should have 2 cards in hand"))))

(deftest project-beale
  ;; Project Beale
  (do-game
    (new-game {:corp {:deck [(qty "Project Beale" 2)]}})
    (core/gain state :corp :click 8 :credit 8)
    (play-from-hand state :corp "Project Beale" "New remote")
    (let [pb1 (get-content state :remote1 0)]
      (advance state pb1 4)
      (score state :corp (refresh pb1))
      (is (= 2 (:agenda-point (get-corp))) "Only 4 advancements: scored for standard 2 points")
      (play-from-hand state :corp "Project Beale" "New remote"))
    (let [pb2 (get-content state :remote2 0)]
      (advance state pb2 5)
      (score state :corp (refresh pb2))
      (is (= 5 (:agenda-point (get-corp))) "5 advancements: scored for 3 points"))))

(deftest project-kusanagi
  ;; Project Kusanagi
  (do-game
    (new-game {:corp {:deck [(qty "Project Kusanagi" 2) "Ice Wall"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (core/gain state :corp :click 10 :credit 10)
    (play-and-score state "Project Kusanagi")
    (let [pk-scored (get-scored state :corp 0)]
      (is (zero? (get-counters (refresh pk-scored) :agenda)) "Kusanagi should start with 0 agenda counters"))
    (play-from-hand state :corp "Project Kusanagi" "New remote")
    (let [pk (get-content state :remote2 0)]
      (advance state pk 3)
      (is (= 3 (get-counters (refresh pk) :advancement)) "Kusanagi should have 3 advancement tokens")
      (score state :corp (refresh pk)))
    (let [pk-scored (get-scored state :corp 1)]
      (is (= 1 (get-counters (refresh pk-scored) :agenda)) "Kusanagi should have 1 agenda counter")
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (card-ability state :corp pk-scored 0)
      (click-card state :corp (get-ice state :hq 0))
      (is (last-log-contains? state "Do 1 net damage"))
      (is (= 2 (count (:subroutines (get-ice state :hq 0)))) "Ice Wall gains 1 subroutine")
      (is (zero? (get-counters (refresh pk-scored) :agenda)) "Kusanagi should have 0 agenda counters"))))

(deftest project-vacheron
  ;; Project Vacheron
  (do-game
      (new-game {:corp {:deck ["Project Vacheron"]}})
      (take-credits state :corp)
      (run-empty-server state :hq)
      (is (= 0 (:agenda-point (get-runner))) "Runner should have 0 agenda points")
      (click-prompt state :runner "Steal")
      (dotimes [n 5]
        (if (> 4 n)
          (is (= 0 (:agenda-point (get-runner))) "Runner should still have 0 agenda points")
          (is (= 3 (:agenda-point (get-runner))) "Runner should now have 0 agenda points"))
        (let [target-tokens (- 4 n)]
          (is (= target-tokens (get-counters (get-scored state :runner 0) :agenda)) (str "Vacheron should have " target-tokens " agenda tokens")))
        (take-credits state :runner)
        (take-credits state :corp))))

(deftest project-vacheron-still-adding-agenda-tokens-when-using-film-critic
    ;; Still adding agenda tokens when using Film Critic
    (do-game
      (new-game {:corp {:deck ["Project Vacheron"]}
                 :runner {:deck ["Film Critic"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (run-empty-server state :hq)
      (is (= "Host Project Vacheron on Film Critic?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes") ; host on Film Critic
      (card-ability state :runner (get-resource state 0) 0)
      ;; remove counters
      (is (zero? (:agenda-point (get-runner))) "Runner should still have 0 agenda points")
      (is (= 4 (get-counters (get-scored state :runner 0) :agenda)) (str "Vacheron should have " 4 " agenda tokens"))
      (take-credits state :runner)
      (take-credits state :corp)
      (is (zero? (:agenda-point (get-runner))) "Runner should still have 0 agenda points")
      (is (= 3 (get-counters (get-scored state :runner 0) :agenda)) (str "Vacheron should have " 3 " agenda tokens"))
      (take-credits state :runner)
      (take-credits state :corp)
      (is (zero? (:agenda-point (get-runner))) "Runner should still have 0 agenda points")
      (is (= 2 (get-counters (get-scored state :runner 0) :agenda)) (str "Vacheron should have " 2 " agenda tokens"))
      (take-credits state :runner)
      (take-credits state :corp)
      (is (zero? (:agenda-point (get-runner))) "Runner should still have 0 agenda points")
      (is (= 1 (get-counters (get-scored state :runner 0) :agenda)) (str "Vacheron should have " 1 " agenda token"))
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 3 (:agenda-point (get-runner))) "Runner should now have 3 agenda points")
      (is (zero? (get-counters (get-scored state :runner 0) :agenda)) (str "Vacheron should have " 0 " agenda tokens"))))

(deftest project-vacheron-scoring-as-corp-gives-3-points
    ;; Scoring as Corp gives 3 points
    (do-game
      (new-game {:corp {:deck ["Project Vacheron"]}})
      (play-and-score state "Project Vacheron")
      (is (= 3 (:agenda-point (get-corp))) "Corp gets 3 points instantly")))

(deftest project-vacheron-steal-from-archives-gives-3-points
    ;; Steal from Archives gives 3 points
    (do-game
      (new-game {:corp {:hand ["Project Vacheron"]}})
      (trash-from-hand state :corp "Project Vacheron")
      (take-credits state :corp)
      (is (= 1 (count (:discard (get-corp)))) "Project Vacheron is trashed")
      (run-empty-server state :archives)
      (click-prompt state :runner "Steal")
      (is (zero? (get-counters (get-scored state :runner 0) :agenda)) "Project Vacheron has zero tokens")
      (is (= 3 (:agenda-point (get-runner))) "Runner gets 3 points instantly")))

(deftest project-vacheron-additional-cards-added-to-the-runner-s-score-area-shouldn-t-add-counters-to-it-issue-4715
    ;; Additional cards added to the runner's score area shouldn't add counters to it. Issue #4715
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Project Vacheron"]}
                 :runner {:hand ["Mad Dash"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Mad Dash")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))) "Runner should only have 1 agenda point as Project Vacheron has agenda tokens on itself")
      (is (= 4 (get-counters (get-scored state :runner 0) :agenda)) "Project Vacheron should have 4 tokens on itself")))

(deftest project-vacheron-scoring-other-agendas-shouldn-t-increase-number-of-agenda-counters-issue-4715
    ;; Scoring other agendas shouldn't increase number of agenda counters. Issue #4715
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Project Vacheron" "Hostile Takeover"]}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (run-empty-server state :hq)
      (click-prompt state :runner "Steal")
      (is (= 4 (get-counters (get-scored state :runner 0) :agenda)) "Project Vacheron should have 4 tokens on itself")
      (run-empty-server state :remote1)
      (click-prompt state :runner "Steal")
      (is (= 4 (get-counters (get-scored state :runner 0) :agenda)) "Project Vacheron should have 4 tokens on itself")))

(deftest project-vacheron-stealing-from-archives-shouldn-t-add-any-counters-issue-4799
    ;; Stealing from Archives shouldn't add any counters. Issue #4799
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hostile Takeover"]
                        :discard ["Project Vacheron"]}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (run-empty-server state :archives)
      (click-prompt state :runner "Steal")
      (is (zero? (get-counters (get-scored state :runner 0) :agenda)) "Project Vacheron should have 0 tokens on itself")))

(deftest project-vacheron-still-adds-counters-when-swapped-with-turntable-5036
    ;; Still adds counters when swapped with Turntable #5036
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Project Vacheron" "Hostile Takeover"]}
                 :runner {:hand ["Turntable"]}})
      (play-and-score state "Project Vacheron")
      (take-credits state :corp)
      (play-from-hand state :runner "Turntable")
      (run-empty-server state :hq)
      (click-prompt state :runner "Steal")
      (is (prompt-is-card? state :runner (get-hardware state 0)))
      (click-prompt state :runner "Yes")
      (click-card state :runner (find-card "Project Vacheron" (:scored (get-corp))))
      (is (= 4 (get-counters (get-scored state :runner 0) :agenda)) "Gains the counters on swap")
      (is (zero? (:agenda-point (get-runner))) "Got the Project Vacheron")
      (is (= 1 (:agenda-point (get-corp))) "Swapped into an HT")))

(deftest project-vitruvius
  ;; Project Vitruvius
  (do-game
    (new-game {:corp {:deck ["Project Vitruvius"
                             "Hedge Fund"]}})
    ;; Set up
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :discard)
    (is (= 1 (count (:discard (get-corp)))) "Corp should have 1 cards in hand")
    (is (= 1 (count (:hand (get-corp)))) "Corp should have 1 cards in hand")
    (core/gain state :corp :click 10 :credit 10)
    ;; Should gain 1 counter
    (play-from-hand state :corp "Project Vitruvius" "New remote")
    (let [vit (get-content state :remote1 0)]
      (advance state vit 4)
      (is (= 4 (get-counters (refresh vit) :advancement)) "Vitruvius should have 4 advancement tokens")
      (score state :corp (refresh vit)))
    (let [vit-scored (get-scored state :corp 0)]
      (is (= 1 (get-counters (refresh vit-scored) :agenda)) "Vitruvius should have 1 agenda counter")
      (card-ability state :corp vit-scored 0)
      (click-card state :corp (find-card "Hedge Fund" (:discard (get-corp))))
      (is (zero? (get-counters (refresh vit-scored) :agenda)) "Vitruvius should have 0 agenda counters")
      (is (= 1 (count (:hand (get-corp)))) "Corp should have 1 cards in hand"))))

(deftest project-wotan
  ;; Project Wotan
  (do-game
    (new-game {:corp {:deck ["Project Wotan"
                             "Eli 1.0"
                             (qty "Hedge Fund" 3)]}})
    (starting-hand state :corp ["Project Wotan" "Eli 1.0"])
    (play-from-hand state :corp "Eli 1.0" "HQ")
    (play-and-score state "Project Wotan")
    (take-credits state :corp)
    (let [wot-scored (get-scored state :corp 0)
          eli (get-ice state :hq 0)]
      (rez state :corp eli)
      (is (= 3 (get-counters (refresh wot-scored) :agenda)) "Wotan should start with 3 agenda counters")
      (run-on state "HQ")
      (card-ability state :corp wot-scored 0)
      (is (last-log-contains? state "End the run"))
      (is (= 2 (get-counters (refresh wot-scored) :agenda)) "Wotan should only have 2 agenda counters")
      (is (= 3 (count (:subroutines (refresh eli)))) "Eli gains a sub from Project Wotan")
      (run-continue-until state :movement)
      (run-jack-out state)
      (is (= 2 (count (:subroutines (refresh eli)))) "Eli resets to normal number of subs"))))

(deftest project-yagi-uda-swap-ice-from-hq
    ;; Swap ice from HQ
    (do-game
      (new-game {:corp {:deck [(qty "Project Yagi-Uda" 2)
                               "Eli 1.0"
                               "Eli 2.0"
                               "Jackson Howard"
                               "Prisec"
                               "Hedge Fund"]}})
      (core/gain state :corp :click 10 :credit 10)
      (click-draw state :corp)
      (play-from-hand state :corp "Project Yagi-Uda" "New remote")
      (play-from-hand state :corp "Eli 1.0" "New remote")
      (let [pyu (get-content state :remote1 0)]
        (advance state pyu 4)
        (score state :corp (refresh pyu)))
      (take-credits state :corp)
      (let [pyu-scored (get-scored state :corp 0)
            eli1 (get-ice state :remote2 0)]
        (run-on state :remote2)
        (card-ability state :corp pyu-scored 0)
        (click-card state :corp eli1)
        (click-card state :corp "Hedge Fund")
        (is (= (:title (get-ice state :remote2 0)) "Eli 1.0") "Couldn't swap ice for Operation")
        (click-card state :corp "Jackson Howard")
        (is (= (:title (get-ice state :remote2 0)) "Eli 1.0") "Couldn't swap ice for Asset")
        (click-card state :corp "Prisec")
        (is (= (:title (get-ice state :remote2 0)) "Eli 1.0") "Couldn't swap ice for Upgrade")
        (click-card state :corp (find-card "Project Yagi-Uda" (:hand (get-corp))))
        (is (= (:title (get-ice state :remote2 0)) "Eli 1.0") "Couldn't swap ice for Agenda")
        (click-card state :corp "Eli 2.0")
        (is (= (:title (get-ice state :remote2 0)) "Eli 2.0") "Swapped Eli 1.0 for 2.0")
        (click-prompt state :runner "No"))))

(deftest project-yagi-uda-swap-cards-in-server-with-cards-in-hq
    ;; Swap cards in server with cards in HQ
    (do-game
      (new-game {:corp {:deck [(qty "Project Yagi-Uda" 2)
                               "Eli 1.0"
                               "Eli 2.0"
                               "Jackson Howard"
                               "Prisec"
                               "Hedge Fund"]}})
      (core/gain state :corp :click 10 :credit 10)
      (click-draw state :corp)
      (play-from-hand state :corp "Project Yagi-Uda" "New remote")
      (play-from-hand state :corp "Project Yagi-Uda" "New remote")
      (let [pyu (get-content state :remote1 0)]
        (advance state pyu 6)
        (score state :corp (refresh pyu)))
      (take-credits state :corp)
      (let [pyu-scored (get-scored state :corp 0)
            pyu2 (get-content state :remote2 0)]
        (run-on state :remote2)
        (card-ability state :corp pyu-scored 0)
        (click-card state :corp pyu2)
        (click-card state :corp "Hedge Fund")
        (is (= (:title (get-content state :remote2 0)) "Project Yagi-Uda")
            "Couldn't swap Agenda for Operation")
        (click-card state :corp "Eli 2.0")
        (is (= (:title (get-content state :remote2 0)) "Project Yagi-Uda")
            "Couldn't swap Agenda for ice")
        (click-card state :corp "Jackson Howard")
        (is (= (:title (get-content state :remote2 0)) "Jackson Howard")
            "Swapped Agenda for Asset")
        (click-prompt state :runner "No")
        (card-ability state :corp pyu-scored 0)
        (click-card state :corp (get-content state :remote2 0))
        (click-card state :corp "Prisec")
        (is (= (:title (get-content state :remote2 0)) "Prisec")
            "Swapped Asset for Upgrade")
        (click-prompt state :runner "No")
        (card-ability state :corp pyu-scored 0)
        (click-card state :corp (get-content state :remote2 0))
        (click-card state :corp (find-card "Project Yagi-Uda" (:hand (get-corp))))
        (is (= (:title (get-content state :remote2 0)) "Project Yagi-Uda")
            "Swapped Upgrade for Agenda")
        (click-prompt state :runner "No"))))

(deftest project-yagi-uda-cancel-swapping-at-different-stages
    ;; Cancel swapping at different stages
    (do-game
      (new-game {:corp {:deck ["Project Yagi-Uda"
                               "Eli 1.0"
                               "Eli 2.0" ]}})
      (core/gain state :corp :click 10 :credit 10)
      (play-from-hand state :corp "Project Yagi-Uda" "New remote")
      (play-from-hand state :corp "Eli 1.0" "New remote")
      (let [pyu (get-content state :remote1 0)]
        (advance state pyu 4)
        (score state :corp (refresh pyu)))
      (take-credits state :corp)
      (let [pyu-scored (get-scored state :corp 0)
            eli1 (get-ice state :remote2 0)]
        (run-on state :remote2)
        (is (= 1 (get-counters (refresh pyu-scored) :agenda)) "Yagi-Uda should have a counter to start with")
        (card-ability state :corp pyu-scored 0)
        (is (= 0 (get-counters (refresh pyu-scored) :agenda)) "Using Yagi-Uda should remove counter")
        (click-prompt state :corp "Done")
        (is (= 1 (get-counters (refresh pyu-scored) :agenda)) "Cancelling during first selection should bring back counter")
        (card-ability state :corp pyu-scored 0)
        (click-card state :corp eli1)
        (click-prompt state :corp "Done")
        (is (= 1 (get-counters (refresh pyu-scored) :agenda)) "Cancelling during second selection should bring back counter"))))

(deftest project-yagi-uda-jack-out
    ;; jack out
    (do-game
     (new-game {:corp {:deck [(qty "Project Yagi-Uda" 2)
                              "Eli 1.0"
                              "Eli 2.0"
                              "Jackson Howard"
                              "Prisec"
                              "Hedge Fund"]}})
     (core/gain state :corp :click 10 :credit 10)
     (click-draw state :corp)
     (play-from-hand state :corp "Project Yagi-Uda" "New remote")
     (play-from-hand state :corp "Eli 1.0" "New remote")
     (let [pyu (get-content state :remote1 0)]
       (advance state pyu 4)
       (score state :corp (refresh pyu)))
     (take-credits state :corp)
     (let [pyu-scored (get-scored state :corp 0)
           eli1 (get-ice state :remote2 0)]
       (run-on state :remote2)
       (card-ability state :corp pyu-scored 0)
       (click-card state :corp eli1)
       (click-card state :corp "Eli 2.0")
       (is (= (:title (get-ice state :remote2 0)) "Eli 2.0") "Swapped Eli 1.0 for 2.0")
       (click-prompt state :runner "Yes")
       (is (empty? (:run @state))))))

(deftest project-yagi-uda-swap-inner-ice-with-hq-issue-4831
    ;; Swap inner ice with HQ. Issue #4831
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Project Yagi-Uda" "Eli 1.0" (qty "Ice Wall" 2)]
                        :credits 20}})
      (core/gain state :corp :click 10)
      (play-from-hand state :corp "Project Yagi-Uda" "New remote")
      (let [pyu (get-content state :remote1 0)]
        (advance state pyu 4)
        (score state :corp (refresh pyu)))
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (let [pyu-scored (get-scored state :corp 0)]
        (run-on state :hq)
        (card-ability state :corp pyu-scored 0)
        (click-card state :corp (get-ice state :hq 0))
        (click-card state :corp "Eli 1.0")
        (is (= (:title (get-ice state :hq 0)) "Eli 1.0") "Swapped Ice Wall with Eli 1.0")
        (click-prompt state :runner "No"))))

(deftest puppet-master
  ;; Puppet Master - game progresses if no valid targets. Issue #1661.
  (do-game
    (new-game {:corp {:deck ["Puppet Master"]}})
    (play-and-score state "Puppet Master")
    (take-credits state :corp)
    (run-empty-server state :archives)
    (click-prompt state :corp "Done")
    (is (no-prompt? state :runner) "Runner's waiting prompt resolved")))

(deftest quantum-predictive-model
  ;; Quantum Predictive Model
  (do-game
    (new-game {:corp {:deck [(qty "Quantum Predictive Model" 2)]
                      :hand ["Quantum Predictive Model" "Quantum Predictive Model"]}})
    (testing "Set up"
      (play-from-hand state :corp "Quantum Predictive Model" "New remote")
      (play-from-hand state :corp "Quantum Predictive Model" "New remote")
      (take-credits state :corp))
    (testing "Access installed with no tag"
      (run-empty-server state :remote1)
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))) "Runner should steal"))
    (testing "Access R&D with no tag"
      (run-empty-server state :rd)
      (click-prompt state :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Runner should steal"))
    (gain-tags state :runner 1)
    (testing "Access intalled with tag"
      (run-empty-server state :remote2)
      (click-prompt state :runner "OK") ;; this is now a prompt that QPM was added to Corp score area
      (is (= 2 (:agenda-point (get-runner))) "Runner should not steal")
      (is (= 1 (:agenda-point (get-corp))) "Corp should score"))
    (testing "Access R&D with tag"
      (run-empty-server state :rd)
      (click-prompt state :runner "OK")
      (is (= 2 (:agenda-point (get-runner))) "Runner should not steal")
      (is (= 2 (:agenda-point (get-corp))) "Corp should score"))
    (is (zero? (count (:deck (get-corp)))))))

(deftest rebranding-team
  ;; Rebranding Team
  (do-game
    (new-game {:corp {:hand ["Rebranding Team" "Launch Campaign" "City Surveillance"
                             "Jackson Howard" "Museum of History" "Advanced Assembly Lines"]}})
    (play-and-score state "Rebranding Team")
    (is (has-subtype? (find-card "Advanced Assembly Lines" (:hand (get-corp))) "Advertisement"))
    ; #2608 part 2 - retain Advertisement always
    (trash-from-hand state :corp "Advanced Assembly Lines")
    (is (has-subtype? (find-card "Advanced Assembly Lines" (:discard (get-corp))) "Advertisement"))
    (is (has-subtype? (find-card "Launch Campaign" (:hand (get-corp))) "Advertisement"))
    (is (has-subtype? (find-card "City Surveillance" (:hand (get-corp))) "Advertisement"))
    (is (has-subtype? (find-card "Jackson Howard" (:hand (get-corp))) "Advertisement"))
    (is (has-subtype? (find-card "Jackson Howard" (:hand (get-corp))) "Executive"))
    (is (has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Advertisement"))
    (is (has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Alliance"))
    (is (has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Ritzy"))
    (move state :corp (find-card "Rebranding Team" (:scored (get-corp))) :deck)
    (is (has-subtype? (find-card "Launch Campaign" (:hand (get-corp))) "Advertisement"))
    (is (not (has-subtype? (find-card "Advanced Assembly Lines" (:discard (get-corp))) "Advertisement")))
    (is (not (has-subtype? (find-card "City Surveillance" (:hand (get-corp))) "Advertisement")))
    (is (not (has-subtype? (find-card "Jackson Howard" (:hand (get-corp))) "Advertisement")))
    (is (has-subtype? (find-card "Jackson Howard" (:hand (get-corp))) "Executive"))
    (is (not (has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Advertisement")))
    (is (has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Alliance"))
    (is (has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Ritzy"))))

(deftest rebranding-team-not-active-while-in-runner-score-area
  ;; Rebranding Team - not active while in the Runner's score area
  (do-game
    (new-game {:corp {:deck ["Rebranding Team" "Project Beale" "Museum of History" "Exchange of Information" "Exchange of Information"]}})
    (play-from-hand state :corp "Rebranding Team" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (is (not (has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Advertisement")))
    (take-credits state :runner)
    (core/gain state :corp :click 3)
    (play-and-score state "Project Beale")
    (gain-tags state :runner 1)
    (play-from-hand state :corp "Exchange of Information")
    (click-card state :corp (find-card "Rebranding Team" (:scored (get-runner))))
    (click-card state :corp (find-card "Project Beale" (:scored (get-corp))))
    (is (last-log-contains? state "make all assets gain Advertisement")
          "Rebranding Team prints its log")
    (is (has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Advertisement"))
    (play-from-hand state :corp "Exchange of Information")
    (click-card state :corp (find-card "Project Beale" (:scored (get-runner))))
    (click-card state :corp (find-card "Rebranding Team" (:scored (get-corp))))
    (is (not (has-subtype? (find-card "Museum of History" (:hand (get-corp))) "Advertisement")))))

(deftest reeducation-simple-test
    ;; Simple test
    (do-game
      (new-game {:corp {:deck ["Reeducation" "Sweeps Week" "Hedge Fund"
                               "Jackson Howard" "Gutenberg"]}
                 :runner {:deck ["Self-modifying Code" "Clone Chip"
                                 "Corroder" "Sure Gamble" "Desperado"]}})
      (starting-hand state :corp ["Reeducation" "Sweeps Week"])
      (starting-hand state :runner ["Self-modifying Code"])
      (play-and-score state "Reeducation")
      (is (prompt-is-type? state :runner :waiting) "Runner has wait prompt")
      (is (= 1 (count (get-in @state [:corp :hand]))))
      (is (= 1 (count (get-in @state [:runner :hand]))))
      (click-prompt state :corp (find-card "Sweeps Week" (:hand (get-corp)))) ; put Sweeps Week at bottom of R&D
      (click-prompt state :corp "Done") ; finished selecting cards
      (click-prompt state :corp "Done") ; corp prompt for Done/Start Over
      (is (= "Sweeps Week" (:title (last (:deck (get-corp))))))
      (is (= "Self-modifying Code" (:title (last (:deck (get-runner))))))
      (is (= 1 (count (get-in @state [:corp :hand]))))
      (is (zero? (count (get-in @state [:runner :hand]))))))

(deftest reeducation-extra-cards
    ;; If Corp is adding more cards in HQ than Runner has in their Grip, Runner
    ;; is not 'able' to resolve the effect and doesn't have to add to bottom of Stack
    ;; Extra cards
    (do-game
      (new-game {:corp {:deck ["Reeducation" "Sweeps Week" "Hedge Fund"
                               "Jackson Howard" "Gutenberg"]}
                 :runner {:deck ["Self-modifying Code" "Clone Chip"
                                 "Corroder" "Sure Gamble" "Desperado"]}})
      (starting-hand state :corp ["Reeducation" "Sweeps Week" "Hedge Fund"])
      (starting-hand state :runner ["Self-modifying Code"])
      (play-and-score state "Reeducation")
      (is (prompt-is-type? state :runner :waiting) "Runner has wait prompt")
      (is (= 2 (count (:hand (get-corp)))))
      (is (= 1 (count (:hand (get-runner)))))
      (click-prompt state :corp (find-card "Sweeps Week" (:hand (get-corp))))
      (click-prompt state :corp (find-card "Hedge Fund" (:hand (get-corp)))) ; this is the bottom card of R&D
      (click-prompt state :corp "Done") ; finished selecting cards
      (click-prompt state :corp "Done") ; corp prompt for Done/Start Over
      (is (= "Hedge Fund" (:title (last (:deck (get-corp))))))
      (is (= "Sweeps Week" (:title (last (butlast (:deck (get-corp)))))))
      (is (= "Self-modifying Code" (:title (first (:hand (get-runner))))))
      (is (= 2 (count (:hand (get-corp)))))
      (is (= 1 (count (:hand (get-runner)))))))

(deftest regenesis
  ;; Regenesis - if no cards have been added to discard, reveal a face-down agenda
  ;; and add it to score area
  (do-game
    (new-game {:corp {:deck [(qty "Regenesis" 2) "Hansei Review" "Obokata Protocol"]}})
    (play-from-hand state :corp "Hansei Review")
    (click-card state :corp "Obokata Protocol")
    (play-and-score state "Regenesis")
    (is (no-prompt? state :corp) "No prompt because we trashed at least one card")
    (take-credits state :corp)
    (take-credits state :runner)
    (play-and-score state "Regenesis")
    (click-card state :corp "Obokata Protocol")
    (is (= 5 (:agenda-point (get-corp))) "3+1+1 agenda points from obo + regen + regen")))

(deftest regenesis-not-affected-by-subliminal-messaging
  ;; Regenesis - Leaving Subliminal Messaging in Archives doesn't interfere
  (do-game
    (new-game {:corp {:hand ["Regenesis" "Hansei Review" "Obokata Protocol" "Subliminal Messaging"]}})
    (play-from-hand state :corp "Subliminal Messaging")
    (play-from-hand state :corp "Hansei Review")
    (click-card state :corp "Obokata Protocol")
    (take-credits state :corp)
    (take-credits state :runner)
    (click-prompt state :corp "No")
    (play-and-score state "Regenesis")
    (click-card state :corp "Obokata Protocol")
    (is (= 4 (:agenda-point (get-corp))) "3+1 agenda points from obo + regen")))

(deftest regenesis-triggering-hyoubu-institute
  ;; Regenesis - Triggers Hyoubu Institute
  (do-game
    (new-game {:corp {:id "Hyoubu Institute: Absolute Clarity"
                      :hand ["Regenesis" "Hansei Review" "Obokata Protocol"]}})
    (play-from-hand state :corp "Hansei Review")
    (click-card state :corp "Obokata Protocol")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (changed? [(:credit (get-corp)) 1]
          (play-and-score state "Regenesis")
          (click-card state :corp "Obokata Protocol"))
        "Regenesis triggers Hyoubu Institute")))

(deftest regenesis-extra-score-not-prevented-by-runner-discard
  (do-game
    (new-game {:corp {:deck [(qty "Regenesis" 6)]
                      :hand ["Bio-Ethics Association"]
                      :discard ["Obokata Protocol"]}
               :runner {:deck [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Bio-Ethics Association" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 1 (count (:discard (get-runner)))))
    (play-and-score state "Regenesis")
    (prompt-is-card? state :corp (get-content state :scored-area 0))
    (prompt-is-type? state :corp :choice)
    (click-card state :corp "Obokata Protocol")
    (is (= 4 (:agenda-point (get-corp))) "3+1 agenda points from obo + regen")))

(deftest regulatory-capture
  ;; regulatory capture
  (do-game
    (new-game {:corp {:hand [(qty "Regulatory Capture" 2)] :credits 10}})
    (play-from-hand state :corp "Regulatory Capture" "New remote")
    (play-from-hand state :corp "Regulatory Capture" "New remote")
    (let [r1 (get-content state :remote1 0)
          r2 (get-content state :remote1 0)]
      (core/add-prop state :corp (refresh r1) :advance-counter 4)
      (core/add-prop state :corp (refresh r2) :advance-counter 1)
      (score state :corp (refresh r1))
      (is (some? (get-content state :remote1 0))
          "Corp can't score with 4 advancements because of 0 BP")
      (core/gain state :corp :bad-publicity 2)
      (core/fake-checkpoint state)
      (score state :corp (refresh r1))
      (is (not (some? (get-content state :remote1 0)))
          "Corp scored capture with 2 bp and 4 counters")
      (core/gain state :corp :bad-publicity 3)
      (core/fake-checkpoint state)
      (score state :corp (refresh r2))
      (is (some? (get-content state :remote2 0))
          "Corp can't score with 1 advancements and 5 BP (max 4 counted)"))))

(deftest remastered-edition
  ;; Remastered Edition
  (do-game
    (new-game {:corp {:deck [(qty "Remastered Edition" 2) (qty "Enigma" 1)]}})
    (core/gain state :corp :click 3)
    (letfn [(try-place [from to]
              (card-ability state :corp (refresh from) 0)
              (click-card state :corp (refresh to)))
            (place-counter [from to]
              (try-place from to)
              (is (zero? (get-counters (refresh from) :agenda))
                  (str (:title from) " token was used on " (:title to)))
              (is (= 1 (get-counters (refresh to) :advancement))
                  (str "Advancement token placed on " (:title to))))]
      (play-and-score state "Remastered Edition")
      (play-from-hand state :corp "Remastered Edition" "New remote")
      (let [scored-agenda (get-scored state :corp 0)
            installed-agenda (get-content state :remote2 0)]
        (place-counter scored-agenda installed-agenda)
        (advance state installed-agenda 3)
        (score state :corp (refresh installed-agenda)))
      (play-from-hand state :corp "Enigma" "HQ")
      (let [strikeforce (get-scored state :corp 1)
            enigma (get-ice state :hq 0)]
        (place-counter strikeforce enigma)))))

(deftest remote-data-farm
  ;; Remote Data Farm
  (do-game
      (new-game {:corp {:deck ["Remote Data Farm"]}})
      (is (= 5 (hand-size :corp)))
      (play-and-score state "Remote Data Farm")
      (is (= 7 (hand-size :corp)))))

(deftest remote-data-farm-logging-when-entering-the-corp-score-area
  ;; Remote Data Farm - logging when entering the Corp's score area
  (do-game
    (new-game {:corp {:deck ["Remote Data Farm" "Project Beale"
                             "Exchange of Information" "Exchange of Information"]}})
    (play-from-hand state :corp "Remote Data Farm" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (is (= 5 (hand-size :corp)))
    (take-credits state :runner)
    (core/gain state :corp :click 3)
    (play-and-score state "Project Beale")
    (gain-tags state :runner 1)
    (play-from-hand state :corp "Exchange of Information")
    (click-card state :corp (find-card "Remote Data Farm" (:scored (get-runner))))
    (click-card state :corp (find-card "Project Beale" (:scored (get-corp))))
    (is (last-log-contains? state "increase their maximum hand size by 2")
          "Remote Data Farm prints its log")
    (is (= 7 (hand-size :corp)))
    (play-from-hand state :corp "Exchange of Information")
    (click-card state :corp (find-card "Project Beale" (:scored (get-runner))))
    (click-card state :corp (find-card "Remote Data Farm" (:scored (get-corp))))
    (is (= 5 (hand-size :corp)))))

(deftest remote-data-farm-removed-from-runner-score-area-issue-5109
    ;; removed from runner score area. Issue #5109
    (do-game
      (new-game {:corp {:deck ["Remote Data Farm"]}
                 :runner {:deck ["Data Dealer"]}})
      (play-from-hand state :corp "Remote Data Farm" "New remote")
      (is (= 5 (hand-size :corp)))
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (play-from-hand state :runner "Data Dealer")
      (card-ability state :runner (get-resource state 0) 0)
      (click-card state :runner (get-scored state :runner 0))
      (is (= 5 (hand-size :corp)) "Corp hand size is still 5")))

(deftest remote-enforcement
  ;; Remote Enforcement - Search R&D for a piece of ice and install it on a remote at no rez cost
  (do-game
    (new-game {:corp {:deck [(qty "Remote Enforcement" 2)
                             "Archer"
                             "Chiyashi"]}
               :runner {:id "Reina Roja: Freedom Fighter"}})
    (starting-hand state :corp ["Remote Enforcement" "Remote Enforcement"])
    (is (= 2 (count (:deck (get-corp)))))
    (play-and-score state "Remote Enforcement")
    (let [N (:credit (get-corp))]
      (click-prompt state :corp "Yes")
      (click-prompt state :corp (find-card "Chiyashi" (:deck (get-corp))))
      (click-prompt state :corp "New remote")
      (is (rezzed? (get-ice state :remote2 0)) "Chiyashi was installed rezzed")
      (is (= N (:credit (get-corp))) "Rezzing Chiyashi was free"))
    (play-and-score state "Remote Enforcement")
    (let [N (:credit (get-corp))]
      (click-prompt state :corp "Yes")
      (click-prompt state :corp (find-card "Archer" (:deck (get-corp))))
      (click-prompt state :corp "Server 2")
      (is (= (dec N) (:credit (get-corp))) "Installing Archer cost a credit")
      (is (not (no-prompt? state :corp)) "Corp prompted to forfeit an agenda for Archer")
      (is (= (dec N) (:credit (get-corp))) "Rezzing Archer didn't cost any credits"))))

(deftest research-grant
  ;; Research Grant
  (do-game
      (new-game {:corp {:deck [(qty "Research Grant" 2)]}})
      (play-from-hand state :corp "Research Grant" "New remote")
      (play-and-score state "Research Grant")
      (click-card state :corp (get-content state :remote1 0))
      (is (= 2 (count (:scored (get-corp)))) "2 copies of Research Grant scored")))

(deftest research-grant-single-test
    ;; Single test
    (do-game
      (new-game {:corp {:deck [(qty "Research Grant" 1)]}})
      (play-and-score state "Research Grant")
      (is (= 1 (count (:scored (get-corp)))) "1 copy of Research Grant scored")))

(deftest research-grant-with-team-sponsorship
    ;; with Team Sponsorship
    (do-game
      (new-game {:corp {:deck [(qty "Research Grant" 3) (qty "Team Sponsorship" 1)]}})
      (play-from-hand state :corp "Team Sponsorship" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (play-and-score state "Research Grant")
      (click-card state :corp (find-card "Research Grant" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (click-card state :corp (get-content state :remote3 0))
      (click-card state :corp (find-card "Research Grant" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (click-card state :corp (get-content state :remote4 0))
      (is (= 3 (count (:scored (get-corp)))) "3 copies of Research Grant scored")))

(deftest research-grant-vs-leela
    ;; Issue #3069
    ;; vs Leela
    (do-game
      (new-game {:corp {:deck [(qty "Research Grant" 2) (qty "Ice Wall" 2)]}
                 :runner {:id "Leela Patel: Trained Pragmatist"
                          :deck ["Sure Gamble"]}})
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Research Grant" "New remote")
      (play-and-score state "Research Grant")
      (click-card state :corp (get-content state :remote1 0))
      (is (= 2 (count (:scored (get-corp)))) "2 copies of Research Grant scored")
      (click-card state :runner (get-ice state :hq 0))
      (click-card state :runner (get-ice state :rd 0))
      (is (= {} (:effect-completed @state))
          "All score and Leela effects resolved")))

(deftest restructured-datapool
  ;; Restructured Datapool
  (do-game
    (new-game {:corp {:deck ["Restructured Datapool"]}})
    (is (zero? (count-tags state)) "Runner should start with no tags")
    (play-and-score state "Restructured Datapool")
    (let [rd-scored (get-scored state :corp 0)]
      (card-ability state :corp rd-scored 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (count-tags state)) "Runner should gain a tag from Restructured Datapool ability"))))

(deftest salvo-testing
    (do-game
      (new-game {:corp {:hand ["Salvo Testing" "Project Vitruvius"]
                        :credits 10}
                 :runner {:hand [(qty "Sure Gamble" 2)]}})
      (is (changed? [(count (:hand (get-runner))) -1]
            (play-and-score state "Salvo Testing")
            (click-prompt state :corp "Yes"))
          "Runner took 1 damage")
      (is (= 1 (:brain-damage (get-runner))))
      (is (changed? [(count (:hand (get-runner))) -1]
            (play-and-score state "Project Vitruvius")
            (click-prompt state :corp "Yes"))
          "Runner took 1 damage")
      (is (= 2 (:brain-damage (get-runner))))))

(deftest sds-drone-deployment-corp-score-a-program-is-installed
    ;; Corp score, a program is installed
    (do-game
      (new-game {:corp {:hand ["SDS Drone Deployment"]}
                 :runner {:hand ["Cache"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (take-credits state :runner)
      (play-and-score state "SDS Drone Deployment")
      (let [cache (get-program state 0)]
        (is (prompt-is-type? state :corp :select) "Corp must choose an installed program")
        (click-card state :corp "Cache")
        (is (nil? (refresh cache)) "Cache is trashed")
        (is (find-card "Cache" (:discard (get-runner))) "Cache is trashed"))))

(deftest sds-drone-deployment-corp-score-no-program-is-installed
    ;; Corp score, no program is installed
    (do-game
      (new-game {:corp {:hand ["SDS Drone Deployment"]}})
      (play-and-score state "SDS Drone Deployment")
      (is (no-prompt? state :corp) "Corp doesn't get any choices when runner has no installed programs")))

(deftest sds-drone-deployment-runner-steal-a-program-is-installed
    ;; Runner steal, a program is installed
    (do-game
      (new-game {:corp {:hand ["SDS Drone Deployment"]}
                 :runner {:hand ["Cache"]}})
      (play-from-hand state :corp "SDS Drone Deployment" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (run-empty-server state "Server 1")
      (let [cache (get-program state 0)]
        (is (= ["Pay to steal" "No action"] (prompt-buttons :runner)) "Runner should not be able to steal")
        (click-prompt state :runner "Pay to steal")
        (click-card state :runner "Cache")
        (is (nil? (refresh cache)) "Cache is trashed")
        (is (find-card "Cache" (:discard (get-runner))) "Cache is trashed")
        (is (find-card "SDS Drone Deployment" (:scored (get-runner)))))))

(deftest sds-drone-deployment-runner-steal-no-program-is-installed
    ;; Runner steal, no program is installed
    (do-game
      (new-game {:corp {:hand ["SDS Drone Deployment"]}})
      (play-from-hand state :corp "SDS Drone Deployment" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= ["No action"] (prompt-buttons :runner)) "Runner should not be able to steal")))

(deftest sds-drone-deployment-ensure-effect-is-async
    ;; Ensure effect is async
    (do-game
      (new-game {:corp {:hand ["Amani Senai" "Team Sponsorship" "SDS Drone Deployment"
                               "NGO Front"]
                        :credits 10}
                 :runner {:hand ["Cache" "Corroder"]
                          :credits 10}})
      (play-from-hand state :corp "Amani Senai" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (play-from-hand state :corp "Team Sponsorship" "New remote")
      (rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Corroder")
      (take-credits state :runner)
      (play-and-score state "SDS Drone Deployment")
      (is (= "Choose a trigger to resolve" (:msg (prompt-map :corp))))
      (is (= #{"SDS Drone Deployment" "Amani Senai" "Team Sponsorship"} (into #{} (map :title (prompt-buttons :corp)))))
      (click-prompt state :corp "SDS Drone Deployment")
      (click-card state :corp "Cache")
      (click-prompt state :corp "Amani Senai")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-card state :corp "Corroder")
      (click-card state :corp "NGO Front")
      (click-prompt state :corp "New remote")
      (is (no-prompt? state :corp))
      (is (no-prompt? state :runner))))

(deftest see-how-they-run-win-psi
  (do-game
    (new-game {:corp {:hand ["See How They Run"]}
               :runner {:hand ["Sure Gamble"]}})
    (is (changed? [(count-tags state) 1
                   (count (:hand (get-runner))) -1
                   (count (:discard (get-runner))) 1
                   (:brain-damage (get-runner)) 1]
                  (play-and-score state "See How They Run")
                  (click-prompt state :corp "0 [Credits]")
                  (click-prompt state :runner "1 [Credits]"))
        "Runner gained 1 tag and got 1 core damage")))

(deftest see-how-they-run-lose-psi
  (do-game
    (new-game {:corp {:hand ["See How They Run"]}
               :runner {:hand ["Sure Gamble"]}})
    (is (changed? [(count-tags state) 1
                   (count (:hand (get-runner))) -1
                   (count (:discard (get-runner))) 1
                   (:brain-damage (get-runner)) 0]
                  (play-and-score state "See How They Run")
                  (click-prompt state :corp "0 [Credits]")
                  (click-prompt state :runner "0 [Credits]"))
        "Runner gained 1 tag and got 1 net damage")))

(deftest self-destruct-chips
  ;; Self-Destruct Chips
  (do-game
    (new-game {:corp {:deck ["Self-Destruct Chips"]}})
    (is (= 5 (hand-size :runner)) "Runner's hand size starts at 5")
    (play-and-score state "Self-Destruct Chips")
    (is (= 4 (hand-size :runner)) "By scoring Self-Destruct Chips, Runner's hand size is reduced by 1")))

(deftest self-destruct-chips-logging-when-entering-the-corp-score-area
  ;; Self-Destruct Chips - logging when entering the Corp's score area
  (do-game
    (new-game {:corp {:deck ["Self-Destruct Chips" "Project Vitruvius" "Exchange of Information" "Exchange of Information"]}})
    (play-from-hand state :corp "Self-Destruct Chips" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (is (= 5 (hand-size :runner)))
    (take-credits state :runner)
    (core/gain state :corp :click 3)
    (play-and-score state "Project Vitruvius")
    (gain-tags state :runner 1)
    (play-from-hand state :corp "Exchange of Information")
    (click-card state :corp (find-card "Self-Destruct Chips" (:scored (get-runner))))
    (click-card state :corp (find-card "Project Vitruvius" (:scored (get-corp))))
    (is (last-log-contains? state "decrease the Runner's maximum hand size by 1")
          "Self-Destruct Chips prints its log")
    (is (= 4 (hand-size :runner)))
    (play-from-hand state :corp "Exchange of Information")
    (click-card state :corp (find-card "Project Vitruvius" (:scored (get-runner))))
    (click-card state :corp (find-card "Self-Destruct Chips" (:scored (get-corp))))
    (is (= 5 (hand-size :runner)))))

(deftest send-a-message
  ;; Send a Message
  (do-game
     (new-game {:corp {:deck ["Send a Message" "Archer"]}})
     (play-from-hand state :corp "Archer" "HQ")
     (let [archer (get-ice state :hq 0)]
       (play-and-score state "Send a Message")
       (click-card state :corp archer)
       (is (rezzed? (refresh archer)))
       (is (no-prompt? state :runner) "Ability finished resolving")))
  (testing "Basic test - steal"
    (do-game
     (new-game {:corp {:deck ["Send a Message" "Archer"]}})
     (play-from-hand state :corp "Archer" "HQ")
     (play-from-hand state :corp "Send a Message" "New remote")
     (let [archer (get-ice state :hq 0)]
       (take-credits state :corp)
       (run-empty-server state "Server 1")
       (click-prompt state :runner "Steal")
       (click-card state :corp archer)
       (is (rezzed? (refresh archer)))
       (is (no-prompt? state :runner) "Ability finished resolving")
       (is (no-prompt? state :corp) "Ability finished resolving")))))

(deftest sensor-net-activation
  ;; Sensor Net Activation
  (do-game
    (new-game {:corp {:deck [(qty "Sensor Net Activation" 2) "Enforcer 1.0" "Ash 2X3ZB9CY"]}})
    (play-from-hand state :corp "Enforcer 1.0" "HQ")
    (play-and-score state "Sensor Net Activation")
    (let [sna-scored (get-scored state :corp 0)
          enf (get-ice state :hq 0)]
      (is (= 1 (get-counters (refresh sna-scored) :agenda)) "Should start with 1 agenda counter")
      (is (not (rezzed? (refresh enf))) "Enforcer 1.0 should start derezzed")
      (card-ability state :corp (refresh sna-scored) 0)
      (click-card state :corp enf)
      (is (rezzed? (refresh enf)) "Enforcer 1.0 should be rezzed")
      (is (= 1 (count (:scored (get-corp)))) "Enforcer 1.0 should be rezzed without forfeiting agenda")
      (take-credits state :corp)
      (is (not (rezzed? (refresh enf))) "Enforcer 1.0 should be derezzed"))
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Ash 2X3ZB9CY" "New remote")
    (play-and-score state "Sensor Net Activation")
    (let [sna-scored (get-scored state :corp 1)
          ash (get-content state :remote2 0)]
      (is (= 1 (get-counters (refresh sna-scored) :agenda)) "Should start with 1 agenda counter")
      (is (not (rezzed? (refresh ash))) "Ash should start derezzed")
      (card-ability state :corp (refresh sna-scored) 0)
      (click-card state :corp ash)
      (is (rezzed? (refresh ash)) "Ash should be rezzed")
      (take-credits state :corp)
      (is (not (rezzed? (refresh ash))) "Ash should be derezzed"))))

(deftest sentinel-defense-program
  ;; Sentinel Defense Program - Doesn't fire if brain damage is prevented
  (do-game
    (new-game {:corp {:deck ["Sentinel Defense Program" "Viktor 1.0"]}
               :runner {:deck ["Feedback Filter" (qty "Sure Gamble" 3)]}})
    (play-and-score state "Sentinel Defense Program")
    (play-from-hand state :corp "Viktor 1.0" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Feedback Filter")
    (let [viktor (get-ice state :hq 0)
          ff (get-hardware state 0)]
      (run-on state "HQ")
      (rez state :corp viktor)
      (run-continue state)
      (card-subroutine state :corp viktor 0)
      (click-prompt state :runner "Done")  ;; Don't prevent the brain damage
      (is (= 1 (count (:discard (get-runner)))))
      (is (= 1 (:brain-damage (get-runner))))
      (click-prompt state :runner "Done")  ;; So we take the net, but don't prevent it either
      (is (= 2 (count (:discard (get-runner)))))
      (card-subroutine state :corp viktor 0)
      (card-ability state :runner ff 1)  ;; Prevent the brain damage this time
      (is (= 3 (count (:discard (get-runner)))) "Feedback filter trashed, didn't take another net damage")
      (is (= 1 (:brain-damage (get-runner)))))))

(deftest show-of-force
  ;; Show of Force
  (do-game
    (new-game {:corp {:deck ["Show of Force"]}})
    (is (= 3 (count (:hand (get-runner)))) "Runner should start with 3 cards in hand")
    (play-and-score state "Show of Force")
    (is (= 1 (count (:hand (get-runner)))) "Runner should have 1 card in hand")
    (is (= 2 (count (:discard (get-runner)))) "Runner should have discarded 2 cards")))

(deftest sisyphus-protocol-trash-from-hq
  (do-game
    (new-game {:corp {:hand ["Sisyphus Protocol" "Tithe" "Hedge Fund"]}})
    (play-and-score state "Sisyphus Protocol")
    (play-from-hand state :corp "Tithe" "R&D")
    (take-credits state :corp)
     (let [tithe (get-ice state :rd 0)]
       (run-on state "R&D")
       (rez state :corp tithe)
       (run-continue state)
       (run-continue state)
       (is (= 0 (:position (get-in @state [:run]))) "Passed Tithe")
       (is (changed? [(count (:hand (get-corp))) -1
                      (count (:discard (get-corp))) 1]
                     (click-prompt state :corp "Trash 1 card from HQ")
                     (click-card state :corp "Hedge Fund"))
           "Corp discarded 1 card from HQ")
       (is (= 0 (:position (get-run))) "Run should still be at position 0")
       (is (same-card? tithe (core/get-current-ice state)))
       (run-continue state)
       (run-jack-out state)
       (run-on state "R&D")
       (run-continue state)
       (run-continue state)
       (is (no-prompt? state :corp) "Sisyphus Protocol ability is only the first time each turn"))))

(deftest sisyphus-protocol-pay-credits
  (do-game
    (new-game {:corp {:hand ["Sisyphus Protocol" "Whitespace"]}})
    (play-and-score state "Sisyphus Protocol")
    (play-from-hand state :corp "Whitespace" "HQ")
    (take-credits state :corp)
     (let [ws (get-ice state :hq 0)]
       (run-on state "HQ")
       (rez state :corp ws)
       (run-continue state)
       (run-continue state)
       (is (= 0 (:position (get-in @state [:run]))) "Passed Whitespace")
       (is (changed? [(:credit (get-corp)) -1]
                     (click-prompt state :corp "Pay 1 [Credit]"))
           "Corp spent 1 Credit")
       (is (= 0 (:position (get-run))) "Run should still be at position 0")
       (is (same-card? ws (core/get-current-ice state))))))

(deftest slash-and-burn-agriculture
  (do-game
    (new-game {:corp {:deck ["Slash and Burn Agriculture" "PAD Campaign" "Ice Wall"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (let [iw (get-ice state :hq 0)
          pad (get-content state :remote1 0)
          agri (first (:hand (get-corp)))]
      (expend state :corp agri)
      (is (changed? [(get-counters (refresh pad) :advancement) 0]
            (click-card state :corp pad))
          "PAD Campaign cannot be advanced")
      (is (changed? [(get-counters (refresh iw) :advancement) 2]
            (click-card state :corp iw))
          "2 advancement counter placed")
      (is (= 4 (:credit (get-corp))) "Expend cost was payed")
      (is (= 1 (count (:discard (get-corp)))) "Slash and Burn Agriculture discarded as cost"))))

(deftest ssl-endorsement-gain-credits-when-in-corp-score-area-before-turn-begins
    ;; gain credits when in corp score area before turn begins
    (do-game
      (new-game {:corp {:deck ["SSL Endorsement"]}})
      (play-and-score state "SSL Endorsement")
      (take-credits state :runner)
      (is (not (no-prompt? state :corp)) "Corp prompted to take credits")
      (is (= 5 (:credit (get-corp))) "Corp starts with 5 credits")
      (click-prompt state :corp "Yes")
      (is (= 8 (:credit (get-corp))) "Corp gains 3 credits")
      (take-credits state :runner)
      (is (= 8 (:credit (get-corp))) "Corp starts with 8 credits")
      (click-prompt state :corp "No")
      (is (= 8 (:credit (get-corp))) "Corp doesn't gain 3 credits")
      (take-credits state :runner)
      (is (= 8 (:credit (get-corp))) "Corp starts with 8 credits")
      (click-prompt state :corp "Yes")
      (is (= 11 (:credit (get-corp))) "Corp gains 3 credits")
      (take-credits state :runner)
      (is (= 11 (:credit (get-corp))) "Corp starts with 11 credits")
      (click-prompt state :corp "Yes")
      (is (= 14 (:credit (get-corp))) "Corp gains 3 credits")
      (take-credits state :runner)
      (is (no-prompt? state :corp) "Not prompted when out of money")))

(deftest ssl-endorsement-gain-credits-when-in-runner-score-area-before-turn-begins
    ;; gain credits when in runner score area before turn begins
    (do-game
      (new-game {:corp {:deck ["SSL Endorsement"]}})
      (play-from-hand state :corp "SSL Endorsement" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (take-credits state :runner)
      (is (not (no-prompt? state :corp)) "Corp prompted to take credits")
      (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
      (click-prompt state :corp "Yes")
      (is (= 10 (:credit (get-corp))) "Corp gains 3 credits")
      (take-credits state :runner)
      (is (= 10 (:credit (get-corp))) "Corp starts with 10 credits")
      (click-prompt state :corp "No")
      (is (= 10 (:credit (get-corp))) "Corp doesn't gain 3 credits")
      (take-credits state :runner)
      (is (= 10 (:credit (get-corp))) "Corp starts with 10 credits")
      (click-prompt state :corp "Yes")
      (is (= 13 (:credit (get-corp))) "Corp gains 3 credits")
      (take-credits state :runner)
      (is (= 13 (:credit (get-corp))) "Corp starts with 13 credits")
      (click-prompt state :corp "Yes")
      (is (= 16 (:credit (get-corp))) "Corp gains 3 credits")
      (take-credits state :runner)
      (is (no-prompt? state :corp) "Not prompted when out of money")))

(deftest ssl-endorsement-register-event-when-agenda-swapped-with-turntable
    ;; Regression test for #3114
    ;; register event when agenda swapped with Turntable
    (do-game
      (new-game {:corp {:deck ["SSL Endorsement" "Breaking News"]}
                 :runner {:deck ["Turntable"]}})
      (play-from-hand state :corp "Breaking News" "New remote")
      (play-and-score state "SSL Endorsement")
      (take-credits state :corp)
      (play-from-hand state :runner "Turntable")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (click-prompt state :runner "Yes")
      (click-card state :runner (find-card "SSL Endorsement" (:scored (get-corp))))  ;; Swap BN with SSL
      (take-credits state :runner)
      (is (not (no-prompt? state :corp)) "Corp prompted to take credits")
      (is (= 6 (:credit (get-corp))) "Corp starts with 7 credits")
      (click-prompt state :corp "Yes")
      (is (= 9 (:credit (get-corp))) "Corp gains 3 credits from Turntable'd SSL Endorsement")))

(deftest ssl-endorsement-don-t-double-register-event-when-agenda-is-swapped
    ;; don't double register event when agenda is swapped
    (do-game
      (new-game {:corp {:deck ["SSL Endorsement" "Breaking News"
                               "Exchange of Information"]}})
      (play-from-hand state :corp "SSL Endorsement" "New remote")
      (play-and-score state "Breaking News")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (take-credits state :runner)
      (is (not (no-prompt? state :corp)) "Corp prompted to take credits")
      (is (= 6 (:credit (get-corp))) "Corp starts with 6 credits")
      (click-prompt state :corp "Yes")
      (is (= 9 (:credit (get-corp))) "Corp gains 3 credits")
      (gain-tags state :runner 1)
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp (find-card "SSL Endorsement" (:scored (get-runner))))
      (click-card state :corp (find-card "Breaking News" (:scored (get-corp))))
      (take-credits state :runner)
      (is (= 9 (:credit (get-corp))) "Corp starts with 9 credits")
      (click-prompt state :corp "No")
      (is (no-prompt? state :corp) "Not double prompted for credits")
      (is (= 9 (:credit (get-corp))) "Corp doesn't gain 3 credits")
      (take-credits state :runner)
      (is (= 9 (:credit (get-corp))) "Corp starts with 9 credits")
      (click-prompt state :corp "Yes")
      (is (= 12 (:credit (get-corp))) "Corp gains 3 credits")
      (take-credits state :runner)
      (is (= 12 (:credit (get-corp))) "Corp starts with 12 credits")
      (click-prompt state :corp "Yes")
      (is (= 15 (:credit (get-corp))) "Corp gains 3 credits")
      (take-credits state :runner)
      (is (no-prompt? state :corp) "Not prompted when out of money")))

(deftest standoff-runner-declines-first
    ;; Runner declines first
    (do-game
      (new-game {:corp {:deck ["Standoff" "Ice Wall" "News Team"]}
                 :runner {:deck ["Cache"]}})
      (starting-hand state :corp ["Standoff" "Ice Wall"])
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (take-credits state :runner)
      (play-and-score state "Standoff")
      (starting-hand state :corp [])
      (is (zero? (-> (get-runner) :discard count)) "Runner should have no cards in Heap")
      (click-card state :runner (get-program state 0))
      (is (= 1 (-> (get-runner) :discard count)) "Runner should now have 1 card in Heap")
      (is (zero? (-> (get-corp) :discard count)) "Corp should have no cards in Archives")
      (click-card state :corp (get-ice state :hq 0))
      (is (= 1 (-> (get-corp) :discard count)) "Corp should now have 1 card in Archives")
      (is (zero? (-> (get-corp) :hand count)) "Corp should have no cards in hand")
      (let [credits (:credit (get-corp))]
        (click-prompt state :runner "Done")
        (is (= (+ credits 5) (:credit (get-corp))) "Corp should gain 5 credits from Runner declining to trash an installed card")
        (is (= 1 (-> (get-corp) :hand count)) "Corp should draw a card from Runner declining to trash an installed card"))))

(deftest standoff-corp-declines-first
    ;; Corp declines first
    (do-game
      (new-game {:corp {:deck ["Standoff" "Ice Wall" "News Team"]}
                 :runner {:deck ["Cache" "Cache"]}})
      (starting-hand state :corp ["Standoff" "Ice Wall"])
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Cache")
      (take-credits state :runner)
      (play-and-score state "Standoff")
      (starting-hand state :corp [])
      (is (zero? (-> (get-runner) :discard count)) "Runner should have no cards in Heap")
      (click-card state :runner (get-program state 0))
      (is (= 1 (-> (get-runner) :discard count)) "Runner should now have 1 card in Heap")
      (is (zero? (-> (get-corp) :discard count)) "Corp should have no cards in Archives")
      (click-card state :corp (get-ice state :hq 0))
      (is (= 1 (-> (get-corp) :discard count)) "Corp should now have 1 card in Archives")
      (is (zero? (-> (get-corp) :hand count)) "Corp should have no cards in hand")
      (click-card state :runner (get-program state 0))
      (is (= 2 (-> (get-runner) :discard count)) "Runner should now have 2 cards in Heap")
      (let [credits (:credit (get-corp))]
        (click-prompt state :corp "Done")
        (is (= credits (:credit (get-corp))) "Corp should gain no credits from declining to trash an installed card")
        (is (zero? (-> (get-corp) :hand count)) "Corp should draw no cards from declining to trash an installed card"))))

(deftest standoff-interactions-with-arella-salvatore
  ;; Interactions with Arella Salvatore
  (do-game
   (new-game {:corp {:deck ["Standoff" "Arella Salvatore" "Ice Wall"]}
              :runner {:deck ["Cache"]}})
   (starting-hand state :corp ["Standoff" "Arella Salvatore"])
   (play-from-hand state :corp "Arella Salvatore" "New remote")
   (take-credits state :corp)
   (play-from-hand state :runner "Cache")
   (take-credits state :runner)
   (play-from-hand state :corp "Standoff" "Server 1")
   (starting-hand state :corp [])
   (let [arella (get-content state :remote1 0)
         standoff (get-content state :remote1 1)]
     (rez state :corp (refresh arella))
     (score-agenda state :corp (refresh standoff)))
   (is (not (prompt-is-type? state :corp :choice)) "Arella is silent since no installable cards in hand")
   (click-prompt state :runner "Done")
   (is (= 1 (count (:hand (get-corp)))) "Standoff drew a card")
   (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
   (click-prompt state :corp "Server 1")
   (is (= 1 (count (get-ice state :remote1))) "Ice Wall installed protecting server 1")
   (is (= 1 (get-counters (get-ice state :remote1 0) :advancement)) "Agenda has 1 advancement counter")))

(deftest stegodon-mk-iv
  (do-game
    (new-game {:corp {:hand ["Stegodon MK IV" (qty "Ice Wall" 2)]
                      :credits 10}
               :runner {:hand ["Corroder"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (play-from-hand state :corp "Ice Wall" "R&D")
    (rez state :corp (get-ice state :rd 0))
    (play-and-score state "Stegodon MK IV")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (let [corr (get-program state 0)]
      (run-on state "HQ")
      (is (= 2 (get-strength (refresh corr))))
      (is (changed? [(:credit (get-corp)) 1]
            (click-card state :corp (get-ice state :rd 0)))
          "Corp gained 1 credit by derezzing ice")
      (is (zero? (get-strength (refresh corr))) "Corroder's strength lowered")
      (run-continue state)
      (card-ability state :runner (refresh corr) 0)
      (is (no-prompt? state :runner) "Corroder cannot interface with Ice Wall")
      (run-continue state)
      (run-continue state)
      (run-on state "Archives")
      (is (no-prompt? state :corp) "Stegodon MK IV ability is once per turn")
      (is (= 2 (get-strength (refresh corr)))))))

(deftest sting-corp-score-then-runner-steal-then-corp-score
    ;; Corp score, then Runner steal, then Corp score
    (do-game
     (new-game {:corp {:deck [(qty "Sting!" 3)]}
                :runner {:deck [(qty "Spy Camera" 5)]}})
     (is (= 5 (count (:hand (get-runner)))) "5 cards in hand to be damaged away")
     (play-from-hand state :corp "Sting!" "New remote")
     (play-and-score state "Sting!")
     (is (= 1 (-> (get-runner) :discard count)) "Runner should have taken 1 net damage")
     (take-credits state :corp)
     (run-empty-server state :remote1)
     (click-prompt state :runner "Steal")
     (is (= 3 (-> (get-runner) :discard count)) "Runner should take 2 net damage because there is a Sting! in the Corp's score area")
     (take-credits state :runner)
     (play-and-score state "Sting!")
     (is (= 5 (-> (get-runner) :discard count)) "Runner should take 2 net damage because there is a Sting! in the Runner's score area")))

(deftest sting-swapping-agendas-does-no-damage
    ;; Swapping Sting! with another agenda does no damage
    (do-game
      (new-game {:corp {:deck ["Exchange of Information"
                               "Sting!"
                               "Jumon"]}
                 :runner {:deck [(qty "Spy Camera" 5)]}})
      (play-and-score state "Sting!")
      (take-credits state :corp)
      (core/steal state :runner (make-eid state) (find-card "Jumon" (:hand (get-corp))))
      (take-credits state :runner)
      (gain-tags state :runner 1)
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp (find-card "Jumon" (:scored (get-runner))))
      (click-card state :corp (find-card "Sting!" (:scored (get-corp))))
      (is (= 1 (-> (get-runner) :discard count)) "Runner should take no damage from the agendas swap")))

(deftest stoke-the-embers
  (do-game
    (new-game {:corp {:id "Hyoubu Institute: Absolute Clarity"
                      :hand ["Stoke the Embers" "NGO Front" "Restore"]
                      :discard ["Stoke the Embers"]}})
    (play-from-hand state :corp "NGO Front" "New remote")
    (is (changed? [(:credit (get-corp)) 3
                   (get-counters (refresh (get-content state :remote1 0)) :advancement) 1]
                  (play-and-score state "Stoke the Embers")
                  (click-card state :corp "NGO Front"))
        "Corp gained 4 credits and put 1 advancement counter on a card")
    (play-from-hand state :corp "Restore")
    (click-card state :corp (find-card "Stoke the Embers" (:discard (get-corp))))
    (click-prompt state :corp "New remote")
    (is (changed? [(:credit (get-corp)) 3
                   (get-counters (refresh (get-content state :remote3 0)) :advancement) 1]
                  (click-prompt state :corp "Yes")
                  (is (last-n-log-contains? state 3 "reveal itself from Archives"))
                  (click-card state :corp (get-content state :remote3 0)))
        "Corp gained 2 credits (+1 from Hyobu because the agenda was revealed) and put 1 advancement counter on a card")))

(deftest successful-field-test
  ;; Successful Field Test
  (do-game
    (new-game {:corp {:deck ["Successful Field Test" (qty "Ice Wall" 10)]}})
    (starting-hand state :corp (vec (cons "Successful Field Test" (repeat 10 "Ice Wall"))))
    (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
    (play-and-score state "Successful Field Test")
    (dotimes [_ 10]
      (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
      (click-prompt state :corp "HQ"))
    (is (= 5 (:credit (get-corp))) "Should still have 5 credits")
    (is (some? (get-ice state :hq 9)))))

(deftest superconducting-hub
  ;; Superconducting Hub
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Superconducting Hub"]
                      :credits 10}})
    (core/gain state :corp :click 10)
    (play-from-hand state :corp "Superconducting Hub" "New remote")
    (is (changed? [(count (:hand (get-corp))) 2]
          (score-agenda state :corp (get-content state :remote1 0))
          (click-prompt state :corp "Yes"))
        "Superconducting Hub draws 2 cards")
    (is (= 7 (hand-size :corp)))))

(deftest superior-cyberwalls
  ;; Superior Cyberwalls
  (do-game
    (new-game {:corp {:deck ["Superior Cyberwalls" "Ice Wall"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [iw (get-ice state :hq 0)]
      (rez state :corp iw)
      (is (= 1 (get-strength (refresh iw))) "Should start with base strength of 1")
      (is (= 4 (:credit (get-corp))) "Should have 4 credits after rez")
      (play-and-score state "Superior Cyberwalls")
      (is (= 2 (get-strength (refresh iw))) "Should gain 1 strength from 1 to 2")
      (is (= 5 (:credit (get-corp))) "Should gain 1 credit for rezzed barrier"))))

(deftest tgtbt
  ;; TGTBT - Give the Runner 1 tag when they access
  ;; OHG still not working...
  (do-game
    (new-game {:corp {:deck [(qty "TGTBT" 2) "Old Hollywood Grid"]}})
    (play-from-hand state :corp "TGTBT" "New remote")
    (play-from-hand state :corp "Old Hollywood Grid" "Server 1")
    (play-from-hand state :corp "TGTBT" "New remote")
    (take-credits state :corp)
    (let [tg1 (get-content state :remote1 0)
          ohg (get-content state :remote1 1)]
      (rez state :corp ohg)
      (run-empty-server state "Server 1")
      (click-card state :runner tg1)
      ;; Accesses TGTBT but can't steal
      (is (= 1 (count-tags state)) "Runner took 1 tag from accessing without stealing")
      (click-prompt state :runner "No action"))
    (click-prompt state :runner "Pay 4 [Credits] to trash") ;; Trashes OHG
    (run-empty-server state "Server 2")
    ;; Accesses TGTBT and can steal
    (click-prompt state :runner "Steal")
    (is (= 2 (count-tags state)) "Runner took 1 tag from accessing and stealing")))

(deftest the-basalt-spire
  (do-game
    (new-game {:corp {:deck ["Hedge Fund"]
                      :hand [(qty "The Basalt Spire" 2)]
                      :discard ["Ice Wall"]}})
    (play-and-score state "The Basalt Spire")
    (let [tbs (get-scored state :corp 0)]
      (is (= 2 (get-counters (refresh tbs) :agenda)))
      (is (changed? [(get-counters (refresh tbs) :agenda) -1
                     (count (:discard (get-corp))) 1
                     (count (:deck (get-corp))) -1]
            (card-ability state :corp (refresh tbs) 0))
          "The Basalt Spire costs agenda counter and top of R&D to play")
      (is (changed? [(count (:hand (get-corp))) 1
                     (count (:discard (get-corp))) -1]
            (click-card state :corp "Hedge Fund"))
          "The Basalt Spire ability recurs a card"))
    (play-from-hand state :corp "The Basalt Spire" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 2")
    (click-prompt state :runner "Steal")
    (is (changed? [(count (:hand (get-corp))) 1
                   (count (:discard (get-corp))) -1]
          (click-card state :corp "Ice Wall"))
        "The Basalt Spire can recur a card on steal")))

(deftest the-cleaners
  ;; The Cleaners
  (do-game
      (new-game {:corp {:deck ["The Cleaners" "Scorched Earth"]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 3)]}})
      (play-and-score state "The Cleaners")
      (gain-tags state :runner 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (zero? (count (:hand (get-runner)))) "5 damage dealt to Runner")))

(deftest the-cleaners-no-bonus-damage-when-runner-suffers-damage-ie-cybernetics
    ;; No bonus damage when runner 'suffers' damage, ie Cybernetics
    (do-game
      (new-game {:corp {:deck ["The Cleaners"]}
                 :runner {:deck [(qty "Respirocytes" 3)]}})
      (play-and-score state "The Cleaners")
      (take-credits state :corp)
      (play-from-hand state :runner "Respirocytes")
      (is (= 1 (count (:hand (get-runner)))) "Only 1 damage dealt to Runner from Cybernetics")))

(deftest the-future-is-now-with-at-least-one-card-in-deck
    ;; With at least one card in deck
    (do-game
      (new-game {:corp {:deck ["The Future is Now" "Ice Wall"]}})
      (starting-hand state :corp ["The Future is Now"])
      (is (= 1 (count (:hand (get-corp)))))
      (is (= 1 (count (:deck (get-corp)))))
      (play-and-score state "The Future is Now")
      (click-prompt state :corp (find-card "Ice Wall" (:deck (get-corp))))
      (is (= 1 (count (:hand (get-corp)))))
      (is (zero? (count (:deck (get-corp)))))))

(deftest the-future-is-now-with-an-empty-deck
    ;; With an empty deck
    (do-game
      (new-game {:corp {:deck ["The Future is Now"]}})
      (is (= 1 (count (:hand (get-corp)))))
      (is (zero? (count (:deck (get-corp)))))
      (play-and-score state "The Future is Now")
      (is (no-prompt? state :corp) "Ability shouldn't fire if deck is empty")
      (is (zero? (count (:hand (get-corp)))))
      (is (zero? (count (:deck (get-corp)))))))

(deftest the-future-perfect
  ;; The Future Perfect
  (do-game
    (new-game {:corp {:deck [(qty "The Future Perfect" 2)]}})
    (play-from-hand state :corp "The Future Perfect" "New remote")
    (take-credits state :corp)
    (testing "No steal on not-equal Psi game"
      (run-empty-server state "HQ")
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      ;; Cannot steal prompt
      (click-prompt state :runner "No action")
      (is (zero? (:agenda-point (get-runner))) "Runner did not steal TFP"))
    (testing "Successful steal on equal Psi game"
      (run-empty-server state "HQ")
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (click-prompt state :runner "Steal")
      (is (= 3 (:agenda-point (get-runner))) "Runner stole TFP"))
    (testing "No Psi game and successful steal when installed"
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (is (= 6 (:agenda-point (get-runner))) "Runner stole TFP - no Psi game on installed TFP"))))

(deftest timely-public-release-install-outside-run
    ;; Install outside run
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Timely Public Release" "Enigma"]}})
      (play-and-score state "Timely Public Release")
      (let [tpr (get-scored state :corp 0)]
        (is (= 1 (get-counters (refresh tpr) :agenda)) "TPR comes with 1 counter")
        (card-ability state :corp (refresh tpr) 0)
        (core/move state :corp (assoc (find-card "Enigma" (:hand (get-corp))) :seen true) :discard)
        (click-card state :corp "Enigma")
        (is (= ["Archives" "R&D" "HQ" "New remote"] (prompt-buttons :corp)))
        (click-prompt state :corp "HQ")
        (click-prompt state :corp "0")
        (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma was installed")
        (is (empty? (:hand (get-corp))) "Enigma removed from HQ")
        (is (zero? (get-counters (refresh tpr) :agenda)) "Agenda counter was spent"))))

(deftest timely-public-release-install-on-new-remote
    ;; Install on a new remote
    (do-game
      (new-game {:corp {:hand ["Timely Public Release" "Enigma"]}})
      (play-and-score state "Timely Public Release")
      (let [tpr (get-scored state :corp 0)]
        (card-ability state :corp (refresh tpr) 0)
        (click-card state :corp "Enigma")
        (click-prompt state :corp "New remote")
        (click-prompt state :corp "0")
        (is (= "Enigma" (:title (get-ice state :remote2 0))) "Enigma was installed"))))

(deftest timely-public-release-install-on-server-being-run
    ;; Install on server being run
    (testing "when approaching the server"
      (do-game
        (new-game {:corp {:deck ["Enigma" "Timely Public Release"]}})
        (play-and-score state "Timely Public Release")
        (take-credits state :corp)
        (let [tpr (get-scored state :corp 0)
              corp-credits (:credit (get-corp))]
          (run-on state "R&D")
          (is (zero? (get-in @state [:run :position])) "Initial run position is approaching server")
          (card-ability state :corp (refresh tpr) 0)
          (click-card state :corp (find-card "Enigma" (:hand (get-corp))))
          (click-prompt state :corp "R&D")
          (click-prompt state :corp "0")
          (is (= "Enigma" (:title (get-ice state :rd 0))) "Enigma was installed")
          (is (= corp-credits (:credit (get-corp))) "Install was free")
          (is (zero? (get-in @state [:run :position])) "Still approaching server")
          (run-continue state)
          (is (nil? (get-run))))))
    (testing "in front of current ice"
      (do-game
        (new-game {:corp {:deck ["Enigma" "Ice Wall" "Timely Public Release"]}})
        (play-and-score state "Timely Public Release")
        (play-from-hand state :corp "Ice Wall" "R&D")
        (rez state :corp (get-ice state :rd 0))
        (take-credits state :corp)
        (let [tpr (get-scored state :corp 0)
              corp-credits (:credit (get-corp))]
          (run-on state "R&D")
          (is (= 1 (get-in @state [:run :position])) "Still encountering Ice Wall")
          (card-ability state :corp (refresh tpr) 0)
          (click-card state :corp (find-card "Enigma" (:hand (get-corp))))
          (click-prompt state :corp "R&D")
          (click-prompt state :corp "0")
          (is (= "Enigma" (:title (get-ice state :rd 0))) "Enigma was installed")
          (is (= corp-credits (:credit (get-corp))) "Install was free")
          (is (= 2 (get-in @state [:run :position])) "Now approaching new ice")
          (run-continue state :encounter-ice)
          (run-continue-until state :approach-ice)
          (is (= "Enigma" (:title (core/get-current-ice state))) "Now approaching Enigma"))))
    (testing "behind the current ice"
      (do-game
        (new-game {:corp {:deck ["Enigma" "Ice Wall" "Vanilla" "Timely Public Release"]
                          :credits 10}})
        (play-and-score state "Timely Public Release")
        (play-from-hand state :corp "Vanilla" "R&D")
        (rez state :corp (get-ice state :rd 0))
        (play-from-hand state :corp "Ice Wall" "R&D")
        (rez state :corp (get-ice state :rd 1))
        (take-credits state :corp)
        (let [tpr (get-scored state :corp 0)
              corp-credits (:credit (get-corp))]
          (run-on state "R&D")
          (is (= 2 (get-in @state [:run :position])) "Still encountering Ice Wall")
          (card-ability state :corp (refresh tpr) 0)
          (click-card state :corp (find-card "Enigma" (:hand (get-corp))))
          (click-prompt state :corp "R&D")
          (click-prompt state :corp "2")
          (is (= "Enigma" (:title (get-ice state :rd 2))) "Enigma was installed")
          (is (= corp-credits (:credit (get-corp))) "Install was free")
          (is (= 2 (get-in @state [:run :position])))
          (is (= "Ice Wall" (:title (core/get-current-ice state))) "Still approaching Ice Wall")
          (run-continue state :encounter-ice)
          (run-continue-until state :approach-ice)
          (is (= "Vanilla" (:title (core/get-current-ice state))) "Now approaching Vanilla")))))

(deftest tomorrows-headline
  ;;Tomorrow's Headline
  (do-game
      (new-game {:corp {:deck ["Tomorrow's Headline"]}})
      (is (changed? [(count-tags state) 1]
            (play-and-score state "Tomorrow's Headline"))
          "Runner takes 1 tag on Tomorrow's Headline score"))
  (testing "Basic test - stolen"
    (do-game
      (new-game {:corp {:deck ["Tomorrow's Headline"]}})
      (play-from-hand state :corp "Tomorrow's Headline" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (changed? [(count-tags state) 1]
            (click-prompt state :runner "Steal"))
          "Runner takes 1 tag on Tomorrow's Headline steal")))
  (testing "Runner takes no tag when swapping agendas"
    (do-game
      (new-game {:corp {:deck ["Tomorrow's Headline", "Exchange of Information", "Project Beale"]}})
      (play-from-hand state :corp "Project Beale" "New remote")
      (play-and-score state "Tomorrow's Headline")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (take-credits state :runner)
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp (find-card "Project Beale" (:scored (get-runner))))
      (click-card state :corp (find-card "Tomorrow's Headline" (:scored (get-corp))))
      (is (= 1 (count-tags state)) "Runner takes no tag from the agendas swap"))))

(deftest transport-monopoly-basic-functionality
    ;; Basic functionality
    (do-game
      (new-game {:corp {:deck ["Transport Monopoly" "Hedge Fund"]}
                 :runner {:deck [(qty "Dirty Laundry" 3)]}})
      (play-and-score state "Transport Monopoly")
      (take-credits state :corp)
      (let [tm (get-scored state :corp 0)]
        (is (changed? [(:credit (get-runner)) -2]
              (play-from-hand state :runner "Dirty Laundry")
              (click-prompt state :runner "HQ")
              (card-ability state :corp (refresh tm) 0)
              (run-continue state)
              (is (accessing state "Hedge Fund"))
              (click-prompt state :runner "No action"))
            "Did not gain 5c from DL")
        (is (changed? [(:credit (get-runner)) 3]
              (play-from-hand state :runner "Dirty Laundry")
              (click-prompt state :runner "HQ")
              (run-continue state)
              (click-prompt state :runner "No action"))
            "Gained 5c from DL"))))

(deftest transport-monopoly-omar-interaction
    ;; Omar interaction
    (do-game
      (new-game {:corp {:deck ["Transport Monopoly" "Hedge Fund"]}
                 :runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck [(qty "Sure Gamble" 3)]}})
      (play-and-score state "Transport Monopoly")
      (take-credits state :corp)
      (let [tm (get-scored state :corp 0)
            omar (get-in @state [:runner :identity])]
        (card-ability state :runner omar 0)
        (card-ability state :corp (refresh tm) 0)
        (run-continue state)
        (is (empty? (-> (get-runner) :register :successful-run)))
        (is (no-prompt? state :runner) "No omar prompt"))))

(deftest transport-monopoly-stargate-interaction-issue-4713
    ;; Stargate interaction. Issue #4713
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Transport Monopoly"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]
                          :hand ["Stargate"]}})
      (play-and-score state "Transport Monopoly")
      (take-credits state :corp)
      (play-from-hand state :runner "Stargate")
      (let [tm (get-scored state :corp 0)
            stargate (get-program state 0)]
        (card-ability state :runner stargate 0)
        (card-ability state :corp (refresh tm) 0)
        (run-continue state)
        (is (empty? (-> (get-runner) :register :successful-run)))
        (is (accessing state "Hedge Fund") "No stargate prompt")
        (click-prompt state :runner "No action"))))

(deftest transport-monopoly-successful-runs-bugs-issue-4735
    ;; Successful runs bugs. Issue #4735
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Transport Monopoly"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]
                          :hand ["DreamNet"]}})
      (play-and-score state "Transport Monopoly")
      (take-credits state :corp)
      (play-from-hand state :runner "DreamNet")
      (let [tm (get-scored state :corp 0)]
        (run-on state "R&D")
        (card-ability state :corp (refresh tm) 0)
        (run-continue state)
        (is (changed? [(count (:hand (get-runner))) 0]
)
            "Transport Monopoly blocks DreamNet"))))

(deftest underway-renovation
  ;; Underway Renovation
  (do-game
    (new-game {:corp {:deck ["Underway Renovation" "Shipment from SanSan"]}})
    (core/gain state :corp :click 2)
    (starting-hand state :runner [])
    (play-from-hand state :corp "Underway Renovation" "New remote")
    (let [ur (get-content state :remote1 0)]
      (advance state ur)
      (is (last-log-contains? state "Sure Gamble")
          "Underway Renovation trashed card name is in log")
      ; check for #2370
      (is (not (last-log-contains? state "Sure Gamble, Sure Gamble"))
          "Underway Renovation trashed card name is in log")
      (is (= 1 (count (:discard (get-runner)))) "1 card milled from Runner Stack")
      (play-from-hand state :corp "Shipment from SanSan")
      (click-prompt state :corp "2")
      (click-card state :corp ur)
      (is (= 3 (get-counters (refresh ur) :advancement)))
      (is (= 1 (count (:discard (get-runner)))) "No Runner mills; advancements were placed")
      (advance state ur)
      (is (= 4 (get-counters (refresh ur) :advancement)))
      (is (last-log-contains? state "Sure Gamble and Sure Gamble")
          "Underway Renovation trashed card name is in log")
      (is (= 3 (count (:discard (get-runner)))) "2 cards milled from Runner Stack; 4+ advancements"))))

(deftest unorthodox-predictions
  ;; Unorthodox Predictions
  (do-game
    (new-game {:corp {:deck ["Unorthodox Predictions"]}})
    (play-and-score state "Unorthodox Predictions")
    (click-prompt state :corp "Barrier")
    (is (last-log-contains? state "Barrier"))))

(deftest utopia-fragment
  ;; Utopia Fragment
  (do-game
    (new-game {:corp {:deck ["Utopia Fragment"
                             "Hostile Takeover"]}})
    (play-and-score state "Utopia Fragment")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (advance state (get-content state :remote2 0))
    (take-credits state :corp)
    (run-empty-server state "Server 2")
    (is (= ["Pay to steal" "No action"] (prompt-buttons :runner)))
    (click-prompt state :runner "Pay to steal")
    (is (= 1 (:agenda-point (get-runner))))
    (is (= 3 (:credit (get-runner))))))

(deftest vanity-project
  ;; Vanity Project
  (do-game
    (new-game {:corp {:deck ["Vanity Project"]}})
    (play-and-score state "Vanity Project")
    (is (= 4 (:agenda-point (get-corp))))))

(deftest veterans-program
  ;; Veterans Program
  (do-game
      (new-game {:corp {:deck [(qty "Hostile Takeover" 2) "Veterans Program"]}})
      (play-and-score state "Hostile Takeover")
      (play-and-score state "Hostile Takeover")
      (is (= 19 (:credit (get-corp))) "Should gain 14 credits from 5 to 19")
      (is (= 2 (count-bad-pub state)) "Should gain 2 bad publicity")
      (play-and-score state "Veterans Program")
      (is (zero? (count-bad-pub state)) "Should lose 2 bad publicity")))

(deftest veterans-program-removes-up-to-2-bad-publicity
    ;; Removes _up to 2_ bad publicity
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" "Veterans Program"]}})
      (play-and-score state "Hostile Takeover")
      (is (= 12 (:credit (get-corp))) "Should gain 7 credits from 5 to 12")
      (is (= 1 (count-bad-pub state)) "Should gain 1 bad publicity")
      (play-and-score state "Veterans Program")
      (is (zero? (count-bad-pub state)) "Should lose 1 bad publicity")))

(deftest viral-weaponization-score-on-corp-turn
    ;; Score on corp turn
    (do-game
      (new-game {:corp {:deck [(qty "Viral Weaponization" 2)]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (starting-hand state :runner ["Sure Gamble" "Sure Gamble"])
      (play-and-score state "Viral Weaponization")
      (is (= 2 (count (:hand (get-runner)))) "Runner doesn't take damage when scored")
      (take-credits state :corp)
      (is (zero? (count (:hand (get-runner)))) "Runner takes damage at end of turn")
      (click-draw state :runner)
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 1 (count (:hand (get-runner)))) "Runner doesn't take damage in future turns")
      (play-from-hand state :runner "Sure Gamble")
      (take-credits state :runner)
      (is (zero? (count (:hand (get-runner)))) "Runner's hand is empty")
      (play-and-score state "Viral Weaponization")
      (take-credits state :corp)
      (is (zero? (count (:hand (get-runner)))) "Runner's hand is empty")))

(deftest viral-weaponization-score-on-runners-turn
    ;; Score on runners turn
    (do-game
      (new-game {:corp {:deck ["Viral Weaponization" "Plan B"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (starting-hand state :runner ["Sure Gamble" "Sure Gamble"])
      (play-from-hand state :corp "Plan B" "New remote")
      (core/add-prop state :corp (get-content state :remote1 0) :advance-counter 4)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Viral Weaponization" (:hand (get-corp))))
      (is (= ["Pay 1 [Credits] to trash" "No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (= 2 (count (:hand (get-runner)))) "Runner doesn't take damage when scored")
      (take-credits state :runner)
      (is (zero? (count (:hand (get-runner)))) "Runner takes damage at end of turn")))

(deftest viral-weaponization-interaction-with-the-class-act
    ;; interaction with The Class Act
    (testing "Score on corp turn"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Viral Weaponization"]}
                   :runner {:deck ["The Class Act" (qty "Sure Gamble" 10)]
                            :hand ["Artist Colony" "Fan Site" "Sure Gamble" "Sure Gamble"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Artist Colony")
        (play-from-hand state :runner "Fan Site")
        (take-credits state :runner)
        (play-and-score state "Viral Weaponization")
        (card-ability state :runner (get-resource state 0) 0)
        (click-prompt state :runner "The Class Act")
        (click-card state :runner "Fan Site")
        (is (= "The Class Act" (:title (get-resource state 1))))
        (is (= :this-turn (:installed (get-resource state 1))))
        (is (= 2 (count (:hand (get-runner)))) "Runner doesn't take damage when scored")
        (take-credits state :corp)
        (is (= 5 (count (:set-aside (get-runner)))) "Runner takes damage before resolving The Class Act")
        (click-card state :runner (find-card "Sure Gamble" (:set-aside (get-runner))))
        (is (= 4 (count (:hand (get-runner)))) "Runner draws from The Class Act after taking damage")))
    (testing "Scored on the runner's turn"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Viral Weaponization" "Plan B"]}
                   :runner {:deck [(qty "Sure Gamble" 3)]
                            :hand ["Sure Gamble" "Sure Gamble" "The Class Act"]}})
        (play-from-hand state :corp "Plan B" "New remote")
        (core/add-prop state :corp (get-content state :remote1 0) :advance-counter 4)
        (take-credits state :corp)
        (play-from-hand state :runner "The Class Act")
        (run-empty-server state "Server 1")
        (click-prompt state :corp "Yes")
        (click-card state :corp (find-card "Viral Weaponization" (:hand (get-corp))))
        (is (= ["Pay 1 [Credits] to trash" "No action"] (prompt-buttons :runner)))
        (click-prompt state :runner "No action")
        (is (= 2 (count (:hand (get-runner)))) "Runner doesn't take damage when scored")
        (take-credits state :runner)
        (is (= 3 (count (:set-aside (get-runner)))) "Runner doesn't take the damage until after resolving The Class Act")
        (click-card state :runner (find-card "Sure Gamble" (:set-aside (get-runner))))
        (is (= 0 (count (:hand (get-runner)))) "Runner takes damage at end of turn"))))

(deftest voting-machine-initiative-voting-machine-initiative
    ;; Voting Machine Initiative
    (do-game
      (new-game {:corp {:deck ["Voting Machine Initiative"]}})
      (letfn [(vmi-test [vmi choice counter]
                (let [diff (if (= "Yes" choice) 1 0)]
                  (is (= counter (get-counters (refresh vmi) :agenda)))
                  (is (= 4 (:click (get-runner))))
                  (click-prompt state :corp choice)
                  (is (= (- 4 diff) (:click (get-runner))))
                  (is (= (- counter diff) (get-counters (refresh vmi) :agenda)))
                  (take-credits state :runner)
                  (take-credits state :corp)))]
        (play-and-score state "Voting Machine Initiative")
        (take-credits state :corp)
        (let [vmi-scored (get-scored state :corp 0)]
          (vmi-test vmi-scored "Yes" 3)
          (vmi-test vmi-scored "No" 2)
          (vmi-test vmi-scored "Yes" 2)
          (vmi-test vmi-scored "Yes" 1)
          (is (no-prompt? state :corp) "No prompt as there are no agenda counters left")))))

(deftest vulcan-coverup
  ;; Vulcan Coverup
  (do-game
    (new-game {:corp {:deck [(qty "Vulcan Coverup" 2)]}})
    (play-from-hand state :corp "Vulcan Coverup" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :runner "Steal")
    (is (= 1 (count-bad-pub state)) "Took 1 bad pub from stolen agenda")
    (take-credits state :runner)
    (play-and-score state "Vulcan Coverup")
    (is (= 2 (count (:discard (get-runner)))) "Did 2 meat damage upon scoring")))

(deftest vulnerability-audit
  ;; Vulnerability Audit - cannot be scored while installed
  (do-game
   (new-game {:corp {:deck ["Vulnerability Audit" "Project Atlas"]}})
   (play-from-hand state :corp "Vulnerability Audit" "New remote")
   (play-from-hand state :corp "Project Atlas" "New remote")
   (core/add-prop state :corp (get-content state :remote1 0) :advance-counter 4)
   (score state :corp (get-content state :remote1 0))
   (is (= 0 (count (:scored (get-corp)))) "Cannot be scored on installed turn")
   (core/add-prop state :corp (get-content state :remote2 0) :advance-counter 3)
   (score state :corp (get-content state :remote2 0))
   (is (= 1 (count (:scored (get-corp)))) "Can score other agendas just fine")
   (take-credits state :corp)
   (take-credits state :runner)
   (score state :corp (get-content state :remote1 0))
   (is (= 2 (count (:scored (get-corp)))) "Can be scored turn after install")))

(deftest water-monopoly
  ;; Water Monopoly
  (do-game
      (new-game {:corp {:hand ["Water Monopoly"]}
                 :runner {:hand ["Fan Site" "Levy Advanced Research Lab"]}})
      (play-and-score state "Water Monopoly")
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))) "Runner should start with 5 credits")
      (play-from-hand state :runner "Fan Site")
      (is (= 5 (:credit (get-runner))) "Shouldn't lose any credits")
      (play-from-hand state :runner "Levy Advanced Research Lab")
      (is (zero? (:credit (get-runner))) "Should cost an extra credit to play")))

(deftest water-monopoly-interaction-with-installing-facedown
    ;; interaction with installing facedown
    (do-game
      (new-game {:corp {:hand ["Water Monopoly"]}
                 :runner {:hand ["Hunting Grounds"]
                          :deck [(qty "Algo Trading" 3)]}})
      (play-and-score state "Water Monopoly")
      (take-credits state :corp)
      (play-from-hand state :runner "Hunting Grounds")
      (card-ability state :runner (get-resource state 0) 0)
      (is (= 3 (:credit (get-runner))) "Shouldn't lose any credits")))
