(ns game.cards.programs-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.cost-fns :refer [card-ability-cost]]
   [game.core.payment :refer [->c]]
   [game.core.props :refer [add-counter]]
   [game.core.threat :refer [threat-level get-threat-level]]
   [game.test-framework :refer :all]
   [game.utils :as utils]))

;; helper functions for writing tests

(defn- install-hush-and-run
  [card {:keys [hushed counters server tags threat players rig unrezzed scored assets] :as args}]
  (let [;; remap things like ':hand x' to ':hand [(qty 'ipo' x)] (and deck too, for both sides)
        players (if (int? (get-in players [:corp :hand]))
                  (assoc-in players [:corp :hand] (qty "IPO" (get-in players [:corp :hand])))
                  players)
        players (if (int? (get-in players [:corp :deck]))
                  (assoc-in players [:corp :deck] (qty "IPO" (get-in players [:corp :deck])))
                  players)
        players (if (int? (get-in players [:runner :hand]))
                         (assoc-in players [:runner :hand]
                                   (qty "Inti" (get-in players [:runner :hand])))
                         players)
        players (if (int? (get-in players [:runner :deck]))
                  (assoc-in players [:runner :deck]
                            (qty "Inti" (get-in players [:runner :deck])))
                  players)
        ;; add the target ice to the corp hand
        players (assoc-in players [:corp :hand]
                          (cons card (get-in players [:corp :hand])))
        ;; add agendas to score to the corp hand
        players (assoc-in players [:corp :hand]
                          (concat scored (get-in players [:corp :hand])))
        ;; add assets to the corp hand
        players (assoc-in players [:corp :hand]
                          (concat assets (get-in players [:corp :hand])))
        ;; add the rig to the runner hand
        players (assoc-in players [:runner :hand]
                          (concat rig (get-in players [:runner :hand])))
        ;; add hush to the runner hand
        players (if hushed
                  (assoc-in players [:runner :hand]
                            (concat ["Hush"] (get-in players [:runner :hand])))
                  players)
        state (new-game players)
        server (or server "HQ")
        server-key (cond
                     (= server "New remote") :remote1
                     (= server "HQ") :hq
                     (= server "R&D") :rd
                     (= server "Archives") :archives)]
    ;; adjust the threat level for threat: ... subs
    (when threat
      (do (game.core.change-vals/change
            ;; theoretically, either side is fine!
            state (first (shuffle [:corp :runner])) {:key :agenda-point :delta threat})
          (is (threat-level threat state) "Threat set")
          (is (not (threat-level (inc threat) state)) "Threat is accurate")))
    (play-from-hand state :corp card server)
    (let [ice (get-ice state server-key 0)]
      ;; gain credits to rez the ice
      (core/gain state :corp :credit (:cost ice))
      ;; score agendas when needed
      (doseq [s scored]
        (play-and-score state s))
      (doseq [a assets]
        (let [target-card (first (filter #(= (:title %) a) (:hand (:corp @state))))
              cost (:cost target-card)]
          (core/gain state :corp :credit cost)
          (core/gain state :corp :click 1)
          (play-from-hand state :corp a "New remote")
          (rez state :corp
               (get-content state (keyword (str "remote" (dec (:rid @state)))) 0))))
      ;;adjust counters when needed
      (when counters
        ;; counters of the form :counter {:power x :credit x}
        (doseq [[c-type c-count] counters]
          (core/add-counter state :corp (get-ice state server-key 0) c-type c-count)))
      (when-not unrezzed
        (rez state :corp (get-ice state server-key 0)))
      ;; gain tags when required
      (when tags
        (gain-tags state :runner tags)
        (is (= tags (count-tags state)) (str "Have " tags " tags")))
      ;; ensure we start with the specified credit count (default 5)
      ;; by not actually clicking for creds
      (core/lose state :corp :click 2)
      (take-credits state :corp)
      ;; install any cards from the runner rig (cheat click/cred costs)
      (doseq [r rig]
        (let [target-card (first (filter #(= (:title %) r) (:hand (:runner @state))))
              cost (:cost target-card)]
          (core/gain state :runner :credit cost)
          (core/gain state :runner :click 1)
          (play-from-hand state :runner r)))
      ;; install hush if desired
      (when hushed
        (do (core/gain state :runner :credit 1)
            (core/gain state :runner :click 1)
            (play-from-hand state :runner "Hush")
            (click-card state :runner card)))
      (run-on state server-key))
    state))

(defn- advancable-while-hushed-test?
  "Tests that a card is not advanceable while hushed, and also checks the rez requirement too"
  [card rez-req]
  (do-game
    (new-game {:corp {:hand [card] :credits 15}
               :runner {:hand ["Hush"]}})
    (play-from-hand state :corp card "HQ")
    (let [ice (get-ice state :hq 0)]
      (when rez-req
        (is (can-be-advanced? state (refresh ice)) (str card " is advancable while unrezzed")))
      (rez state :corp ice)
      (is (can-be-advanced? state (refresh ice)) (str card " is advancable while rezzed"))
      (take-credits state :corp)
      (play-from-hand state :runner "Hush")
      (click-card state :runner card)
      (is (not (can-be-advanced? state (refresh ice)))
          (str card " is no longer advancable due to hush (rezzed)"))
      (when rez-req
        (derez state :corp (refresh ice))
        (is (not (can-be-advanced? state (refresh ice)))
            (str card " is no longer advancable due to hush (derezzed)"))))))

;; rest of tests

(deftest abaasy
  ;; Abaasy
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Enigma"]}
               :runner {:hand ["Abaasy" "Dirty Laundry"]
                        :deck ["Sure Gamble"]
                        :credits 10}})
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Abaasy")
    (run-on state "HQ")
    (rez state :corp (get-ice state :hq 0))
    (run-continue state)
    (card-ability state :runner (get-program state 0) 1)
    (card-ability state :runner (get-program state 0) 0)
    (click-prompt state :runner "Force the Runner to lose [Click]")
    (click-prompt state :runner "End the run")
    (click-card state :runner "Dirty Laundry")
    (is (no-prompt? state :runner))
    (is (= 1 (count (:discard (get-runner)))))
    (is (= 1 (count (:hand (get-runner)))))))

(deftest abagnale
  ;; Abagnale
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Enigma"]}
               :runner {:hand ["Abagnale"]
                        :credits 10}})
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Abagnale")
    (run-on state "HQ")
    (rez state :corp (get-ice state :hq 0))
    (run-continue state)
    (card-ability state :runner (get-program state 0) 2)
    (is (= :movement (:phase (get-run))) "Run has bypassed Enigma")
    (is (find-card "Abagnale" (:discard (get-runner))) "Abagnale is trashed")))

(deftest adept
  ;; Adept
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Spiderweb" "Cobra"]
                      :credits 10}
               :runner {:deck ["Adept" "Box-E"]
                        :credits 20}})
    (play-from-hand state :corp "Spiderweb" "HQ")
    (play-from-hand state :corp "Cobra" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Adept")
    (let [adept (get-program state 0)
          sw (get-ice state :hq 0)
          cobra (get-ice state :rd 0)]
      (is (= 2 (core/available-mu state)))
      (is (= 4 (get-strength (refresh adept))) "+2 strength for 2 unused MU")
      (play-from-hand state :runner "Box-E")
      (is (= 4 (core/available-mu state)))
      (is (= 6 (get-strength (refresh adept))) "+4 strength for 4 unused MU")
      (run-on state :hq)
      (rez state :corp sw)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -6]
            (card-ability state :runner (refresh adept) 0)
            (click-prompt state :runner "End the run")
            (click-prompt state :runner "End the run")
            (click-prompt state :runner "End the run"))
          "Spent 6 credits to break 3 barrier subs")
      (is (:broken (first (:subroutines (refresh sw)))) "Broke a barrier subroutine")
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state :rd)
      (rez state :corp cobra)
      (run-continue state)
      (card-ability state :runner (refresh adept) 0)
      (click-prompt state :runner "Trash a program")
      (click-prompt state :runner "Done")
      (is (:broken (first (:subroutines (refresh cobra)))) "Broke a sentry subroutine"))))

(deftest afterimage-requires-stealth-credits
  ;; Afterimage requires stealth credits to bypass ice
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Rototurret"]
                        :credits 10}
                 :runner {:hand ["Afterimage"]
                          :credits 10}})
      (play-from-hand state :corp "Rototurret" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Afterimage")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (no-prompt? state :runner) "No bypass prompt")))
      

(deftest afterimage-can-only-be-used-once-per-turn-5032
    ;; Can only be used once per turn. #5032
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Rototurret"]
                        :credits 10}
                 :runner {:hand ["Afterimage" (qty "Mantle" 2)]
                          :credits 10}})
      (play-from-hand state :corp "Rototurret" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Afterimage")
      (play-from-hand state :runner "Mantle")
      (play-from-hand state :runner "Mantle")
      (core/gain state :runner :click 1)
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-card state :runner (get-program state 1))
      (click-card state :runner (get-program state 2))
      (is (= :movement (:phase (get-run))) "Run has bypassed Rototurret")
      (run-jack-out state)
      (run-on state "HQ")
      (run-continue state)
      (is (no-prompt? state :runner) "No bypass prompt")))

(deftest aghora-swap-ability
    ;; Swap ability
    (testing "Doesnt work if no Deva in hand"
      (do-game
        (new-game {:runner {:hand ["Aghora" "Corroder"]
                            :credits 10}})
        (take-credits state :corp)
        (play-from-hand state :runner "Aghora")
        (card-ability state :runner (get-program state 0) 2)
        (is (no-prompt? state :runner) "No select prompt as there's no Deva in hand")))
    (testing "Works with another deva in hand"
      (doseq [deva ["Aghora" "Sadyojata" "Vamadeva"]]
        (do-game
          (new-game {:runner {:hand ["Aghora" deva]
                              :credits 10}})
          (take-credits state :corp)
          (play-from-hand state :runner "Aghora")
          (let [installed-deva (get-program state 0)]
            (card-ability state :runner installed-deva 2)
            (click-card state :runner (first (:hand (get-runner))))
            (is (not (utils/same-card? installed-deva (get-program state 0)))
                "Installed deva is not same as previous deva")
            (is (= deva (:title (get-program state 0)))))))))

(deftest aghora-break-ability-targets-ice-with-rez-cost-5-or-higher
    ;; Break ability targets ice with rez cost 5 or higher
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Endless EULA"]
                        :credits 10}
                 :runner {:hand ["Aghora"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Endless EULA" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Aghora")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (is (no-prompt? state :runner) "No break prompt")
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state "R&D")
      (rez state :corp (get-ice state :rd 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (is (not (no-prompt? state :runner)) "Have a break prompt")
      (click-prompt state :runner "End the run unless the Runner pays 1 [Credits]")
      (click-prompt state :runner "Done")
      (is (:broken (first (:subroutines (get-ice state :rd 0))))
          "The break ability worked")))

(deftest aghora-break-ability-interacts-with-xanadu
    ;; Break ability targets ice with rez cost 4 or higher when using xanadu
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Rototurret"]
                        :credits 10}
                 :runner {:hand ["Aghora" "Xanadu"]
                          :credits 20}})
      (play-from-hand state :corp "Rototurret" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Aghora")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (is (no-prompt? state :runner) "No break prompt")
      (run-continue state :movement)
      (run-jack-out state)
      (play-from-hand state :runner "Xanadu")
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (is (not (no-prompt? state :runner)) "Have a break prompt")
      (click-prompt state :runner "Trash a program")
      (click-prompt state :runner "Done")
      (is (:broken (first (:subroutines (get-ice state :hq 0))))
          "The break ability worked")))

(deftest algernon-use-successful-run
    ;; Use, successful run
    (do-game
      (new-game {:runner {:deck ["Algernon"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Algernon")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 8 (:credit (get-runner))) "Runner starts with 8 credits")
      (is (= 4 (:click (get-runner))) "Runner starts with 4 clicks")
      (click-prompt state :runner "Yes")
      (is (= 6 (:credit (get-runner))) "Runner pays 2 credits")
      (is (= 5 (:click (get-runner))) "Runner gains 1 click")
      (run-empty-server state "Archives")
      (take-credits state :runner)
      (is (empty? (:discard (get-runner))) "No cards trashed")
      (is (= "Algernon" (:title (get-program state 0))) "Algernon still installed")))

(deftest algernon-use-no-successful-run
    ;; Use, no successful run
    (do-game
      (new-game {:runner {:deck ["Algernon"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Algernon")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 8 (:credit (get-runner))) "Runner starts with 8 credits")
      (is (= 4 (:click (get-runner))) "Runner starts with 4 clicks")
      (click-prompt state :runner "Yes")
      (is (= 6 (:credit (get-runner))) "Runner pays 2 credits")
      (is (= 5 (:click (get-runner))) "Runner gains 1 click")
      (run-on state "Archives")
      (run-jack-out state)
      (take-credits state :runner)
      (is (= 1 (count (:discard (get-runner)))) "Algernon trashed")
      (is (empty? (get-program state)) "No programs installed")))

(deftest algernon-not-used-no-successful-run
    ;; Not used, no successful run
    (do-game
      (new-game {:runner {:deck ["Algernon"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Algernon")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 8 (:credit (get-runner))) "Runner starts with 8 credits")
      (is (= 4 (:click (get-runner))) "Runner starts with 4 clicks")
      (click-prompt state :runner "No")
      (is (= 8 (:credit (get-runner))) "No credits spent")
      (is (= 4 (:click (get-runner))) "No clicks gained")
      (run-on state "Archives")
      (run-jack-out state)
      (take-credits state :runner)
      (is (empty? (:discard (get-runner))) "No cards trashed")
      (is (= "Algernon" (:title (get-program state 0))) "Algernon still installed")))

(deftest algernon-allows-payment-from-recurring-sources
    ;; Allows payment from recurring sources
    (do-game
     (new-game {:runner {:deck ["Algernon" "Multithreader"]}})
     (take-credits state :corp)
     (play-from-hand state :runner "Algernon")
     (play-from-hand state :runner "Multithreader")
     (take-credits state :runner)
     (take-credits state :corp)
     (is (= 4 (:credit (get-runner))) "Runner starts with 4 credit")
     (click-prompt state :runner "Yes")
     (is (= "Choose a credit providing card (0 of 2 [Credits])" (:msg (prompt-map :runner))) "Runner prompted to choose credit sources")
     (click-card state :runner (get-program state 1))
     (click-card state :runner (get-program state 1))
     (is (= 4 (:credit (get-runner))) "Runner pays 0 credits from their pool")
     (is (= 5 (:click (get-runner))) "Runner gains 1 click")))

(deftest ^{:card-title "alias"}
  alias-breaker
  ;; Alias
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Zed 1.0" 2)]}
               :runner {:hand ["Alias"]
                        :credits 10}})
    (play-from-hand state :corp "Zed 1.0" "HQ")
    (play-from-hand state :corp "Zed 1.0" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Alias")
    (let [alias (get-program state 0)
          zed1 (get-ice state :hq 0)
          zed2 (get-ice state :remote1 0)]
      (is (= 1 (get-strength (refresh alias))) "Starts with 2 strength")
      (card-ability state :runner (refresh alias) 1)
      (is (= 4 (get-strength (refresh alias))) "Can gain strength outside of a run")
      (run-on state :hq)
      (rez state :corp (refresh zed1))
      (run-continue state)
      (card-ability state :runner (refresh alias) 0)
      (click-prompt state :runner "Do 1 core damage")
      (click-prompt state :runner "Done")
      (is (= 1 (count (filter :broken (:subroutines (refresh zed1))))) "The subroutine is broken")
      (run-continue state :movement)
      (run-jack-out state)
      (is (= 1 (get-strength (refresh alias))) "Drops back down to base strength on run end")
      (run-on state :remote1)
      (rez state :corp (refresh zed2))
      (run-continue state)
      (card-ability state :runner (refresh alias) 0)
      (is (no-prompt? state :runner) "No break prompt because we're running a remote"))))

(deftest amina
  ;; Amina ability
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Enigma"]}
               :runner {:hand ["Amina"]
                        :credits 15}})
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Amina")
    (let [amina (get-program state 0)
          enigma (get-ice state :hq 0)]
      (run-on state :hq)
      (rez state :corp (refresh enigma))
      (is (= 4 (:credit (get-corp))))
      (run-continue state)
      (card-ability state :runner (refresh amina) 0)
      (click-prompt state :runner "Force the Runner to lose [Click]")
      (click-prompt state :runner "Done")
      (run-continue state)
      (is (= 4 (:credit (get-corp))) "Corp did not lose 1c because not all subs were broken")
      (run-jack-out state)
      (run-on state :hq)
      (run-continue state)
      (card-ability state :runner (refresh amina) 0)
      (click-prompt state :runner "Force the Runner to lose [Click]")
      (click-prompt state :runner "End the run")
      (run-continue state)
      (is (= 3 (:credit (get-corp))) "Corp lost 1 credit")
      (run-jack-out state)
      (run-on state :hq)
      (run-continue state)
      (card-ability state :runner (refresh amina) 0)
      (click-prompt state :runner "Force the Runner to lose [Click]")
      (click-prompt state :runner "End the run")
      (run-continue state)
      (is (= 3 (:credit (get-corp))) "Ability only once per turn"))))

(deftest amina-amina-only-triggers-on-itself-issue-4716
    ;; Amina only triggers on itself. Issue #4716
    (do-game
      (new-game {:runner {:deck ["Amina" "Yog.0"]}
                 :corp {:deck ["Enigma"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 20 :click 1)
      (play-from-hand state :runner "Amina")
      (play-from-hand state :runner "Yog.0")
      (let [amina (get-program state 0)
            yog (get-program state 1)
            enima (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp (refresh enima))
        (run-continue state)
        (card-ability state :runner (refresh amina) 0)
        (click-prompt state :runner "Force the Runner to lose [Click]")
        (click-prompt state :runner "End the run")
        (run-continue state :movement)
        (run-jack-out state)
        (run-on state :hq)
        (run-continue state)
        (card-ability state :runner (refresh yog) 0)
        (click-prompt state :runner "Force the Runner to lose [Click]")
        (is (changed? [(:credit (get-corp)) 0]
              (click-prompt state :runner "End the run")
              (run-continue state))
            "No credit gain from Amina")
        (run-jack-out state))))

(deftest analog-dreamers
  ;; Analog Dreamers
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Hostile Takeover" "PAD Campaign" "Oaktown Renovation"]}
               :runner {:hand ["Analog Dreamers"]}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (advance state (get-content state :remote1 0) 1)
    (play-from-hand state :corp "Oaktown Renovation" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Analog Dreamers")
    (card-ability state :runner (get-program state 0) 0)
    (run-continue state)
    (click-prompt state :runner "Analog Dreamers")
    (click-card state :runner "Hostile Takeover")
    (is (= "Choose a card to shuffle into R&D" (:msg (prompt-map :runner)))
        "Can't click on Hostile Takeover")
    (click-card state :runner "Oaktown Renovation")
    (is (= "Choose a card to shuffle into R&D" (:msg (prompt-map :runner)))
        "Can't click on Oaktown Renovation because it's face up")
    (let [number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))
          pad (get-content state :remote2 0)]
      (click-card state :runner "PAD Campaign")
      (is (< number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))) "Should be shuffled")
      (is (find-card "PAD Campaign" (:deck (get-corp))) "PAD Campaign is shuffled into R&D")
      (is (nil? (refresh pad)) "PAD Campaign is shuffled into R&D"))))

(deftest ankusa-boost-1-strength-for-1-credit
    ;; Boost 1 strength for 1 credit
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Battlement"]}
                 :runner {:hand ["Ankusa"]
                          :credits 15}})
      (play-from-hand state :corp "Battlement" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Ankusa")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (let [ankusa (get-program state 0)
            credits (:credit (get-runner))]
          (card-ability state :runner ankusa 1)
          (is (= (dec credits) (:credit (get-runner))) "Boost 1 for 1 credit")
          (is (= (inc (get-strength ankusa)) (get-strength (refresh ankusa)))
              "Ankusa gains 1 strength"))))

(deftest ankusa-break-1-for-2-credits
    ;; Break 1 for 2 credits
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Battlement"]}
                 :runner {:hand ["Ankusa"]
                          :credits 15}})
      (play-from-hand state :corp "Battlement" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Ankusa")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (let [ankusa (get-program state 0)]
        (card-ability state :runner ankusa 1)
        (card-ability state :runner ankusa 1)
        (is (changed? [(:credit (get-runner)) -2]
              (card-ability state :runner ankusa 0)
              (click-prompt state :runner "End the run")
              (click-prompt state :runner "Done"))
            "Break ability costs 2 credits"))))

(deftest ankusa-breaking-an-ice-fully-returns-it-to-hand-issue-4711
    ;; Breaking an ice fully returns it to hand. Issue #4711
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Battlement"]}
                 :runner {:hand ["Ankusa"]
                          :credits 15}})
      (play-from-hand state :corp "Battlement" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Ankusa")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (let [ankusa (get-program state 0)]
        (card-ability state :runner ankusa 1)
        (card-ability state :runner ankusa 1)
        (card-ability state :runner ankusa 0)
        (click-prompt state :runner "End the run")
        (click-prompt state :runner "End the run")
        (is (find-card "Battlement" (:hand (get-corp))) "Battlement should be back in hand"))))

(deftest atman-installing-with-0-power-counters
    ;; Installing with 0 power counters
    (do-game
      (new-game {:runner {:deck ["Atman"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Atman")
      (click-prompt state :runner "0")
      (is (= 3 (core/available-mu state)))
      (let [atman (get-program state 0)]
        (is (zero? (get-counters atman :power)) "0 power counters")
        (is (zero? (get-strength atman)) "0 current strength"))))

(deftest atman-installing-with-2-power-counters
    ;; Installing with 2 power counters
    (do-game
      (new-game {:runner {:deck ["Atman"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Atman")
      (click-prompt state :runner "2")
      (is (= 3 (core/available-mu state)))
      (let [atman (get-program state 0)]
        (is (= 2 (get-counters atman :power)) "2 power counters")
        (is (= 2 (get-strength atman)) "2 current strength"))))

(deftest atman-paying-for-install-with-multithreader
    ;; Paying for install with multithreader
    (do-game
      (new-game {:runner {:deck ["Atman" "Multithreader"] :credits 8}})
      (take-credits state :corp)
      (play-from-hand state :runner "Multithreader")
      (play-from-hand state :runner "Atman")
      (click-prompt state :runner "4")
      (click-card state :runner (get-program state 0))
      (click-card state :runner (get-program state 0))
      (is (= 2 (core/available-mu state)))
      (let [atman (get-program state 1)]
        (is (= 4 (get-counters atman :power)) "4 power counters")
        (is (= 4 (get-strength atman)) "4 current strength"))))

(deftest au-revoir
  ;; Au Revoir - Gain 1 credit every time you jack out
  (do-game
    (new-game {:runner {:deck [(qty "Au Revoir" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Au Revoir")
    (run-on state "Archives")
    (run-jack-out state)
    (is (= 5 (:credit (get-runner))) "Gained 1 credit from jacking out")
    (play-from-hand state :runner "Au Revoir")
    (run-on state "Archives")
    (run-jack-out state)
    (is (= 6 (:credit (get-runner))) "Gained 1 credit from each copy of Au Revoir")))

(deftest audrey-v2
  (do-game
    (new-game {:corp {:hand ["Rashida Jaheem" "Vanilla"]}
               :runner {:hand ["Knifed" "Audrey v2" "Sure Gamble"]}})
    (play-from-hand state :corp "Vanilla" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Audrey v2")
    (run-on state :hq)
    (run-continue state)
    (run-continue state)
    (click-prompt state :runner "Pay 1 [Credits] to trash")
    (is (= 1 (get-counters (get-program state 0) :virus)) "Audrey gains virus counter from trash")
    (play-from-hand state :runner "Knifed")
    (click-prompt state :runner "HQ")
    (rez state :corp (get-ice state :hq 0))
    (run-continue state :encounter-ice)
    (card-ability state :runner (get-program state 0) 0)
    (click-prompt state :runner "End the run")
    (is (= 2 (count (:discard (get-corp)))) "Vanilla Campaign trashed")
    (is (= 0 (get-counters (get-program state 0) :virus)) "No virus counter because not accessed")))

(deftest aumakua-gain-counter-on-no-trash
    ;; Gain counter on no trash
    (do-game
      (new-game {:corp {:deck [(qty "PAD Campaign" 3)]}
                 :runner {:deck ["Aumakua"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Aumakua")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (is (= 1 (get-counters (get-program state 0) :virus)) "Aumakua gains virus counter from no-trash")
      (core/gain state :runner :credit 5)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 4 [Credits] to trash")
      (is (= 1 (get-counters (get-program state 0) :virus)) "Aumakua does not gain virus counter from trash")))

(deftest aumakua-gain-counters-on-empty-archives
    ;; Gain counters on empty archives
    (do-game
      (new-game {:runner {:deck ["Aumakua"]}
                 :options {:start-as :runner}})
      (play-from-hand state :runner "Aumakua")
      (run-empty-server state :archives)
      (is (= 1 (get-counters (get-program state 0) :virus)) "Aumakua gains virus counter from accessing empty Archives")))

(deftest aumakua-neutralize-all-threats-interaction
    ;; Neutralize All Threats interaction
    (do-game
      (new-game {:corp {:deck [(qty "PAD Campaign" 3)]}
                 :runner {:deck ["Aumakua" "Neutralize All Threats"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Aumakua")
      (play-from-hand state :runner "Neutralize All Threats")
      (core/gain state :runner :credit 5)
      (run-empty-server state "Server 1")
      (is (zero? (get-counters (get-program state 0) :virus)) "Aumakua does not gain virus counter from ABT-forced trash")))

(deftest aumakua-gang-sign-interaction
  ;; Gang sign should give turtle counters
  (do-game
   (new-game {:corp {:hand ["Hostile Takeover" "NGO Front"]}
              :runner {:hand ["Aumakua" (qty "Gang Sign" 2)]}})
   (take-credits state :corp)
   (play-from-hand state :runner "Aumakua")
   (play-from-hand state :runner "Gang Sign")
   (play-from-hand state :runner "Gang Sign")
   (take-credits state :runner)
   (play-and-score state "Hostile Takeover")
   (click-prompt state :runner "Gang Sign")
   (click-prompt state :runner "No action")
   (is (= 1 (get-counters (get-program state 0) :virus)) "Aumakua gained a virus counter from the first breach")
   (click-prompt state :runner "Pay 1 [Credits] to trash")
   (is (= 1 (get-counters (get-program state 0) :virus)) "Aumakua gained no virus counter from the second breach")))

(deftest aumakua-divide-and-conquer
  ;; divide and conquer should proc turtle for each access
  (do-game
   (new-game {:corp {:discard ["Hostile Takeover"] :deck ["Ice Wall"] :hand ["Ice Wall"]}
              :runner {:hand ["Aumakua" "Sure Gamble" "Divide and Conquer"]}})
   (take-credits state :corp)
   (play-from-hand state :runner "Sure Gamble")
   (play-from-hand state :runner "Aumakua")
   (play-run-event state "Divide and Conquer" :archives)
   (click-prompt state :runner "Steal")
   (is (= 0 (get-counters (get-program state 0) :virus)) "Aumakua gained no virus counter")
   (click-prompt state :runner "No action")
   (is (= 1 (get-counters (get-program state 0) :virus)) "Aumakua gained a virus counter")
   (click-prompt state :runner "No action")
   (is (= 2 (get-counters (get-program state 0) :virus)) "Aumakua gained a virus counter")))

(deftest baba-yaga
  ;; Baba Yaga
  (do-game
    (new-game {:runner {:deck ["Baba Yaga" "Faerie" "Yog.0" "Sharpshooter"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Baba Yaga")
    (play-from-hand state :runner "Sharpshooter")
    (let [baba (get-program state 0)
          base-abicount (count (:abilities baba))]
      (card-ability state :runner baba 0)
      (click-card state :runner "Faerie")
      (is (= (+ 2 base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga gained 2 subroutines from Faerie")
      (card-ability state :runner (refresh baba) 0)
      (click-card state :runner (find-card "Yog.0" (:hand (get-runner))))
      (is (= (+ 3 base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga gained 1 subroutine from Yog.0")
      (trash state :runner (first (:hosted (refresh baba))))
      (is (= (inc base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga lost 2 subroutines from trashed Faerie")
      (card-ability state :runner baba 1)
      (click-card state :runner (find-card "Sharpshooter" (:program (:rig (get-runner)))))
      (is (= 2 (count (:hosted (refresh baba)))) "Faerie and Sharpshooter hosted on Baba Yaga")
      (is (= 1 (core/available-mu state)) "1 MU left with 2 breakers on Baba Yaga")
      (is (= 4 (:credit (get-runner))) "-5 from Baba, -1 from Sharpshooter played into Rig, -5 from Yog"))))

(deftest bankroll
  ;; Bankroll - Includes check for Issue #4334
  (do-game
    (new-game {:runner {:deck ["Bankroll" "Jak Sinclair"]}})
    (take-credits state :corp)
    (core/gain state :runner :click 10)
    (play-from-hand state :runner "Bankroll")
    (play-from-hand state :runner "Jak Sinclair")
    (is (= 3 (core/available-mu state)) "Bankroll uses up 1 MU")
    (is (= 1 (:credit (get-runner))) "Bankroll cost 1 to install")
    (let [bankroll (get-program state 0)
          hosted-credits #(get-counters (refresh bankroll) :credit)]
      (is (zero? (hosted-credits)) "No counters on Bankroll on install")
      (run-empty-server state "Archives")
      (is (= 1 (hosted-credits)) "One credit counter on Bankroll after one successful run")
      (run-empty-server state "R&D")
      (is (= 2 (hosted-credits)) "Two credit counter on Bankroll after two successful runs")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (= 3 (hosted-credits)) "Three credit counter on Bankroll after three successful runs")
      (take-credits state :runner)
      (take-credits state :corp)
      (end-phase-12 state :runner)
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "R&D")
      (run-continue state)
      (let [credits (:credit (get-runner))]
        (is (= 3 (hosted-credits)) "Jak Sinclair didn't trigger Bankroll")
        (card-ability state :runner bankroll 0)
        (is (= (+ 3 credits) (:credit (get-runner))) "Gained 3 credits when trashing Bankroll"))
      (is (= 1 (-> (get-runner) :discard count)) "Bankroll was trashed"))))

(deftest banner
  (do-game
    (new-game {:runner {:hand ["Banner" "Sure Gamble"]}
               :corp {:hand ["Border Control"]}})
    (play-from-hand state :corp "Border Control" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Banner")
    (run-on state :hq)
    (let [bc (get-ice state :hq 0)
          banner (get-program state 0)]
      (rez state :corp (refresh bc))
      (run-continue state :encounter-ice)
      (is (changed? [(:credit (get-runner)) -2]
            (card-ability state :runner (refresh banner) 0))
          "banner costs 2")
      (is (changed? [(:credit (get-corp)) +1]
            (fire-subs state (refresh bc)))
          "corp gained 1 from BC")
      (is (:run @state) "Run still ongoing (banner prevented)")
      (card-ability state :corp (refresh bc) 0)
      (is (nil? (refresh bc)))
      (is (nil? (get-run))))) "BC ability ended run (banner no prevento)")

(deftest begemot
  (do-game
    (new-game {:runner {:hand ["Begemot" (qty "Sure Gamble" 4)] :credits 10}})
    (take-credits state :corp)
    (play-from-hand state :runner "Begemot")
    (is (= 3 (count (:hand (get-runner)))) "suffed 1 brain, 3 cards in hand")
    (let [vege (get-program state 0)]
      (is (= 3 (get-strength (refresh vege))))
      (is (changed? [(get-strength (refresh vege)) 1]
            (damage state :runner :brain 1))
          "Gained 1str from core damage")
      (is (changed? [(get-strength (refresh vege)) 1]
            (damage state :runner :brain 1))
          "Gained 1str from core damage")
      (is (changed? [(get-strength (refresh vege)) 1]
            (damage state :runner :brain 1))
          "Gained 1str from core damage")
      (is (= 6 (get-strength (refresh vege)))))))

(deftest berserker
  ;; Berserker
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall" "Hive" "Enigma"]
                      :credits 100}
               :runner {:hand ["Berserker"]}})
    (play-from-hand state :corp "Ice Wall" "Archives")
    (play-from-hand state :corp "Hive" "R&D")
    (play-from-hand state :corp "Enigma" "HQ")
    (rez state :corp (get-ice state :archives 0))
    (rez state :corp (get-ice state :rd 0))
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Berserker")
    (let [berserker (get-program state 0)]
      (is (= 2 (get-strength (refresh berserker))) "Berserker strength starts at 2")
      (run-on state :archives)
      (run-continue state)
      (is (= 3 (get-strength (refresh berserker))) "Berserker gains 1 strength from Ice Wall")
      (run-continue state :movement)
      (run-jack-out state)
      (is (= 2 (get-strength (refresh berserker))) "Berserker strength resets at end of run")
      (run-on state :rd)
      (run-continue state)
      (is (= 7 (get-strength (refresh berserker))) "Berserker gains 5 strength from Hive")
      (run-continue state :movement)
      (run-jack-out state)
      (is (= 2 (get-strength (refresh berserker))) "Berserker strength resets at end of run")
      (run-on state :hq)
      (run-continue state)
      (is (= 2 (get-strength (refresh berserker))) "Berserker gains 0 strength from Enigma (non-barrier)"))))

(deftest black-orchestra
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Macrophage"]
                        :credits 10}
                 :runner {:hand ["Black Orchestra"]
                          :credits 100}})
      (play-from-hand state :corp "Macrophage" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Black Orchestra")
      (let [bo (get-program state 0)]
        (run-on state :hq)
        (run-continue state)
        (card-ability state :runner bo 0)
        (card-ability state :runner bo 0)
        (is (no-prompt? state :runner) "Has no break prompt as strength isn't high enough")
        (card-ability state :runner bo 0)
        (is (= 8 (get-strength (refresh bo))) "Pumped Black Orchestra up to str 8")
        (click-prompt state :runner "Trace 4 - Purge virus counters")
        (click-prompt state :runner "Trace 3 - Trash a virus")
        (is (= 2 (count (filter :broken (:subroutines (get-ice state :hq 0)))))))))

(deftest black-orchestra-auto-pump
    ;; auto-pump
    (testing "Pumping more than once, breaking more than once"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Macrophage"]
                          :credits 10}
                   :runner {:hand ["Black Orchestra"]
                            :credits 100}})
        (play-from-hand state :corp "Macrophage" "HQ")
        (rez state :corp (get-ice state :hq 0))
        (take-credits state :corp)
        (play-from-hand state :runner "Black Orchestra")
        (let [bo (get-program state 0)]
          (run-on state :hq)
          (run-continue state)
          (is (changed? [(:credit (get-runner)) -12]
                (auto-pump-and-break state (refresh bo)))
              "Paid 12 to fully break Macrophage with Black Orchestra")
          (is (= 10 (get-strength (refresh bo))) "Pumped Black Orchestra up to str 10")
          (is (= 0 (count (remove :broken (:subroutines (get-ice state :hq 0))))) "Broken all subroutines"))))
    (testing "No pumping, breaking more than once"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Aiki"]
                          :credits 10}
                   :runner {:hand ["Black Orchestra"]
                            :credits 100}})
        (play-from-hand state :corp "Aiki" "HQ")
        (rez state :corp (get-ice state :hq 0))
        (take-credits state :corp)
        (play-from-hand state :runner "Black Orchestra")
        (let [bo (get-program state 0)]
          (run-on state :hq)
          (run-continue state)
          (is (changed? [(:credit (get-runner)) -6]
                (auto-pump-and-break state (refresh bo)))
              "Paid 6 to fully break Aiki with Black Orchestra")
          (is (= 6 (get-strength (refresh bo))) "Pumped Black Orchestra up to str 6")
          (is (= 0 (count (remove :broken (:subroutines (get-ice state :hq 0))))) "Broken all subroutines"))))
    (testing "Pumping and breaking once"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Enigma"]
                          :credits 10}
                   :runner {:hand ["Black Orchestra"]
                            :credits 100}})
        (play-from-hand state :corp "Enigma" "HQ")
        (rez state :corp (get-ice state :hq 0))
        (take-credits state :corp)
        (play-from-hand state :runner "Black Orchestra")
        (let [bo (get-program state 0)]
          (run-on state :hq)
          (run-continue state)
          (is (changed? [(:credit (get-runner)) -3]
                (auto-pump-and-break state (refresh bo)))
              "Paid 3 to fully break Enigma with Black Orchestra")
          (is (= 4 (get-strength (refresh bo))) "Pumped Black Orchestra up to str 6")
          (is (= 0 (count (remove :broken (:subroutines (get-ice state :hq 0))))) "Broken all subroutines"))))
    (testing "No auto-break on unbreakable subs"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Afshar"]
                          :credits 10}
                   :runner {:hand ["Black Orchestra"]
                            :credits 100}})
        (play-from-hand state :corp "Afshar" "HQ")
        (rez state :corp (get-ice state :hq 0))
        (take-credits state :corp)
        (play-from-hand state :runner "Black Orchestra")
        (let [bo (get-program state 0)]
          (run-on state :hq)
          (run-continue state)
          (is (empty? (filter #(:dynamic %) (:abilities (refresh bo)))) "No auto-pumping option for Afshar"))))
    (testing "Install and Auto-pump-and-break for forced encounters"
      (do-game
        (new-game {:corp {:deck [(qty "Archangel" 5)]
                          :hand ["Archangel"]
                          :credits 10}
                   :runner {:hand ["Black Orchestra"]
                            :credits 100}})
        (take-credits state :corp)
        (trash-from-hand state :runner "Black Orchestra")
        (run-empty-server state :hq)
        (click-prompt state :corp "Yes")
        (is (= "Install Black Orchestra from the heap?" (:msg (prompt-map :runner))) "Prompted to install Black Orchestra")
        (click-prompt state :runner "Yes")
        (let [bo (get-program state 0)]
          (is (installed? bo) "Black Orchestra is installed")
          (is (changed? [(:credit (get-runner)) -6]
                (auto-pump-and-break state (refresh bo)))
              "Paid 6 to fully break Macrophage with Black Orchestra")
          (is (= 6 (get-strength (refresh bo))) "Pumped Black Orchestra up to str 10")
          (is (= 0 (count (remove :broken (:subroutines (core/get-current-ice state))))) "Broken all subroutines")))))

(deftest black-orchestra-heap-locked
    ;; Heap Locked
    (do-game
      (new-game {:corp {:deck ["Enigma" "Blacklist"]}
                 :runner {:deck [(qty "Black Orchestra" 1)]}})
      (play-from-hand state :corp "Enigma" "Archives")
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (trash-from-hand state :runner "Black Orchestra")
      (run-on state "Archives")
      (rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (is (no-prompt? state :runner) "Black Orchestra prompt did not come up")))

(deftest blackstone-pay-credits-prompt
  ;; Pay-credits prompt
  (do-game
    (new-game {:runner {:deck ["Cloak" "Blackstone"]
                        :credits 10}})
    (take-credits state :corp)
    (play-from-hand state :runner "Cloak")
    (play-from-hand state :runner "Blackstone")
    (let [cl (get-program state 0)
          bs (get-program state 1)]
      (is (= ["Break 1 Barrier subroutine"
              "Add 4 strength for the remainder of the run (using at least 1 stealth [Credits])"]
             (mapv :label (:abilities bs))))
      (is (changed? [(:credit (get-runner)) -2
                     (get-strength (refresh bs)) 4]
                    (card-ability state :runner bs 1)
                    (click-card state :runner cl))
          "Used 1 credit from Cloak"))))

(deftest boi-tata
  (do-game
    (new-game {:corp {:credits 6 :deck ["Ansel 1.0"] }
               :runner {:credits 15
                        :hand ["Boi-tatá" "No Free Lunch"]}})
    (play-from-hand state :corp "Ansel 1.0" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Boi-tatá")
    (play-from-hand state :runner "No Free Lunch")
    (run-on state "HQ")
    (run-continue state)
    (let [boi (get-program state 0)]
      (is (= [(->c :credit 3)] (card-ability-cost
                                 state :runner
                                 (second (:abilities (refresh boi)))
                                 (refresh boi))))
      (card-ability state :runner (get-resource state 0) 0)
      (is (= [(->c :credit 2)] (card-ability-cost
                                 state :runner
                                 (second (:abilities (refresh boi)))
                                 (refresh boi)))
          "Cost reduction from trash"))))

(deftest botulus
  ;; Botulus
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall"]}
               :runner {:credits 15
                        :hand ["Botulus"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Botulus")
    (click-card state :runner (get-ice state :hq 0))
    (let [iw (get-ice state :hq 0)
          bot (first (:hosted (refresh iw)))]
      (run-on state :hq)
      (rez state :corp iw)
      (run-continue state)
      (card-ability state :runner (refresh bot) 0)
      (click-prompt state :runner "End the run")
      (is (zero? (count (remove :broken (:subroutines (refresh iw))))) "All subroutines have been broken"))))

(deftest brahman
  ;; Brahman
  (do-game
      (new-game {:runner {:deck ["Brahman" "Paricia" "Cache"]}
                 :corp {:deck ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Brahman")
      (play-from-hand state :runner "Paricia")
      (play-from-hand state :runner "Cache")
      (core/gain state :runner :credit 1)
      (let [brah (get-program state 0)
            par (get-program state 1)
            cache (get-program state 2)
            iw (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp iw)
        (run-continue state)
        (card-ability state :runner brah 0) ;break sub
        (click-prompt state :runner "End the run")
        (run-continue state)
        (is (= 0 (count (:deck (get-runner)))) "Stack is empty.")
        (click-card state :runner cache)
        (is (= 0 (count (:deck (get-runner)))) "Did not put Cache on top.")
        (click-card state :runner par)
        (is (= 1 (count (:deck (get-runner)))) "Paricia on top of Stack now."))))

(deftest brahman-prompt-on-etr
    ;; Prompt on ETR
    (do-game
      (new-game {:runner {:deck ["Brahman" "Paricia"]}
                 :corp {:deck ["Spiderweb"]}})
      (play-from-hand state :corp "Spiderweb" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Brahman")
      (play-from-hand state :runner "Paricia")
      (let [brah (get-program state 0)
            par (get-program state 1)
            spi (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp spi)
        (run-continue state)
        (card-ability state :runner brah 0) ;break sub
        (click-prompt state :runner "End the run")
        (click-prompt state :runner "End the run")
        (card-subroutine state :corp spi 0) ;ETR
        (is (= 0 (count (:deck (get-runner)))) "Stack is empty.")
        (click-card state :runner par)
        (is (= 1 (count (:deck (get-runner)))) "Paricia on top of Stack now."))))

(deftest brahman-brahman-works-with-nisei-tokens
    ;; Brahman works with Nisei tokens
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "Nisei MK II"]}
                 :runner {:deck ["Brahman"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-and-score state "Nisei MK II")
      (take-credits state :corp)
      (play-from-hand state :runner "Brahman")
      (let [brah (get-program state 0)
            iw (get-ice state :hq 0)
            nisei (get-scored state :corp 0)]
        (run-on state "HQ")
        (rez state :corp iw)
        (run-continue state)
        (card-ability state :runner brah 0) ;break sub
        (click-prompt state :runner "End the run")
        (card-ability state :corp (refresh nisei) 0) ; Nisei Token
        (is (= 0 (count (:deck (get-runner)))) "Stack is empty.")
        (click-card state :runner brah)
        (is (= 1 (count (:deck (get-runner)))) "Brahman on top of Stack now."))))

(deftest brahman-works-with-dynamic-ability
    ;; Works with dynamic ability
    (do-game
      (new-game {:runner {:deck ["Brahman" "Paricia"]}
                 :corp {:deck ["Spiderweb"]}})
      (play-from-hand state :corp "Spiderweb" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Brahman")
      (play-from-hand state :runner "Paricia")
      (core/gain state :runner :credit 1)
      (let [brah (get-program state 0)
            par (get-program state 1)
            spi (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp spi)
        (run-continue state)
        (auto-pump-and-break state (refresh brah))
        (core/continue state :corp nil)
        (is (= 0 (count (:deck (get-runner)))) "Stack is empty.")
        (click-card state :runner par)
        (is (= 1 (count (:deck (get-runner)))) "Paricia on top of Stack now."))))

(deftest bukhgalter-2c-for-breaking-subs-only-with-bukhgalter
    ;; 2c for breaking subs only with Bukhgalter
    (do-game
      (new-game {:runner {:deck ["Bukhgalter" "Mimic"]}
                 :corp {:deck ["Pup"]}})
      (play-from-hand state :corp "Pup" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 20 :click 1)
      (play-from-hand state :runner "Bukhgalter")
      (play-from-hand state :runner "Mimic")
      (let [bukhgalter (get-program state 0)
            mimic (get-program state 1)
            pup (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp (refresh pup))
        (run-continue state)
        (card-ability state :runner (refresh mimic) 0)
        (click-prompt state :runner "Do 1 net damage unless the Runner pays 1 [Credits]")
        (is (changed? [(:credit (get-runner)) -1]
              (click-prompt state :runner "Do 1 net damage unless the Runner pays 1 [Credits]"))
            "No credit gain from Bukhgalter for breaking with only Mimic")
        (run-continue state :movement)
        (run-jack-out state)
        (run-on state :hq)
        (run-continue state)
        (card-ability state :runner (refresh bukhgalter) 0)
        (click-prompt state :runner "Do 1 net damage unless the Runner pays 1 [Credits]")
        (click-prompt state :runner "Done")
        (card-ability state :runner (refresh mimic) 0)
        (is (changed? [(:credit (get-runner)) -1]
              (click-prompt state :runner "Do 1 net damage unless the Runner pays 1 [Credits]"))
            "No credit gain from Bukhgalter")
        (run-continue state :movement)
        (run-jack-out state)
        (run-on state :hq)
        (run-continue state)
        (card-ability state :runner (refresh bukhgalter) 0)
        (click-prompt state :runner "Do 1 net damage unless the Runner pays 1 [Credits]")
        (is (changed? [(:credit (get-runner)) (+ 2 -1)]
              (click-prompt state :runner "Do 1 net damage unless the Runner pays 1 [Credits]"))
            "2 credits gained from Bukhgalter"))))

(deftest bukhgalter-gaining-2c-only-once-per-turn
    ;; gaining 2c only once per turn
    (do-game
      (new-game {:runner {:deck ["Bukhgalter" "Mimic"]}
                 :corp {:deck ["Pup"]}})
      (play-from-hand state :corp "Pup" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Bukhgalter")
      (let [bukhgalter (get-program state 0)
            pup (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp (refresh pup))
        (run-continue state)
        (card-ability state :runner (refresh bukhgalter) 0)
        (click-prompt state :runner "Do 1 net damage unless the Runner pays 1 [Credits]")
        (is (changed? [(:credit (get-runner)) (+ 2 -1)]
              (click-prompt state :runner "Do 1 net damage unless the Runner pays 1 [Credits]"))
            "2 credits gained from Bukhgalter")
        (run-continue state :movement)
        (run-jack-out state)
        (run-on state :hq)
        (run-continue state)
        (card-ability state :runner (refresh bukhgalter) 0)
        (click-prompt state :runner "Do 1 net damage unless the Runner pays 1 [Credits]")
        (is (changed? [(:credit (get-runner)) -1]
              (click-prompt state :runner "Do 1 net damage unless the Runner pays 1 [Credits]"))
            "No credits gained from Bukhgalter"))))

(deftest bukhgalter-bukhgalter-only-triggers-on-itself-issue-4716
    ;; Bukhgalter only triggers on itself. Issue #4716
    (do-game
      (new-game {:runner {:deck ["Bukhgalter" "Mimic"]}
                 :corp {:deck ["Pup"]}})
      (play-from-hand state :corp "Pup" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 20 :click 1)
      (play-from-hand state :runner "Bukhgalter")
      (play-from-hand state :runner "Mimic")
      (let [bukhgalter (get-program state 0)
            mimic (get-program state 1)
            pup (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp (refresh pup))
        (run-continue state)
        (card-ability state :runner (refresh bukhgalter) 0)
        (click-prompt state :runner "Do 1 net damage unless the Runner pays 1 [Credits]")
        (click-prompt state :runner "Do 1 net damage unless the Runner pays 1 [Credits]")
        (run-continue state :movement)
        (run-jack-out state)
        (run-on state :hq)
        (run-continue state)
        (card-ability state :runner (refresh mimic) 0)
        (click-prompt state :runner "Do 1 net damage unless the Runner pays 1 [Credits]")
        (is (changed? [(:credit (get-runner)) -1]
              (click-prompt state :runner "Do 1 net damage unless the Runner pays 1 [Credits]"))
            "No credit gain from Bukhgalter")
        (run-continue state :movement)
        (run-jack-out state))))

(deftest bug
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Anonymous Tip" 4)]}
               :runner {:hand ["Bug"]}})
    (take-credits state :corp)
    (run-empty-server state :hq)
    (click-prompt state :runner "No action")
    (play-from-hand state :runner "Bug")
    (take-credits state :runner)
    (click-prompt state :runner "No")
    (play-from-hand state :corp "Anonymous Tip")
    (is (changed? [(:credit (get-runner)) -6]
          (click-prompt state :runner "Yes")
          (click-prompt state :runner "3"))
        "Runner uses Bug")
    (is (last-log-contains? state "Runner pays 6 [Credits] to use Bug to force the Corp to reveal they drew Hedge Fund, Hedge Fund, and Hedge Fund."))))

(deftest buzzsaw
  ;; Buzzsaw
  (before-each [state (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                                        :hand ["Enigma"]
                                        :credits 20}
                                 :runner {:deck ["Buzzsaw"]
                                          :credits 20}})
                _ (do (play-from-hand state :corp "Enigma" "HQ")
                      (rez state :corp (get-ice state :hq 0))
                      (take-credits state :corp)
                      (play-from-hand state :runner "Buzzsaw")
                      (run-on state :hq)
                      (run-continue state))
                enigma (get-ice state :hq 0)
                buzzsaw (get-program state 0)]
    (testing "pump ability"
      (do-game state
        (is (changed? [(:credit (get-runner)) -3]
              (card-ability state :runner buzzsaw 1))
            "Runner spends 3 credits to pump Buzzsaw")
        (is (changed? [(get-strength (refresh buzzsaw)) 1]
              (card-ability state :runner buzzsaw 1))
            "Buzzsaw gains 1 strength per pump")))
    (testing "break ability"
      (do-game state
        (is (changed? [(:credit (get-runner)) -1]
              (card-ability state :runner buzzsaw 0)
              (click-prompt state :runner "Force the Runner to lose [Click]")
              (click-prompt state :runner "End the run"))
            "Runner spends 1 credits to break with Buzzsaw")
        (is (every? :broken (:subroutines (refresh enigma))) "Buzzsaw breaks 2 subs at once")))))

(deftest carmen
  ;; Carmen
  (before-each [state (new-game {:runner {:hand [(qty "Carmen" 2)]
                                          :credits 20}
                                 :corp {:deck [(qty "Hedge Fund" 5)]
                                        :hand ["Swordsman"]
                                        :credits 20}})
                _ (do (play-from-hand state :corp "Swordsman" "HQ")
                      (take-credits state :corp))
                swordsman (get-ice state :hq 0)]
  (testing "install cost discount"
      (do-game state
        (take-credits state :corp)
        (is (changed? [(:credit (get-runner)) -5]
              (play-from-hand state :runner "Carmen"))
            "Without discount")
        (take-credits state :runner)
        (take-credits state :corp)
        (run-empty-server state :archives)
        (is (changed? [(:credit (get-runner)) -3]
              (play-from-hand state :runner "Carmen"))
            "With discount")))
  (testing "pump ability"
    (do-game state
      (play-from-hand state :runner "Carmen")
      (let [carmen (get-program state 0)]
        (run-on state :hq)
        (rez state :corp swordsman)
        (run-continue state :encounter-ice)
        (is (changed? [(:credit (get-runner)) -2]
              (card-ability state :runner carmen 1))
            "Pump costs 2")
        (is (changed? [(get-strength (refresh carmen)) 3]
              (card-ability state :runner carmen 1))
            "Carmen gains 3 str"))))
  (testing "break ability"
    (do-game state
      (play-from-hand state :runner "Carmen")
      (let [carmen (get-program state 0)]
        (run-on state :hq)
        (rez state :corp swordsman)
        (run-continue state :encounter-ice)
        (is (changed? [(:credit (get-runner)) -1]
              (card-ability state :runner carmen 0)
              (click-prompt state :runner "Trash an AI program"))
            "Break costs 1")
        (click-prompt state :runner "Do 1 net damage")
        (is (no-prompt? state :runner) "Only breaks 1 sub at a time"))))))

(deftest cerberus-rex-h2
  ;; Cerberus "Rex" H2 - boost 1 for 1 cred, break for 1 counter
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Enigma"]}
               :runner {:deck ["Cerberus \"Rex\" H2"]}})
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Cerberus \"Rex\" H2")
    (is (= 2 (:credit (get-runner))) "2 credits left after install")
    (let [enigma (get-ice state :hq 0)
          rex (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp enigma)
      (run-continue state)
      (is (= 4 (get-counters rex :power)) "Start with 4 counters")
      ;; boost strength
      (card-ability state :runner rex 1)
      (is (= 1 (:credit (get-runner))) "Spend 1 credit to boost")
      (is (= 2 (get-strength (refresh rex))) "At strength 2 after boost")
      ;; break
      (card-ability state :runner rex 0)
      (click-prompt state :runner "Force the Runner to lose [Click]")
      (click-prompt state :runner "End the run")
      (is (= 1 (:credit (get-runner))) "No credits spent to break")
      (is (= 3 (get-counters (refresh rex) :power)) "One counter used to break"))))

(deftest cezve
  ;; 2 recurring credits for runs on central servers
  (do-game
    (new-game {:runner {:hand ["Sure Gamble" "Cezve" "Marjanah"] :credits 10}
               :corp {:hand ["PAD Campaign"]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :click 10)
    (play-from-hand state :runner "Cezve")
    (play-from-hand state :runner "Sure Gamble")
    (is (no-prompt? state :runner) "No prompt outside of run")
    (play-from-hand state :runner "Marjanah")
    (run-on state :remote1)
    (let [mar (get-program state 1)]
      (card-ability state :runner (refresh mar) 1)
      (is (no-prompt? state :runner) "no prompt on remote run")
      (run-jack-out state)
      (run-on state :hq)
      (card-ability state :runner (refresh mar) 1)
      (is (not (no-prompt? state :runner)) "prompt to spend credits")
      (click-card state :runner (get-program state 0))
      (is (no-prompt? state :runner) "credit spent"))))

(deftest chakana-gain-counters-on-r-d-runs
    ;; gain counters on r&d runs
    (do-game
      (new-game {:runner {:hand ["Chakana"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Chakana")
      (let [chakana (get-program state 0)]
        (is (zero? (get-counters (refresh chakana) :virus)) "Chakana starts with 0 counters")
        (run-empty-server state "R&D")
        (is (= 1 (get-counters (refresh chakana) :virus)) "Chakana starts with 0 counters")
        (run-empty-server state "R&D")
        (is (= 2 (get-counters (refresh chakana) :virus)) "Chakana starts with 0 counters")
        (run-empty-server state "HQ")
        (is (= 2 (get-counters (refresh chakana) :virus)) "Chakana starts with 0 counters"))))

(deftest chakana-3-counters-increases-advancement-requirements-by-1
    ;; 3 counters increases advancement requirements by 1
    (do-game
      (new-game {:corp {:hand ["Hostile Takeover"]}
                 :runner {:hand ["Chakana"]}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Chakana")
      (is (= 2 (core/get-advancement-requirement (get-content state :remote1 0)))
          "Hostile Takeover is a 2/1 by default")
      (let [chakana (get-program state 0)]
        (core/gain state :runner :click 10)
        (run-empty-server state "R&D")
        (run-empty-server state "R&D")
        (run-empty-server state "R&D")
        (is (= 3 (get-counters (refresh chakana) :virus)))
        (is (= 3 (core/get-advancement-requirement (get-content state :remote1 0)))
            "Hostile Takeover is affected by Chakana"))))

(deftest chameleon-with-clone-chip
    ;; with Clone Chip
    (do-game
      (new-game {:runner {:deck ["Chameleon" "Clone Chip"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Clone Chip")
      (core/move state :runner (find-card "Chameleon" (:hand (get-runner))) :discard)
      (take-credits state :runner)
      (is (zero? (count (:hand (get-runner)))))
      ;; Install Chameleon on corp turn
      (take-credits state :corp 1)
      (let [chip (get-hardware state 0)]
        (card-ability state :runner chip 0)
        (click-card state :runner (find-card "Chameleon" (:discard (get-runner))))
        (click-prompt state :runner "Sentry"))
      (take-credits state :corp)
      (is (zero? (count (:hand (get-runner)))) "Chameleon not returned to hand at end of corp turn")
      (take-credits state :runner)
      (is (= 1 (count (:hand (get-runner)))) "Chameleon returned to hand at end of runner's turn")))

(deftest chameleon-returns-to-hand-after-hosting-977
    ;; Returns to hand after hosting. #977
    (do-game
      (new-game {:runner {:deck [(qty "Chameleon" 2) "Scheherazade"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Chameleon")
      (click-prompt state :runner "Barrier")
      (is (= 3 (:credit (get-runner))) "-2 from playing Chameleon")
      ;; Host the Chameleon on Scheherazade that was just played (as in Personal Workshop/Hayley ability scenarios)
      (play-from-hand state :runner "Scheherazade")
      (let [scheherazade (get-program state 1)]
        (card-ability state :runner scheherazade 1) ; Host an installed program
        (click-card state :runner (find-card "Chameleon" (:program (:rig (get-runner)))))
        (is (= 4 (:credit (get-runner))) "+1 from hosting onto Scheherazade")
        ;; Install another Chameleon directly onto Scheherazade
        (card-ability state :runner scheherazade 0) ; Install and host a program from Grip
        (click-card state :runner (find-card "Chameleon" (:hand (get-runner))))
        (click-prompt state :runner "Code Gate")
        (is (= 2 (count (:hosted (refresh scheherazade)))) "2 Chameleons hosted on Scheherazade")
        (is (= 3 (:credit (get-runner))) "-2 from playing Chameleon, +1 from installing onto Scheherazade"))
      (is (zero? (count (:hand (get-runner)))) "Both Chameleons in play - hand size 0")
      (take-credits state :runner)
      (click-prompt state :runner "Chameleon")
      (is (= 2 (count (:hand (get-runner)))) "Both Chameleons returned to hand - hand size 2")))

(deftest chameleon-can-break-subroutines-only-on-chosen-subtype
    ;; Can break subroutines only on chosen subtype
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Enigma" "Rototurret"]
                        :credits 10}
                 :runner {:hand ["Chameleon"]
                          :credits 100}})
      (play-from-hand state :corp "Ice Wall" "Archives")
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Rototurret" "R&D")
      (rez state :corp (get-ice state :archives 0))
      (rez state :corp (get-ice state :hq 0))
      (rez state :corp (get-ice state :rd 0))
      (take-credits state :corp)
      (testing "Choosing Barrier"
        (play-from-hand state :runner "Chameleon")
        (click-prompt state :runner "Barrier")
        (run-on state :archives)
        (run-continue state)
        (card-ability state :runner (get-program state 0) 0)
        (click-prompt state :runner "End the run")
        (is (no-prompt? state :runner) "Broke all subroutines on Ice Wall")
        (run-continue state :movement)
        (run-jack-out state)
        (run-on state :hq)
        (run-continue state)
        (card-ability state :runner (get-program state 0) 0)
        (is (no-prompt? state :runner) "Can't use Chameleon on Enigma")
        (run-continue state :movement)
        (run-jack-out state)
        (run-on state :rd)
        (run-continue state)
        (card-ability state :runner (get-program state 0) 0)
        (is (no-prompt? state :runner) "Can't use Chameleon on Rototurret")
        (run-continue state :movement)
        (run-jack-out state))
      (take-credits state :runner)
      (take-credits state :corp)
      (testing "Choosing Code Gate"
        (play-from-hand state :runner "Chameleon")
        (click-prompt state :runner "Code Gate")
        (run-on state :archives)
        (run-continue state)
        (card-ability state :runner (get-program state 0) 0)
        (is (no-prompt? state :runner) "Can't use Chameleon on Ice Wall")
        (run-continue state :movement)
        (run-jack-out state)
        (run-on state :hq)
        (run-continue state)
        (card-ability state :runner (get-program state 0) 0)
        (click-prompt state :runner "Force the Runner to lose [Click]")
        (click-prompt state :runner "End the run")
        (is (no-prompt? state :runner) "Broke all subroutines on Engima")
        (run-continue state :movement)
        (run-jack-out state)
        (run-on state :rd)
        (run-continue state)
        (card-ability state :runner (get-program state 0) 0)
        (is (no-prompt? state :runner) "Can't use Chameleon on Rototurret")
        (run-continue state :movement)
        (run-jack-out state))
      (take-credits state :runner)
      (take-credits state :corp)
      (testing "Choosing Sentry"
        (play-from-hand state :runner "Chameleon")
        (click-prompt state :runner "Sentry")
        (run-on state :archives)
        (run-continue state)
        (card-ability state :runner (get-program state 0) 0)
        (is (no-prompt? state :runner) "Can't use Chameleon on Ice Wall")
        (run-continue state :movement)
        (run-jack-out state)
        (run-on state :hq)
        (run-continue state)
        (card-ability state :runner (get-program state 0) 0)
        (is (no-prompt? state :runner) "Can't use Chameleon on Enigma")
        (run-continue state :movement)
        (run-jack-out state)
        (run-on state :rd)
        (run-continue state)
        (card-ability state :runner (get-program state 0) 0)
        (click-prompt state :runner "Trash a program")
        (click-prompt state :runner "End the run")
        (is (no-prompt? state :runner) "Broke all subroutines on Rototurret"))))

(deftest chisel
  ;; Chisel
  (do-game
      (new-game {:corp {:hand ["Ice Wall"]}
                 :runner {:hand ["Chisel"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Chisel")
      (click-card state :runner "Ice Wall")
      (let [iw (get-ice state :hq 0)
            chisel (first (:hosted (refresh iw)))]
        (run-on state "HQ")
        (rez state :corp iw)
        (is (zero? (get-counters (refresh chisel) :virus)))
        (run-continue state)
        (is (= 1 (get-counters (refresh chisel) :virus)))
        (is (refresh iw) "Ice Wall still around, just at 0 strength")
        (run-continue state :movement)
        (run-jack-out state)
        (run-on state "HQ")
        (run-continue state)
        (is (nil? (refresh iw)) "Ice Wall should be trashed")
        (is (nil? (refresh chisel)) "Chisel should likewise be trashed"))))

(deftest chisel-doesn-t-work-if-the-ice-is-unrezzed
    ;; Doesn't work if the ice is unrezzed
    (do-game
      (new-game {:corp {:hand ["Ice Wall"]}
                 :runner {:hand ["Chisel"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (play-from-hand state :runner "Chisel")
      (click-card state :runner "Ice Wall")
      (let [iw (get-ice state :hq 0)
            chisel (first (:hosted (refresh iw)))]
        (run-on state "HQ")
        (is (zero? (get-counters (refresh chisel) :virus)))
        (is (zero? (get-counters (refresh chisel) :virus)) "Chisel gains no counters as Ice Wall is unrezzed")
        (is (refresh iw) "Ice Wall still around, still at 1 strength")
        (core/jack-out state :runner nil)
        (run-on state "HQ")
        (rez state :corp iw)
        (run-continue state)
        (is (= 1 (get-counters (refresh chisel) :virus)) "Chisel now has 1 counter")
        (core/jack-out state :runner nil)
        (derez state :corp iw)
        (run-on state "HQ")
        (run-continue state)
        (is (refresh iw) "Ice Wall should still be around as it's unrezzed"))))

(deftest chisel-chisel-does-not-account-for-other-sources-of-strength-modification-on-hosted-ice-5391
    ;; Chisel does not account for other sources of strength modification on hosted ice #5391
    (do-game
      (new-game {:corp {:hand ["Ice Wall"]}
                 :runner {:hand ["Chisel" "Devil Charm"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Chisel")
      (click-card state :runner "Ice Wall")
      (play-from-hand state :runner "Devil Charm")
      (let [iw (get-ice state :hq 0)
            chisel (first (:hosted (refresh iw)))]
        (run-on state "HQ")
        (rez state :corp iw)
        (run-continue state)
        (click-prompt state :runner "Devil Charm")
        (click-prompt state :runner "Yes")
        (is (nil? (refresh iw)) "Ice Wall should be trashed")
        (is (nil? (refresh chisel)) "Chisel should likewise be trashed"))))

(deftest cats-cradle
  ;; cats cradle: 1str decoder, 1/1 break, code gates cost 1 more
  (do-game
    (new-game {:corp {:hand [(qty "Enigma" 2)] :credits 20}
               :runner {:hand [(qty "Cat's Cradle" 2)] :credits 20}})
    (play-from-hand state :corp "Enigma" "HQ")
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Cat's Cradle")
    (is (changed? [(:credit (get-corp)) -4]
          (rez state :corp (get-ice state :hq 0)))
        "Enigma costs 3 + 1 to rez")
    (play-from-hand state :runner "Cat's Cradle")
    (is (changed? [(:credit (get-corp)) -5]
          (rez state :corp (get-ice state :hq 1)))
        "Enigma costs 3 + 2 to rez")
    (run-on state :hq)
    (run-continue state)
    (is (changed? [(:credit (get-runner)) -3]
          (card-ability state :runner (get-program state 0) 1)
          (card-ability state :runner (get-program state 0) 0)
          (click-prompt state :runner "End the run")
          (click-prompt state :runner "Force the Runner to lose [Click]"))
        "3c to break enigma")))

(deftest cleaver
  ;; Cleaver
  (before-each [state (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                                        :hand ["Battlement"]}
                                 :runner {:hand ["Cleaver"]
                                          :credits 15}})
                _ (do (play-from-hand state :corp "Battlement" "HQ")
                      (take-credits state :corp)
                      (play-from-hand state :runner "Cleaver")
                      (run-on state :hq)
                      (rez state :corp (get-ice state :hq 0))
                      (run-continue state :encounter-ice))
                battlement (get-ice state :hq 0)
                cleaver (get-program state 0)]
    (testing "pump ability"
      (do-game state
        (is (changed? [(:credit (get-runner)) -2]
              (card-ability state :runner cleaver 1))
            "costs 2")
        (is (changed? [(get-strength (refresh cleaver)) 1]
              (card-ability state :runner cleaver 1))
            "Gains 1 strength")))
    (testing "pump ability"
      (do-game state
        (is (changed? [(:credit (get-runner)) -1]
              (card-ability state :runner cleaver 0)
              (click-prompt state :runner "End the run")
              (click-prompt state :runner "End the run"))
            "costs 1")
        (is (every? :broken (:subroutines (refresh battlement))))))))

(deftest cloak-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Cloak" "Refractor"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cloak")
      (play-from-hand state :runner "Refractor")
      (let [cl (get-program state 0)
            refr (get-program state 1)]
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner refr 1)
              (click-card state :runner cl))
            "Used 1 credit from Cloak"))))

(deftest clot
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Hostile Takeover" 2)]}
               :runner {:deck ["Clot"]}})
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (let [ht (get-content state :remote1 0)]
      (advance state ht)
      (advance state ht))
    (take-credits state :corp)
    (play-from-hand state :runner "Clot")
    (take-credits state :runner)
    (is (changed? [(count (:scored (get-corp))) 1]
          (score state :corp (get-content state :remote1 0)))
        "Clot should not block score for agenda installed previous turn")
    (is (changed? [(count (:scored (get-corp))) 0]
          (play-from-hand state :corp "Hostile Takeover" "New remote")
          (let [ht2 (get-content state :remote2 0)]
                         (advance state ht2)
                         (advance state ht2)
                         (score state :corp ht2)))
        "Clot should not allow score for agenda on same turn it was installed")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (changed? [(count (:scored (get-corp))) 1]
          (score state :corp (get-content state :remote2 0)))
        "Clot should not block score on turn after agenda is installed")))

(deftest clot-from-smc-should-prevent-score
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Hostile Takeover"]}
               :runner {:deck ["Clot"]
                        :hand ["Self-modifying Code"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Self-modifying Code")
    (take-credits state :runner)
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (is (changed? [(count (:scored (get-corp))) 0]
          (let [ht (get-content state :remote1 0)]
                         (advance state ht)
                         (advance state ht)
                         (card-ability state :runner (get-program state 0) 0)
                         (click-prompt state :runner "Clot")
                         (score state :corp ht)))
        "Clot should block on same turn even when installed after agenda")))

(deftest clot-trashed-on-purge-triggers-reaver
  (do-game
    (new-game {:corp {:deck ["Hedge Fund"]}
               :runner {:deck ["Clot" (qty "Reaver" 5)]}})
    (starting-hand state :runner ["Clot" "Reaver"])
    (take-credits state :corp)
    (play-from-hand state :runner "Clot")
    (play-from-hand state :runner "Reaver")
    (take-credits state :runner)
    (is (= 0 (count (:hand (get-runner)))) "No cards in hand")
    (purge state :corp)
    (is (= "Clot" (-> (get-runner) :discard first :title)) "Clot was trashed on purge")
    (is (= 1 (count (:hand (get-runner)))) "Reaver triggered when Clot was trashed")))

(deftest coalescence
  (do-game
    (new-game {:runner {:hand ["Coalescence"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Coalescence")
    (is (changed? [(:credit (get-runner)) 2
                   (get-counters (get-program state 0) :power) -1]
                  (card-ability state :runner (get-program state 0) 0))
        "1 power counter for 2 credits")
    (is (= 1 (get-counters (get-program state 0) :power)))
    (take-credits state :runner)
    (is (changed? [(:credit (get-runner)) 0
                   (get-counters (get-program state 0) :power) 0]
                  (card-ability state :runner (get-program state 0) 0))
        "Cannot use Coalescence to gain credits on Corp turn")))

(deftest conduit
  ;; Conduit
  (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 8)]
                        :hand ["Hedge Fund"]}
                 :runner {:deck ["Conduit"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Conduit")
      (let [conduit (get-program state 0)]
        (card-ability state :runner conduit 0)
        (is (:run @state) "Run initiated")
        (run-continue state)
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Yes")
        (is (no-prompt? state :runner) "Prompt closed")
        (is (= 1 (get-counters (refresh conduit) :virus)))
        (is (not (:run @state)) "Run ended")
        (card-ability state :runner conduit 0)
        (run-continue state)
        (is (= 1 (core/access-bonus-count state :runner :rd)) "Runner should access 1 additional card")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Yes")
        (is (= 2 (get-counters (refresh conduit) :virus)))
        (run-empty-server state :rd)
        (is (= 0 (core/access-bonus-count state :runner :rd)) "Runner should access 0 additional card on normal run"))))

(deftest conduit-knobkierie-interaction-issue-5748-knobkierie-before-conduit
    ;; Knobkierie interaction (Issue #5748) - Knobkierie before Conduit
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 8)]
                        :hand ["Hedge Fund"]}
                 :runner {:deck ["Conduit" "Knobkierie"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Conduit")
      (play-from-hand state :runner "Knobkierie")
      (let [conduit (get-program state 0)]
        (card-ability state :runner conduit 0)
        (is (:run @state) "Run initiated")
        (run-continue state :success)
        (click-prompt state :runner "Knobkierie")
        (click-prompt state :runner "Yes")
        (click-card state :runner (refresh conduit))
        (is (= 1 (core/access-bonus-count state :runner :rd)) "Runner should access 1 additional card"))))

(deftest conduit-knobkierie-interaction-issue-5748-conduit-before-knobkierie
    ;; Knobkierie interaction (Issue #5748) - Conduit before Knobkierie
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 8)]
                        :hand ["Hedge Fund"]}
                 :runner {:deck ["Conduit" "Knobkierie"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Conduit")
      (play-from-hand state :runner "Knobkierie")
      (let [conduit (get-program state 0)]
        (card-ability state :runner conduit 0)
        (is (:run @state) "Run initiated")
        (run-continue state :success)
        (click-prompt state :runner "Conduit")
        (click-prompt state :runner "Yes")
        (click-card state :runner (refresh conduit))
        (is (= 0 (core/access-bonus-count state :runner :rd)) "Runner should not access additional cards"))))

(deftest consume-trash-and-cash-out
    ;; Trash and cash out
    (do-game
      (new-game {:corp {:deck ["Adonis Campaign"]}
                 :runner {:deck ["Consume"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Consume")
      (let [c (get-program state 0)]
        (is (zero? (get-counters (refresh c) :virus)) "Consume starts with no counters")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (click-prompt state :runner "Yes")
        (is (= 1 (count (:discard (get-corp)))) "Adonis Campaign trashed")
        (is (= 1 (get-counters (refresh c) :virus)) "Consume gains a counter")
        (is (zero? (:credit (get-runner))) "Runner starts with no credits")
        (card-ability state :runner c 0)
        (is (= 2 (:credit (get-runner))) "Runner gains 2 credits")
        (is (zero? (get-counters (refresh c) :virus)) "Consume loses counters"))))

(deftest consume-hivemind-interaction
    ;; Hivemind interaction
    (do-game
      (new-game {:corp {:deck ["Adonis Campaign"]}
                 :runner {:deck ["Consume" "Hivemind"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 3)
      (play-from-hand state :runner "Consume")
      (play-from-hand state :runner "Hivemind")
      (let [c (get-program state 0)
            h (get-program state 1)]
        (is (zero? (get-counters (refresh c) :virus)) "Consume starts with no counters")
        (is (= 1 (get-counters (refresh h) :virus)) "Hivemind starts with a counter")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (click-prompt state :runner "Yes")
        (is (= 1 (count (:discard (get-corp)))) "Adonis Campaign trashed")
        (is (= 1 (get-counters (refresh c) :virus)) "Consume gains a counter")
        (is (= 1 (get-counters (refresh h) :virus)) "Hivemind retains counter")
        (is (zero? (:credit (get-runner))) "Runner starts with no credits")
        (card-ability state :runner c 0)
        (is (= 4 (:credit (get-runner))) "Runner gains 4 credits")
        (is (zero? (get-counters (refresh c) :virus)) "Consume loses counters")
        (is (zero? (get-counters (refresh h) :virus)) "Hivemind loses counters"))))

(deftest consume-hivemind-counters-only
    ;; Hivemind counters only
    (do-game
      (new-game {:runner {:deck ["Consume" "Hivemind"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Consume")
      (play-from-hand state :runner "Hivemind")
      (let [c (get-program state 0)
            h (get-program state 1)]
        (is (zero? (get-counters (refresh c) :virus)) "Consume starts with no counters")
        (is (= 1 (get-counters (refresh h) :virus)) "Hivemind starts with a counter")
        (is (zero? (:credit (get-runner))) "Runner starts with no credits")
        (card-ability state :runner c 0)
        (is (= 2 (:credit (get-runner))) "Runner gains 2 credits")
        (is (zero? (get-counters (refresh c) :virus)) "Consume loses counters")
        (is (zero? (get-counters (refresh h) :virus)) "Hivemind loses counters"))))

(deftest cordyceps
  ;; Cordyceps
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Enigma" "Hedge Fund"]}
                 :runner {:hand ["Cordyceps"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Cordyceps")
      (run-on state "HQ")
      (let [iw (get-ice state :hq 0)
            enig (get-ice state :hq 1)
            cor (get-program state 0)]
        (is (= 2 (get-counters (refresh cor) :virus)) "Cordyceps was installed with 2 virus tokens")
        (run-continue-until state :success)
        (click-prompt state :runner "Yes")
        (click-card state :runner (refresh enig))
        (click-card state :runner (refresh iw))
        (click-prompt state :runner "No action"))
      (let [iw (get-ice state :hq 1)
            enig (get-ice state :hq 0)
            cor (get-program state 0)]
        (is (= "Ice Wall" (:title iw)) "Ice Wall now outermost ice")
        (is (= "Enigma" (:title enig)) "Enigma now outermost ice")
        (is (= 1 (get-counters (refresh cor) :virus)) "Used 1 virus token"))
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state "R&D")
      (run-continue state)
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "No prompt on uniced server")))

(deftest cordyceps-no-prompt-with-less-than-2-ice-installed
    ;; No prompt with less than 2 ice installed
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Hedge Fund"]}
                 :runner {:hand ["Cordyceps"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Cordyceps")
      (run-on state "HQ")
      (run-continue-until state :success)
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "No prompt with only 1 installed ice")))

(deftest cordyceps-no-prompt-when-empty
    ;; No prompt when empty
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Enigma" "Hedge Fund"]}
                 :runner {:hand ["Cordyceps"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Cordyceps")
      (core/add-counter state :runner (get-program state 0) :virus -2)
      (is (= 0 (get-counters (get-program state 0) :virus)) "Has no virus tokens")
      (run-on state "HQ")
      (run-continue-until state :success)
      (is (accessing state "Hedge Fund"))
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "No prompt with no virus counters")))

(deftest cordyceps-works-with-hivemind-installed
    ;; Works with Hivemind installed
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Enigma" "Hedge Fund"]}
                 :runner {:hand ["Cordyceps" "Hivemind"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Cordyceps")
      (core/add-counter state :runner (get-program state 0) :virus -2)
      (play-from-hand state :runner "Hivemind")
      (run-on state "HQ")
      (run-continue-until state :success)
      (is (not (no-prompt? state :runner)) "Cordyceps prompt")
      (click-prompt state :runner "Yes")
      (is (= "Choose a piece of ice protecting this server" (:msg (prompt-map :runner))))
      (is (= :select (prompt-type :runner)))
      (click-card state :runner "Ice Wall")
      (click-card state :runner "Enigma")
      (is (= "Choose a card with virus counters (0 of 1 virus counters)" (:msg (prompt-map :runner))))
      (click-card state :runner "Hivemind")
      (is (= "Enigma" (:title (get-ice state :hq 0))))
      (is (= "Ice Wall" (:title (get-ice state :hq 1))))
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "No prompt with only 1 installed ice")))

(deftest corroder
  ;; Corroder
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall"]}
               :runner {:credits 15
                        :hand ["Corroder"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (run-on state "HQ")
    (let [iw (get-ice state :hq 0)
          cor (get-program state 0)]
      (rez state :corp iw)
      (run-continue state)
      (card-ability state :runner cor 1)
      (card-ability state :runner cor 0)
      (click-prompt state :runner "End the run")
      (is (zero? (count (remove :broken (:subroutines (refresh iw))))) "All subroutines have been broken"))))

(deftest cradle
  ;; Cradle
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Cradle" (qty "Cache" 100)]}})
    (starting-hand state :runner ["Cradle"])
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 100 :click 100)
    (play-from-hand state :runner "Cradle")
    (let [cradle (get-program state 0)
          strength (get-strength (refresh cradle))]
      (dotimes [n 5]
        (when (pos? n)
          (draw state :runner n))
        (core/fake-checkpoint state)
        (is (= (- strength n) (get-strength (refresh cradle))) (str "Cradle should lose " n " strength"))
        (starting-hand state :runner [])
        (core/fake-checkpoint state)
        (is (= strength (get-strength (refresh cradle))) (str "Cradle should be back to original strength")))
      (click-draw state :runner)
      (is (= (dec strength) (get-strength (refresh cradle))) "Cradle should lose 1 strength")
      (play-from-hand state :runner "Cache")
      (is (= strength (get-strength (refresh cradle))) (str "Cradle should be back to original strength")))))

(deftest crescentus
  ;; Crescentus should only work on rezzed ice
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Crescentus" "Corroder"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Crescentus")
    (run-on state "HQ")
    (let [cor (get-program state 0)
          cres (get-program state 1)
          iw (get-ice state :hq 0)]
      (rez state :corp iw)
      (run-continue state)
      (is (rezzed? (refresh iw)) "Ice Wall is now rezzed")
      (card-ability state :runner cor 0)
      (click-prompt state :runner "End the run")
      (card-ability state :runner cres 0)
      (is (nil? (get-program state 1)) "Crescentus could be used because the piece of ice is rezzed")
      (is (not (rezzed? (refresh iw))) "Ice Wall is no longer rezzed"))))

(deftest crypsis-breaking-a-sub-spends-a-virus-counter
    ;; Breaking a sub spends a virus counter
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Crypsis"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Crypsis")
      (let [crypsis (get-program state 0)]
        (card-ability state :runner crypsis 2)
        (is (= 1 (get-counters (refresh crypsis) :virus)) "Crypsis has 1 virus counter")
        (run-on state "Archives")
        (rez state :corp (get-ice state :archives 0))
        (run-continue state)
        (card-ability state :runner (refresh crypsis) 1) ; Match strength
        (card-ability state :runner (refresh crypsis) 0) ; Break
        (click-prompt state :runner "End the run")
        (is (= 1 (get-counters (refresh crypsis) :virus)) "Crypsis has 1 virus counter")
        (run-continue state)
        (is (zero? (get-counters (refresh crypsis) :virus)) "Crypsis has 0 virus counters"))))

(deftest crypsis-inability-to-remove-a-virus-counter-trashes-crypsis
    ;; Inability to remove a virus counter trashes Crypsis
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Crypsis"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Crypsis")
      (let [crypsis (get-program state 0)]
        (is (zero? (get-counters (refresh crypsis) :virus)) "Crypsis has 0 virus counters")
        (run-on state "Archives")
        (rez state :corp (get-ice state :archives 0))
        (run-continue state)
        (card-ability state :runner (refresh crypsis) 1) ; Match strength
        (card-ability state :runner (refresh crypsis) 0) ; Break
        (click-prompt state :runner "End the run")
        (run-continue state)
        (is (find-card "Crypsis" (:discard (get-runner))) "Crypsis was trashed"))))

(deftest crypsis-working-with-auto-pump-and-break
    ;; Working with auto-pump-and-break
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Crypsis"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Crypsis")
      (let [crypsis (get-program state 0)
            iw (get-ice state :hq 0)]
        (card-ability state :runner crypsis 2)
        (is (= 1 (get-counters (refresh crypsis) :virus)) "Crypsis has 1 virus counter")
        (run-on state "HQ")
        (rez state :corp (refresh iw))
        (run-continue state)
        (auto-pump-and-break state (refresh crypsis))
        (core/continue state :corp nil)
        (is (= 0 (get-counters (refresh crypsis) :virus)) "Used up virus token on Crypsis"))))

(deftest cupellation
  (do-game
    (new-game {:runner {:hand ["Cupellation"]}
               :corp {:hand [(qty "Hedge Fund" 5)]
                      :discard ["Project Beale"]}})
    (take-credits state :corp)
    (core/gain state :runner :click 1)
    (play-from-hand state :runner "Cupellation")
    (let [cup (get-program state 0)]
      (run-empty-server state "Archives")
      ;; No Cupellation host prompt
      (click-prompt state :runner "Steal")
      (run-empty-server state "HQ")
      (is (changed? [(:credit (get-runner)) -1
                     (count (:hand (get-corp))) -1
                     (count (:hosted (refresh cup))) 1]
                    (click-prompt state :runner "[Cupellation] 1 [Credits]: Host card"))
          "Card is hosted on Cupellation")
      (run-empty-server state "HQ")
      ;; Cupellation breach prompt
      (click-prompt state :runner "No")
      ;; No Cupellation host prompt
      (click-prompt state :runner "No action")
      (run-empty-server state "HQ")
      (is (changed? [(:credit (get-runner)) -1
                     (count (:discard (get-runner))) 1
                     (count (:discard (get-corp))) 1]
                    (click-prompt state :runner "Yes")
                    (dotimes [_ 3] (click-prompt state :runner "No action")))
          "Cupellation and its hosted card are trashed"))))

(deftest cupellation-disables-cards
  (do-game
    (new-game {:runner {:hand ["Cupellation"]}
               :corp {:hand ["Marilyn Campaign"]}})
    (play-from-hand state :corp "Marilyn Campaign" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Cupellation")
    (is (= 8 (get-counters (get-content state :remote1 0) :credit)) "Marilyn Campaign should start with 8 credits")
    (run-on state "Server 1")
    (run-continue state)
    (click-prompt state :runner "[Cupellation] 1 [Credits]: Host card")
    (take-credits state :runner)
    (is (= 8 (get-counters (first (:hosted (get-program state 0))) :credit)))))

(deftest cupellation-ansel-interaction-7363
  ;; Interaction with Ansel 1.0, issue #7363
  (do-game
    (new-game {:corp {:hand ["Ansel 1.0" "PAD Campaign"]
                      :deck [(qty "Hedge Fund" 10)]
                      :credits 10}
               :runner {:hand ["Rezeki" "Cupellation"]
                        :credits 10}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Ansel 1.0" "Server 1")
    (let [ansel (get-ice state :remote1 0)]
      (rez state :corp ansel)
      (take-credits state :corp)
      (play-from-hand state :runner "Cupellation")
      (play-from-hand state :runner "Rezeki")
      (let [cup (get-program state 0)]
        (run-on state "Server 1")
        (run-continue-until state :encounter-ice)
        (fire-subs state (refresh ansel))
        (click-card state :corp "Rezeki")
        (click-prompt state :corp "Done")
        (run-continue-until state :success)
        (is (changed? [(:credit (get-runner)) -1
                       (count (get-content state :remote1)) -1
                       (count (:hosted (refresh cup))) 1]
              (click-prompt state :runner "[Cupellation] 1 [Credits]: Host card"))
            "Card is hosted on Cupellation")))))

(deftest curupira
  (do-game
    (new-game {:runner {:hand ["Curupira"]
                        :credits 50}
               :corp {:hand ["Battlement"]}})
    (play-from-hand state :corp "Battlement" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (core/gain state :runner :click 10)
    (play-from-hand state :runner "Curupira")
    (let [curu (get-program state 0)]
      (dotimes [_ 3] 
        (run-on state "HQ")
        (run-continue state)
        (is (changed? [(get-counters (refresh curu) :power) 1]
              (auto-pump-and-break state (refresh curu)))
            "Placed 1 power counter on Curupira")
        (core/continue state :corp nil)
        (run-jack-out state))
      (run-on state "HQ")
      (run-continue state)
      (is (changed? [(get-counters (refresh curu) :power) -3]
            (click-prompt state :runner "Yes"))
          "Spent 3 hosted power counters")
      (is (= :movement (:phase (get-run))) "Run has bypassed Battlement")
      )))

(deftest customized-secretary
  ;; Customized Secretary - shuffles the stack even when no program is found
  (do-game
    (new-game {:runner {:deck ["Aniccam" "Bravado" "Creative Commission" "Deuces Wild" "Encore"]
                        :hand ["Customized Secretary"]
                        :credits 50}})
    (take-credits state :corp)
    (is (changed? [(count (core/turn-events state :runner :runner-shuffle-deck)) 1]
          (play-from-hand state :runner "Customized Secretary")))))

(deftest customized-secretary-shuffles-stack-when-last-program-is-hosted
  ;; Customized Secretary - shuffles the stack when last program is hosted
  (do-game
    (new-game {:runner {:deck ["Aniccam" "Bravado" "Cleaver" "Deuces Wild" "Encore"]
                        :hand ["Customized Secretary"]
                        :credits 50}})
    (take-credits state :corp)
    (core/move state :runner (find-card "Aniccam" (:deck (get-runner))) :deck)
    (core/move state :runner (find-card "Bravado" (:deck (get-runner))) :deck)
    (core/move state :runner (find-card "Cleaver" (:deck (get-runner))) :deck)
    (core/move state :runner (find-card "Deuces Wild" (:deck (get-runner))) :deck)
    (core/move state :runner (find-card "Encore" (:deck (get-runner))) :deck)
    (is (changed? [(count (core/turn-events state :corp :runner-shuffle-deck)) 1]
          (play-from-hand state :runner "Customized Secretary")
          (click-prompt state :runner "Cleaver"))
        "Runner stack is not shufled")))

(deftest customized-secretary-shuffles-stack-when-no-program-is-hosted
  ;; Customized Secretary - shuffles the stack when no program is hosted
  (do-game
    (new-game {:runner {:deck ["Aniccam" "Bravado" "Councilman" "Deuces Wild" "Encore"]
                        :hand ["Customized Secretary"]
                        :credits 50}})
    (take-credits state :corp)
    (core/move state :runner (find-card "Aniccam" (:deck (get-runner))) :deck)
    (core/move state :runner (find-card "Bravado" (:deck (get-runner))) :deck)
    (core/move state :runner (find-card "Councilman" (:deck (get-runner))) :deck)
    (core/move state :runner (find-card "Deuces Wild" (:deck (get-runner))) :deck)
    (core/move state :runner (find-card "Encore" (:deck (get-runner))) :deck)
    (is (changed? [(count (core/turn-events state :corp :runner-shuffle-deck)) 1]
          (play-from-hand state :runner "Customized Secretary"))
        "Runner stack is not shufled")
    (is (no-prompt? state :corp))))

(deftest cyber-cypher
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Macrophage" "Macrophage"]
                      :credits 100}
               :runner {:hand ["Cyber-Cypher"]
                        :credits 100}})
    (play-from-hand state :corp "Macrophage" "R&D")
    (play-from-hand state :corp "Macrophage" "HQ")
    (rez state :corp (get-ice state :rd 0))
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Cyber-Cypher")
    (click-prompt state :runner "HQ")
    (let [cc (get-program state 0)]
      (run-on state :hq)
      (run-continue state)
      (card-ability state :runner cc 1)
      (card-ability state :runner cc 1)
      (card-ability state :runner cc 1)
      (is (= 7 (get-strength (refresh cc))) "Can pump Cyber-Cypher on the right server")
      (card-ability state :runner cc 0)
      (click-prompt state :runner "Trace 4 - Purge virus counters")
      (click-prompt state :runner "Trace 3 - Trash a virus")
      (click-prompt state :runner "Done")
      (is (= 2 (count (filter :broken (:subroutines (get-ice state :hq 0))))) "Can break subs on the right server")
      (run-continue state :movement)
      (run-jack-out state))
    (let [cc (get-program state 0)]
      (run-on state :rd)
      (run-continue state)
      (card-ability state :runner cc 1)
      (is (= 4 (get-strength (refresh cc))) "Can't pump Cyber-Cyper on a different server")
      (core/update! state :runner (assoc (refresh cc) :current-strength 7))
      (is (= 7 (get-strength (refresh cc))) "Manually set equal strength")
      (card-ability state :runner cc 0)
      (is (no-prompt? state :runner) "Can't break subs on a different server")
      (is (zero? (count (filter :broken (:subroutines (get-ice state :rd 0))))) "No subs are broken"))))

(deftest d4v1d-can-break-5-strength-ice
    ;; Can break 5+ strength ice
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "Hadrian's Wall"]}
                 :runner {:deck ["D4v1d"]}})
      (core/gain state :corp :credit 10)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Hadrian's Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "D4v1d")
      (let [had (get-ice state :hq 1)
            iw (get-ice state :hq 0)
            d4 (get-program state 0)]
        (is (= 3 (get-counters d4 :power)) "D4v1d installed with 3 power counters")
        (run-on state :hq)
        (rez state :corp had)
        (run-continue state)
        (card-ability state :runner d4 0)
        (click-prompt state :runner "End the run")
        (click-prompt state :runner "End the run")
        (is (= 1 (get-counters (refresh d4) :power)) "Used 2 power counters from D4v1d to break")
        (run-continue state)
        (rez state :corp iw)
        (run-continue state)
        (card-ability state :runner d4 0)
        (is (no-prompt? state :runner) "No prompt for breaking 1 strength Ice Wall"))))

(deftest dai-v
  ;; Dai V
  (do-game
      (new-game {:corp {:deck ["Enigma"]}
                 :runner {:deck [(qty "Cloak" 2) "Dai V"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Cloak")
      (play-from-hand state :runner "Cloak")
      (play-from-hand state :runner "Dai V")
      (run-on state :hq)
      (let [enig (get-ice state :hq 0)
            cl1 (get-program state 0)
            cl2 (get-program state 1)
            daiv (get-program state 2)]
        (rez state :corp enig)
        (run-continue state)
        (is (changed? [(:credit (get-runner)) -1]
              (card-ability state :runner daiv 1)
              (click-prompt state :runner "Done")
              (card-ability state :runner daiv 0)
              (click-prompt state :runner "Force the Runner to lose [Click]")
              (click-prompt state :runner "End the run")
              (is (str/includes? (:msg (prompt-map :runner)) "2 stealth") "The prompt tells us how many stealth credits we need")
              (click-card state :runner cl1)
              (click-card state :runner cl2))
            "Used 1 credit to pump and 2 credits from Cloaks to break"))))

(deftest dai-v-additional-costs-are-also-stealth
    ; Additional costs are also stealth
    (do-game
      (new-game {:corp {:deck ["Enigma" "Midway Station Grid"]}
                 :runner {:deck [(qty "Cloak" 2) "Dai V"]}})
      (core/gain state :corp :credit 10)
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Midway Station Grid" "HQ")
      (rez state :corp (get-content state :hq 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Cloak")
      (play-from-hand state :runner "Cloak")
      (play-from-hand state :runner "Dai V")
      (run-on state :hq)
      (let [enig (get-ice state :hq 0)
            daiv (get-program state 2)]
        (rez state :corp enig)
        (run-continue state)
        (is (changed? [(:credit (get-runner)) -1]
              (card-ability state :runner daiv 1)
              (click-prompt state :runner "Done")
              (card-ability state :runner daiv 0)
              (click-prompt state :runner "Force the Runner to lose [Click]")
              (click-prompt state :runner "End the run")
              (is (no-prompt? state :runner) "We are incapable of paying")
              (is (= 0 (count (filter :broken (:subroutines (refresh enig))))) "No subroutines were broken"))
            "Was only charged the 1 credit to pump"))))

(deftest dai-v-can-t-pay-with-a-non-stealth-source
    ;; Can't pay with a non-stealth source
    (do-game
      (new-game {:corp {:deck ["Enigma"]}
                 :runner {:deck ["Multithreader" "Dai V"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Multithreader")
      (play-from-hand state :runner "Dai V")
      (run-on state :hq)
      (let [enig (get-ice state :hq 0)
            daiv (get-program state 1)]
        (rez state :corp enig)
        (run-continue state)
        (card-ability state :runner daiv 1)
        (click-prompt state :runner "Done")
        (card-ability state :runner daiv 0)
        (is (no-prompt? state :runner)) "We are incapable of paying")))

(deftest dai-v-can-t-pay-from-credit-pool
    ;; Can't pay from credit pool
    (do-game
      (new-game {:corp {:deck ["Enigma"]}
                 :runner {:deck ["Dai V"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Dai V")
      (run-on state :hq)
      (let [enig (get-ice state :hq 0)
            daiv (get-program state 0)]
        (rez state :corp enig)
        (run-continue state)
        (card-ability state :runner daiv 1)
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner daiv 0))
            "Credits are not taken from the pool"))))

(deftest darwin
  ;; Darwin - starts at 0 strength
  (do-game
    (new-game {:runner {:deck ["Darwin"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Darwin")
    (let [darwin (get-program state 0)]
      (is (zero? (get-counters (refresh darwin) :virus)) "Darwin starts with 0 virus counters")
      (is (zero? (get-strength (refresh darwin))) "Darwin starts at 0 strength")
      (take-credits state :runner)
      (take-credits state :corp)
      (card-ability state :runner (refresh darwin) 1) ; add counter
      (is (= 1 (get-counters (refresh darwin) :virus)) "Darwin gains 1 virus counter")
      (is (= 1 (get-strength (refresh darwin))) "Darwin is at 1 strength"))))

(deftest datasucker
  ;; Datasucker - Reduce strength of encountered piece of ice
  (do-game
      (new-game {:corp {:deck ["Fire Wall"]}
                 :runner {:deck ["Datasucker"]}})
      (play-from-hand state :corp "Fire Wall" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :click 3)
      (play-from-hand state :runner "Datasucker")
      (let [ds (get-program state 0)
            fw (get-ice state :remote1 0)]
        (run-empty-server state "Archives")
        (is (= 1 (get-counters (refresh ds) :virus)))
        (run-empty-server state "Archives")
        (is (= 2 (get-counters (refresh ds) :virus)))
        (run-on state "Server 1")
        (run-continue state)
        (run-continue state)
        (is (= 2 (get-counters (refresh ds) :virus)) "No counter gained, not a central server")
        (run-on state "Server 1")
        (rez state :corp fw)
        (run-continue state)
        (is (= 5 (get-strength (refresh fw))))
        (card-ability state :runner ds 0)
        (is (= 1 (get-counters (refresh ds) :virus)) "1 counter spent from Datasucker")
        (is (= 4 (get-strength (refresh fw))) "Fire Wall strength lowered by 1"))))

(deftest datasucker-does-not-affect-next-ice-when-current-is-trashed-issue-1788
    ;; does not affect next ice when current is trashed. Issue #1788
    (do-game
      (new-game {:corp {:deck ["Wraparound" "Spiderweb"]}
                 :runner {:deck ["Datasucker" "Parasite"]}})
      (play-from-hand state :corp "Wraparound" "HQ")
      (play-from-hand state :corp "Spiderweb" "HQ")
      (take-credits state :corp)
      (core/gain state :corp :credit 10)
      (play-from-hand state :runner "Datasucker")
      (let [sucker (get-program state 0)
            wrap (get-ice state :hq 0)
            spider (get-ice state :hq 1)]
        (core/add-counter state :runner sucker :virus 2)
        (rez state :corp spider)
        (rez state :corp wrap)
        (play-from-hand state :runner "Parasite")
        (click-card state :runner "Spiderweb")
        (run-on state "HQ")
        (run-continue state)
        (card-ability state :runner (refresh sucker) 0)
        (card-ability state :runner (refresh sucker) 0)
        (is (find-card "Spiderweb" (:discard (get-corp))) "Spiderweb trashed by Parasite + Datasucker")
        (is (= 7 (get-strength (refresh wrap))) "Wraparound not reduced by Datasucker"))))

(deftest davinci-gain-1-counter-on-successful-run
    ;; Gain 1 counter on successful run
    (do-game
      (new-game {:runner {:hand ["DaVinci"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "DaVinci")
      (run-on state "HQ")
      (is (changed? [(get-counters (get-program state 0) :power) 1]
            (run-continue state))
          "DaVinci gains 1 counter on successful run")))

(deftest davinci-gain-no-counters-on-unsuccessful-run
    ;; Gain no counters on unsuccessful run
    (do-game
      (new-game {:runner {:hand ["DaVinci"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "DaVinci")
      (run-on state "HQ")
      (is (changed? [(get-counters (get-program state 0) :power) 0]
            (run-jack-out state))
          "DaVinci does not gain counter on unsuccessful run")))

(deftest davinci-install-a-card-with-install-cost-lower-than-number-of-counters
    ;; Install a card with install cost lower than number of counters
    (do-game
      (new-game {:runner {:hand ["DaVinci" "The Turning Wheel"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "DaVinci")
      (let [davinci (get-program state 0)]
        (core/add-counter state :runner davinci :power 2)
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner (refresh davinci) 0)
              (click-card state :runner "The Turning Wheel"))
            "DaVinci installs The Turning Wheel for free")
        (is (get-resource state 0) "The Turning Wheel is installed")
        (is (find-card "DaVinci" (:discard (get-runner))) "DaVinci is trashed"))))

(deftest davinci-using-ability-should-trigger-trash-effects-first-issue-4987
    ;; Using ability should trigger trash effects first. Issue #4987
    (do-game
      (new-game {:runner {:id "Armand \"Geist\" Walker: Tech Lord"
                          :deck ["Simulchip"]
                          :hand ["DaVinci" "The Turning Wheel"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "DaVinci")
      (let [davinci (get-program state 0)]
        (core/add-counter state :runner davinci :power 2)
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner (refresh davinci) 0)
              (click-card state :runner "Simulchip"))
            "DaVinci installs Simulchip for free")
        (is (get-hardware state 0) "Simulchip is installed")
        (is (find-card "DaVinci" (:discard (get-runner))) "DaVinci is trashed"))))

(deftest demara
  ;; Demara
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall"]}
               :runner {:hand ["Demara"]
                        :credits 10}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Demara")
    (run-on state "HQ")
    (rez state :corp (get-ice state :hq 0))
    (run-continue state)
    (card-ability state :runner (get-program state 0) 2)
    (is (= :movement (:phase (get-run))) "Run has bypassed Ice Wall")
    (is (find-card "Demara" (:discard (get-runner))) "Demara is trashed")))

(deftest deus-x-vs-multiple-hostile-infrastructure
    ;; vs Multiple Hostile Infrastructure
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Infrastructure" 3)]}
                 :runner {:deck [(qty "Deus X" 3) (qty "Sure Gamble" 2)]}})
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (core/gain state :corp :credit 10)
      (rez state :corp (get-content state :remote1 0))
      (rez state :corp (get-content state :remote2 0))
      (rez state :corp (get-content state :remote3 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Deus X")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (let [dx (get-program state 0)]
        (card-ability state :runner dx 1)
        (is (= 2 (count (:hand (get-runner)))) "Deus X prevented one Hostile net damage"))))

(deftest deus-x-vs-multiple-sources-of-net-damage
    ;; vs Multiple sources of net damage
    (do-game
      (new-game {:corp {:id "Jinteki: Personal Evolution"
                        :deck [(qty "Fetal AI" 6)]}
                 :runner {:deck [(qty "Deus X" 3) (qty "Sure Gamble" 2)]}})
      (play-from-hand state :corp "Fetal AI" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Deus X")
      (run-empty-server state "Server 1")
      (let [dx (get-program state 0)]
        (card-ability state :runner dx 1)
        (click-prompt state :runner "Pay to steal")
        (is (= 3 (count (:hand (get-runner)))) "Deus X prevented net damage from accessing Fetal AI, but not from Personal Evolution")
        (is (= 1 (count (:scored (get-runner)))) "Fetal AI stolen"))))

(deftest dhegdheer-with-credit-savings
    ;; with credit savings
    (do-game
      (new-game {:runner {:deck ["Adept" "Dhegdheer"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Dhegdheer")
      (play-from-hand state :runner "Adept")
      (is (= 3 (:credit (get-runner))) "3 credits left after individual installs")
      (is (= 2 (core/available-mu state)) "2 MU used")
      (let [dheg (get-program state 0)
            adpt (get-program state 1)]
        (is (= 4 (get-strength (refresh adpt))) "Adept at 4 strength individually")
        (card-ability state :runner dheg 1)
        (click-card state :runner (refresh adpt))
        (let [hosted-adpt (first (:hosted (refresh dheg)))]
          (is (= 4 (:credit (get-runner))) "4 credits left after hosting")
          (is (= 4 (core/available-mu state)) "0 MU used")
          (is (= 6 (get-strength (refresh hosted-adpt))) "Adept at 6 strength hosted")))))

(deftest dhegdheer-without-credit-savings
    ;; without credit savings
    (do-game
      (new-game {:runner {:deck ["Adept" "Dhegdheer"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Dhegdheer")
      (play-from-hand state :runner "Adept")
      (is (= 3 (:credit (get-runner))) "3 credits left after individual installs")
      (is (= 2 (core/available-mu state)) "2 MU used")
      (let [dheg (get-program state 0)
            adpt (get-program state 1)]
        (is (= 4 (get-strength (refresh adpt))) "Adept at 4 strength individually")
        (card-ability state :runner dheg 2)
        (click-card state :runner (refresh adpt))
        (let [hosted-adpt (first (:hosted (refresh dheg)))]
          (is (= 3 (:credit (get-runner))) "4 credits left after hosting")
          (is (= 4 (core/available-mu state)) "0 MU used")
          (is (= 6 (get-strength (refresh hosted-adpt))) "Adept at 6 strength hosted")))))

(deftest disrupter
  ;; Disrupter
  (do-game
    (new-game {:corp {:deck [(qty "SEA Source" 2)]}
               :runner {:deck ["Disrupter"]}})
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (play-from-hand state :runner "Disrupter")
    (take-credits state :runner)
    (play-from-hand state :corp "SEA Source")
    (click-prompt state :runner "Yes")
    (is (zero? (:base (prompt-map :corp))) "Base trace should now be 0")
    (is (= 1 (-> (get-runner) :discard count)) "Disrupter should be in Heap")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (zero? (count-tags state)) "Runner should gain no tag from beating trace")
    (play-from-hand state :corp "SEA Source")
    (is (= 3 (:base (prompt-map :corp))) "Base trace should be reset to 3")))

(deftest diwan
  ;; Diwan - Full test
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3) (qty "Fire Wall" 3) (qty "Crisium Grid" 2)]
                      :hand []}
               :runner {:deck ["Diwan"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Diwan")
    (click-prompt state :runner "HQ")
    (take-credits state :runner)
    (is (= 8 (:credit (get-corp))) "8 credits for corp at start of second turn")
    (starting-hand state :corp ["Ice Wall"])
    (play-from-hand state :corp "Ice Wall" "R&D")
    (is (= 8 (:credit (get-corp))) "Diwan did not charge extra for install on another server")
    (starting-hand state :corp ["Ice Wall"])
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (= 7 (:credit (get-corp))) "Diwan charged 1cr to install ice protecting the named server")
    (starting-hand state :corp ["Crisium Grid"])
    (play-from-hand state :corp "Crisium Grid" "HQ")
    (is (= 6 (:credit (get-corp))) "Diwan charged to install another upgrade in root of HQ")
    (take-credits state :corp)
    (take-credits state :runner)
    (starting-hand state :corp ["Ice Wall"])
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (= 4 (:credit (get-corp))) "Diwan charged 1cr + 1cr to install a second ice protecting the named server")
    (core/gain state :corp :click 1)
    (purge state :corp)
    (starting-hand state :corp ["Fire Wall"])
    (play-from-hand state :corp "Fire Wall" "HQ") ; 2cr cost from normal install cost
    (is (= "Diwan" (-> (get-runner) :discard first :title)) "Diwan was trashed from purge")
    (is (= 2 (:credit (get-corp))) "No charge for installs after Diwan purged")))

(deftest djinn-hosted-chakana-does-not-disable-advancing-agendas-issue-750
    ;; Hosted Chakana does not disable advancing agendas. Issue #750
    (do-game
      (new-game {:corp {:deck ["Priority Requisition"]}
                 :runner {:deck ["Djinn" "Chakana"]}})
      (play-from-hand state :corp "Priority Requisition" "New remote")
      (take-credits state :corp 2)
      (play-from-hand state :runner "Djinn")
      (let [djinn (get-program state 0)
            agenda (get-content state :remote1 0)]
        (is agenda "Agenda was installed")
        (card-ability state :runner djinn 1)
        (click-card state :runner (find-card "Chakana" (:hand (get-runner))))
        (let [chak (first (:hosted (refresh djinn)))]
          (is (= "Chakana" (:title chak)) "Djinn has a hosted Chakana")
          ;; manually add 3 counters
          (core/add-counter state :runner (first (:hosted (refresh djinn))) :virus 3)
          (take-credits state :runner 2)
          (click-advance state :corp agenda)
          (is (= 1 (get-counters (refresh agenda) :advancement)) "Agenda was advanced")))))

(deftest djinn-host-a-non-icebreaker-program
    ;; Host a non-icebreaker program
    (do-game
      (new-game {:runner {:deck ["Djinn" "Chakana"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Djinn")
      (is (= 3 (core/available-mu state)))
      (let [djinn (get-program state 0)]
        (card-ability state :runner djinn 1)
        (click-card state :runner (find-card "Chakana" (:hand (get-runner))))
        (is (= 3 (core/available-mu state)) "No memory used to host on Djinn")
        (is (= "Chakana" (:title (first (:hosted (refresh djinn))))) "Djinn has a hosted Chakana")
        (is (= 1 (:credit (get-runner))) "Full cost to host on Djinn"))))

(deftest djinn-tutor-a-virus-program
    ;; Tutor a virus program
    (do-game
      (new-game {:runner {:deck ["Djinn" "Parasite"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Djinn")
      (core/move state :runner (find-card "Parasite" (:hand (get-runner))) :deck)
      (is (zero? (count (:hand (get-runner)))) "No cards in hand after moving Parasite to deck")
      (let [djinn (get-program state 0)]
        (card-ability state :runner djinn 0)
        (click-prompt state :runner (find-card "Parasite" (:deck (get-runner))))
        (is (= "Parasite" (:title (first (:hand (get-runner))))) "Djinn moved Parasite to hand")
        (is (= 2 (:credit (get-runner))) "1cr to use Djinn ability")
        (is (= 2 (:click (get-runner))) "1click to use Djinn ability"))))

(deftest eater
  (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
                 :runner {:deck [(qty "Eater" 2)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Eater")
      (let [eater (get-program state 0)
            iw (get-ice state :hq 0)]
        (run-on state "HQ")
        (rez state :corp (refresh iw))
        (run-continue state)
        (card-ability state :runner eater 0)
        (click-prompt state :runner "End the run")
        (run-continue state)
        (run-continue state)
        (is (no-prompt? state :runner) "No prompt for accessing cards"))))

(deftest eater-eater-interaction-with-remote-server-issue-4536
    ;; Eater interaction with remote server. Issue #4536
    (do-game
      (new-game {:corp {:deck [(qty "Rototurret" 2) "NGO Front"]}
                 :runner {:deck [(qty "Eater" 2)]}})
      (play-from-hand state :corp "NGO Front" "New remote")
      (play-from-hand state :corp "Rototurret" "Server 1")
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Eater")
      (let [eater (get-program state 0)
            rt (get-ice state :remote1 0)]
        (run-on state "Server 1")
        (rez state :corp (refresh rt))
        (run-continue state)
        (card-ability state :runner eater 0)
        (click-prompt state :runner "End the run")
        (click-prompt state :runner "Done")
        (fire-subs state (refresh rt))
        (click-card state :corp eater)
        (run-continue state)
        (run-continue state)
        (is (find-card "Eater" (:discard (get-runner))) "Eater is trashed")
        (is (no-prompt? state :runner) "No prompt for accessing cards"))))

(deftest echelon
  ;; Echelon
  (before-each [state (new-game {:runner {:hand [(qty "Echelon" 5)]
                                          :credits 20}
                                 :corp {:deck [(qty "Hedge Fund" 5)]
                                        :hand ["Owl"]
                                        :credits 20}})
                _ (do (play-from-hand state :corp "Owl" "HQ")
                      (take-credits state :corp))
                owl (get-ice state :hq 0)]
    (testing "pump ability"
      (do-game state
        (play-from-hand state :runner "Echelon")
        (let [echelon (get-program state 0)]
          (run-on state :hq)
          (rez state :corp owl)
          (run-continue state :encounter-ice)
          (is (changed? [(:credit (get-runner)) -3]
                (card-ability state :runner echelon 1))
              "Pump costs 3")
          (is (changed? [(get-strength (refresh echelon)) 2]
                (card-ability state :runner echelon 1))
              "Echelon gains 2 str"))))
    (testing "break ability"
      (do-game state
        (play-from-hand state :runner "Echelon")
        (let [echelon (get-program state 0)]
          (run-on state :hq)
          (rez state :corp owl)
          (run-continue state :encounter-ice)
          (is (changed? [(:credit (get-runner)) -1]
                (card-ability state :runner echelon 0)
                (click-prompt state :runner "Add installed program to the top of the stack"))
              "Break costs 1")
          (is (no-prompt? state :runner) "Only breaks 1 sub at a time"))))
    (testing "Gains str per icebreaker"
      (do-game state
        (take-credits state :corp)
        (play-from-hand state :runner "Echelon")
        (let [echelon (get-program state 0)]
          (is (= 1 (get-strength (refresh echelon))) "0 + 1 icebreaker installed")
          (play-from-hand state :runner "Echelon")
          (is (= 2 (get-strength (refresh echelon))) "0 + 2 icebreaker installed")
          (play-from-hand state :runner "Echelon")
          (is (= 3 (get-strength (refresh echelon))) "0 + 3 icebreaker installed"))))))

(deftest egret
  ;; Egret
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Mother Goddess"]}
               :runner {:hand [(qty "Egret" 2)]}})
    (play-from-hand state :corp "Mother Goddess" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (let [mg (get-ice state :hq 0)]
      (take-credits state :corp)
      (play-from-hand state :runner "Egret")
      (click-card state :runner mg)
      (is (has-subtype? (refresh mg) "Barrier"))
      (is (has-subtype? (refresh mg) "Code Gate"))
      (is (has-subtype? (refresh mg) "Sentry"))
      (trash state :runner (first (:hosted (refresh mg))))
      (is (not (has-subtype? (refresh mg) "Barrier")))
      (is (not (has-subtype? (refresh mg) "Code Gate")))
      (is (not (has-subtype? (refresh mg) "Sentry"))))))

(deftest engolo-subtype-is-removed-when-engolo-is-trashed-mid-encounter-issue-4039
    ;; Subtype is removed when Engolo is trashed mid-encounter. Issue #4039
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Rototurret"]}
                 :runner {:hand ["Engolo"]
                          :credits 10}})
      (play-from-hand state :corp "Rototurret" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Engolo")
      (let [roto (get-ice state :hq 0)
            engolo (get-program state 0)]
        (run-on state :hq)
        (rez state :corp roto)
        (run-continue state)
        (click-prompt state :runner "Yes")
        (is (has-subtype? (refresh roto) "Code Gate"))
        (card-subroutine state :corp roto 0)
        (click-card state :corp engolo)
        (run-continue state)
        (is (nil? (refresh engolo)) "Engolo is trashed")
        (is (not (has-subtype? (refresh roto) "Code Gate")) "Rototurret loses subtype even when Engolo is trashed"))))

(deftest engolo-marks-eid-as-ability-4962
    ;; Marks eid as :ability #4962
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Rototurret"]}
                 :runner {:hand ["Engolo" "Trickster Taka"]
                          :credits 20}})
      (play-from-hand state :corp "Rototurret" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Trickster Taka")
      (core/add-counter state :runner (get-resource state 0) :credit 2)
      (play-from-hand state :runner "Engolo")
      (let [roto (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp roto)
        (run-continue state)
        (is (changed? [(:credit (get-runner)) 0]
              (click-prompt state :runner "Yes")
              (click-card state :runner "Trickster Taka")
              (click-card state :runner "Trickster Taka"))
            "Runner spends credits on Taka")
        (is (zero? (get-counters (get-resource state 0) :credit)) "Taka has been used")
        (run-continue state :movement)
        (run-jack-out state)
        (is (nil? (get-run)))
        (is (no-prompt? state :corp))
        (is (no-prompt? state :runner)))))

(deftest equivocation
  ;; Equivocation - interactions with other successful-run events.
  (do-game
    (new-game {:corp {:deck [(qty "Restructure" 3) (qty "Hedge Fund" 3)]}
               :runner {:id "Laramy Fisk: Savvy Investor"
                        :deck ["Equivocation" "Desperado"]}})
    (starting-hand state :corp ["Hedge Fund"])
    (take-credits state :corp)
    (play-from-hand state :runner "Equivocation")
    (play-from-hand state :runner "Desperado")
    (run-empty-server state :rd)
    (click-prompt state :runner "Laramy Fisk: Savvy Investor")
    (click-prompt state :runner "Yes")
    (is (= 2 (count (:hand (get-corp)))) "Corp forced to draw by Fisk")
    (click-prompt state :runner "Yes") ; Equivocation prompt
    (click-prompt state :runner "Yes") ; force the draw
    (is (= 1 (:credit (get-runner))) "Runner gained 1cr from Desperado")
    (is (= 3 (count (:hand (get-corp)))) "Corp forced to draw by Equivocation")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")))

(deftest euler
  ;; Euler
  (do-game
      (new-game {:runner {:hand ["Euler"]
                          :credits 20}
                 :corp {:hand ["Enigma"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Euler")
      (run-on state :hq)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (changed? [(:credit (get-runner)) 0]
            (auto-pump-and-break state (get-program state 0))
            (core/continue state :corp nil))
          "Broke Enigma for 0c")
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -2]
            (auto-pump-and-break state (get-program state 0))
            (core/continue state :corp nil))
          "Broke Enigma for 2c")))

(deftest euler-correct-log-test
    ;; Correct log test
    (do-game
      (new-game {:runner {:hand ["Euler"]
                          :credits 20}
                 :corp {:hand ["Enigma"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Euler")
      (run-on state :hq)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (auto-pump-and-break state (get-program state 0))
      (is (second-last-log-contains? state "Runner pays 0 [Credits] to use Euler to break all 2 subroutines on Enigma.") "Correct log with correct cost")
      (core/continue state :corp nil)
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (auto-pump-and-break state (get-program state 0))
      (is (second-last-log-contains? state "Runner pays 2 [Credits] to use Euler to break all 2 subroutines on Enigma.") "Correct second log with correct cost")
      (core/continue state :corp nil)))

(deftest expert-schedule-analyzer
  ;; Expert Schedule Analyzer
  (do-game
    (new-game {:runner {:deck ["Expert Schedule Analyzer"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Expert Schedule Analyzer")
    (card-ability state :runner (get-program state 0) 0)
    (run-continue state)
    (is (= "Choose a breach replacement ability" (:msg (prompt-map :runner)))
        "Replacement effect is optional")
    (click-prompt state :runner "Expert Schedule Analyzer")
    (is (last-log-contains? state "Runner uses Expert Schedule Analyzer to reveal Hedge Fund, Hedge Fund, and Hedge Fund from HQ")
        "All of HQ is revealed correctly")))

(deftest faerie-trash-after-encounter-is-over-not-before
    ;; Trash after encounter is over, not before
    (do-game
      (new-game {:corp {:deck ["Caduceus"]}
                 :runner {:deck ["Faerie"]}})
      (play-from-hand state :corp "Caduceus" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Faerie")
      (let [fae (get-program state 0)]
        (run-on state :archives)
        (rez state :corp (get-ice state :archives 0))
        (run-continue state)
        (card-ability state :runner fae 1)
        (card-ability state :runner fae 0)
        (click-prompt state :runner "Trace 3 - Gain 3 [Credits]")
        (click-prompt state :runner "Trace 2 - End the run")
        (is (refresh fae) "Faerie not trashed until encounter over")
        (run-continue state)
        (is (find-card "Faerie" (:discard (get-runner))) "Faerie trashed"))))

(deftest faerie-trash-does-not-trigger-dummy-box
    ;; Faerie trash doesn't trigger Dummy Box
    (do-game
      (new-game {:corp {:deck ["Caduceus"]}
                 :runner {:deck [(qty "Faerie" 2) "Dummy Box"]}})
      (play-from-hand state :corp "Caduceus" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Faerie")
      (play-from-hand state :runner "Dummy Box")
      (let [fae (get-program state 0)]
        (run-on state :archives)
        (rez state :corp (get-ice state :archives 0))
        (run-continue state)
        (card-ability state :runner fae 1)
        (card-ability state :runner fae 0)
        (click-prompt state :runner "Trace 3 - Gain 3 [Credits]")
        (click-prompt state :runner "Trace 2 - End the run")
        (run-continue state)
        (is (no-prompt? state :runner) "Dummy Box not prompting to prevent trash"))))

(deftest faerie-works-with-auto-pump-and-break
    ;; Works with auto-pump-and-break
    (do-game
      (new-game {:corp {:deck ["Caduceus"]}
                 :runner {:deck ["Faerie"]}})
      (play-from-hand state :corp "Caduceus" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Faerie")
      (let [fae (get-program state 0)]
        (run-on state :archives)
        (rez state :corp (get-ice state :archives 0))
        (run-continue state)
        (auto-pump-and-break state (refresh fae))
        (core/continue state :corp nil)
        (is (find-card "Faerie" (:discard (get-runner))) "Faerie trashed"))))

(deftest false-echo-add-to-hq
    ;; Add to HQ
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["False Echo"]}})
      (play-from-hand state :corp "Ice Wall" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "False Echo")
      (run-on state "Archives")
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-prompt state :corp "Add Ice Wall to HQ")
      (is (find-card "Ice Wall" (:hand (get-corp))) "Ice Wall added to HQ")
      (is (find-card "False Echo" (:discard (get-runner))) "False Echo trashed")))

(deftest false-echo-rez
    ;; Rez
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["False Echo"]}})
      (play-from-hand state :corp "Ice Wall" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "False Echo")
      (run-on state "Archives")
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-prompt state :corp "Rez Ice Wall")
      (is (rezzed? (get-ice state :archives 0)) "Ice Wall rezzed")
      (is (find-card "False Echo" (:discard (get-runner))) "False Echo trashed")))

(deftest faust
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:deck ["Faust" "Sure Gamble"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Faust")
      (let [faust (get-program state 0)]
        (run-on state :hq)
        (rez state :corp (get-ice state :hq 0))
        (run-continue state)
        (card-ability state :runner faust 0)
        (click-prompt state :runner "End the run")
        (click-card state :runner "Sure Gamble")
        (is (= 1 (count (:discard (get-runner)))) "1 card trashed")))
  (testing "Basic test: Pump by discarding"
    (do-game
      (new-game {:runner {:deck ["Faust" "Sure Gamble"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Faust")
      (let [faust (get-program state 0)]
        (card-ability state :runner faust 1)
        (click-card state :runner "Sure Gamble")
        (is (= 4 (get-strength (refresh faust))) "4 current strength")
        (is (= 1 (count (:discard (get-runner)))) "1 card trashed")))))

(deftest faust-pump-does-not-trigger-trash-prevention-760
    ;; Pump does not trigger trash prevention. #760
    (do-game
      (new-game {:runner {:hand ["Faust"
                                 "Sacrificial Construct"
                                 "Fall Guy"
                                 "Astrolabe"
                                 "Gordian Blade"
                                 "Armitage Codebusting"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Faust")
      (play-from-hand state :runner "Fall Guy")
      (play-from-hand state :runner "Sacrificial Construct")
      (is (= 2 (count (get-resource state))) "Resources installed")
      (let [faust (get-program state 0)]
        (card-ability state :runner faust 1)
        (click-card state :runner "Astrolabe")
        (is (no-prompt? state :runner) "No trash-prevention prompt for hardware")
        (card-ability state :runner faust 1)
        (click-card state :runner "Gordian Blade")
        (is (no-prompt? state :runner) "No trash-prevention prompt for program")
        (card-ability state :runner faust 1)
        (click-card state :runner "Armitage Codebusting")
        (is (no-prompt? state :runner) "No trash-prevention prompt for resource"))))

(deftest fawkes-requires-a-stealth-credit-to-pump
    ;; Requires a stealth credit to pump
    (do-game (new-game {:runner {:hand ["Fawkes"] :credits 20}})
      (take-credits state :corp)
      (play-from-hand state :runner "Fawkes")
      (let [fawkes (get-program state 0)]
        (is (changed? [(get-strength (refresh fawkes)) 0]
              (card-ability state :runner fawkes 1)
              (is (no-prompt? state :runner) "Not asked how many credits to pay"))
            "Strength was not increased"))))

(deftest fawkes-charges-the-correct-amount
    ;; Charges the correct amount
    (do-game (new-game {:runner {:hand ["Fawkes" "Cloak"] :credits 20}})
      (take-credits state :corp)
      (play-from-hand state :runner "Fawkes")
      (play-from-hand state :runner "Cloak")
      (let [fawkes (get-program state 0)
            cloak (get-program state 1)]
        (is (changed? [(:credit (get-runner)) -2]
              (card-ability state :runner fawkes 1)
              (click-prompt state :runner "3")
              (click-card state :runner cloak))
            "Runner was charged correctly"))))

(deftest fawkes-pumps-the-correct-amount
    ;; Pumps the correct amount
    (do-game (new-game {:runner {:hand ["Fawkes" "Cloak"] :credits 20}})
      (take-credits state :corp)
      (play-from-hand state :runner "Fawkes")
      (play-from-hand state :runner "Cloak")
      (let [fawkes (get-program state 0)
            cloak (get-program state 1)]
        (is (changed? [(get-strength (refresh fawkes)) 3]
              (card-ability state :runner fawkes 1)
              (click-prompt state :runner "3")
              (click-card state :runner cloak))
            "Strength increased correctly"))))

(deftest femme-fatale-bypass-functionality
    ;; Bypass functionality
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Femme Fatale"]
                          :credits 20}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (let [iw (get-ice state :hq 0)]
        (play-from-hand state :runner "Femme Fatale")
        (click-card state :runner iw)
        (run-on state "HQ")
        (rez state :corp iw)
        (run-continue state)
        (is (= "Pay 1 [Credits] to bypass Ice Wall?" (:msg (prompt-map :runner))))
        (click-prompt state :runner "Yes")
        (is (= :movement (:phase (get-run))) "Femme Fatale has bypassed Ice Wall"))))

(deftest femme-fatale-bypass-leaves-if-uninstalled
    ;; Bypass leaves if uninstalled
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Femme Fatale"]
                          :credits 20}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (let [iw (get-ice state :hq 0)]
        (play-from-hand state :runner "Femme Fatale")
        (click-card state :runner iw)
        (core/move state :runner (get-program state 0) :deck)
        (run-on state "HQ")
        (rez state :corp iw)
        (run-continue state)
        (is (no-prompt? state :runner)) "Femme ability doesn't fire after uninstall")))

(deftest femme-fatale-bypass-doesn-t-persist-if-ice-is-uninstalled-and-reinstalled
    ;; Bypass doesn't persist if ice is uninstalled and reinstalled
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Femme Fatale"]
                          :credits 20}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Femme Fatale")
      (click-card state :runner (get-ice state :hq 0))
      (core/move state :corp (get-ice state :hq 0) :hand)
      (take-credits state :runner)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (no-prompt? state :runner)) "Femme ability doesn't fire after uninstall"))

(deftest fermenter-trash-and-cash-out
    ;; Trash and cash out
    (do-game
      (new-game {:runner {:deck ["Fermenter"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Fermenter")
      (let [fermenter (get-program state 0)]
        (is (= 1 (get-counters (refresh fermenter) :virus)) "Fermenter has 1 counter from install")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 2 (get-counters (refresh fermenter) :virus)) "Fermenter has 2 counters")
        (is (changed? [(:credit (get-runner)) 4]
              (card-ability state :runner fermenter 0))
            "Gain 4 credits from Fermenter ability")
        (is (= 1 (count (:discard (get-runner)))) "Fermenter is trashed"))))

(deftest fermenter-hivemind-interaction
    ;; Hivemind interaction
    (do-game
      (new-game {:corp {:deck ["Adonis Campaign"]}
                 :runner {:deck ["Fermenter" "Hivemind"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Fermenter")
      (play-from-hand state :runner "Hivemind")
      (take-credits state :runner)
      (take-credits state :corp)
      (let [fermenter (get-program state 0)
            hivemind (get-program state 1)]
        (is (= 2 (get-counters (refresh fermenter) :virus)) "Fermenter has 2 counters")
        (is (= 1 (get-counters (refresh hivemind) :virus)) "Hivemind has 1 counter")
        (is (changed? [(:credit (get-runner)) 6]
              (card-ability state :runner fermenter 0))
            "Gain 6 credits from Fermenter ability")
        (is (= 1 (count (:discard (get-runner)))) "Fermenter is trashed")
        (is (= 1 (get-counters (refresh hivemind) :virus)) "Hivemind has still 1 counter"))))

(deftest flux-capacitor
  ;; Flux Capacitor
  (do-game
    (new-game {:runner {:hand ["Earthrise Hotel" "Corroder" "Flux Capacitor"]
                        :credits 10}
               :corp {:hand ["Spiderweb"]}})
    (play-from-hand state :corp "Spiderweb" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Earthrise Hotel")
    (play-from-hand state :runner "Corroder")
    (let [sweb (get-ice state :hq 0)
          corr (get-program state 0)
          hotel (get-resource state 0)]
      (play-from-hand state :runner "Flux Capacitor")
      (click-card state :runner sweb)
      (run-on state :hq)
      (rez state :corp sweb)
      (run-continue state)
      (card-ability state :runner corr 0)
      (click-prompt state :runner "End the run")
      (is (changed? [(get-counters (refresh corr) :power) 0]
            (click-card state :runner corr))
          "Cannot charge Corroder")
      (is (changed? [(get-counters (refresh hotel) :power) 1]
            (click-card state :runner hotel))
          "Charged Earthrise Hotel")
      (click-prompt state :runner "End the run")
      (is (not (= :select (prompt-type :runner))) "No charge prompt"))))

(deftest flux-capacitor-triggers-on-first-event-only
  ;; Flux Capacitor - triggers on first event only
  (do-game
    (new-game {:runner {:hand ["Earthrise Hotel" "Corroder" "Flux Capacitor"]
                        :credits 10}
               :corp {:hand ["Spiderweb"]}})
    (play-from-hand state :corp "Spiderweb" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Earthrise Hotel")
    (play-from-hand state :runner "Corroder")
    (let [sweb (get-ice state :hq 0)
          corr (get-program state 0)
          hotel (get-resource state 0)]
      (play-from-hand state :runner "Flux Capacitor")
      (click-card state :runner sweb)
      (run-on state :hq)
      (rez state :corp sweb)
      (run-continue state)
      (card-ability state :runner corr 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "Done")
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "End the run"))))

(deftest flux-capacitor-works-on-every-encounter
  ;; Flux Capacitor - works on every encounter with host ice
  (do-game
    (new-game {:runner {:hand ["Earthrise Hotel" "Buzzsaw" "Flux Capacitor"]
                        :credits 10}
               :corp {:hand ["Ice Wall" "Thimblerig"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Thimblerig" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Earthrise Hotel")
    (play-from-hand state :runner "Buzzsaw")
    (let [iw (get-ice state :hq 0)
          thim (get-ice state :hq 1)
          buzz (get-program state 0)
          hotel (get-resource state 0)]
      (play-from-hand state :runner "Flux Capacitor")
      (click-card state :runner thim)
      (run-on state :hq)
      (rez state :corp thim)
      (run-continue state)
      (card-ability state :runner buzz 0)
      (click-prompt state :runner "End the run")
      (is (changed? [(get-counters (refresh hotel) :power) 1]
            (click-card state :runner hotel))
          "Charged Earthrise Hotel")
      (run-continue state)
      (click-prompt state :corp "Yes")
      (click-card state :corp iw)
      (run-continue-until state :encounter-ice thim)
      (card-ability state :runner buzz 0)
      (click-prompt state :runner "End the run")
      (is (changed? [(get-counters (refresh hotel) :power) 1]
            (click-card state :runner hotel))
          "Charged Earthrise Hotel"))))

(deftest gauss-loses-strength-at-end-of-runner-s-turn
    ;; Loses strength at end of Runner's turn
    (do-game
      (new-game {:runner {:deck ["Gauss"]}
                 :options {:start-as :runner}})
      (play-from-hand state :runner "Gauss")
      (let [gauss (get-program state 0)]
        (is (= 4 (get-strength (refresh gauss))) "+3 base strength")
        (run-on state :hq)
        (card-ability state :runner (refresh gauss) 1) ;; boost
        (is (= 6 (get-strength (refresh gauss))) "+3 base and boosted strength")
        (run-jack-out state)
        (is (= 4 (get-strength (refresh gauss))) "Boost lost after run")
        (take-credits state :runner)
        (is (= 1 (get-strength (refresh gauss))) "Back to normal strength"))))

(deftest gauss-loses-strength-at-end-of-corp-s-turn
    ;; Loses strength at end of Corp's turn
    (do-game
      (new-game {:runner {:deck ["Gauss"]}})
      (core/gain state :runner :click 1)
      (play-from-hand state :runner "Gauss")
      (let [gauss (get-program state 0)]
        (is (= 4 (get-strength (refresh gauss))) "+3 base strength")
        (take-credits state :corp)
        (is (= 1 (get-strength (refresh gauss))) "Back to normal strength"))))

(deftest god-of-war
  ;; God of War - Take 1 tag to place 2 virus counters
  (do-game
    (new-game {:runner {:deck ["God of War"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "God of War")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [gow (get-program state 0)]
      (card-ability state :runner gow 2)
      (is (= 1 (count-tags state)))
      (is (= 2 (get-counters (refresh gow) :virus)) "God of War has 2 virus counters"))))

(deftest gorman-drip-v1
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand ["Hedge Fund" "Anonymous Tip"]}
               :runner {:hand ["Gorman Drip v1"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Gorman Drip v1")
    (take-credits state :runner)
    (let [gorman (get-program state 0)]
      (is (changed? [(get-counters (refresh gorman) :virus) 2]
            (click-credit state :corp)
            (click-draw state :corp))
          "Clicking gains a counter")
      (is (changed? [(get-counters (refresh gorman) :virus) 0]
            (play-from-hand state :corp "Hedge Fund")
            (play-from-hand state :corp "Anonymous Tip"))
          "Playing a card gains none")
      (take-credits state :corp)
      (is (changed? [(:credit (get-runner)) 2]
            (card-ability state :runner gorman 0))
          "Ability gains credits")
      (is (nil? (refresh gorman)) "Gorman is trashed"))))

(deftest grappling-hook
  ;; Grappling Hook
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Little Engine"]
                        :credits 10}
                 :runner {:hand [(qty "Grappling Hook" 2) "Corroder" "Torch"]
                          :credits 100}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Little Engine" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (play-from-hand state :runner "Grappling Hook")
      (play-from-hand state :runner "Grappling Hook")
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Torch")
      (let [iw (get-ice state :hq 0)
            le (get-ice state :remote1 0)
            gh1 (get-program state 0)
            gh2 (get-program state 1)
            cor (get-program state 2)
            torch (get-program state 3)]
        (run-on state :hq)
        (rez state :corp iw)
        (run-continue state)
        (card-ability state :runner gh1 0)
        (is (no-prompt? state :runner) "No break prompt as Ice Wall only has 1 subroutine")
        (is (refresh gh1) "Grappling Hook isn't trashed")
        (card-ability state :runner cor 0)
        (click-prompt state :runner "End the run")
        (card-ability state :runner gh1 0)
        (is (no-prompt? state :runner) "No break prompt as Ice Wall has no unbroken subroutines")
        (is (refresh gh1) "Grappling Hook isn't trashed")
        (run-continue state :movement)
        (run-jack-out state)
        (run-on state :remote1)
        (rez state :corp le)
        (run-continue state)
        (card-ability state :runner gh1 0)
        (is (not (no-prompt? state :runner)) "Grappling Hook creates break prompt")
        (click-prompt state :runner "End the run")
        (is (= 2 (count (filter :broken (:subroutines (refresh le))))) "Little Engine has 2 of 3 subroutines broken")
        (is (nil? (refresh gh1)) "Grappling Hook is now trashed")
        (run-continue state :movement)
        (run-jack-out state)
        (run-on state :remote1)
        (run-continue state)
        (core/update! state :runner (assoc (refresh torch) :current-strength 7))
        (card-ability state :runner torch 0)
        (click-prompt state :runner "End the run")
        (click-prompt state :runner "End the run")
        (click-prompt state :runner "Done")
        (card-ability state :runner gh2 0)
        (is (no-prompt? state :runner) "No break prompt as Little Engine has more than 1 broken sub")
        (is (refresh gh2) "Grappling Hook isn't trashed"))))

(deftest grappling-hook-interaction-with-fairchild-3-0
    ;; Interaction with Fairchild 3.0
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Fairchild 3.0"]
                        :credits 6}
                 :runner {:hand ["Grappling Hook"] }})
      (play-from-hand state :corp "Fairchild 3.0" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Grappling Hook")
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "Do 1 core damage or end the run")
      (is (= 1 (count (remove :broken (:subroutines (get-ice state :hq 0))))) "Broke all but one subroutine")
      (is (= "Do 1 core damage or end the run" (:label (first (remove :broken (:subroutines (get-ice state :hq 0)))))) "Broke all but selected sub")
      (is (nil? (refresh (get-program state 0))) "Grappling Hook is now trashed")))

(deftest grappling-hook-interaction-with-news-hound-4988
    ;; interaction with News Hound #4988
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["News Hound" "Surveillance Sweep"]
                        :credits 10}
                 :runner {:hand ["Grappling Hook"]
                          :credits 100}})
      (play-from-hand state :corp "News Hound" "HQ")
      (play-from-hand state :corp "Surveillance Sweep")
      (take-credits state :corp)
      (play-from-hand state :runner "Grappling Hook")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "Trace 3 - Give the Runner 1 tag")
      (fire-subs state (get-ice state :hq 0))
      (click-prompt state :runner "10")
      (click-prompt state :corp "1")
      (is (zero? (count-tags state)) "Runner gained no tags")
      (is (get-run) "Run hasn't ended")
      (is (no-prompt? state :corp) "Corp shouldn't have a prompt")
      (is (no-prompt? state :runner) "Runner shouldn't have a prompt")))

(deftest grappling-hook-selecting-a-sub-when-multiple-of-the-same-title-exist-5291
    ;; Selecting a sub when multiple of the same title exist #5291
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hive"]
                        :credits 10}
                 :runner {:hand ["Grappling Hook" "Gbahali"]
                          :credits 10}})
      (play-from-hand state :corp "Hive" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Grappling Hook")
      (play-from-hand state :runner "Gbahali")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) "Break all but 1 subroutine")
      (click-prompt state :runner "End the run" {:idx 4})
      (card-ability state :runner (get-resource state 0) "Break the last subroutine")
      (is (core/all-subs-broken? (get-ice state :hq 0))
          "Grappling Hook and Gbahali worked together")))

(deftest gravedigger
  ;; Gravedigger - Gain counters when Corp cards are trashed, spend click-counter to mill Corp
  (do-game
    (new-game {:corp {:deck [(qty "Launch Campaign" 2) (qty "Enigma" 2)]}
               :runner {:deck ["Gravedigger"]}})
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Gravedigger")
    (let [gd (get-program state 0)]
      (trash state :corp (get-content state :remote1 0))
      (is (= 1 (get-counters (refresh gd) :virus)) "Gravedigger gained 1 counter")
      (trash state :corp (get-content state :remote2 0))
      (is (= 2 (get-counters (refresh gd) :virus)) "Gravedigger gained 1 counter")
      (core/move state :corp (find-card "Enigma" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Enigma" (:hand (get-corp))) :deck)
      (is (= 2 (count (:deck (get-corp)))))
      (card-ability state :runner gd 0)
      (is (= 1 (get-counters (refresh gd) :virus)) "Spent 1 counter from Gravedigger")
      (is (= 2 (:click (get-runner))) "Spent 1 click")
      (is (= 1 (count (:deck (get-corp)))))
      (is (= 3 (count (:discard (get-corp)))) "Milled 1 card from R&D"))))

(deftest harbinger-install-facedown-when-blacklist-installed
    ;; install facedown when Blacklist installed
    (do-game
      (new-game {:corp {:deck ["Blacklist"]}
                 :runner {:deck ["Harbinger"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Harbinger")
      (trash state :runner (-> (get-runner) :rig :program first))
      (is (zero? (count (:discard (get-runner)))) "Harbinger not in heap")
      (is (-> (get-runner) :rig :facedown first :facedown) "Harbinger installed facedown")))

(deftest heliamphora-purge
  (do-game
    (new-game {:corp {:hand [(qty "Hedge Fund" 5)]}
               :runner {:hand ["Heliamphora"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Heliamphora")
    (take-credits state :runner)
    (is (changed? [(count (:hand (get-corp))) -2
                   (count (:discard (get-corp))) 2
                   (count (:discard (get-runner))) 1]
                  (purge state :corp))
        "Corp purges and trashes 2 random cards from HQ and Heliamphora")))

(deftest houdini-must-use-a-single-stealth-credit-to-pump
  ;; Must use a single stealth credit to pump
  (do-game (new-game {:runner {:deck ["Houdini" "Cloak"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Houdini")
    (play-from-hand state :runner "Cloak")
    (let [houdini (get-program state 0) cloak (get-program state 1)]
      (is (changed? [(get-strength (refresh houdini)) 4]
            (card-ability state :runner houdini 1)
            (click-card state :runner cloak))
          "Houdini gains strength"))))

(deftest houdini-can-t-pump-without-a-stealth-credit
    ;; Can't pump without a stealth credit
    (do-game (new-game {:runner {:deck ["Houdini"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Houdini")
      (let [houdini (get-program state 0)]
        (is (changed? [(:credit (get-runner)) 0
                       (get-strength houdini) 0]
              (card-ability state :runner houdini 1))
            "Runner has not been charged, strength hasn't changed"))))

(deftest hyperbaric
  ;; Hyperbaric - I can't believe it's not Study Guide
  ;; Starts with a counter, 2c to add a power counter; +1 strength per counter
  (do-game
    (new-game {:runner {:deck ["Hyperbaric" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Hyperbaric")
    (let [sg (get-program state 0)]
      (card-ability state :runner sg 1)
      (is (= 4 (:credit (get-runner))) "Paid 2c")
      (is (= 2 (get-counters (refresh sg) :power)) "Has 2 power counters")
      (is (= 2 (get-strength (refresh sg))) "2 strength")
      (card-ability state :runner sg 1)
      (is (= 2 (:credit (get-runner))) "Paid 2c")
      (is (= 3 (get-counters (refresh sg) :power)) "Has 3 power counters")
      (is (= 3 (get-strength (refresh sg))) "3 strength"))))

(deftest hush-vs-afshar
  ;; Hush vs. Afshar
  (do-game
    (install-hush-and-run "Afshar" {:hushed true :rig ["Buzzsaw"]})
    (let [buzz (get-program state 0)]
      (run-continue-until state :encounter-ice)
      (card-ability state :runner (refresh buzz) 0)
      (click-prompt state :runner "Make the Runner lose 2 [Credits]")
      (click-prompt state :runner "End the run")
      (is (no-prompt? state :runner) "No break prompt as Afshar has no unbroken subroutines"))))

(deftest hush-vs-akhet
  ;; Hush vs. Akhet
  (advancable-while-hushed-test? "Akhet" true)
  (do-game
    (install-hush-and-run "Akhet" {:hushed true :rig ["Cleaver"] :counters {:advancement 3}})
    (let [akhet (get-ice state :hq 0)
          cleaver (get-program state 0)]
      (is (= 2 (get-strength (refresh akhet))) "No str gain while hushed")
      (run-continue-until state :encounter-ice)
      (card-ability state :runner cleaver 0)
      (click-prompt state :runner "Gain 1 [Credit]. Place 1 advancement token")
      (click-prompt state :runner "End the run"))))

(deftest hush-vs-anansi
  ;;Hush vs. Anansi
  (do-game
    (new-game {:corp {:hand ["Anansi"] :credits 15}
               :runner {:hand ["Hush" (qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Anansi" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Hush")
    (click-card state :runner "Anansi")
    (run-on state :hq)
    (run-continue-until state :encounter-ice)
    (run-continue state :pass-ice)
    (is (not (seq (:discard (get-runner)))) "No anansi damage")))

(deftest hush-vs-attini
  ;;hush interacts with attini
  (do-game
    (new-game {:corp {:hand ["Attini" "City Works Project"]
                      :credits 50}
               :runner {:hand ["Hush"]}})
    (play-and-score state "City Works Project")
    (play-from-hand state :corp "Attini" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Hush")
    (click-card state :runner "Attini")
    (run-on state :hq)
    (run-continue-until state :encounter-ice)
    (card-subroutine state :corp (get-ice state :hq 0) 0)
    (is (not (no-prompt? state :runner)) "(hush)Can pay for attini despite threat")
    (click-prompt state :runner "Pay 2 [Credits]")))

(deftest hush-vs-blockchain
  ;;hush interacts with blockchain
  (do-game
    (new-game {:corp {:hand ["Blockchain" (qty "Hedge Fund" 2)]}
               :runner {:hand ["Hush" "Spec Work"]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Blockchain" "HQ")
    (let [block (get-ice state :hq 0)]
      (rez state :corp block)
      (is (= 3 (count (:subroutines (refresh block)))) "3 subs to start")
      (take-credits state :corp)
      (play-from-hand state :runner "Hush")
      (click-card state :runner "Blockchain")
      (is (= 2 (count (:subroutines (refresh block)))) "blockchain lost a sub to hush")
      (play-from-hand state :runner "Spec Work")
      (click-card state :runner "Hush")
      (is (= 3 (count (:subroutines (refresh block)))) "blockchain is back to 3 subs"))))

(deftest hush-vs-echo
  ;;  hush vs. echo
  (do-game
    (install-hush-and-run "Echo" {:rig ["Simulchip"]
                                  :players {:runner {:discard ["Fermenter"]}}
                                  :counters {:power 5}
                                  :hushed true})
    (let [echo (get-ice state :hq 0)
          sim (get-hardware state 0)]
      (is (= 0 (count (:subroutines echo))) "No subroutines because we're hushed")
      (card-ability state :runner sim 0)
      (click-card state :runner "Hush")
      (click-card state :runner "Fermenter")
      (is (= 6 (count (:subroutines (refresh echo)))) "5+1 subs now"))))

(deftest hush-vs-envelopment
  ;; hush vs envelopment
  (do-game
    (install-hush-and-run "Envelopment" {:rig ["Simulchip"]
                                         :players {:runner {:discard ["Fermenter"]}}
                                         :hushed true})
    (let [env (get-ice state :hq 0)
          sim (get-hardware state 0)]
      (is (= 1 (count (:subroutines env))) "1 subroutine because we're hushed")
      (card-ability state :runner sim 0)
      (click-card state :runner "Hush")
      (click-card state :runner "Fermenter")
      (is (= 5 (count (:subroutines (refresh env)))) "4+1 subs now"))))

(deftest hush-vs-funhouse
  ;; hush vs. funhouse
  (do-game
    (install-hush-and-run "Funhouse" {:hushed true})
    (run-continue-until state :encounter-ice)
    (is (no-prompt? state :runner) "No funhouse prompt because of hush")))

(deftest hush-vs-hive
  ;; hush vs. hive
  (do-game
    (install-hush-and-run "Hive" {:scored ["City Works Project"]
                                  :hushed true})
    (let [ice (get-ice state :hq 0)]
      (is (= 5 (count (:subroutines ice))) "full subs on hive because hush")
      (trash state :runner (first (:hosted (refresh ice))))
      (is (= 2 (count (:subroutines (refresh ice)))) "5-3 subs on hive now"))))

(deftest hush-vs-hortum
  ;; hush vs. hortum
  (advancable-while-hushed-test? "Hortum" true)
  (do-game
    (install-hush-and-run "Hortum" {:rig ["Alpha"] :counters {:advancement 3} :hushed true})
    (run-continue-until state :encounter-ice)
    (let [prog (get-program state 0)]
      (card-ability state :runner prog 1)
      (card-ability state :runner prog 1)
      (card-ability state :runner prog 1)
      (card-ability state :runner prog 0)
      (click-prompt state :runner "Gain 1 [Credits] (Gain 4 [Credits])")
      (click-prompt state :runner "End the run (Search R&D for up to 2 cards and add them to HQ, shuffle R&D, end the run)"))))

(deftest hush-vs-information-overload
  ;; hush vs. information overload
  (do-game
    (install-hush-and-run "Information Overload" {:hushed true :tags 5})
    (run-continue-until state :encounter-ice)
    (is (no-prompt? state :runner) "No Info Overload prompt because of hush")
    (let [ice (get-ice state :hq 0)]
      (is (= 0 (count (:subroutines (refresh ice)))) "No subs due to hush")
      (trash state :runner (first (:hosted (refresh ice))))
      (is (= 5 (count (:subroutines (refresh ice)))) "5 subs now"))))

(deftest hush-vs-masvingo
  ;;  masvingo *
  (advancable-while-hushed-test? "Masvingo" true)
  (do-game
    (install-hush-and-run "Masvingo" {:counters {:advancement 5}
                                      :hushed true})
    (let [ice (get-ice state :hq 0)]
      (is (= 0 (count (:subroutines ice))) "0 subroutine because we're hushed")
      (trash state :runner (first (:hosted (refresh ice))))
      (is (= 6 (count (:subroutines (refresh ice)))) "5+1 on masvingo subs now"))))

(deftest hush-vs-mausolus
  ;;  mausolus
  (advancable-while-hushed-test? "Mausolus" true))

(deftest hush-vs-cloud-eater
  ;;  cloud eater
  (do-game
    (install-hush-and-run "Cloud Eater" {:hushed true})
    (run-continue-until state :encounter-ice)
    (run-continue state)
    (is (no-prompt? state :runner) "No Cloud Eater prompt because of hush")))

(deftest hush-vs-next-bronze
  ;; NEXT Bronze
  (do-game
    (install-hush-and-run "NEXT Bronze" {:hushed true})
    (let [ice (get-ice state :hq 0)]
      (is (= 0 (get-strength ice)) "NEXt Bronze: X is 0 while hushed")
      (trash state :runner (first (:hosted (refresh ice))))
      (is (= 1 (get-strength (refresh ice))) "NEXt Bronze: X is 1 post-hush"))))

(deftest hush-vs-next-gold
  ;; NEXT Gold
  (do-game
    (install-hush-and-run "NEXT Gold" {:hushed true :runner {:hand 2}})
    (let [ice (get-ice state :hq 0)]
      (run-continue-until state :encounter-ice)
      (fire-subs state ice)
      (is (zero? (count (:discard (get-runner)))) "X is 0, so gold does 0 net")
      (is (no-prompt? state :corp) "X is 0, so gold trashes 0 programs"))))

(deftest hush-vs-next-silver
  ;; NEXT Silver
  (do-game
    (install-hush-and-run "NEXT Silver" {:hushed true})
    (let [ice (get-ice state :hq 0)]
      (is (= 0 (count (:subroutines ice))) "0 subroutine because we're hushed")
      (trash state :runner (first (:hosted (refresh ice))))
      (is (= 1 (count (:subroutines (refresh ice)))) "1 on NEXT Silver subs now"))))

(deftest hush-vs-next-opal
  ;; NEXT Opal
  (do-game
    (install-hush-and-run "NEXT Opal" {:hushed true})
    (let [ice (get-ice state :hq 0)]
      (is (= 0 (count (:subroutines ice))) "0 subroutine because we're hushed")
      (trash state :runner (first (:hosted (refresh ice))))
      (is (= 1 (count (:subroutines (refresh ice)))) "1 on NEXT Opal subs now"))))

(deftest hush-vs-space-ice
  ;; Orion, Nebula, Wormhole, Asteroid Belt
  (doseq [space ["Orion" "Wormhole" "Nebula" "Asteroid Belt"]]
    (advancable-while-hushed-test? space true)
    (do-game
      (install-hush-and-run space {:hushed true
                                   :unrezzed true
                                   :counters {:advancement 5}})
      (let [ice (get-ice state :hq 0)
            creds (:credit (get-corp))]
        (rez state :corp ice)
        (is (not= creds (:credit (get-corp))) (str "Paid full price for " space " (hushed)"))))))

(deftest hush-vs-saisentan
  ;;  saisentan
  (do-game
    (install-hush-and-run "Saisentan" {:hushed true})
    (run-continue-until state :encounter-ice)
    (is (no-prompt? state :corp) "No Saisentan prompt because of hush")))

(deftest hush-vs-salvage
  ;;  salbage
  (advancable-while-hushed-test? "Salvage" false)
  (do-game
    (install-hush-and-run "Salvage" {:rig ["Simulchip"]
                                     :counters {:advancement 5}
                                     :players {:runner {:discard ["Fermenter"]}}
                                     :hushed true})
    (let [ice (get-ice state :hq 0)
          sim (get-hardware state 0)]
      (is (= 0 (count (:subroutines ice))) "0 subroutine because we're hushed")
      (card-ability state :runner sim 0)
      (click-card state :runner "Hush")
      (click-card state :runner "Fermenter")
      (is (= 5 (count (:subroutines (refresh ice)))) "5 subs on salvage now"))))

(deftest hush-vs-seraph
  ;;  seraph
  (do-game
    (install-hush-and-run "Seraph" {:hushed true})
    (run-continue-until state :encounter-ice)
    (is (no-prompt? state :runner) "No Seraph prompt because of hush")))

(deftest hush-vs-searchlight
  ;;  searchlight
  (advancable-while-hushed-test? "Searchlight" true))

(deftest hush-vs-stavka
  ;; Stavka
  (do-game
    (install-hush-and-run "Stavka" {:hushed true :unrezzed true :assets ["PAD Campaign"]})
    (rez state :corp (get-ice state :hq 0))
    (is (no-prompt? state :corp) "No stavka prompt due to hush")))

(deftest hush-vs-surveyor
  ;;  surveyor
  (do-game
    (install-hush-and-run "Surveyor" {:hushed true})
    (run-continue-until state :encounter-ice)
    (fire-subs state (get-ice state :hq 0))
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (zero? (count-tags state)) "No tags from surveyor")
    (is (:run @state) "run didn't end (X = 0, surveyor)")))

(deftest hush-vs-swarm
  ;;  swarm
  (advancable-while-hushed-test? "Swarm" true)
  (do-game
    (install-hush-and-run "Swarm" {:rig ["Simulchip"]
                                   :counters {:advancement 5}
                                   :players {:runner {:discard ["Fermenter"]}}
                                   :hushed true})
    (let [ice (get-ice state :hq 0)
          sim (get-hardware state 0)]
      (is (= 0 (count (:subroutines ice))) "0 subroutine because we're hushed")
      (card-ability state :runner sim 0)
      (click-card state :runner "Hush")
      (click-card state :runner "Fermenter")
      (is (= 5 (count (:subroutines (refresh ice)))) "5 subs on swarm now"))))

(deftest hush-vs-thoth
  ;;  thoth
  (do-game
    (install-hush-and-run "Thoth" {:hushed true})
    (run-continue-until state :encounter-ice)
    (is (no-prompt? state :corp) "No Thoth prompt because of hush")))

(deftest hush-vs-tithonium
  ;;  tithonium
  (do-game
    (install-hush-and-run "Tithonium" {:hushed true :unrezzed true :scored ["City Works Project"]})
    (rez state :corp (get-ice state :hq 0))
    (is (no-prompt? state :corp) "No alternate cost prompt")
    (is (zero? (count (:discard (get-runner)))) "Hush not trashed")))

(deftest hush-vs-tollbooth
  ;;  tollbooth
  (do-game
    (install-hush-and-run "Tollbooth" {:hushed true})
    (is (= 5 (:credit (get-runner))))
    (run-continue-until state :encounter-ice)
    (is (= 5 (:credit (get-runner))) "No payment to tollbooth")
    (is (no-prompt? state :runner) "No tollbooth prompt because of hush")))

(deftest hush-vs-tour-guide
  ;;  tour guide *
  (do-game
    (install-hush-and-run "Tour Guide" {:hushed true
                                        :assets ["PAD Campaign" "NGO Front"]})
    (run-continue-until state :encounter-ice)
    (let [ice (get-ice state :hq 0)]
      (is (= 0 (count (:subroutines (refresh ice)))) "No subs on tour guide due to hush")
      (trash state :runner (first (:hosted (refresh ice))))
      (is (= 2 (count (:subroutines (refresh ice)))) "2 subs on tour guide now"))))

(deftest hush-vs-turing
  ;;  turing
  (do-game
    (install-hush-and-run "Turing" {:hushed true
                                    :rig ["Alpha"]
                                    :server "New remote"})
    (run-continue-until state :encounter-ice)
    (let [prog (get-program state 0)
          tur (get-ice state :remote1 0)]
      (is (= 2 (get-strength tur)) "Turing is 2 strength due to hush")
      (card-ability state :runner prog 1)
      (is (= 2 (get-strength (refresh prog))) "Alpha is 2 strength")
      (card-ability state :runner prog 0)
      (click-prompt state :runner "End the run unless the Runner pays [Click][Click][Click]"))))

(deftest hush-vs-tyr
  ;;  tyr
  (do-game
    (install-hush-and-run "Týr" {:hushed true})
    (run-continue state :encounter-ice)
    (card-side-ability state :runner (get-ice state :hq 0) 0)
    ;; NOTE - this isn't possible in game, but it is in the test....
    (is (no-prompt? state :runner) "No prompt to break, the ability is not active")))

(deftest hush-vs-tyrant
  ;;  tyrant *
  (advancable-while-hushed-test? "Tyrant" false)
  (do-game
    (install-hush-and-run "Tyrant" {:rig ["Simulchip"]
                                    :counters {:advancement 5}
                                    :players {:runner {:discard ["Fermenter"]}}
                                    :hushed true})
    (let [ice (get-ice state :hq 0)
          sim (get-hardware state 0)]
      (is (= 0 (count (:subroutines ice))) "0 subroutine because we're hushed")
      (card-ability state :runner sim 0)
      (click-card state :runner "Hush")
      (click-card state :runner "Fermenter")
      (is (= 5 (count (:subroutines (refresh ice)))) "5 subs on tyrant now"))))

(deftest hush-vs-woodcutter
  ;; woodcutter
  (advancable-while-hushed-test? "Woodcutter" false)
  (do-game
    (install-hush-and-run "Woodcutter" {:rig ["Simulchip"]
                                        :counters {:advancement 5}
                                        :players {:runner {:discard ["Fermenter"]}}
                                        :hushed true})
    (let [ice (get-ice state :hq 0)
          sim (get-hardware state 0)]
      (is (= 0 (count (:subroutines ice))) "0 subroutine because we're hushed")
      (card-ability state :runner sim 0)
      (click-card state :runner "Hush")
      (click-card state :runner "Fermenter")
      (is (= 5 (count (:subroutines (refresh ice)))) "5 subs on woodcutter now"))))

(deftest hush-vs-wraparound
  ;;  wraparound
  (do-game
    (install-hush-and-run "Wraparound" {:hushed true})
    (is (= 0 (get-strength (get-ice state :hq 0))) "Hushed wrap is 0 str")))


(deftest hyperdriver
  ;; Hyperdriver - Remove from game to gain 3 clicks
  (do-game
      (new-game {:runner {:deck ["Hyperdriver"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Hyperdriver")
      (is (= 1 (core/available-mu state)) "3 MU used")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (:runner-phase-12 @state) "Runner in Step 1.2")
      (let [hyp (get-program state 0)]
        (card-ability state :runner hyp 0)
        (end-phase-12 state :runner)
        (is (= 7 (:click (get-runner))) "Gained 3 clicks")
        (is (= 1 (count (:rfg (get-runner)))) "Hyperdriver removed from game"))))

(deftest hyperdriver-triggering-a-dhegdeered-hyperdriver-should-not-grant-3-mu
    ;; triggering a Dhegdeered Hyperdriver should not grant +3 MU
    (do-game
      (new-game {:runner {:deck ["Hyperdriver" "Dhegdheer"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Dhegdheer")
      (let [dheg (get-program state 0)]
        (card-ability state :runner dheg 0)
        (click-card state :runner (find-card "Hyperdriver" (:hand (get-runner))))
        (is (= 4 (core/available-mu state)) "0 MU used by Hyperdriver hosted on Dhegdheer")
        (is (= 2 (:click (get-runner))) "2 clicks used")
        (is (= 3 (:credit (get-runner))) "2 credits used")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (:runner-phase-12 @state) "Runner in Step 1.2")
        (let [hyp (first (:hosted (refresh dheg)))]
          (card-ability state :runner hyp 0)
          (end-phase-12 state :runner)
          (is (= 7 (:click (get-runner))) "Used Hyperdriver")
          (is (= 4 (core/available-mu state)) "Still 0 MU used after Hyperdriver removed from game")))))

(deftest ika-can-be-hosted-on-both-rezzed-unrezzed-ice-respects-no-host-is-blanked-by-magnet
    ;; Can be hosted on both rezzed/unrezzed ice, respects no-host, is blanked by Magnet
    (do-game
      (new-game {:corp {:deck ["Tithonium" "Enigma" "Magnet"]}
                 :runner {:deck ["Ika"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Tithonium" "Archives")
      (play-from-hand state :corp "Magnet" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Ika")
      (core/gain state :runner :credit 100)
      (core/gain state :corp :credit 100)
      (let [ika (get-program state 0)
            enigma (get-ice state :hq 0)
            tithonium (get-ice state :archives 0)
            magnet (get-ice state :rd 0)]
        (let [creds (:credit (get-runner))]
          (card-ability state :runner ika 0) ; host on a piece of ice
          (click-card state :runner tithonium)
          (is (utils/same-card? ika (first (:hosted (refresh tithonium)))) "Ika was rehosted")
          (is (= (- creds 2) (:credit (get-runner))) "Rehosting from rig cost 2 creds"))
        (run-on state :archives)
        (run-continue state)
        (let [creds (:credit (get-runner))
              ika (first (:hosted (refresh tithonium)))]
          (card-ability state :runner ika 0)
          (click-card state :runner enigma)
          (is (utils/same-card? ika (first (:hosted (refresh enigma)))) "Ika was rehosted")
          (is (= (- creds 2) (:credit (get-runner))) "Rehosting from ice during run cost 2 creds"))
        (rez state :corp tithonium)
        (let [creds (:credit (get-runner))
              ika (first (:hosted (refresh enigma)))]
          (card-ability state :runner ika 0)
          (click-card state :runner tithonium)
          (is (zero?(count (:hosted (refresh tithonium)))) "Ika was not hosted on Tithonium")
          (is (= creds (:credit (get-runner))) "Clicking invalid targets is free")
          (click-prompt state :runner "Done")
          (rez state :corp magnet)
          (click-card state :corp ika)
          (is (zero?(count (:hosted (refresh enigma)))) "Ika was removed from Enigma")
          (is (not (:playable (first (:abilities (refresh ika))))) "Ika abilities are not playable")
          (is (not (:playable (second (:abilities (refresh ika))))) "Ika abilities are not playable")
          (is (= 1 (count (:hosted (refresh magnet)))) "Ika was hosted onto Magnet")))))

(deftest imp-full-test
    ;; Full test
    (letfn [(imp-test [card]
              (do-game
                (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                                  :hand [card]}
                           :runner {:deck ["Imp"]}})
                (take-credits state :corp)
                (play-from-hand state :runner "Imp")
                (run-empty-server state "HQ")
                (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
                (is (= 1 (count (:discard (get-corp)))))))]
      (doall (map imp-test
                  ["Hostile Takeover"
                   "Dedicated Response Team"
                   "Beanstalk Royalties"
                   "Ice Wall"
                   "Oberth Protocol"]))))

(deftest imp-cannot-trash-flag-interaction
  ;; Interaction with :can-trash flag
  (do-game
    (new-game {:corp {:hand ["Project Atlas"]}
               :runner {:hand ["Imp" "Pinhole Threading"]}})
    (play-from-hand state :corp "Project Atlas" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Imp")
    (play-from-hand state :runner "Pinhole Threading")
    (click-prompt state :runner "Archives")
    (run-continue state)
    (click-card state :runner "Project Atlas")
    (is (= ["No action"] (prompt-buttons :runner)))
    (click-prompt state :runner "No action")))

(deftest imp-vs-an-ambush
    ;; vs an ambush
    (do-game
      (new-game {:corp {:deck ["Prisec"]}
                 :runner {:deck ["Imp" (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Prisec" "New remote")
      (take-credits state :corp)
      (let [credits (:credit (get-corp))
            tags (count-tags state)
            grip (count (:hand (get-runner)))
            archives (count (:discard (get-corp)))]
        (play-from-hand state :runner "Imp")
        (run-empty-server state :remote1)
        (click-prompt state :corp "Yes")
        (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
        (is (= 2 (- credits (:credit (get-corp)))) "Corp paid 2 for Prisec")
        (is (= 1 (- (count-tags state) tags)) "Runner has 1 tag")
        (is (= 2 (- grip (count (:hand (get-runner))))) "Runner took 1 meat damage")
        (is (= 1 (- (count (:discard (get-corp))) archives)) "Used Imp to trash Prisec"))))

(deftest imp-vs-the-future-perfect
    ;; Psi-game happens on access [5.5.1], Imp is a trash ability [5.5.2]
    ;; vs The Future Perfect
    (do-game
      (new-game {:corp {:deck ["The Future Perfect"]}
                 :runner {:deck ["Imp"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (testing "Access, corp wins psi-game"
        (run-empty-server state "HQ")
        ;; Should access TFP at this point
        (click-prompt state :corp "1 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
        (take-credits state :runner)
        (is (= "The Future Perfect" (get-in @state [:corp :discard 0 :title])) "TFP trashed")
        (is (zero? (:agenda-point (get-runner))) "Runner did not steal TFP")
        (core/move state :corp (find-card "The Future Perfect" (:discard (get-corp))) :hand))
      (take-credits state :runner)
      (take-credits state :corp)
      (testing "Access, runner wins psi-game"
        (run-empty-server state "HQ")
        ;; Access prompt for TFP
        (click-prompt state :corp "0 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        ;; Fail psi game
        (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
        (is (= "The Future Perfect" (get-in @state [:corp :discard 0 :title])) "TFP trashed")
        (is (zero? (:agenda-point (get-runner))) "Runner did not steal TFP"))))

(deftest imp-vs-cards-in-archives
    ;; vs cards in Archives
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"]}
                 :runner {:deck ["Imp"]}})
      (core/move state :corp (find-card "Hostile Takeover" (:hand (get-corp))) :discard)
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (run-empty-server state "Archives")
      (is (= ["Steal"] (prompt-buttons :runner)) "Should only get the option to steal Hostile on access in Archives")))

(deftest imp-hivemind-installed-5000
    ;; Hivemind installed #5000
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:deck ["Imp" "Hivemind"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Hivemind")
      (is (= 1 (get-counters (get-program state 0) :virus)))
      (play-from-hand state :runner "Imp")
      (core/add-counter state :runner (get-program state 1) :virus -2)
      (is (= 0 (get-counters (get-program state 1) :virus)))
      (run-empty-server state "HQ")
      (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
      (click-card state :runner "Hivemind")
      (is (= 1 (count (:discard (get-corp)))))
      (is (= 0 (get-counters (get-program state 0) :virus)))))

(deftest imp-can-t-be-used-when-empty-5190
    ;; can't be used when empty #5190
    (do-game
      (new-game {:corp {:hand ["Hostile Takeover"]}
                 :runner {:hand ["Imp" "Cache"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Imp")
      (core/update! state :runner (assoc-in (get-program state 1) [:counter :virus] 0))
      (run-empty-server state "HQ")
      (is (= ["Steal"] (prompt-buttons :runner)) "Should only get the option to steal Hostile on access in Archives")))

(deftest incubator
  ;; Incubator - Gain 1 virus counter per turn; trash to move them to an installed virus program
  (do-game
    (new-game {:runner {:deck ["Incubator" "Datasucker"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Datasucker")
    (play-from-hand state :runner "Incubator")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [ds (get-program state 0)
          incub (get-program state 1)]
      (is (= 1 (get-counters (refresh incub) :virus)) "Incubator gained 1 virus counter")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 2 (get-counters (refresh incub) :virus)) "Incubator has 2 virus counters")
      (card-ability state :runner incub 0)
      (click-card state :runner ds)
      (is (= 2 (get-counters (refresh ds) :virus)) "Datasucker has 2 virus counters moved from Incubator")
      (is (= 1 (count (get-program state))))
      (is (= 1 (count (:discard (get-runner)))) "Incubator trashed")
      (is (= 3 (:click (get-runner)))))))

(deftest inversificator-shouldn-t-hook-up-events-for-unrezzed-ice
    ;; Shouldn't hook up events for unrezzed ice
    (do-game
      (new-game {:corp {:deck ["Turing" "Kakugo"]}
                 :runner {:deck ["Inversificator" "Sure Gamble"]}})
      (play-from-hand state :corp "Kakugo" "HQ")
      (play-from-hand state :corp "Turing" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Inversificator")
      (let [inv (get-program state 0)
            tur (get-ice state :hq 1)]
        (is (= 1 (count (:hand (get-runner)))) "Runner starts with 1 card in hand")
        (run-on state "HQ")
        (rez state :corp (refresh tur))
        (run-continue state)
        (card-ability state :runner (refresh inv) 0)
        (click-prompt state :runner "End the run unless the Runner pays [Click][Click][Click]")
        (run-continue state)
        (click-prompt state :runner "Yes")
        (click-card state :runner (get-ice state :hq 1))
        (click-card state :runner (get-ice state :hq 0))
        (run-jack-out state)
        (is (= 1 (count (:hand (get-runner)))) "Runner still has 1 card in hand")
        (run-on state :hq)
        (run-continue state)
        (is (= 1 (count (:hand (get-runner)))) "Kakugo doesn't fire when unrezzed"))))

(deftest inversificator-switched-ice-resets-broken-subs-issue-4857
    ;; Switched ice resets broken subs. Issue #4857
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Viktor 1.0" 2)]
                        :credits 20}
                 :runner {:hand ["Inversificator"]
                          :credits 20}})
      (play-from-hand state :corp "Viktor 1.0" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (play-from-hand state :corp "Viktor 1.0" "New remote")
      (rez state :corp (get-ice state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Inversificator")
      (run-on state "HQ")
      (run-continue state)
      (let [inv (get-program state 0)]
        (card-ability state :runner (refresh inv) 1)
        (card-ability state :runner (refresh inv) 0)
        (click-prompt state :runner "Do 1 core damage")
        (click-prompt state :runner "End the run")
        (run-continue state)
        (click-prompt state :runner "Yes")
        (click-card state :runner (get-ice state :hq 0))
        (click-card state :runner (get-ice state :remote1 0))
        (run-continue state)
        (is (not-any? :broken (:subroutines (get-ice state :remote1 0)))
            "None of the subs are marked as broken anymore"))))

(deftest inversificator-doesn-t-fire-when-other-programs-break-an-ice-issue-4858
    ;; Doesn't fire when other programs break an ice. Issue #4858
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Viktor 1.0" 2)]
                        :credits 20}
                 :runner {:hand ["Inversificator" "Maven" "Cache"]
                          :credits 20}})
      (play-from-hand state :corp "Viktor 1.0" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (play-from-hand state :corp "Viktor 1.0" "New remote")
      (rez state :corp (get-ice state :remote1 0))
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Maven")
      (play-from-hand state :runner "Inversificator")
      ;; Use Inversificator in another run first
      (run-on state "Server 1")
      (run-continue state)
      (let [maven (get-program state 1)
            inv (get-program state 2)]
        (card-ability state :runner (refresh inv) 1)
        (card-ability state :runner (refresh inv) 0)
        (click-prompt state :runner "Do 1 core damage")
        (click-prompt state :runner "End the run")
        (run-continue state)
        (click-prompt state :runner "No")
        (run-continue state)
        ;; Use non-Inversificator breaker
        (run-on state "HQ")
        (run-continue state)
        (card-ability state :runner (refresh maven) 0)
        (click-prompt state :runner "Do 1 core damage")
        (click-prompt state :runner "End the run")
        (run-continue state)
        (is (not (prompt-is-card? state :runner inv)) "Prompt shouldn't be Inversificator")
        (is (no-prompt? state :corp) "Corp shouldn't have a prompt")
        (is (no-prompt? state :runner) "Runner shouldn't have a prompt"))))

(deftest inversificator-inversificator-shouldn-t-fire-when-ice-is-unrezzed-issue-4859
    ;; Inversificator shouldn't fire when ice is unrezzed. Issue #4859
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Viktor 1.0" "Quandary"]
                        :credits 20}
                 :runner {:hand ["Inversificator"]
                          :credits 20}})
      (play-from-hand state :corp "Quandary" "HQ")
      (play-from-hand state :corp "Viktor 1.0" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Inversificator")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 1))
      (run-continue state)
      (let [inv (get-program state 0)]
        (card-ability state :runner (refresh inv) 1)
        (card-ability state :runner (refresh inv) 0)
        (click-prompt state :runner "Do 1 core damage")
        (click-prompt state :runner "End the run")
        (run-continue state)
        (click-prompt state :runner "No")
        (run-continue state)
        (run-continue state)
        (is (not (prompt-is-card? state :runner inv)) "Prompt shouldn't be Inversificator")
        (is (no-prompt? state :corp) "Corp shouldn't have a prompt")
        (is (no-prompt? state :runner) "Runner shouldn't have a prompt"))))

(deftest inversificator-should-fire-ice-s-on-pass-ability
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Kakugo" "Quandary"]
                        :credits 20}
                 :runner {:id "Rielle \"Kit\" Peddler: Transhuman"
                          :hand ["Sure Gamble" "Inversificator"]
                          :credits 20}})
      (play-from-hand state :corp "Quandary" "HQ")
      (play-from-hand state :corp "Kakugo" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Inversificator")
      (is (changed? [(count (:hand (get-runner))) -1]
            (run-on state "HQ")
            (rez state :corp (get-ice state :hq 1))
            (run-continue state :encounter-ice)
            (let [inv (get-program state 0)]
              (card-ability state :runner (refresh inv) 1)
              (card-ability state :runner (refresh inv) 0)
              (click-prompt state :runner "End the run")
              (run-continue state)
              (click-prompt state :runner "Yes")
              (click-card state :runner (get-ice state :hq 1))
              (click-card state :runner (get-ice state :hq 0))))
          "Kakugo should still do 1 net damage even though it was swapped")))

(deftest inversificator-async-issue-with-thimblerig-5042
    ;; Async issue with Thimblerig #5042
    (do-game
      (new-game {:corp {:hand ["Drafter" "Border Control" "Vanilla" "Thimblerig"]
                        :credits 100}
                 :runner {:hand ["Inversificator"]
                          :credits 100}})
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Border Control" "R&D")
      (play-from-hand state :corp "Drafter" "R&D")
      (play-from-hand state :corp "Vanilla" "HQ")
      (play-from-hand state :corp "Thimblerig" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Inversificator")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 1))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-card state :runner "Drafter")
      (is (= ["Border Control" "Thimblerig"] (map :title (get-ice state :rd))))
      (is (= ["Vanilla" "Drafter"] (map :title (get-ice state :hq))))
      (is (no-prompt? state :corp) "Corp gets no Thimblerig prompt")
      (is (no-prompt? state :runner) "No more prompts open")))

(deftest inversificator-swap-vs-subtype-issues-5170
    ;; Swap vs subtype issues #5170
    (do-game
      (new-game {:corp {:hand ["Drafter" "Data Raven" "Vanilla"]
                        :credits 100}
                 :runner {:id "Rielle \"Kit\" Peddler: Transhuman"
                          :hand ["Inversificator" "Hunting Grounds" "Stargate"]
                          :credits 100}})
      (play-from-hand state :corp "Data Raven" "R&D")
      (play-from-hand state :corp "Drafter" "R&D")
      (play-from-hand state :corp "Vanilla" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Inversificator")
      (play-from-hand state :runner "Hunting Grounds")
      (play-from-hand state :runner "Stargate")
      (core/gain state :runner :click 10)
      (let [inv (get-program state 0)
            hg (get-resource state 0)
            sg (get-program state 1)]
        (card-ability state :runner sg 0)
        (run-continue-until state :approach-ice)
        (rez state :corp (get-ice state :rd 0))
        (card-ability state :runner hg 0)
        (run-continue state)
        (card-ability state :runner inv 1)
        (card-ability state :runner inv 1)
        (card-ability state :runner inv 0)
        (click-prompt state :runner "Trace 3 - Place 1 power counter")
        (run-continue state)
        (click-prompt state :runner "Yes")
        (click-card state :runner "Vanilla")
        (is (= ["Vanilla" "Drafter"] (map :title (get-ice state :rd))))
        (is (= ["Data Raven"] (map :title (get-ice state :archives)))))))

(deftest ixodidae
  ;; Ixodidae should not trigger on psi-games
  (do-game
    (new-game {:corp {:deck ["Snowflake"]}
               :runner {:deck ["Ixodidae" "Lamprey"]}})
    (play-from-hand state :corp "Snowflake" "HQ")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))) "Corp at 7 credits")
    (play-from-hand state :runner "Ixodidae")
    (play-from-hand state :runner "Lamprey")
    (is (= 3 (:credit (get-runner))) "Runner paid 3 credits to install Ixodidae and Lamprey")
    (run-on state :hq)
    (let [s (get-ice state :hq 0)]
      (rez state :corp s)
      (run-continue state)
      (card-subroutine state :corp s 0)
      (is (prompt-is-card? state :corp s) "Corp prompt is on Snowflake")
      (is (prompt-is-card? state :runner s) "Runner prompt is on Snowflake")
      (is (= 6 (:credit (get-corp))) "Corp paid 1 credit to rezz Snowflake")
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (= 5 (:credit (get-corp))) "Corp paid 1 credit to psi game")
      (is (= 2 (:credit (get-runner))) "Runner did not gain 1 credit from Ixodidae when corp spent on psi game")
      (run-continue state)
      (run-continue state)
      (is (= 4 (:credit (get-corp))) "Corp lost 1 credit to Lamprey")
      (is (= 3 (:credit (get-runner))) "Runner gains 1 credit from Ixodidae due to Lamprey"))))

(deftest k2cp-turbine
  ;; K2CP Turbine
  (do-game
    (new-game {:runner {:hand ["K2CP Turbine" "Ika" "Mayfly" "Corroder" "Buzzsaw" "Keiko"]
                        :credits 50}})
    (take-credits state :corp)
    (core/gain state :runner :click 2)
    (play-from-hand state :runner "Keiko")
    (play-from-hand state :runner "Buzzsaw")
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Ika")
    (play-from-hand state :runner "Mayfly")
    (play-from-hand state :runner "K2CP Turbine")
    (is (= 5 (get-strength (refresh (get-program state 0)))))
    (is (= 4 (get-strength (refresh (get-program state 1)))))
    (is (= 4 (get-strength (refresh (get-program state 2)))))
    (is (= 1 (get-strength (refresh (get-program state 3)))))
    (core/move state :runner (get-program state 4) :hand)
    (is (= 3 (get-strength (refresh (get-program state 0)))))
    (is (= 2 (get-strength (refresh (get-program state 1)))))
    (is (= 2 (get-strength (refresh (get-program state 2)))))))

(deftest keyhole
  ;; Keyhole
  (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 10)]
                        :hand ["Herald" "Troll"]}
                 :runner {:hand ["Keyhole"]}})
      (core/move state :corp (find-card "Herald" (:hand (get-corp))) :deck {:front true})
      (core/move state :corp (find-card "Troll" (:hand (get-corp))) :deck {:front true})
      (is (= "Troll" (-> (get-corp) :deck first :title)) "Troll on top of deck")
      (is (= "Herald" (-> (get-corp) :deck second :title)) "Herald 2nd")
      (take-credits state :corp)
      (play-from-hand state :runner "Keyhole")
      (card-ability state :runner (get-program state 0) 0)
      (is (:run @state) "Run initiated")
      (run-continue state)
      (let [number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))]
        (click-prompt state :runner "Troll")
        (is (no-prompt? state :runner) "Prompt closed")
        (is (not (:run @state)) "Run ended")
        (is (-> (get-corp) :discard first :seen) "Troll is faceup")
        (is (= "Troll" (-> (get-corp) :discard first :title)) "Troll was trashed")
        (is (find-card "Herald" (:deck (get-corp))) "Herald now in R&D")
        (is (< number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck)))
            "Corp has shuffled R&D"))
      (card-ability state :runner (get-program state 0) 0)
      (is (:run @state) "Keyhole can be used multiple times per turn")))

(deftest kyuban-gain-creds-when-passing-a-piece-of-ice-both-when-rezzed-and-when-unrezzed
    ;; Gain creds when passing a piece of ice, both when rezzed and when unrezzed.
    (do-game
      (new-game {:corp {:deck [(qty "Lockdown" 3)]}
                 :runner {:deck [(qty "Kyuban" 1)]}})
      (play-from-hand state :corp "Lockdown" "HQ")
      (play-from-hand state :corp "Lockdown" "Archives")
      (let [ld1 (get-ice state :archives 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Kyuban")
        (click-card state :runner ld1)
        (let [starting-creds (:credit (get-runner))]
          (run-on state "HQ")
          (run-continue state)
          (is (= starting-creds (:credit (get-runner))) "Gained no money for passing other ice")
          (run-jack-out state)
          (run-on state "Archives")
          (run-continue state)
          (is (= (+ starting-creds 2) (:credit (get-runner)))
              "Gained 2 creds for passing unrezzed host ice"))
        (let [starting-creds-2 (:credit (get-runner))]
          (core/jack-out state :runner nil)
          (run-on state "Archives")
          (rez state :corp ld1)
          (run-continue state)
          (run-continue state)
          (run-continue state)
          (is (= (+ starting-creds-2 2) (:credit (get-runner)))
              "Gained 2 creds for passing rezzed host ice")))))

(deftest kyuban-hb-architects-of-tomorrow-interaction
    ;; HB: Architects of Tomorrow interaction
    (do-game
      (new-game {:corp {:id "Haas-Bioroid: Architects of Tomorrow"
                        :deck ["Eli 1.0"]}
                 :runner {:deck ["Kyuban"]}})
      (play-from-hand state :corp "Eli 1.0" "HQ")
      (let [eli (get-ice state :hq 0)]
        (rez state :corp eli)
        (take-credits state :corp)
        (play-from-hand state :runner "Kyuban")
        (click-card state :runner eli))
      (let [starting-creds (:credit (get-runner))]
        (run-on state "HQ")
        (run-continue state)
        (run-continue state)
        (click-prompt state :corp "Done")
        (run-continue state)
        (is (= (+ starting-creds 2) (:credit (get-runner)))
            "Only gained 2 credits for passing Eli"))))

(deftest laamb-ability-gives-an-card-barrier-subtype
    ;; Ability gives an card Barrier subtype
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Enigma"]}
                 :runner {:hand ["Laamb"]
                          :credits 30}})
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Laamb")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (click-prompt state :runner "Yes")
      (is (has-subtype? (get-ice state :hq 0) "Barrier") "Enigma has been given Barrier")))

(deftest laamb-ability-only-lasts-until-end-of-encounter
    ;; Ability only lasts until end of encounter
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Enigma"]}
                 :runner {:hand ["Laamb"]
                          :credits 30}})
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Laamb")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (click-prompt state :runner "Yes")
      (run-continue state)
      (is (not (has-subtype? (get-ice state :hq 0) "Barrier")) "Enigma no longer has Barrier subtype")))

(deftest laamb-returning-the-ice-to-hand-after-using-ability-resets-subtype-issue-3193
    ;; Returning the ice to hand after using ability resets subtype. Issue #3193
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Enigma"]}
                 :runner {:hand ["Laamb" "Ankusa"]
                          :credits 30}})
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Laamb")
      (play-from-hand state :runner "Ankusa")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (click-prompt state :runner "Yes")
      (let [laamb (get-program state 0)
            ankusa (get-program state 1)]
        (card-ability state :runner ankusa 1)
        (card-ability state :runner ankusa 1)
        (card-ability state :runner ankusa 0)
        (click-prompt state :runner "Force the Runner to lose [Click]")
        (click-prompt state :runner "End the run")
        (is (nil? (get-ice state :hq 0)) "Enigma has been returned to HQ")
        (is (find-card "Enigma" (:hand (get-corp))) "Enigma has been returned to HQ")
        (core/continue state :corp nil)
        (run-jack-out state)
        (take-credits state :runner)
        (play-from-hand state :corp "Enigma" "HQ")
        (take-credits state :corp)
        (run-on state "HQ")
        (rez state :corp (get-ice state :hq 0))
        (run-continue state)
        (is (not (has-subtype? (get-ice state :hq 0) "Barrier")) "Enigma doesn't has Barrier subtype")
        (is (prompt-is-card? state :runner laamb) "Laamb opens the prompt a second time"))))

(deftest lamprey
  ;; Lamprey - Corp loses 1 credit for each successful HQ run; trashed on purge
  (do-game
    (new-game {:runner {:deck ["Lamprey"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Lamprey")
    (run-empty-server state :hq)
    (is (= 7 (:credit (get-corp))) "Corp lost 1 credit")
    (click-prompt state :runner "No action")
    (run-empty-server state :hq)
    (is (= 6 (:credit (get-corp))) "Corp lost 1 credit")
    (click-prompt state :runner "No action")
    (run-empty-server state :hq)
    (is (= 5 (:credit (get-corp))) "Corp lost 1 credit")
    (click-prompt state :runner "No action")
    (take-credits state :runner)
    (purge state :corp)
    (is (empty? (get-program state)) "Lamprey trashed by purge")))

(deftest laser-pointer
  (do-game
    (new-game {:runner {:hand [(qty "Laser Pointer" 3)]
                        :credits 10}
               :corp {:hand ["Envelope" "Starlit Knight" "Rototurret"]
                      :credits 50}})
    (play-from-hand state :corp "Envelope" "HQ")
    (play-from-hand state :corp "Starlit Knight" "HQ")
    (play-from-hand state :corp "Rototurret" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (rez state :corp (get-ice state :hq 1))
    (rez state :corp (get-ice state :hq 2))
    (take-credits state :corp)
    (play-from-hand state :runner "Laser Pointer")
    (play-from-hand state :runner "Laser Pointer")
    (play-from-hand state :runner "Laser Pointer")
    (run-on state :hq)
    (run-continue state)
    (is (changed? [(count (:discard (get-runner))) 1]
          (click-prompt state :runner "Yes"))
        "Laser Pointer trashed")
    (is (= :movement (:phase (get-run))) "Runner bypassed Rototurret")
    (run-continue state)
    (run-continue state)
    (is (changed? [(count (:discard (get-runner))) 1]
          (click-prompt state :runner "Yes"))
        "Laser Pointer trashed")
    (is (= :movement (:phase (get-run))) "Runner bypassed Starlit Knight")
    (run-continue state)
    (run-continue state)
    (is (changed? [(count (:discard (get-runner))) 1]
          (click-prompt state :runner "Yes"))
        "Laser Pointer trashed")
    (is (= :movement (:phase (get-run))) "Runner bypassed Envelope")))

(deftest leech
  ;; Leech - Reduce strength of encountered piece of ice
  (do-game
      (new-game {:corp {:deck ["Fire Wall"]}
                 :runner {:deck ["Leech"]}})
      (play-from-hand state :corp "Fire Wall" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :click 3)
      (play-from-hand state :runner "Leech")
      (let [le (get-program state 0)
            fw (get-ice state :remote1 0)]
        (run-empty-server state "Archives")
        (is (= 1 (get-counters (refresh le) :virus)))
        (run-empty-server state "Archives")
        (is (= 2 (get-counters (refresh le) :virus)))
        (run-on state "Server 1")
        (run-continue state)
        (run-continue state)
        (is (= 2 (get-counters (refresh le) :virus)) "No counter gained, not a central server")
        (run-on state "Server 1")
        (rez state :corp fw)
        (run-continue state)
        (is (= 5 (get-strength (refresh fw))))
        (card-ability state :runner le 0)
        (is (= 1 (get-counters (refresh le) :virus)) "1 counter spent from Leech")
        (is (= 4 (get-strength (refresh fw))) "Fire Wall strength lowered by 1"))))

(deftest leech-does-not-affect-next-ice-when-current-is-trashed-issue-1788
    ;; does not affect next ice when current is trashed. Issue #1788
    (do-game
      (new-game {:corp {:deck ["Wraparound" "Spiderweb"]}
                 :runner {:deck ["Leech" "Parasite"]}})
      (play-from-hand state :corp "Wraparound" "HQ")
      (play-from-hand state :corp "Spiderweb" "HQ")
      (take-credits state :corp)
      (core/gain state :corp :credit 10)
      (play-from-hand state :runner "Leech")
      (let [leech (get-program state 0)
            wrap (get-ice state :hq 0)
            spider (get-ice state :hq 1)]
        (core/add-counter state :runner leech :virus 2)
        (rez state :corp spider)
        (rez state :corp wrap)
        (play-from-hand state :runner "Parasite")
        (click-card state :runner "Spiderweb")
        (run-on state "HQ")
        (run-continue state)
        (card-ability state :runner (refresh leech) 0)
        (card-ability state :runner (refresh leech) 0)
        (is (find-card "Spiderweb" (:discard (get-corp))) "Spiderweb trashed by Parasite + Leech")
        (is (= 7 (get-strength (refresh wrap))) "Wraparound not reduced by Leech"))))

(deftest leech-works-during-access-encounter
    ;; Works during access encounter
    (do-game
      (new-game {:corp {:hand ["Chrysalis"]}
                 :runner {:hand ["Leech" "Bukhgalter"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Leech")
      (play-from-hand state :runner "Bukhgalter")
      (run-empty-server state "HQ")
      (let [leech (get-program state 0)
            bukh (get-program state 1)
            chry (core/get-current-ice state)]
        (is chry "Encountering Chrysalis")
        (is (= 2 (get-strength (refresh chry))) "Chrysalis at base 2 strength")
        (card-ability state :runner (refresh leech) 0)
        (is (= 1 (get-strength (refresh chry))) "Chrysalis reduced to 1 strength")
        (card-ability state :runner (refresh bukh) 0)
        (click-prompt state :runner "Do 2 net damage")
        (encounter-continue state)
        (is (= 2 (get-strength (refresh chry))) "Chrysalis's strength reset after encounter")
        (click-prompt state :runner "No action"))))

(deftest leprechaun
  ;; Leprechaun - hosting a breaker with strength based on unused MU should calculate correctly
  (do-game
      (new-game {:runner {:deck ["Adept" "Leprechaun"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Leprechaun")
      (play-from-hand state :runner "Adept")
      (is (= 1 (core/available-mu state)) "3 MU used")
      (let [lep (get-program state 0)
            adpt (get-program state 1)]
        (is (= 3 (get-strength (refresh adpt))) "Adept at 3 strength individually")
        (card-ability state :runner lep 1)
        (click-card state :runner (refresh adpt))
        (let [hosted-adpt (first (:hosted (refresh lep)))]
          (is (= 3 (core/available-mu state)) "1 MU used")
          (is (= 5 (get-strength (refresh hosted-adpt))) "Adept at 5 strength hosted")))))

(deftest leprechaun-keep-mu-the-same-when-hosting-or-trashing-hosted-programs
    ;; Keep MU the same when hosting or trashing hosted programs
    (do-game
      (new-game {:runner {:deck ["Leprechaun" "Hyperdriver" "Imp"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Leprechaun")
      (let [lep (get-program state 0)]
        (card-ability state :runner lep 0)
        (click-card state :runner (find-card "Hyperdriver" (:hand (get-runner))))
        (is (= 2 (:click (get-runner))))
        (is (= 2 (:credit (get-runner))))
        (is (= 3 (core/available-mu state)) "Hyperdriver 3 MU not deducted from available MU")
        (card-ability state :runner lep 0)
        (click-card state :runner (find-card "Imp" (:hand (get-runner))))
        (is (= 1 (:click (get-runner))))
        (is (zero? (:credit (get-runner))))
        (is (= 3 (core/available-mu state)) "Imp 1 MU not deducted from available MU")
        ;; Trash Hyperdriver
        (core/move state :runner (find-card "Hyperdriver" (:hosted (refresh lep))) :discard)
        (is (= 3 (core/available-mu state)) "Hyperdriver 3 MU not added to available MU")
        (core/move state :runner (find-card "Imp" (:hosted (refresh lep))) :discard) ; trash Imp
        (is (= 3 (core/available-mu state)) "Imp 1 MU not added to available MU"))))

(deftest living-mural
  (do-game
    (new-game {:corp {:hand ["Anansi" (qty "Tithe" 2)]
               :credits 50}
               :runner {:hand ["Living Mural"]
                        :credits 50}})
    (play-from-hand state :corp "Anansi" "HQ")
    (play-from-hand state :corp "Tithe" "HQ")
    (play-from-hand state :corp "Tithe" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Living Mural")
    (click-card state :runner (get-ice state :hq 1))
    (let [an (get-ice state :hq 0)
          tithe (get-ice state :hq 1)
          lm (first (:hosted (refresh tithe)))]
      (run-on state "HQ")
      (rez state :corp tithe)
      (run-continue state)
      (auto-pump-and-break state (refresh lm))
      (core/continue state :corp nil)
      (run-continue state)
      (rez state :corp an)
      (run-continue-until state :encounter-ice)
      (auto-pump-and-break state (refresh lm))
      (run-continue-until state :success)
      (run-on state "R&D")
      (rez state :corp (get-ice state :rd 0))
      (run-continue state)
      (card-ability state :runner (refresh lm) 0)
      (is (no-prompt? state :runner) "Can't break subs on a different server"))))

(deftest living-mural-threat-ability
  (do-game
    (new-game {:corp {:hand ["Anansi" (qty "Project Atlas" 2)]
               :credits 10}
               :runner {:hand ["Living Mural"]}})
    (play-and-score state "Project Atlas")
    (play-and-score state "Project Atlas")
    (play-from-hand state :corp "Anansi" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Living Mural")
    (click-card state :runner (get-ice state :hq 0))
    (let [an (get-ice state :hq 0)
          lm (first (:hosted (refresh an)))]
      (is (= 4 (get-strength lm))))))

(deftest lobisomem
  (do-game
    (new-game {:corp {:hand ["Enigma" "Vanilla"]}
               :runner {:hand ["Lobisomem"]
                        :credits 20}})
    (play-from-hand state :corp "Enigma" "HQ")
    (play-from-hand state :corp "Vanilla" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Lobisomem")
    (let [lob (get-program state 0)]
      (is (= 1 (get-counters (refresh lob) :power)))
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (changed? [(get-counters (refresh lob) :power) 0]
                    (card-ability state :runner lob 0)
                    (click-prompt state :runner "End the run")
                    (click-prompt state :runner "Done"))
          "No power counter gained for non-fully breaking a code gate")
      (is (changed? [(get-counters (refresh lob) :power) 1]
                    (core/play-dynamic-ability state :runner {:dynamic "auto-pump-and-break" :card (refresh lob)}))
          "1 power counter gained for fully breaking a code gate")
      (core/continue state :corp nil)
      (run-jack-out state)
      (run-on state "Archives")
      (rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -1
                     (get-counters (refresh lob) :power) -1]
                    (card-ability state :runner lob 1)
                    (click-prompt state :runner "1")
                    (click-prompt state :runner "End the run"))
          "Runner spent 1 credit and 1 hosted power counter to fully break Vanilla"))))

(deftest lustig
  ;; Lustig
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Rototurret"]}
               :runner {:hand ["Lustig"]
                        :credits 10}})
    (play-from-hand state :corp "Rototurret" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Lustig")
    (run-on state "HQ")
    (rez state :corp (get-ice state :hq 0))
    (run-continue state)
    (card-ability state :runner (get-program state 0) 2)
    (is (= :movement (:phase (get-run))) "Run has bypassed Rototurret")
    (is (find-card "Lustig" (:discard (get-runner))) "Lustig is trashed")))

(deftest magnum-opus
  ;; Magnum Opus - Gain 2 cr
  (do-game
    (new-game {:runner {:deck ["Magnum Opus"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Magnum Opus")
    (is (= 2 (core/available-mu state)))
    (is (zero? (:credit (get-runner))))
    (let [mopus (get-program state 0)]
      (card-ability state :runner mopus 0)
      (is (= 2 (:credit (get-runner))) "Gain 2cr"))))

(deftest malandragem
  (do-game
    (new-game {:runner {:hand ["Malandragem"]}
               :corp {:hand ["Magnet" "Lotus Field"]
                      :credits 10}})
    (play-from-hand state :corp "Magnet" "HQ")
    (play-from-hand state :corp "Lotus Field" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Malandragem")
    (is (= 2 (get-counters (refresh (get-program state 0)) :power)) "Malandragem has 2 power counters")
    (run-on state "Archives")
    (rez state :corp (get-ice state :archives 0))
    (run-continue state)
    ;; No Malandragem prompt because ice strength is > 3
    (is (no-prompt? state :runner))
    (fire-subs state (refresh (get-ice state :archives 0)))
    (run-on state "HQ")
    (rez state :corp (get-ice state :hq 0))
    (run-continue state)
    (is (changed? [(get-counters (refresh (get-program state 0)) :power) -1]
                  (click-prompt state :runner "Yes"))
        "Spent 1 power counter on Malandragem")
    (is (= :movement (:phase (get-run))) "Run has bypassed Magnet")
    (run-jack-out state)
    (run-on state "HQ")
    (run-continue state)
    ;; No Malandragem prompt because it's once per turn
    (is (no-prompt? state :runner))))

(deftest malandragem-rfg-when-empty
  (do-game
    (new-game {:runner {:hand ["Malandragem"]}
               :corp {:hand ["Magnet"]}})
    (play-from-hand state :corp "Magnet" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Malandragem")
    (run-on state "HQ")
    (rez state :corp (get-ice state :hq 0))
    (run-continue state)
    (click-prompt state :runner "Yes")
    (is (= :movement (:phase (get-run))) "Run has bypassed Magnet")
    (run-jack-out state)
    (take-credits state :runner)
    (take-credits state :corp)
    (run-on state "HQ")
    (run-continue state)
    (is (changed? [(count (:rfg (get-runner))) 1]
                  (click-prompt state :runner "Yes"))
        "Malandragem was rfg-ed")
    (is (= :movement (:phase (get-run))) "Run has bypassed Magnet")))

(deftest malandragem-threat
  (do-game
    (new-game {:runner {:hand ["Malandragem"]}
               :corp {:hand ["Lotus Field" "Vanity Project"]}})
    (play-and-score state "Vanity Project")
    (play-from-hand state :corp "Lotus Field" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Malandragem")
    (run-on state "Archives")
    (rez state :corp (get-ice state :archives 0))
    (run-continue state)
    (is (changed? [(count (:rfg (get-runner))) 1]
                  (click-prompt state :runner "Yes"))
        "RFG Malandragem")
    (is (= :movement (:phase (get-run))) "Run has bypassed Lotus Field")))

(deftest makler-break-ability-costs-2-for-2-subroutines
    ;; Break ability costs 2 for 2 subroutines
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Battlement"]}
                 :runner {:hand ["Makler"]
                          :credits 20}})
      (play-from-hand state :corp "Battlement" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Makler")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -2]
            (card-ability state :runner (get-program state 0) 0)
            (click-prompt state :runner "End the run")
            (click-prompt state :runner "End the run"))
          "Break ability costs 2 credits")))

(deftest makler-boost-ability-costs-2-credits-increases-strength-by-2
    ;; Boost ability costs 2 credits, increases strength by 2
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Bastion"]}
                 :runner {:hand ["Makler"]
                          :credits 20}})
      (play-from-hand state :corp "Bastion" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Makler")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -2]
            (card-ability state :runner (get-program state 0) 1))
          "Boost ability costs 2 credits")
      (is (changed? [(get-strength (get-program state 0)) 2]
            (card-ability state :runner (get-program state 0) 1))
          "Boost ability increases strength by 2")))

(deftest makler-break-all-subs-ability-gives-1-credit
    ;; Break all subs ability gives 1 credit
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Battlement" 2)]}
                 :runner {:hand ["Makler"]
                          :credits 20}})
      (play-from-hand state :corp "Battlement" "HQ")
      (play-from-hand state :corp "Battlement" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Makler")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "End the run")
      (is (changed? [(:credit (get-runner)) 1]
            (run-continue state))
          "Break all subs ability gives 1 credit")))

(deftest mammon
  ;; Mammon - Pay to add X power counters at start of turn, all removed at end of turn
  (do-game
    (new-game {:runner {:deck ["Mammon"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Mammon")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [mam (get-program state 0)]
      (is (= 5 (:credit (get-runner))) "Starts with 5 credits")
      (card-ability state :runner mam 0)
      (click-prompt state :runner "3")
      (is (= 2 (:credit (get-runner))) "Spent 3 credits")
      (is (= 3 (get-counters (refresh mam) :power)) "Mammon has 3 power counters")
      (take-credits state :runner)
      (is (zero? (get-counters (refresh mam) :power)) "All power counters removed"))))

(deftest mantle-works-with-non-icebreaker-programs
    ;; Works with programs
    (do-game
      ;; Using Tracker to demonstrate that it's not just icebreakers
      (new-game {:runner {:hand ["Mantle" "Tracker"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Mantle")
      (play-from-hand state :runner "Tracker")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "HQ")
      (let [mantle (get-program state 0)
            tracker (get-program state 1)]
        (card-ability state :runner tracker 0)
        (is (changed? [(get-counters (refresh mantle) :recurring) -1]
              (click-card state :runner mantle))
            "Can spend credits on Mantle for programs"))))

(deftest marjanah
  ;; Marjanah
  (before-each [state (new-game {:runner {:hand [(qty "Marjanah" 2)]
                                          :credits 20}
                                 :corp {:deck [(qty "Hedge Fund" 5)]
                                        :hand ["Ice Wall"]
                                        :credits 20}})
                _ (do (play-from-hand state :corp "Ice Wall" "HQ")
                      (take-credits state :corp)
                      (play-from-hand state :runner "Marjanah")
                      (run-on state :hq)
                      (rez state :corp (get-ice state :hq 0))
                      (run-continue state :encounter-ice))
                marjanah (get-program state 0)]
    (testing "pump ability"
      (do-game state
        (is (changed? [(:credit (get-runner)) -1]
              (card-ability state :runner marjanah 1))
            "Pump costs 1")
        (is (changed? [(get-strength (refresh marjanah)) 1]
              (card-ability state :runner marjanah 1))
            "Marjanah gains 1 str")))
    (testing "break ability"
      (do-game state
        (is (changed? [(:credit (get-runner)) -2]
              (card-ability state :runner marjanah 0)
              (is (= "2 [Credits]" (get-in (refresh marjanah) [:abilities 0 :cost-label])) "Break label lists cost as 2 credits")
              (click-prompt state :runner "End the run"))
            "Break costs 2")))
    (testing "discount after successful run"
      (do-game state
        (run-continue state :movement)
        (run-continue state nil)
        (run-on state :hq)
        (run-continue state :encounter-ice)
        (is (changed? [(:credit (get-runner)) -1]
              (card-ability state :runner marjanah 0)
              (is (= "1 [Credits]" (get-in (refresh marjanah) [:abilities 0 :cost-label])) "Break label lists cost as 1 credit")
              (click-prompt state :runner "End the run"))
            "Break costs 1 after run")))))

(deftest mass-driver
  ;; Mass-Driver
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
        (fire-subs state (refresh eula))
        ; only resolve 3 subs
        (click-prompt state :runner "Pay 1 [Credits]")
        (click-prompt state :runner "Pay 1 [Credits]")
        (click-prompt state :runner "Pay 1 [Credits]")
        (is (no-prompt? state :runner) "No more prompts open"))))

(deftest mass-driver-interaction-with-spooned
    ;; Interaction with Spooned
    (do-game
      (new-game {:corp {:deck ["Enigma" "Endless EULA"]}
                 :runner {:deck ["Mass-Driver" "Spooned"]}})
      (play-from-hand state :corp "Endless EULA" "HQ")
      (play-from-hand state :corp "Enigma" "HQ")
      (core/gain state :corp :credit 20)
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Mass-Driver")
      (let [eula (get-ice state :hq 0)
            enigma (get-ice state :hq 1)
            mass-driver (get-program state 0)]
        (play-from-hand state :runner "Spooned")
        (click-prompt state :runner "HQ")
        (rez state :corp enigma)
        (run-continue state)
        (card-ability state :runner mass-driver 1)
        (card-ability state :runner mass-driver 0)
        (click-prompt state :runner "Force the Runner to lose [Click]")
        (click-prompt state :runner "End the run")
        (run-continue-until state :approach-ice eula)
        (is (= 1 (count (:discard (get-corp)))) "Enigma is trashed")
        (rez state :corp eula)
        (run-continue state)
        (fire-subs state (refresh eula))
        ; only resolve 3 subs
        (click-prompt state :runner "Pay 1 [Credits]")
        (click-prompt state :runner "Pay 1 [Credits]")
        (click-prompt state :runner "Pay 1 [Credits]")
        (is (no-prompt? state :runner) "No more prompts open"))))

(deftest matryoshka
  ;; Matryoshka basic functionality
  (do-game
    (new-game {:corp {:hand ["Ice Wall"]}
               :runner {:hand [(qty "Matryoshka" 5)]
                        :credits 20}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :click 10)
    (play-from-hand state :runner "Matryoshka")
    (let [iwall (get-ice state :hq 0)
          mat (get-program state 0)]
      (rez state :corp iwall)
      (card-ability state :runner mat 0)
      (click-card state :runner (first (:hand (get-runner))))
      (card-ability state :runner mat 0)
      (click-card state :runner (first (:hand (get-runner))))
      (card-ability state :runner mat 0)
      (click-card state :runner (first (:hand (get-runner))))
      (card-ability state :runner mat 0)
      (click-card state :runner (first (:hand (get-runner))))
      (let [face-up (fn [card] (count (filter #(not (:facedown %)) (:hosted (refresh card)))))
            face-down (fn [card] (count (filter #(:facedown %) (:hosted (refresh card)))))
            do-mat-run (fn [card up total]
                         (is (= up (face-up card)) (str up " face-up copies of Matryoshka"))
                         (run-on state :hq)
                         (run-continue state)
                         (card-ability state :runner (refresh card) 1)
                         (click-prompt state :runner "1")
                         (click-prompt state :runner "End the run")
                         (is (= (- up 1) (face-up card))
                             (str (- up 1) " face-up copies of Matryoshka left"))
                         (is (= (+ 1 (- total up)) (face-down card))
                             (str (+ 1 (- total up)) "face-down copies of Matryoshka"))
                         (run-continue state)
                         (run-continue state))]
        (do-mat-run mat 4 4)
        (do-mat-run mat 3 4)
        (do-mat-run mat 2 4)
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 4 (face-up mat)) "All copies of Matryoshka turned back up again")))))

(deftest maven
  ;; Maven
  (do-game
      (new-game {:corp {:deck ["Border Control"]
                        :credits 20}
                 :runner {:hand ["Maven" "Datasucker"]
                          :credits 20}})
      (play-from-hand state :corp "Border Control" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Maven")
      (let [maven (get-program state 0)]
        (is (= 1 (get-strength (refresh maven))) "Maven boosts itself")
        (play-from-hand state :runner "Datasucker")
        (is (= 2 (get-strength (refresh maven))) "+1 str from Datasucker")
        (run-on state "HQ")
        (run-continue state)
        (auto-pump-and-break state (refresh maven))
        (is (second-last-log-contains? state "Runner pays 4 [Credits] to use Maven to break all 2 subroutines on Border Control.") "Correct log with autopump ability")
        (core/continue state :corp nil)
        (run-jack-out state)
        (run-on state "HQ")
        (run-continue state)
        (card-ability state :runner (refresh maven) 0)
        (click-prompt state :runner "End the run")
        (is (last-log-contains? state "Runner pays 2 [Credits] to use Maven to break 1 subroutine on Border Control.") "Correct log with single sub break"))))

(deftest mayfly
  ;; Mayfly
  (do-game
      (new-game {:corp {:deck ["Anansi"]
                        :credits 20}
                 :runner {:hand ["Mayfly"]
                          :credits 20}})
      (play-from-hand state :corp "Anansi" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Mayfly")
      (let [mayfly (get-program state 0)]
        (run-on state "HQ")
        (run-continue state)
        (is (changed? [(:credit (get-runner)) -7]
              (auto-pump-and-break state (refresh mayfly)))
            "Paid 7 to fully break Anansi with Mayfly")
        (is (= 0 (count (remove :broken (:subroutines (get-ice state :hq 0))))) "Broken all subroutines")
        (core/continue state :corp nil)
        (run-jack-out state)
        (is (no-prompt? state :runner) "Mayfly not prompting to resolve each of its events")
        (is (= 1 (count (:discard (get-runner)))) "Mayfly trashed when run ends"))))

(deftest mayfly-trash-does-not-trigger-dummy-box
  ;; Mayfly trash doesn't trigger Dummy Box
  (do-game
      (new-game {:corp {:deck ["Spiderweb"]
                        :credits 20}
                 :runner {:hand [(qty "Mayfly" 2) "Dummy Box"]
                          :credits 20}})
      (play-from-hand state :corp "Spiderweb" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Mayfly")
      (play-from-hand state :runner "Dummy Box")
      (let [mayfly (get-program state 0)]
        (run-on state "HQ")
        (run-continue state)
        (auto-pump-and-break state (refresh mayfly))
        (core/continue state :corp nil)
        (run-jack-out state)
        (is (no-prompt? state :runner) "Dummy Box not prompting to prevent trash"))))

(deftest mimic
  ;; Mimic
  (do-game
      (new-game {:corp {:hand ["Pup"]}
                 :runner {:hand [(qty "Mimic" 5)]}})
      (play-from-hand state :corp "Pup" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Mimic")
      (let [mimic (get-program state 0)]
        (is (= 2 (:credit (get-runner))) "Runner starts with 2 credits")
        (is (= 4 (count (:hand (get-runner)))) "Runner has 4 cards in hand")
        (run-on state "HQ")
        (run-continue state)
        (auto-pump-and-break state (refresh mimic))
        (is (second-last-log-contains? state "Runner pays 2 [Credits] to use Mimic to break all 2 subroutines on Pup") "Correct log with autopump ability")
        (core/continue state :corp nil)
        (run-jack-out state)
        (is (zero? (:credit (get-runner))) "Runner spent 2 credits to break Pup")
        (is (= 4 (count (:hand (get-runner)))) "Runner still has 4 cards in hand"))))

(deftest mimic-no-dynamic-options-if-below-strength
    ;; No dynamic options if below strength
    (do-game
      (new-game {:corp {:hand ["Anansi"]
                        :credits 10}
                 :runner {:hand [(qty "Mimic" 5)]
                          :credits 10}})
      (play-from-hand state :corp "Anansi" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Mimic")
      (let [mimic (get-program state 0)]
        (is (= 7 (:credit (get-runner))) "Runner starts with 7 credits")
        (is (= 4 (count (:hand (get-runner)))) "Runner has 4 cards in hand")
        (run-on state "HQ")
        (run-continue state)
        (is (= 1 (count (:abilities (refresh mimic)))) "Auto pump and break ability on Mimic is not available"))))

(deftest mimic-dynamic-options-when-ice-weakened
    ;; Dynamic options when ice weakened
    (do-game
      (new-game {:corp {:hand ["Zed 2.0"]
                        :credits 10}
                 :runner {:hand ["Mimic" "Ice Carver" ]
                          :credits 10}})
      (play-from-hand state :corp "Zed 2.0" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Ice Carver")
      (play-from-hand state :runner "Mimic")
      (let [mimic (get-program state 0)]
        (is (= 4 (:credit (get-runner))) "Runner starts with 4 credits")
        (run-on state "HQ")
        (run-continue state)
        (is (= 2 (count (:abilities (refresh mimic)))) "Auto pump and break ability on Mimic is available")
        (auto-pump-and-break state (refresh mimic))
        (is (second-last-log-contains? state "Runner pays 3 [Credits] to use Mimic to break all 3 subroutines on Zed 2.0") "Correct log with autopump ability")
        (core/continue state :corp nil)
        (run-jack-out state)
        (is (= 1 (:credit (get-runner))) "Runner spent 3 credits to break Zed 2.0"))))

(deftest misdirection-recurring-credits-interaction-issue-4868
    ;; Recurring credits interaction. Issue #4868
    (do-game
      (new-game {:runner {:hand ["Misdirection" "Multithreader"]
                          :credits 10
                          :tags 2}})
      (take-credits state :corp)
      (core/gain state :runner :click 2)
      (play-from-hand state :runner "Misdirection")
      (play-from-hand state :runner "Multithreader")
      (let [mis (get-program state 0)
            multi (get-program state 1)]
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner mis 0)
              (click-prompt state :runner "2")
              (is (= "Choose a credit providing card (0 of 2 [Credits])"
                 (:msg (prompt-map :runner)))
              "Runner has pay-credit prompt")
              (click-card state :runner multi)
              (click-card state :runner multi))
            "Using recurring credits")
        (is (zero? (count-tags state)) "Runner has lost both tags"))))

(deftest misdirection-using-credits-from-mantle-and-credit-pool
    ;; Using credits from Mantle and credit pool
    (do-game
      (new-game {:runner {:hand ["Misdirection" "Mantle"]
                          :credits 10
                          :tags 4}})
      (take-credits state :corp)
      (core/gain state :runner :click 2)
      (play-from-hand state :runner "Misdirection")
      (play-from-hand state :runner "Mantle")
      (let [mis (get-program state 0)
            mantle (get-program state 1)]
        (is (changed? [(:credit (get-runner)) -3]
              (card-ability state :runner mis 0)
              (click-prompt state :runner "4")
              (is (= "Choose a credit providing card (0 of 4 [Credits])"
                 (:msg (prompt-map :runner)))
              "Runner has pay-credit prompt")
              (click-card state :runner mantle))
            "Using recurring credits and credits from credit pool")
        (is (zero? (count-tags state)) "Runner has lost all 4 tags"))))

(deftest misdirection-basic-behavior
    ;; Basic behavior
    (do-game
      (new-game {:runner {:hand ["Misdirection"]
                          :credits 5
                          :tags 2}})
      (take-credits state :corp)
      (play-from-hand state :runner "Misdirection")
      (let [mis (get-program state 0)]
        (is (= 5 (:credit (get-runner))) "Runner starts with 5 credits")
        (is (= 3 (:click (get-runner))) "Runner starts with 3 clicks")
        (card-ability state :runner mis 0)
        (click-prompt state :runner "2")
        (is (zero? (count-tags state)) "Runner has lost both tags")
        (is (= 1 (:click (get-runner))) "Runner spent 2 clicks (1 remaining)")
        (is (= 3 (:credit (get-runner))) "Runner spent 2 credits (3 remaining)"))))

(deftest monkeywrench
    (do-game
      (new-game {:runner {:hand ["Monkeywrench"]}
                 :corp {:hand ["Enigma" "Wraparound" "Ice Wall"]
                        :credits 10}})
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Wraparound" "HQ")
      (play-from-hand state :corp "Ice Wall" "R&D")
      (take-credits state :corp)
      (let [enigma (get-ice state :hq 0)
            wr (get-ice state :hq 1)
            iw (get-ice state :rd 0)]
        (play-from-hand state :runner "Monkeywrench")
        (click-card state :runner wr)
        (rez state :corp wr)
        (rez state :corp enigma)
        (rez state :corp iw)
        (is (= 5 (get-strength (refresh wr))))
        (is (= 1 (get-strength (refresh enigma))))
        (is (= 1 (get-strength (refresh iw)))))))

(deftest mkultra-auto-pump
    ;; auto-pump
    (testing "Pumping and breaking for 1"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Rototurret"]
                          :credits 10}
                   :runner {:hand ["MKUltra"]
                            :credits 100}})
        (play-from-hand state :corp "Rototurret" "HQ")
        (rez state :corp (get-ice state :hq 0))
        (take-credits state :corp)
        (play-from-hand state :runner "MKUltra")
        (let [pc (get-program state 0)]
          (run-on state :hq)
          (run-continue state)
          (is (changed? [(:credit (get-runner)) -3]
                (auto-pump-and-break state (refresh pc)))
              "Paid 3 to fully break Rototurret with MKUltra")
          (is (= 3 (get-strength (refresh pc))) "Pumped MKUltra up to str 3")
          (is (= 0 (count (remove :broken (:subroutines (get-ice state :hq 0))))) "Broken all subroutines")))))

(deftest mkultra-heap-locked
    ;; Heap Locked
    (do-game
      (new-game {:corp {:deck ["Rototurret" "Blacklist"]}
                 :runner {:deck [(qty "MKUltra" 1)]}})
      (play-from-hand state :corp "Rototurret" "Archives")
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (trash-from-hand state :runner "MKUltra")
      (run-on state "Archives")
      (rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (is (no-prompt? state :runner) "MKUltra prompt did not come up")))

(deftest multithreader-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Multithreader" "Abagnale"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Multithreader")
      (play-from-hand state :runner "Abagnale")
      (let [mt (get-program state 0)
            ab (get-program state 1)]
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner ab 1)
              (is (= "Choose a credit providing card (0 of 2 [Credits])"
                                  (:msg (prompt-map :runner)))
                               "Runner has pay-credit prompt")
              (click-card state :runner mt)
              (click-card state :runner mt))
            "Used 2 credits from Multithreader"))))

(deftest musaazi
  ;; Musaazi gains virus counters on successful runs and can spend virus counters from any installed card
  (do-game
    (new-game {:corp {:deck ["Lancelot"]}
               :runner {:deck ["Musaazi" "Imp"]}})
    (play-from-hand state :corp "Lancelot" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Musaazi")
    (play-from-hand state :runner "Imp")
    (let [lancelot (get-ice state :hq 0)
          musaazi (get-program state 0)
          imp (get-program state 1)]
      (run-empty-server state "Archives")
      (is (= 1 (get-counters (refresh musaazi) :virus)) "Musaazi has 1 virus counter")
      (is (= 1 (get-strength (refresh musaazi))) "Initial Musaazi strength")
      (is (= 2 (get-counters (refresh imp) :virus)) "Initial Imp virus counters")
      (run-on state "HQ")
      (rez state :corp lancelot)
      (run-continue state)
      (card-ability state :runner musaazi 1) ; match strength
      (click-card state :runner imp)
      (is (= 1 (get-counters (refresh imp) :virus)) "Imp lost 1 virus counter to pump")
      (is (= 2 (get-strength (refresh musaazi))) "Musaazi strength 2")
      (is (no-prompt? state :runner) "No prompt open")
      (card-ability state :runner musaazi 0)
      (click-prompt state :runner "Trash a program")
      (click-card state :runner musaazi)
      (click-prompt state :runner "Resolve a Grail ice subroutine from HQ")
      (click-card state :runner imp)
      (is (zero? (get-counters (refresh imp) :virus)) "Imp lost its final virus counter")
      (is (zero? (get-counters (refresh imp) :virus)) "Musaazi lost its virus counter"))))

(deftest muse-install-from-the-stack
  (do-game
    (new-game {:runner {:deck ["Ika" "Fermenter" "Leprechaun"]
                        :hand ["Muse"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Muse")
    (let [muse (get-program state 0)]
      (click-prompt state :runner "Stack")
      (is (= 3 (count (:choices (prompt-map :runner)))) "Ika, Fermenter and 'Done' are listed")
      (is (changed? [(count (:deck (get-runner))) -1
                    (:credit (get-runner)) -1]
          (click-prompt state :runner "Fermenter"))
          "Fermenter installed from the stack")
      (is (= 1 (count (:hosted (refresh muse)))) "Fermenter is hosted on Muse"))))

(deftest muse-install-from-the-grip
  (do-game
    (new-game {:runner {:hand [(qty "Muse" 2) "Marjanah"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Muse")
    (let [muse (get-program state 0)]
      (click-prompt state :runner "Grip")
      (is (= 2 (count (:choices (prompt-map :runner)))) "Marjanah and 'Done' are listed")
      (is (changed? [(count (:hand (get-runner))) -1
                    (:credit (get-runner)) 0]
          (click-prompt state :runner "Marjanah"))
          "Marjanah installed from the grip")
      (is (= 1 (count (:hosted (refresh muse)))) "Marjanah is hosted on Muse"))))

(deftest muse-install-trojan-from-the-heap
  (do-game
    (new-game {:runner {:hand ["Muse"]
                        :discard ["Slap Vandal" "Muse"]}
               :corp {:hand ["Vanilla"]}})
    (play-from-hand state :corp "Vanilla" "HQ")
    (let [vanilla (get-ice state :hq 0)]
      (take-credits state :corp)
      (play-from-hand state :runner "Muse")
      (click-prompt state :runner "Heap")
      (is (= 2 (count (:choices (prompt-map :runner)))) "Slap Vandal and 'Done' are listed")
      (is (changed? [(count (:discard (get-runner))) -1
                    (:credit (get-runner)) -1]
          (click-prompt state :runner "Slap Vandal")
          (click-card state :runner vanilla))
        "Slap Vandal installed from the heap")
    (is (= "Slap Vandal" (:title (first (:hosted (refresh vanilla))))) "Slap Vandal is hosted on Vanilla"))))

(deftest na-not-k
  ;; Na'Not'K - Strength adjusts accordingly when ice installed during run
  (do-game
      (new-game {:corp {:deck ["Architect" "Eli 1.0"]}
                 :runner {:deck ["Na'Not'K"]}})
      (play-from-hand state :corp "Architect" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Na'Not'K")
      (let [nanotk (get-program state 0)
            architect (get-ice state :hq 0)]
        (is (= 1 (get-strength (refresh nanotk))) "Default strength")
        (run-on state "HQ")
        (rez state :corp architect)
        (run-continue state)
        (is (= 2 (get-strength (refresh nanotk))) "1 ice on HQ")
        (card-subroutine state :corp (refresh architect) 1)
        (click-card state :corp (find-card "Eli 1.0" (:hand (get-corp))))
        (click-prompt state :corp "HQ")
        (is (= 3 (get-strength (refresh nanotk))) "2 ice on HQ")
        (run-continue-until state :movement)
        (run-jack-out state)
        (is (= 1 (get-strength (refresh nanotk))) "Back to default strength"))))

(deftest na-not-k-strength-adjusts-accordingly-when-run-redirected-to-another-server
    ;; Strength adjusts accordingly when run redirected to another server
    (do-game
      (new-game {:corp {:deck ["Susanoo-no-Mikoto" "Crick" "Cortex Lock"]
                        :credits 20}
                 :runner {:deck ["Na'Not'K"]}})
      (play-from-hand state :corp "Cortex Lock" "HQ")
      (play-from-hand state :corp "Susanoo-no-Mikoto" "HQ")
      (play-from-hand state :corp "Crick" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Na'Not'K")
      (let [nanotk (get-program state 0)
            susanoo (get-ice state :hq 1)]
        (is (= 1 (get-strength (refresh nanotk))) "Default strength")
        (run-on state "HQ")
        (rez state :corp susanoo)
        (run-continue state)
        (is (= 3 (get-strength (refresh nanotk))) "2 ice on HQ")
        (card-subroutine state :corp (refresh susanoo) 0)
        (is (= 2 (get-strength (refresh nanotk))) "1 ice on Archives")
        (run-continue-until state :movement)
        (run-jack-out state)
        (is (= 1 (get-strength (refresh nanotk))) "Back to default strength"))))

(deftest nanuq
  ;; Nanuq
  (do-game
      (new-game {:runner {:deck [(qty "Nanuq" 3) "Uninstall"]
                          :credits 50}
                 :corp {:deck ["Spiderweb" (qty "Hostile Takeover" 2)]}})
      (play-from-hand state :corp "Spiderweb" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Nanuq")
      (run-on state "R&D")
      (rez state :corp (get-ice state :rd 0))
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -4]
            (card-ability state :runner (refresh (get-program state 0)) 0)
            (click-prompt state :runner "End the run")
            (click-prompt state :runner "End the run")
            (click-prompt state :runner "End the run"))
          "Broke all subroutines")
      (run-continue-until state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (play-and-score state "Hostile Takeover")
      (is (not (get-program state 0)) "Nanuq no installed anymore")
      (is (= 1 (count (:rfg (get-runner)))) "Nanuq removed from the game on agenda scored")
      (take-credits state :corp)
      (play-from-hand state :runner "Nanuq")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (is (not (get-program state 0)) "Nanuq no installed anymore")
      (is (= 2 (count (:rfg (get-runner)))) "Nanuq removed from the game on agenda stolen")
      (play-from-hand state :runner "Nanuq")
      (play-from-hand state :runner "Uninstall")
      (click-card state :runner (get-program state 0))
      (is (= 3 (count (:rfg (get-runner)))) "Nanuq removed from the game when uninstalled")))

(deftest nfr
  ;; Nfr
  (do-game
      (new-game {:runner {:deck ["Nfr"]}
                 :corp {:deck ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Nfr")
      (let [nfr (get-program state 0)
            icew (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp (refresh icew))
        (run-continue state)
        (card-ability state :runner (refresh nfr) 0)
        (click-prompt state :runner "End the run")
        (is (changed? [(get-counters (refresh nfr) :power) 1]
              (run-continue state))
            "Got 1 token"))))

(deftest nga
  ;; Nga
  (do-game
    (new-game {:runner {:hand ["Nga"]}
               :corp {:hand ["Hedge Fund" "NGO Front" "IPO"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Nga")
    (let [nga (get-program state 0)]
      (is (= 3 (get-counters (refresh nga) :power)))
      (run-on state "R&D")
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-card state :corp "Hedge Fund")
      (is (= 2 (get-counters (refresh nga) :power))))))

(deftest nyashia
  ;; Nyashia
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand ["Hedge Fund"]}
               :runner {:deck ["Nyashia"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Nyashia")
    (run-on state "R&D")
    (run-continue state)
    (click-prompt state :runner "Yes")
    (is (= 2 (:random-access-limit (core/num-cards-to-access state :runner :rd nil))))))

(deftest odore
  (do-game
      (new-game {:corp {:deck ["Cobra"]}
                 :runner {:deck ["Odore" (qty "Logic Bomb" 3)]}})
      (play-from-hand state :corp "Cobra" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Odore")
      (let [odore (get-program state 0)
            cobra (get-ice state :hq 0)]
        (core/gain state :runner :click 2 :credit 20)
        (run-on state "HQ")
        (rez state :corp cobra)
        (run-continue state)
        (is (changed? [(:credit (get-runner)) -5]
              (card-ability state :runner odore 2)
              (card-ability state :runner odore 0)
              (click-prompt state :runner "Trash a program")
              (click-prompt state :runner "Do 2 net damage"))
            "Paid 3 to pump and 2 to break"))))

(deftest odore-auto-pump-and-break-with-and-without-3-virtual-resources
    ;; auto-pump-and-break with and without 3 virtual resources
    (do-game
      (new-game {:corp {:deck ["Cobra"]}
                 :runner {:deck ["Odore" (qty "Logic Bomb" 3)]}})
      (play-from-hand state :corp "Cobra" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Odore")
      (let [odore (get-program state 0)
            cobra (get-ice state :hq 0)]
        (core/gain state :runner :click 2 :credit 20)
        (run-on state "HQ")
        (rez state :corp cobra)
        (run-continue state)
        (is (changed? [(:credit (get-runner)) -5]
              (auto-pump-and-break state (refresh odore)))
            "Paid 3 to pump and 2 to break")
        (core/continue state :corp nil)
        (run-jack-out state)
        (dotimes [_ 3] (play-from-hand state :runner "Logic Bomb"))
        (run-on state "HQ")
        (run-continue state)
        (is (changed? [(:credit (get-runner)) -3]
              (auto-pump-and-break state (refresh odore)))
            "Paid 3 to pump and 0 to break"))))

(deftest orca
  ;; Orca
  (do-game
    (new-game {:runner {:hand ["Orca" "Earthrise Hotel"]
                        :credits 50}
               :corp {:hand ["Saisentan"]}})
    (play-from-hand state :corp "Saisentan" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Orca")
    (play-from-hand state :runner "Earthrise Hotel")
    (let [orca (get-program state 0)
          hotel (get-resource state 0)
          sais (get-ice state :hq 0)]
      (run-on state "HQ")
      (rez state :corp sais)
      (run-continue state)
      (click-prompt state :corp "Event")
      (auto-pump-and-break state (refresh orca))
      (is (changed? [(get-counters (refresh hotel) :power) 1]
            (click-card state :runner hotel))
          "Charged Earthrise Hotel")
      (core/continue state :corp nil)
      (run-jack-out state)
      (run-on state "HQ")
      (run-continue state)
      (click-prompt state :corp "Event")
      (auto-pump-and-break state (refresh orca))
      (is (no-prompt? state :runner) "No second prompt to charge"))))

(deftest orca-triggers-when-breaking-with-itself-only
  ;; Orca - no trigger when subs were broken by something else as well
  (do-game
    (new-game {:runner {:hand ["Orca" "Earthrise Hotel" "Boomerang"]
                        :credits 50}
               :corp {:hand ["Saisentan"]}})
    (play-from-hand state :corp "Saisentan" "HQ")
    (take-credits state :corp)
    (core/gain-clicks state :runner 3)
    (play-from-hand state :runner "Orca")
    (play-from-hand state :runner "Earthrise Hotel")
    (let [orca (get-program state 0)
          sais (get-ice state :hq 0)]
      (play-from-hand state :runner "Boomerang")
      (click-card state :runner sais)
      (run-on state "HQ")
      (rez state :corp sais)
      (run-continue state)
      (click-prompt state :corp "Event")
      (card-ability state :runner (get-hardware state 0) 0)
      (click-prompt state :runner "Do 1 net damage")
      (click-prompt state :runner "Do 1 net damage")
      (auto-pump-and-break state (refresh orca))
      (is (no-prompt? state :runner) "No prompt to charge"))))

(deftest origami
  ;; Origami - Increases Runner max hand size
  (do-game
    (new-game {:runner {:deck [(qty "Origami" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Origami")
    (is (= 6 (hand-size :runner)))
    (play-from-hand state :runner "Origami")
    (is (= 9 (hand-size :runner)) "Max hand size increased by 2 for each copy installed")))

(deftest overmind
  ;; Overmind - Start with counters equal to unused MU
  (do-game
    (new-game {:runner {:deck ["Overmind" "Deep Red" "Sure Gamble" "Akamatsu Mem Chip" ]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (is (= 5 (core/available-mu state)))
    (play-from-hand state :runner "Deep Red")
    (is (= 8 (core/available-mu state)))
    (play-from-hand state :runner "Overmind")
    (is (= 7 (core/available-mu state)))
    (let [ov (get-program state 0)]
      (is (= 7 (get-counters (refresh ov) :power)) "Overmind has 5 counters"))))

(deftest paintbrush
  ;; Paintbrush - Give rezzed piece of ice a chosen subtype until the end of the next run
  (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Paintbrush"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Paintbrush")
      (is (= 2 (core/available-mu state)))
      (let [iwall (get-ice state :hq 0)
            pb (get-program state 0)]
        (card-ability state :runner pb 0)
        (click-card state :runner iwall)
        (is (= 3 (:click (get-runner))) "Ice Wall not rezzed, so no click charged")
        (click-prompt state :runner "Done") ; cancel out
        (rez state :corp iwall)
        (card-ability state :runner pb 0)
        (click-card state :runner iwall)
        (click-prompt state :runner "Code Gate")
        (is (= 2 (:click (get-runner))) "Click charged")
        (is (has-subtype? (refresh iwall) "Code Gate") "Ice Wall gained Code Gate")
        (run-empty-server state "Archives")
        (is (not (has-subtype? (refresh iwall) "Code Gate")) "Ice Wall lost Code Gate at the end of the run"))))

(deftest paintbrush-encounters-outside-of-a-run-do-not-end-paintbrush-s-effect
    ;; Encounters outside of a run do not end Paintbrush's effect
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "Ganked!"]}
                 :runner {:deck ["Paintbrush" "Quest Completed"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Ganked!" "Server 1")
      (take-credits state :corp)
      (core/gain-clicks state :runner 3)
      (play-from-hand state :runner "Paintbrush")
      (let [iwall (get-ice state :remote1 0)
            pb (get-program state 0)]
        (run-empty-server state "Archives")
        (run-empty-server state "R&D")
        (run-empty-server state "HQ")
        (rez state :corp iwall)
        (card-ability state :runner pb 0)
        (click-card state :runner iwall)
        (click-prompt state :runner "Code Gate")
        (is (has-subtype? (refresh iwall) "Code Gate") "Ice Wall gained Code Gate")
        (play-from-hand state :runner "Quest Completed")
        (click-card state :runner (get-content state :remote1 0))
        (click-prompt state :corp "Yes")
        (click-card state :corp iwall)
        (is (= (refresh iwall) (core/get-current-ice state)) "Runner is encountering Ice Wall")
        (fire-subs state (refresh iwall))
        (is (has-subtype? (refresh iwall) "Code Gate") "Ice Wall still has the Code Gate subtype")
        (run-empty-server state "Archives")
        (is (not (has-subtype? (refresh iwall) "Code Gate")) "Ice Wall lost Code Gate at the end of the run"))))

(deftest panchatantra
  ;; Panchatantra
  (before-each [state (new-game {:corp {:hand ["Ice Wall"]}
                                 :runner {:hand ["Panchatantra"]}})
                _ (do (play-from-hand state :corp "Ice Wall" "HQ")
                      (rez state :corp (get-ice state :hq 0))
                      (take-credits state :corp)
                      (play-from-hand state :runner "Panchatantra")
                      (run-on state :hq)
                      (run-continue state))
                iw (get-ice state :hq 0)]
    (testing "Choices do not include Barrier, Code Gate, or Sentry"
      (do-game state
        (click-prompt state :runner "Yes")
        (is (not (some #{"Barrier" "Code Gate" "Sentry"} (prompt-buttons :runner))))))
    (testing "Encountered ice gains the subtype"
      (do-game state
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "AP")
        (is (has-subtype? (refresh iw) "AP"))))
    (testing "Encountered ice loses subtype at the end of the run"
      (do-game state
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "AP")
        (run-continue state)
        (is (has-subtype? (refresh iw) "AP"))
        (run-jack-out state)
        (is (not (has-subtype? (refresh iw) "AP")))))))

(deftest paperclip
  ;; Paperclip - prompt to install on encounter, but not if another is installed
  (do-game
      (new-game {:corp {:deck ["Vanilla"]}
                 :runner {:deck [(qty "Paperclip" 3)]
                          :credits 10}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (take-credits state :corp)
      (trash-from-hand state :runner "Paperclip")
      (run-on state "Archives")
      (rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (click-prompt state :runner "Yes") ; install paperclip
      (run-continue-until state :success)
      (is (not (:run @state)) "Run ended")
      (trash-from-hand state :runner "Paperclip")
      (trash-from-hand state :runner "Paperclip")
      (run-on state "Archives")
      (run-continue state)
      (click-prompt state :runner "No")
      (is (no-prompt? state :runner) "No prompt to install third Paperclip")))

(deftest paperclip-firing-on-facedown-ice-shouldn-t-crash
    ;; firing on facedown ice shouldn't crash
    (do-game
      (new-game {:corp {:deck ["Vanilla"]}
                 :runner {:deck ["Paperclip"]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Paperclip")
      (run-on state "Archives")
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "0")))

(deftest paperclip-do-not-show-a-second-install-prompt-if-user-said-no-to-first-when-multiple-are-in-heap
    ;; do not show a second install prompt if user said No to first, when multiple are in heap
    (do-game
      (new-game {:corp {:deck [(qty "Vanilla" 2)]}
                 :runner {:deck [(qty "Paperclip" 3)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (play-from-hand state :corp "Vanilla" "Archives")
      (take-credits state :corp)
      (trash-from-hand state :runner "Paperclip")
      (trash-from-hand state :runner "Paperclip")
      (trash-from-hand state :runner "Paperclip")
      (run-on state "Archives")
      (rez state :corp (get-ice state :archives 1))
      (run-continue state)
      (click-prompt state :runner "No")
      (is (no-prompt? state :runner) "No additional prompts to rez other copies of Paperclip")
      (run-continue-until state :approach-ice)
      (rez state :corp (get-ice state :archives 0))
      (run-continue state)
      ;; we should get the prompt on a second ice even after denying the first
      (click-prompt state :runner "No")
      (is (no-prompt? state :runner) "No additional prompts to rez other copies of Paperclip")
      (run-continue state :movement)
      (run-jack-out state)
      ;; Run again, make sure we get the prompt to install again
      (run-on state "Archives")
      (run-continue state)
      (click-prompt state :runner "No")
      (is (no-prompt? state :runner) "No additional prompts to rez other copies of Paperclip")))

(deftest paperclip-auto-pump
    ;; auto-pump
    (testing "Pumping and breaking for 1"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Vanilla"]
                          :credits 10}
                   :runner {:hand ["Paperclip"]
                            :credits 100}})
        (play-from-hand state :corp "Vanilla" "HQ")
        (rez state :corp (get-ice state :hq 0))
        (take-credits state :corp)
        (play-from-hand state :runner "Paperclip")
        (let [pc (get-program state 0)]
          (run-on state :hq)
          (run-continue state)
          (is (changed? [(:credit (get-runner)) -1]
                (auto-pump-and-break state (refresh pc)))
              "Paid 1 to fully break Vanilla with Paperclip")
          (is (= 2 (get-strength (refresh pc))) "Pumped Paperclip up to str 2")
          (is (= 0 (count (remove :broken (:subroutines (get-ice state :hq 0))))) "Broken all subroutines"))))
    (testing "Pumping for >1 and breaking for 1"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Fire Wall"]
                          :credits 10}
                   :runner {:hand ["Paperclip"]
                            :credits 100}})
        (play-from-hand state :corp "Fire Wall" "HQ")
        (rez state :corp (get-ice state :hq 0))
        (take-credits state :corp)
        (play-from-hand state :runner "Paperclip")
        (let [pc (get-program state 0)]
          (run-on state :hq)
          (run-continue state)
          (is (changed? [(:credit (get-runner)) -4]
                (auto-pump-and-break state (refresh pc)))
              "Paid 4 to fully break Fire Wall with Paperclip")
          (is (= 5 (get-strength (refresh pc))) "Pumped Paperclip up to str 5")
          (is (= 0 (count (remove :broken (:subroutines (get-ice state :hq 0))))) "Broken all subroutines"))))
    (testing "Pumping for 1 and breaking for >1"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Spiderweb"]
                          :credits 10}
                   :runner {:hand ["Paperclip"]
                            :credits 100}})
        (play-from-hand state :corp "Spiderweb" "HQ")
        (rez state :corp (get-ice state :hq 0))
        (take-credits state :corp)
        (play-from-hand state :runner "Paperclip")
        (let [pc (get-program state 0)]
          (run-on state :hq)
          (run-continue state)
          (is (changed? [(:credit (get-runner)) -3]
                (auto-pump-and-break state (refresh pc)))
              "Paid 3 to fully break Spiderweb with Paperclip")
          (is (= 4 (get-strength (refresh pc))) "Pumped Paperclip up to str 4")
          (is (= 0 (count (remove :broken (:subroutines (get-ice state :hq 0))))) "Broken all subroutines"))))
    (testing "Pumping for >1 and breaking for >1"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Chiyashi"]
                          :credits 12}
                   :runner {:hand ["Paperclip"]
                            :credits 100}})
        (play-from-hand state :corp "Chiyashi" "HQ")
        (rez state :corp (get-ice state :hq 0))
        (take-credits state :corp)
        (play-from-hand state :runner "Paperclip")
        (let [pc (get-program state 0)]
          (run-on state :hq)
          (run-continue state)
          (is (changed? [(:credit (get-runner)) -7]
                (auto-pump-and-break state (refresh pc)))
              "Paid 7 to fully break Chiyashi with Paperclip")
          (is (= 8 (get-strength (refresh pc))) "Pumped Paperclip up to str 8")
          (is (= 0 (count (remove :broken (:subroutines (get-ice state :hq 0))))) "Broken all subroutines"))))
    (testing "No auto-pump on unbreakable subs"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Akhet"]
                          :credits 10}
                   :runner {:hand ["Paperclip"]
                            :credits 100}})
        (core/gain state :corp :click 1)
        (play-from-hand state :corp "Akhet" "HQ")
        (rez state :corp (get-ice state :hq 0))
        (dotimes [_ 3]
          (advance state (get-ice state :hq 0)))
        (take-credits state :corp)
        (play-from-hand state :runner "Paperclip")
        (let [pc (get-program state 0)]
          (run-on state :hq)
          (run-continue state)
          (is (empty? (filter #(:dynamic %) (:abilities (refresh pc)))) "No auto-pumping option for Akhet"))))
    (testing "Orion triggers all heap breakers once"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Orion"]
                          :credits 15}
                   :runner {:discard [(qty "Paperclip" 2) (qty "MKUltra" 2) (qty "Black Orchestra" 2)]
                            :credits 100}})
        (play-from-hand state :corp "Orion" "HQ")
        (take-credits state :corp)
        (run-on state :hq)
        (rez state :corp (get-ice state :hq 0))
        (run-continue state)
        (click-prompt state :runner "No")
        (click-prompt state :runner "No")
        (click-prompt state :runner "No")
        (is (no-prompt? state :runner) "No further prompts to install heap breakers"))))

(deftest paperclip-heap-locked
    ;; Heap Locked
    (do-game
      (new-game {:corp {:deck ["Vanilla" "Blacklist"]}
                 :runner {:deck [(qty "Paperclip" 2)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (trash-from-hand state :runner "Paperclip")
      (run-on state "Archives")
      (rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (is (no-prompt? state :runner) "Paperclip prompt did not come up")
      (fire-subs state (get-ice state :archives 0))
      (is (not (:run @state)) "Run ended")))

(deftest paperclip-breaking-some-subs
    ;; Breaking some subs
    (do-game
      (new-game {:corp {:hand ["Hive"]}
                 :runner {:hand ["Paperclip"]
                          :credits 10}})
      (play-from-hand state :corp "Hive" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Paperclip")
      (let [pc (get-program state 0)]
        (run-on state :hq)
        (run-continue state)
        (is (changed? [(:credit (get-runner)) -2]
              (is (= 5 (count (:subroutines (get-ice state :hq 0)))) "Hive starts with 5 subs")
              (is (= 3 (get-strength (get-ice state :hq 0))) "Hive has strength 3")
              (card-ability state :runner pc 0)
              (click-prompt state :runner "2")
              (click-prompt state :runner "End the run")
              (click-prompt state :runner "End the run")
              (is (= 3 (get-strength (refresh pc))) "Pumped Paperclip up to str 3")
              (is (= 3 (count (remove :broken (:subroutines (get-ice state :hq 0))))) "Broke all but 3 subs"))
            "Paid 2 to break two of the subs on Hive"))))

(deftest parasite-basic-functionality-gain-1-counter-every-runner-turn
    ;; Basic functionality: Gain 1 counter every Runner turn
    (do-game
      (new-game {:corp {:deck [(qty "Wraparound" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Parasite" 3) (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Wraparound" "HQ")
      (let [wrap (get-ice state :hq 0)]
        (rez state :corp wrap)
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
          (is (= 6 (get-strength (refresh wrap))) "Wraparound reduced to 6 strength")))))

(deftest parasite-installed-facedown-w-apex
    ;; Installed facedown w/ Apex
    (do-game
      (new-game {:runner {:id "Apex: Invasive Predator"
                          :deck ["Parasite"]}})
      (take-credits state :corp)
      (end-phase-12 state :runner)
      (click-card state :runner (find-card "Parasite" (:hand (get-runner))))
      (is (no-prompt? state :runner) "No prompt to host Parasite")
      (is (= 1 (count (get-runner-facedown state))) "Parasite installed face down")))

(deftest parasite-installed-on-untrashable-architect-should-keep-gaining-counters-past-3-and-make-strength-go-negative
    ;; Installed on untrashable Architect should keep gaining counters past 3 and make strength go negative
    (do-game
      (new-game {:corp {:deck [(qty "Architect" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Parasite" 3) "Grimoire"]}})
      (play-from-hand state :corp "Architect" "HQ")
      (let [arch (get-ice state :hq 0)]
        (rez state :corp arch)
        (take-credits state :corp)
        (play-from-hand state :runner "Grimoire")
        (play-from-hand state :runner "Parasite")
        (click-card state :runner arch)
        (click-prompt state :runner "Grimoire")
        (let [psite (first (:hosted (refresh arch)))]
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (take-credits state :runner)
          (take-credits state :corp)
          (take-credits state :runner)
          (take-credits state :corp)
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 4 (get-counters (refresh psite) :virus)) "Parasite has 4 counters")
          (is (= -1 (get-strength (refresh arch))) "Architect at -1 strength")))))

(deftest parasite-should-stay-on-hosted-card-moved-by-builder
    ;; Should stay on hosted card moved by Builder
    (do-game
      (new-game {:corp {:deck [(qty "Builder" 3) "Ice Wall"]}
                 :runner {:deck [(qty "Parasite" 3)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Builder" "Archives")
      (let [builder (get-ice state :archives 0)]
        (rez state :corp builder)
        (take-credits state :corp)
        (play-from-hand state :runner "Parasite")
        (click-card state :runner builder)
        (let [psite (first (:hosted (refresh builder)))]
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 3 (get-strength (refresh builder))) "Builder reduced to 3 strength")
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (take-credits state :runner))
        (let [orig-builder (refresh builder)]
          (card-ability state :corp builder 0)
          (click-prompt state :corp "HQ")
          (let [moved-builder (get-ice state :hq 1)]
            (is (= (get-strength orig-builder) (get-strength moved-builder)) "Builder's state is maintained")
            (let [orig-psite (dissoc (first (:hosted orig-builder)) :host)
                  moved-psite (dissoc (first (:hosted moved-builder)) :host)]
              (is (= orig-psite moved-psite) "Hosted Parasite is maintained"))
            (take-credits state :corp)
            (let [updated-builder (refresh moved-builder)
                  updated-psite (first (:hosted updated-builder))]
              (is (= 2 (get-strength updated-builder)) "Builder strength still reduced")
              (is (= 2 (get-counters (refresh updated-psite) :virus)) "Parasite counters still incremented")))))))

(deftest parasite-use-hivemind-counters-when-installed-instantly-trash-ice-if-counters-ice-strength
    ;; Use Hivemind counters when installed; instantly trash ice if counters >= ice strength
    (do-game
      (new-game {:corp {:deck [(qty "Enigma" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck ["Parasite"
                                 "Grimoire"
                                 "Hivemind"
                                 "Sure Gamble"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (let [enig (get-ice state :hq 0)]
        (rez state :corp enig)
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

(deftest parasite-trashed-along-with-host-ice-when-its-strength-has-been-reduced-to-0
    ;; Trashed along with host ice when its strength has been reduced to 0
    (do-game
      (new-game {:corp {:deck [(qty "Enigma" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Parasite" 3) "Grimoire"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (let [enig (get-ice state :hq 0)]
        (rez state :corp enig)
        (take-credits state :corp)
        (play-from-hand state :runner "Grimoire")
        (play-from-hand state :runner "Parasite")
        (click-card state :runner enig)
        (click-prompt state :runner "Grimoire")
        (let [psite (first (:hosted (refresh enig)))]
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (is (= 1 (get-strength (refresh enig))) "Enigma reduced to 1 strength")
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 1 (count (:discard (get-corp)))) "Enigma trashed")
          (is (= 1 (count (:discard (get-runner)))) "Parasite trashed when Enigma was trashed")))))

(deftest parasite-interaction-with-customized-secretary-2672
    ;; Interaction with Customized Secretary #2672
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Enigma"]}
                 :runner {:deck ["Parasite"]
                          :hand ["Djinn" "Customized Secretary"]
                          :credits 10}})
      (play-from-hand state :corp "Enigma" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Djinn")
      (card-ability state :runner (get-program state 0) 1)
      (is (= "Choose a non-Icebreaker program" (:msg (prompt-map :runner))))
      (click-card state :runner "Customized Secretary")
      (is (= "Choose a program to host" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Parasite")
      (card-ability state :runner (first (:hosted (get-program state 0))) 0)
      (is (= "Parasite" (:title (first (:hosted (first (:hosted (get-program state 0))))))))
      (is (= "Choose a program to install" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Parasite")
      (is (= "Choose a card to host Parasite on" (:msg (prompt-map :runner))))
      (click-card state :runner "Enigma")
      (is (= "Customized Secretary" (:title (first (:hosted (get-program state 0))))))
      (is (empty? (:hosted (first (:hosted (get-program state 0))))))))

(deftest parasite-triggers-hostile-infrastructure
    ;; Triggers Hostile Infrastructure
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hostile Infrastructure" "Vanilla"]}
                 :runner {:deck [(qty "Parasite" 2)]}})
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (play-from-hand state :corp "Vanilla" "HQ")
      (let [van (get-ice state :hq 0)
            hi (get-content state :remote1 0)]
        (rez state :corp hi)
        (rez state :corp van)
        (take-credits state :corp)
        (is (changed? [(count (:discard (get-runner))) 2]
              (play-from-hand state :runner "Parasite")
              (click-card state :runner van))
            "Took net damage (Parasite on Vanilla was trashed + card from hand"))))

(deftest paricia
  ;; Paricia
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["PAD Campaign" "Shell Corporation"]}
               :runner {:hand ["Paricia"]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Shell Corporation" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Paricia")
    (let [pad (get-content state :remote1 0)
          shell (get-content state :remote2 0)]
      (run-empty-server state :remote2)
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (is (no-prompt? state :runner) "No pay-credit prompt as it's an upgrade")
      (is (nil? (refresh shell)) "Shell Corporation successfully trashed")
      (run-empty-server state :remote1)
      (is (= 2 (:credit (get-runner))) "Runner can't afford to trash PAD Campaign")
      (click-prompt state :runner "Pay 4 [Credits] to trash")
      (dotimes [_ 2]
        (click-card state :runner "Paricia"))
      (is (nil? (refresh pad)) "PAD Campaign successfully trashed"))))

(deftest pawn-happy-path
    ;; Happy Path
    (do-game
      (new-game {:corp   {:deck ["Enigma" "Blacklist" "Hedge Fund"]}
                 :runner {:deck ["Pawn" "Knight"]}})
      (play-from-hand state :corp "Enigma" "Archives")
      (take-credits state :corp)
      (trash-from-hand state :runner "Knight")
      (play-from-hand state :runner "Pawn")
      ;;currently Pawn successful run check is not implemented, nor is hosted location check
      (card-ability state :runner (get-program state 0) 2)
      (click-card state :runner (find-card "Knight" (:discard (get-runner))))
      (is (not (nil? (find-card "Pawn" (:discard (get-runner))))))))

(deftest pawn-heap-locked
    ;; Heap locked
    (do-game
      (new-game {:corp   {:deck ["Enigma" "Blacklist" "Hedge Fund"]}
                 :runner {:deck ["Pawn" "Knight"]}})
      (play-from-hand state :corp "Enigma" "Archives")
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (trash-from-hand state :runner "Knight")
      (play-from-hand state :runner "Pawn")
      (card-ability state :runner (get-program state 0) 2)
      (is (no-prompt? state :runner) "Install prompt did not come up")
      (is (nil? (find-card "Pawn" (:discard (get-runner)))))
      (is (not (nil? (find-card "Knight" (:discard (get-runner))))))))

(deftest pelangi
  ;; Pelangi
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand ["Ice Wall"]}
               :runner {:hand ["Pelangi"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Pelangi")
    (let [iw (get-ice state :hq 0)
          pelangi (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp iw)
      (run-continue state)
      (card-ability state :runner pelangi 0)
      (click-prompt state :runner "Code Gate")
      (is (has-subtype? (refresh iw) "Code Gate") "Ice Wall gained Code Gate")
      (run-continue state)
      (run-jack-out state)
      (is (not (has-subtype? (refresh iw) "Code Gate")) "Ice Wall lost Code Gate at the end of the encounter"))))

(deftest penrose-pay-credits-prompt-and-first-turn-ability
    ;; Pay-credits prompt and first turn ability
    (do-game
      (new-game {:runner {:deck ["Cloak" "Penrose"]}
                 :corp {:deck ["Enigma" "Vanilla"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Vanilla" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Cloak")
      (play-from-hand state :runner "Penrose")
      (core/gain state :runner :credit 1)
      (run-on state :hq)
      (let [enig (get-ice state :hq 0)
            van (get-ice state :hq 1)
            cl (get-program state 0)
            penr (get-program state 1)]
        (is (= 3 (count (:abilities penr))) "3 abilities on Penrose")
        (rez state :corp van)
        (run-continue state)
        (is (= 4 (count (:abilities (refresh penr)))) "Auto pump and break ability on Penrose active")
        (is (changed? [(:credit (get-runner)) 0]
              (auto-pump-and-break state (refresh penr))
              (click-card state :runner cl))
            "Used 1 credit from Cloak")
        (core/continue state :corp nil)
        (run-continue state :approach-ice)
        (rez state :corp enig)
        (run-continue state)
        (is (changed? [(:credit (get-runner)) -2]
              (auto-pump-and-break state (refresh penr)))
            "Paid 2 credits to break all subroutines on Enigma")
        (core/continue state :corp nil)
        (run-jack-out state)
        (take-credits state :runner)
        (take-credits state :corp)
        (run-on state :hq)
        (run-continue state :encounter-ice)
        (is (= 3 (count (:abilities (refresh penr)))) "Auto pump and break ability on Penrose is not active")
        (card-ability state :runner (refresh penr) 0)
        (is (no-prompt? state :runner) "No cloak prompt because the ability to break barriers is not active anymore"))))

(deftest peregrine
  ;; Peregrine - 2c to return to grip and derez an encountered code gate
  (do-game
    (new-game {:corp {:deck ["Paper Wall" (qty "Bandwidth" 2)]}
               :runner {:deck ["Peregrine"]}})
    (play-from-hand state :corp "Bandwidth" "Archives")
    (play-from-hand state :corp "Bandwidth" "Archives")
    (play-from-hand state :corp "Paper Wall" "Archives")
    (take-credits state :corp)
    (core/gain state :runner :credit 20)
    (play-from-hand state :runner "Peregrine")
    (let [bw1 (get-ice state :archives 0)
          pw (get-ice state :archives 2)
          per (get-program state 0)]
      (run-on state "Archives")
      (rez state :corp pw)
      (run-continue state)
      (rez state :corp bw1)
      (is (changed? [(:credit (get-runner)) 0]
            (card-ability state :runner per 2))
          "Can't use Peregrine on a barrier")
      (run-continue-until state :approach-ice)
      (is (changed? [(:credit (get-runner)) 0]
            (card-ability state :runner per 2))
          "Can't use Peregrine on an unrezzed code gate")
      (run-continue-until state :encounter-ice bw1)
      (is (changed? [(:credit (get-runner)) -3]
            (card-ability state :runner per 1))
          "Paid 3 to pump strength")
      (is (changed? [(:credit (get-runner)) -1]
            (card-ability state :runner per 0)
            (click-prompt state :runner "Give the Runner 1 tag"))
          "Paid 1 to break sub")
      (is (changed? [(:credit (get-runner)) -2]
            (card-ability state :runner per 2)
            (run-continue state))
          "Paid 2 to derez Bandwidth")
      (is (= 1 (count (:hand (get-runner)))) "Peregrine returned to grip")
      (is (not (rezzed? (refresh bw1))) "Bandwidth derezzed"))))

(deftest persephone-triggers-ar-enhanced-security-issue-3187
    ;; Triggers AR-Enhanced Security. Issue #3187
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Zed 1.0" "Zed 2.0" "AR-Enhanced Security"]}
                 :runner {:deck [(qty "Persephone" 10)]}})
      (play-from-hand state :corp "AR-Enhanced Security" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (play-from-hand state :corp "Zed 1.0" "Archives")
      (rez state :corp (get-ice state :archives 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Persephone")
      (run-on state "Archives")
      (run-continue state)
      (fire-subs state (get-ice state :archives 0))
      (run-continue state)
      (click-prompt state :runner "Yes")
      (is (= 1 (count-tags state)) "Runner took 1 tag from using Persephone's ability while AR-Enhanced Security is scored")
      (run-jack-out state)
      (take-credits state :runner)
      ;; Gotta move the discarded cards back to the deck
      (core/move state :corp (find-card "Zed 2.0" (:discard (get-corp))) :deck)
      (core/move state :corp (find-card "Zed 2.0" (:discard (get-corp))) :deck)
      (take-credits state :corp)
      (run-on state "Archives")
      (run-continue state)
      (fire-subs state (get-ice state :archives 0))
      (run-continue state)
      (click-prompt state :runner "Yes")
      (is (= 2 (count-tags state)) "Runner took 1 tag from using Persephone's ability while AR-Enhanced Security is scored")))

(deftest pheromones
  ;; Pheromones ability shouldn't have a NullPointerException when fired with 0 virus counter
  (do-game
      (new-game {:runner {:deck ["Pheromones"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Pheromones")
      (let [ph (get-program state 0)]
        (card-ability state :runner (refresh ph) 0)
        (run-empty-server state "HQ")
        (click-prompt state :runner "No action")
        (is (= 1 (get-counters (refresh ph) :virus)) "Pheromones gained 1 counter")
        (card-ability state :runner (refresh ph) 0))) ; this doesn't do anything, but shouldn't crash
)

(deftest pheromones-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Pheromones" "Inti"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Pheromones")
      (play-from-hand state :runner "Inti")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (take-credits state :corp)
      (let [phero (get-program state 0)
            inti (get-program state 1)]
        (is (changed? [(:credit (get-runner)) -2]
              (card-ability state :runner inti 1)))
        (is (changed? [(:credit (get-runner)) 0]
              (run-on state "HQ")
              (card-ability state :runner inti 1)
              (click-card state :runner phero)
              (click-card state :runner phero))
            "Used 2 credits from Pheromones"))))

(deftest physarum-entangler
  (do-game
    (new-game {:runner {:hand [(qty "Physarum Entangler" 2)]}
               :corp {:hand ["Vanilla" "Whitespace"]}})
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "Whitespace" "Archives")
    (take-credits state :corp)
    (let [van (get-ice state :hq 0)
          ws (get-ice state :archives 0)]
      (play-from-hand state :runner "Physarum Entangler")
      (click-card state :runner van)
      (play-from-hand state :runner "Physarum Entangler")
      (click-card state :runner ws)
      (run-on state :hq)
      (rez state :corp van)
      (run-continue state)
      (is (no-prompt? state :runner) "No Physarum Entangler prompt")
      (fire-subs state (refresh van))
      (run-on state :archives)
      (rez state :corp ws)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -2]
                    (click-prompt state :runner "Yes"))
          "Physarum Entangler cost was paid")
      (is (= :movement (:phase (get-run))) "Runner has bypassed Whitespace")
      (run-jack-out state)
      (take-credits state :runner)
      (is (changed? [(count (:discard (get-runner))) 2]
                    (purge state :corp))
          "Both Physarum Entanglers trashed on purge"))))

(deftest pichacao
  ;; Pichação
  (do-game
      (new-game {:runner {:hand [(qty "Pichação" 2)]}
                 :corp {:hand [(qty "Ice Wall" 2)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Pichação")
      (click-card state :runner (get-ice state :hq 0))
      (play-from-hand state :runner "Pichação")
      (click-card state :runner (get-ice state :hq 1))
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 1))
      (run-continue state)
      (is (changed? [(:click (get-runner)) 1]
            (run-continue state)
            (click-prompt state :runner "Yes"))
          "Gained 1 click")
      (run-continue state)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (changed? [(:click (get-runner)) 1]
            (run-continue state)
            (click-prompt state :runner "Yes"))
          "Gained 1 click")
      (is (changed? [(count (:hand (get-runner))) 1]
            (click-prompt state :runner "Yes"))
          "Pichação returned to the grip")))

(deftest plague
  ;; Plague
  (do-game
    (new-game {:corp {:deck ["Mark Yale"]}
               :runner {:deck ["Plague"]}})
    (play-from-hand state :corp "Mark Yale" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Plague")
    (click-prompt state :runner "Server 1")
    (let [plague (get-program state 0)]
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (is (= 2 (get-counters (refresh plague) :virus)) "Plague gained 2 counters")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (is (= 4 (get-counters (refresh plague) :virus)) "Plague gained 2 counters")
      (run-empty-server state "Archives")
      (is (= 4 (get-counters (refresh plague) :virus)) "Plague did not gain counters"))))

(deftest pressure-spike
  (do-game
    (new-game {:corp {:hand ["Eli 1.0" "Chiyashi" "Vanity Project"]
                      :credits 20}
               :runner {:hand ["Pressure Spike"]
                        :credits 10}})
    (play-from-hand state :corp "Eli 1.0" "Archives")
    (play-from-hand state :corp "Chiyashi" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Pressure Spike")
    (let [ps (get-program state 0)]
      (run-on state "Archives")
      (rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -4]
                    (core/play-dynamic-ability state :runner {:dynamic "auto-pump-and-break" :card (refresh ps)}))
          "Runner spent 4 credits to fully break Eli 1.0")
      (core/continue state :corp nil)
      (run-jack-out state)
      (take-credits state :runner)
      (play-and-score state "Vanity Project")
      (take-credits state :corp)
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -2
                     (get-strength (refresh ps)) 9]
                    (card-ability state :runner (refresh ps) 2))
          "Runner spent 2 credits to match ice strength")
      (card-ability state :runner (refresh ps) 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "Done"))))

(deftest pressure-spike-once-per-run-ability
  (do-game
    (new-game {:corp {:hand ["Chiyashi" "Vanity Project"]
                      :credits 20}
               :runner {:hand ["Pressure Spike"]
                        :credits 10}})
    (play-and-score state "Vanity Project")
    (play-from-hand state :corp "Chiyashi" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Pressure Spike")
    (let [ps (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -2
                     (get-strength (refresh ps)) 9]
            (card-ability state :runner (refresh ps) 2)
            ;; second pump shouldn't be allowed
            (card-ability state :runner (refresh ps) 2))
          "Runner spent 2 credits to match ice strength"))))

(deftest progenitor-hosting-hivemind-using-virus-breeding-ground-issue-738
    ;; Hosting Hivemind, using Virus Breeding Ground. Issue #738
    (do-game
      (new-game {:runner {:deck ["Progenitor" "Virus Breeding Ground" "Hivemind"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Progenitor")
      (play-from-hand state :runner "Virus Breeding Ground")
      (is (= 4 (core/available-mu state)))
      (let [prog (get-program state 0)
            vbg (get-resource state 0)]
        (card-ability state :runner prog 0)
        (click-card state :runner (find-card "Hivemind" (:hand (get-runner))))
        (is (= 4 (core/available-mu state)) "No memory used to host on Progenitor")
        (let [hive (first (:hosted (refresh prog)))]
          (is (= "Hivemind" (:title hive)) "Hivemind is hosted on Progenitor")
          (is (= 1 (get-counters hive :virus)) "Hivemind has 1 counter")
          (is (zero? (:credit (get-runner))) "Full cost to host on Progenitor")
          (take-credits state :runner 1)
          (take-credits state :corp)
          (card-ability state :runner vbg 0) ; use VBG to transfer 1 token to Hivemind
          (click-card state :runner hive)
          (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind gained 1 counter")
          (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter")))))

(deftest progenitor-keep-mu-the-same-when-hosting-or-trashing-hosted-programs
    ;; Keep MU the same when hosting or trashing hosted programs
    (do-game
      (new-game {:runner {:deck ["Progenitor" "Hivemind"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Progenitor")
      (let [pro (get-program state 0)]
        (card-ability state :runner pro 0)
        (click-card state :runner (find-card "Hivemind" (:hand (get-runner))))
        (is (= 2 (:click (get-runner))))
        (is (= 2 (:credit (get-runner))))
        (is (= 4 (core/available-mu state)) "Hivemind 2 MU not deducted from available MU")
        ;; Trash Hivemind
        (core/move state :runner (find-card "Hivemind" (:hosted (refresh pro))) :discard)
        (is (= 4 (core/available-mu state)) "Hivemind 2 MU not added to available MU"))))

(deftest propeller
  ;; counter: +2 str, 0 str start, 1c: break barrier
  (do-game
    (new-game {:runner {:hand ["Propeller"] :credits 10}
               :corp {:hand ["Ice Wall"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Propeller")
    (let [prop (get-program state 0)]
      (run-on state :hq)
      (run-continue state)
      (is (changed? [(get-counters (refresh prop) :power) -1]
            (card-ability state :runner prop 1)
            (is (= 2 (get-strength (refresh prop))) "At strength 2 after boost"))
          "Spent 1 power counter to boost")
      (is (changed? [(:credit (get-runner)) -1]
            (card-ability state :runner prop 0)
            (click-prompt state :runner "End the run"))
          "Spent 1 credit to break"))))

(deftest reaver
  ;; Reaver - Draw a card the first time you trash an installed card each turn
  (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:deck ["Reaver" (qty "Fall Guy" 5)]}})
      (starting-hand state :runner ["Reaver" "Fall Guy"])
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (core/gain state :runner :click 1)
      (play-from-hand state :runner "Reaver")
      (is (= 1 (count (:hand (get-runner)))) "One card in hand")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 4 [Credits] to trash") ; Trash PAD campaign
      (is (= 2 (count (:hand (get-runner)))) "Drew a card from trash of corp card")
      (play-from-hand state :runner "Fall Guy")
      (play-from-hand state :runner "Fall Guy")
      (is (zero? (count (:hand (get-runner)))) "No cards in hand")
      ; No draw from Fall Guy trash as Reaver already fired this turn
      (card-ability state :runner (get-resource state 0) 1)
      (is (zero? (count (:hand (get-runner)))) "No cards in hand")
      (take-credits state :runner)
      ; Draw from Fall Guy trash on corp turn
      (card-ability state :runner (get-resource state 0) 1)
      (is (= 1 (count (:hand (get-runner)))) "One card in hand")))

(deftest reaver-not-triggering-on-non-installed-cards
  ;; Reaver - Doesn't trigger when trashing non-installed cards
  (do-game
      (new-game {:corp {:hand [(qty "PAD Campaign" 2)]}
                 :runner {:hand ["Reaver"]
                          :deck [(qty "Fall Guy" 5)]
                          :credits 10}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Reaver")
      (run-empty-server state "HQ")
      (is (changed? [(count (:hand (get-runner))) 0]
            (click-prompt state :runner "Pay 4 [Credits] to trash"))
          "Drew 0 cards from Reaver")
      (run-empty-server state "Server 1")
      (is (changed? [(count (:hand (get-runner))) 1]
            (click-prompt state :runner "Pay 4 [Credits] to trash"))
          "Drew 1 card from Reaver")))

(deftest reaver-with-freelance-coding-construct-should-not-draw-when-trash-from-hand-2671
    ;; with Freelance Coding Construct - should not draw when trash from hand #2671
    (do-game
      (new-game {:runner {:deck [(qty "Reaver" 9) "Imp" "Snitch" "Freelance Coding Contract"]}})
      (starting-hand state :runner ["Reaver" "Imp" "Snitch" "Freelance Coding Contract"])
      (take-credits state :corp)
      (play-from-hand state :runner "Reaver")
      (is (= 3 (count (:hand (get-runner)))) "Four cards in hand")
      (is (= 3 (:credit (get-runner))) "3 credits")
      (play-from-hand state :runner "Freelance Coding Contract")
      (click-card state :runner "Snitch")
      (click-card state :runner "Imp")
      (click-prompt state :runner "Done")
      (is (= 7 (:credit (get-runner))) "7 credits - FCC fired")
      (is (zero? (count (:hand (get-runner)))) "No cards in hand")))

(deftest revolver-automatic-breaking
  (do-game
   (new-game {:corp {:hand ["Anansi"] :credits 10}
              :runner {:hand ["Revolver" "Sure Gamble"] :credits 10}})
   (play-from-hand state :corp "Anansi" "HQ")
   (take-credits state :corp)
   (play-from-hand state :runner "Revolver")
   (let [anansi (get-ice state :hq 0)
         revolver (get-program state 0)]
     (is (= 6 (get-counters revolver :power)) "Starts with 6 counters")
     (run-on state "HQ")
     (rez state :corp anansi)
     (run-continue state)
     ;; boost/break
     (is (changed?
           [(:credit (get-runner)) -4
            (get-counters (refresh revolver) :power) -3]
           (auto-pump-and-break state (refresh revolver))
           (core/continue state :corp nil))
         "Pumping changes stuff")
     (is (= 0 (count (:discard (get-runner)))) "0 cards in discard"))))

(deftest revolver-manual-breaking
  (do-game
   (new-game {:corp {:hand ["Anansi"] :credits 10}
              :runner {:hand ["Revolver" "Sure Gamble"] :credits 10}})
   (play-from-hand state :corp "Anansi" "HQ")
   (take-credits state :corp)
   (play-from-hand state :runner "Revolver")
   (let [anansi (get-ice state :hq 0)
         revolver (get-program state 0)]
     (is (= 6 (get-counters revolver :power)) "Starts with 6 counters")
     (run-on state "HQ")
     (rez state :corp anansi)
     (run-continue state)
     ;; boost
     (is (changed? [(:credit (get-runner)) -4]
           (card-ability state :runner revolver 2)
           (card-ability state :runner revolver 2))
         "Spent 4 credits matching Anansi strength")
     ;; break
     (is (changed? [(get-counters (refresh revolver) :power) -3]
           (card-ability state :runner revolver 0)
           (click-prompt state :runner "Do 1 net damage")
           (click-prompt state :runner "Rearrange the top 5 cards of R&D")
           (click-prompt state :runner "Draw 1 card, runner draws 1 card"))
         "Used 3 counters from revolver")
     (run-continue state :movement)
     (run-jack-out state)
     (is (= 0 (count (:discard (get-runner)))) "0 cards in discard"))))

(deftest revolver-trash-ability
  (do-game
   (new-game {:corp {:hand ["Anansi"] :credits 10}
              :runner {:hand ["Revolver" "Sure Gamble"] :credits 10}})
   (play-from-hand state :corp "Anansi" "HQ")
   (take-credits state :corp)
   (play-from-hand state :runner "Revolver")
   (let [anansi (get-ice state :hq 0)
         revolver (get-program state 0)]
     (is (= 6 (get-counters revolver :power)) "Starts with 6 counters")
     (run-on state "HQ")
     (rez state :corp anansi)
     (run-continue state)
     ;; boost
     (is (changed? [(:credit (get-runner)) -4]
           (card-ability state :runner revolver 2)
           (card-ability state :runner revolver 2))
         "Spent 4 credits matching Anansi strength")
     ;; break with counters
     (is (changed? [(get-counters (refresh revolver) :power) -2]
           (card-ability state :runner revolver 0)
           (click-prompt state :runner "Do 1 net damage")
           (click-prompt state :runner "Rearrange the top 5 cards of R&D")
           (click-prompt state :runner "Done"))
         "Used 2 counters from revolver")
     (is (changed? [(count (:discard (get-runner))) 1]
           (card-ability state :runner revolver 1)
           (click-prompt state :runner "Draw 1 card, runner draws 1 card"))
         "One card added to discard")
     (run-continue state :movement)
     (run-jack-out state)
     (is (= 1 (count (:discard (get-runner)))) "1 cards (revolver) in discard"))))

(deftest rezeki
  ;; Rezeki - gain 1c when turn begins
  (do-game
    (new-game {:runner {:deck ["Rezeki"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Rezeki")
    (take-credits state :runner)
    (let [credits (:credit (get-runner))]
      (take-credits state :corp)
      (is (= (:credit (get-runner)) (+ credits 1)) "Gain 1 from Rezeki"))))

(deftest rng-key-basic-behaviour-first-successful-run-on-rd-hq-guess-a-number-gain-credits-or-cards-if-number-matches-card-cost
    ;; Basic behaviour - first successful run on RD/HQ, guess a number, gain credits or cards if number matches card cost
    (do-game
      (new-game {:corp {:deck [(qty "Enigma" 5) "Hedge Fund"]}
                 :runner {:deck ["RNG Key" (qty "Paperclip" 2)]}})
      (starting-hand state :corp ["Hedge Fund"])
      (starting-hand state :runner ["RNG Key"])
      (take-credits state :corp)
      (testing "Gain 3 credits"
        (play-from-hand state :runner "RNG Key")
        (is (= 5 (:credit (get-runner))) "Starts at 5 credits")
        (run-empty-server state "HQ")
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "5")
        (click-prompt state :runner "Gain 3 [Credits]")
        (is (= 8 (:credit (get-runner))) "Gained 3 credits")
        (click-prompt state :runner "No action"))
      (testing "Do not trigger on second successful run"
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (take-credits state :runner)
        (take-credits state :corp))
      (testing "Do not trigger on archives"
        (run-empty-server state "Archives")
        (take-credits state :runner)
        (take-credits state :corp))
      (testing "Do not get choice if trigger declined"
        (run-empty-server state "R&D")
        (click-prompt state :runner "No")
        (click-prompt state :runner "No action"))
      (run-empty-server state "R&D")
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (take-credits state :corp)
      (testing "Do not gain credits / cards if guess incorrect"
        (run-empty-server state "R&D")
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "2")
        (click-prompt state :runner "No action"))
      (take-credits state :runner)
      (take-credits state :corp)
      (testing "Gain 2 cards"
        (is (zero? (count (:hand (get-runner)))) "Started with 0 cards")
        (run-empty-server state "R&D")
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "3")
        (click-prompt state :runner "Draw 2 cards")
        (click-prompt state :runner "No action")
        (is (= 2 (count (:hand (get-runner)))) "Gained 2 cards")
        (is (zero? (count (:deck (get-runner)))) "Cards came from stack"))))

(deftest rng-key-pays-out-when-accessing-an-upgrade-issue-4804
    ;; Pays out when accessing an Upgrade. Issue #4804
    (do-game
      (new-game {:corp {:deck ["Hokusai Grid" "Hedge Fund"]}
                 :runner {:deck ["RNG Key"]}})
      (play-from-hand state :corp "Hokusai Grid" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "RNG Key")
      (is (= 5 (:credit (get-runner))) "Starts at 5 credits")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "2")
      (click-prompt state :runner "Unrezzed upgrade")
      (is (= "Choose one" (:msg (prompt-map :runner))) "Runner gets RNG Key reward")))

(deftest rng-key-works-when-running-a-different-server-first-5292
    ;; Works when running a different server first #5292
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hokusai Grid" "Hedge Fund"]}
                 :runner {:deck ["RNG Key"]}})
      (play-from-hand state :corp "Hokusai Grid" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "RNG Key")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "5")
      (is (= "Choose one" (:msg (prompt-map :runner))) "Runner gets RNG Key reward")))

(deftest rng-key-works-after-running-vs-crisium-grid-3772
    ;; Works after running vs Crisium Grid #3772
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Crisium Grid" "Hedge Fund"]
                        :credits 10}
                 :runner {:hand ["RNG Key"]
                          :credits 10}})
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (rez state :corp (get-content state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "RNG Key")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Crisium Grid")
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (click-prompt state :runner "No action")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "5")
      (is (= "Choose one" (:msg (prompt-map :runner))) "Runner gets RNG Key reward")))

(deftest saci
    (do-game
      (new-game {:corp {:hand ["Ice Wall"]}
                 :runner {:hand ["Saci" "\"Baklan\" Bochkin"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (let [iw (get-ice state :hq 0)]
        (play-from-hand state :runner "\"Baklan\" Bochkin")
        (play-from-hand state :runner "Saci")
        (click-card state :runner iw)
        (run-on state "HQ")
        (is (changed? [(:credit (get-runner)) 3]
              (rez state :corp iw))
            "Gained 3 credits on rez")
        (run-continue state)
        (is (changed? [(:credit (get-runner)) 3]
              (card-ability state :runner (get-resource state 0) 0))
            "Gained 3 credits on derez"))))

(deftest saci-magnet
    ;; Saci should not trigger on Magent rez but should on Magnet derez
    (do-game
      (new-game {:corp {:hand ["Magnet" "Ice Wall" "Divert Power"]}
                 :runner {:hand ["Saci"]}})
      (play-from-hand state :corp "Magnet" "R&D")
      (play-from-hand state :corp "Ice Wall" "Archives")
      (take-credits state :corp)
      (let [magnet (get-ice state :rd 0)]
        (play-from-hand state :runner "Saci")
        (click-card state :runner magnet)
        (run-on state "R&D")
        (is (changed? [(:credit (get-runner)) 0]
              (rez state :corp magnet))
            "Should not gain 3 credits on rez")
        (run-continue state)
        (card-subroutine state :corp (refresh magnet) 0)
        (take-credits state :runner)
        (play-from-hand state :corp "Divert Power")
        (is (changed? [(:credit (get-runner)) 3]
              (click-card state :corp magnet))
            "Gained 3 credits on derez"))))

(deftest sadyojata-swap-ability
    ;; Swap ability
    (testing "Doesnt work if no Deva in hand"
      (do-game
        (new-game {:runner {:hand ["Sadyojata" "Corroder"]
                            :credits 10}})
        (take-credits state :corp)
        (play-from-hand state :runner "Sadyojata")
        (card-ability state :runner (get-program state 0) 2)
        (is (no-prompt? state :runner) "No select prompt as there's no Deva in hand")))
    (testing "Works with another deva in hand"
      (doseq [deva ["Aghora" "Sadyojata" "Vamadeva"]]
        (do-game
          (new-game {:runner {:hand ["Sadyojata" deva]
                              :credits 10}})
          (take-credits state :corp)
          (play-from-hand state :runner "Sadyojata")
          (let [installed-deva (get-program state 0)]
            (card-ability state :runner installed-deva 2)
            (click-card state :runner (first (:hand (get-runner))))
            (is (not (utils/same-card? installed-deva (get-program state 0)))
                "Installed deva is not same as previous deva")
            (is (= deva (:title (get-program state 0)))))))))

(deftest sadyojata-break-ability-targets-ice-with-3-or-more-subtypes
    ;; Break ability targets ice with 3 or more subtypes
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Executive Functioning"]
                        :credits 10}
                 :runner {:hand ["Sadyojata"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Executive Functioning" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Sadyojata")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (is (no-prompt? state :runner) "No break prompt")
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state "R&D")
      (rez state :corp (get-ice state :rd 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (is (not (no-prompt? state :runner)) "Have a break prompt")
      (click-prompt state :runner "Trace 4 - Do 1 core damage")
      (is (:broken (first (:subroutines (get-ice state :rd 0))))
          "The break ability worked")))

(deftest sage
  ;; Sage
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall" "Enigma"]
                      :credits 10}
               :runner {:deck ["Sage" "Box-E"]
                        :credits 20}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Enigma" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Sage")
    (let [sage (get-program state 0)
          iw (get-ice state :hq 0)
          engima (get-ice state :rd 0)]
      (is (= 2 (core/available-mu state)))
      (is (= 2 (get-strength (refresh sage))) "+2 strength for 2 unused MU")
      (play-from-hand state :runner "Box-E")
      (is (= 4 (core/available-mu state)))
      (is (= 4 (get-strength (refresh sage))) "+4 strength for 4 unused MU")
      (run-on state :hq)
      (rez state :corp iw)
      (run-continue state)
      (card-ability state :runner (refresh sage) 0)
      (click-prompt state :runner "End the run")
      (is (:broken (first (:subroutines (refresh iw)))) "Broke a barrier subroutine")
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state :rd)
      (rez state :corp engima)
      (run-continue state)
      (card-ability state :runner (refresh sage) 0)
      (click-prompt state :runner "Force the Runner to lose [Click]")
      (click-prompt state :runner "Done")
      (is (:broken (first (:subroutines (refresh engima)))) "Broke a code gate subroutine"))))

(deftest sahasrara-pay-credits-prompt
  ;; Pay-credits prompt
    (do-game
    (new-game {:runner {:deck ["Sahasrara" "Equivocation"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sahasrara")
    (let [rara (get-program state 0)]
        (is (changed? [(:credit (get-runner)) 0]
              (play-from-hand state :runner "Equivocation")
              (click-card state :runner rara)
              (click-card state :runner rara))
            "Used 2 credits from Sahasrara"))))

(deftest savant
  ;; Savant
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Cobra" "Enigma" "Quandary"]
                      :credits 10}
               :runner {:deck ["Savant" "Box-E"]
                        :credits 20}})
    (play-from-hand state :corp "Cobra" "HQ")
    (play-from-hand state :corp "Quandary" "R&D")
    (play-from-hand state :corp "Enigma" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Savant")
    (let [savant (get-program state 0)
          cobra (get-ice state :hq 0)
          quandary (get-ice state :rd 0)
          enigma (get-ice state :rd 1)]
      (is (= 2 (core/available-mu state)))
      (is (= 3 (get-strength (refresh savant))) "+2 strength for 2 unused MU")
      (play-from-hand state :runner "Box-E")
      (is (= 4 (core/available-mu state)))
      (is (= 5 (get-strength (refresh savant))) "+4 strength for 4 unused MU")
      (run-on state :hq)
      (rez state :corp cobra)
      (run-continue state)
      (card-ability state :runner (refresh savant) 0)
      (click-prompt state :runner "Trash a program")
      (click-prompt state :runner "Done")
      (is (:broken (first (:subroutines (refresh cobra)))) "Broke a sentry subroutine")
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state :rd)
      (rez state :corp enigma)
      (run-continue state)
      (card-ability state :runner (refresh savant) 0)
      (click-prompt state :runner "Force the Runner to lose [Click]")
      (click-prompt state :runner "End the run")
      (is (:broken (first (:subroutines (refresh enigma)))) "Broke code gate first subroutine")
      (is (:broken (last (:subroutines (refresh enigma)))) "Broke code gate second subroutine")
      (run-continue state :movement)
      (run-continue state :approach-ice)
      (rez state :corp quandary)
      (run-continue state)
      (card-ability state :runner (refresh savant) 0)
      (click-prompt state :runner "End the run")
      (is (:broken (first (:subroutines (refresh quandary)))) "Broke a code gate subroutine on a single-sub ice"))))

(deftest scheherazade
  ;; Scheherazade - Gain 1 credit when it hosts a program
  (do-game
    (new-game {:runner {:deck ["Scheherazade" "Cache"
                               "Inti" "Fall Guy"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Scheherazade")
    (let [sch (get-program state 0)]
      (card-ability state :runner sch 0)
      (click-card state :runner (find-card "Inti" (:hand (get-runner))))
      (is (= 1 (count (:hosted (refresh sch)))))
      (is (= 2 (:click (get-runner))) "Spent 1 click to install and host")
      (is (= 6 (:credit (get-runner))) "Gained 1 credit")
      (is (= 3 (core/available-mu state)) "Programs hosted on Scheh consume MU")
      (card-ability state :runner sch 0)
      (click-card state :runner (find-card "Cache" (:hand (get-runner))))
      (is (= 2 (count (:hosted (refresh sch)))))
      (is (= 6 (:credit (get-runner))) "Gained 1 credit")
      (card-ability state :runner sch 0)
      (click-card state :runner (find-card "Fall Guy" (:hand (get-runner))))
      (is (= 2 (count (:hosted (refresh sch)))) "Can't host non-program")
      (is (= 1 (count (:hand (get-runner))))))))

(deftest self-modifying-code-trash-pay-2-to-search-deck-for-a-program-and-install-it-shuffle
    ;; Trash & pay 2 to search deck for a program and install it. Shuffle
    (do-game
      (new-game {:runner {:deck [(qty "Self-modifying Code" 3) "Reaver"]}})
      (starting-hand state :runner ["Self-modifying Code" "Self-modifying Code"])
      (core/gain state :runner :credit 5)
      (take-credits state :corp)
      (play-from-hand state :runner "Self-modifying Code")
      (play-from-hand state :runner "Self-modifying Code")
      (let [smc1 (get-program state 0)
            smc2 (get-program state 1)]
        (card-ability state :runner smc1 0)
        (click-prompt state :runner (find-card "Reaver" (:deck (get-runner))))
        (is (= 6 (:credit (get-runner))) "Paid 2 for SMC, 2 for install - 6 credits left")
        (is (= 1 (core/available-mu state)) "SMC MU refunded")
        (take-credits state :runner)
        (take-credits state :corp)
        (card-ability state :runner smc2 0)
        (is (= 1 (count (:hand (get-runner)))) "1 card drawn due to Reaver before SMC program selection")
        (is (zero? (count (:deck (get-runner)))) "Deck empty"))))

(deftest self-modifying-code-pay-for-smc-with-multithreader-issue-4961
    ;; Pay for SMC with Multithreader. Issue #4961
    (do-game
      (new-game {:runner {:deck [(qty "Self-modifying Code" 3) "Rezeki"]
                          :hand ["Self-modifying Code" "Multithreader"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Self-modifying Code")
      (play-from-hand state :runner "Multithreader")
      (let [smc (get-program state 0)
            multi (get-program state 1)]
        (card-ability state :runner smc 0)
        (click-card state :runner multi)
        (click-card state :runner multi)
        (click-prompt state :runner (find-card "Rezeki" (:deck (get-runner))))
        (is (= "Rezeki" (:title (get-program state 1))) "Rezeki is installed")
        (is (= 0 (:credit (get-runner))) "Runner had 2 credits before SMC, paid 2 for SMC from Multithreader, 2 for Rezeki install - 0 credits left"))))

(deftest shibboleth
    (do-game
      (new-game {:runner {:hand ["Shibboleth"]}
                 :corp {:hand ["Magnet" (qty "Project Vitruvius" 2)]
                        :credits 10}})
      (play-and-score state "Project Vitruvius")
      (play-from-hand state :corp "Magnet" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Shibboleth")
      (let [shi (get-program state 0)]
        (run-on state "HQ")
        (run-continue state)
        (is (changed? [(:credit (get-runner)) -1]
              (auto-pump-and-break state (refresh shi)))
            "No need to pump strength")
        (core/continue state :corp nil)
        (run-jack-out state)
        (take-credits state :runner)
        (is (changed? [(get-strength (refresh shi)) -2]
              (play-and-score state "Project Vitruvius"))
            "Strength gets reduced by threat")
        (take-credits state :corp)
        (run-on state "HQ")
        (run-continue state)
        (is (changed? [(:credit (get-runner)) -3]
              (auto-pump-and-break state (refresh shi)))
            "Pump strength and break sub"))))

(deftest shiv
  ;; Shiv - Gain 1 strength for each installed breaker; no MU cost when 2+ link
  (do-game
    (new-game {:runner {:id "Nasir Meidan: Cyber Explorer"
                        :deck ["Shiv" (qty "Inti" 2)
                               "Access to Globalsec"]}})
    (is (= 1 (get-link state)) "1 link")
    (take-credits state :corp)
    (play-from-hand state :runner "Shiv")
    (let [shiv (get-program state 0)]
      (is (= 1 (get-strength (refresh shiv))) "1 installed breaker; 1 strength")
      (play-from-hand state :runner "Inti")
      (is (= 2 (get-strength (refresh shiv))) "2 installed breakers; 2 strength")
      (play-from-hand state :runner "Inti")
      (is (= 3 (get-strength (refresh shiv))) "3 installed breakers; 3 strength")
      (is (= 1 (core/available-mu state)) "3 MU consumed")
      (play-from-hand state :runner "Access to Globalsec")
      (is (= 2 (get-link state)) "2 link")
      (is (= 2 (core/available-mu state)) "Shiv stops using MU when 2+ link"))))

(deftest slap-vandal
  (do-game
    (new-game {:runner {:hand ["Slap Vandal"]}
               :corp {:hand ["Tithe"]}})
    (play-from-hand state :corp "Tithe" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Slap Vandal")
    (click-card state :runner (get-ice state :hq 0))
    (let [tithe (get-ice state :hq 0)
          slap (first (:hosted (refresh tithe)))]
      (run-on state "HQ")
      (rez state :corp tithe)
      (run-continue state)
      (card-ability state :runner (refresh slap) 0)
      (click-prompt state :runner "Do 1 net damage")
      (is (changed? [(:credit (get-corp)) 1]
            (fire-subs state (refresh tithe)))
          "Gained 1 credit from unbroken sub"))))

(deftest sneakdoor-beta-gabriel-santiago-ash-on-hq-should-prevent-sneakdoor-hq-access-but-still-give-gabe-credits-issue-1138
    ;; Gabriel Santiago, Ash on HQ should prevent Sneakdoor HQ access but still give Gabe credits. Issue #1138.
    (do-game
      (new-game {:corp {:deck ["Ash 2X3ZB9CY"]}
                 :runner {:id "Gabriel Santiago: Consummate Professional"
                          :deck ["Sneakdoor Beta"]}})
      (play-from-hand state :corp "Ash 2X3ZB9CY" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Sneakdoor Beta")
      (is (= 1 (:credit (get-runner))) "Sneakdoor cost 4 credits")
      (let [sb (get-program state 0)
            ash (get-content state :hq 0)]
        (rez state :corp ash)
        (card-ability state :runner sb 0)
        (run-continue state)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 3 (:credit (get-runner))) "Gained 2 credits from Gabe's ability")
        (is (= (:cid ash) (-> (prompt-map :runner) :card :cid)) "Ash interrupted HQ access after Sneakdoor run")
        (is (= :hq (-> (get-runner) :register :successful-run first)) "Successful Run on HQ recorded"))))

(deftest sneakdoor-beta-crisium-grid-on-hq-should-prevent-gabriel-gaining-credits
    ;; Crisium grid on HQ should prevent Gabriel gaining credits
    (do-game
      (new-game {:corp {:deck ["Crisium Grid"]}
                 :runner {:id "Gabriel Santiago: Consummate Professional"
                          :deck ["Sneakdoor Beta"]}})
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Sneakdoor Beta")
      (is (= 1 (:credit (get-runner))) "Sneakdoor cost 4 credits")
      (let [sb (get-program state 0)
            crisium (get-content state :hq 0)]
        (rez state :corp crisium)
        (card-ability state :runner sb 0)
        (run-continue state)
        (is (= 1 (:credit (get-runner))) "Did not gain 2 credits from Gabe's ability")
        (is (not= :hq (-> (get-runner) :register :successful-run first)) "Successful Run on HQ was not recorded"))))

(deftest sneakdoor-beta-do-not-switch-to-hq-if-archives-has-crisium-grid-issue-1229
    ;; do not switch to HQ if Archives has Crisium Grid. Issue #1229.
    (do-game
      (new-game {:corp {:deck ["Crisium Grid" "Priority Requisition" "Private Security Force"]}
                 :runner {:deck ["Sneakdoor Beta"]}})
      (play-from-hand state :corp "Crisium Grid" "Archives")
      (trash-from-hand state :corp "Priority Requisition")
      (take-credits state :corp)
      (play-from-hand state :runner "Sneakdoor Beta")
      (let [sb (get-program state 0)
            cr (get-content state :archives 0)]
        (rez state :corp cr)
        (card-ability state :runner sb 0)
        (run-continue state)
        (is (= :archives (get-in @state [:run :server 0])) "Crisium Grid stopped Sneakdoor Beta from switching to HQ"))))

(deftest sneakdoor-beta-allow-nerve-agent-to-gain-counters-issue-1158-955
    ;; Allow Nerve Agent to gain counters. Issue #1158/#955
    (do-game
      (new-game {:runner {:deck ["Sneakdoor Beta" "Nerve Agent"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Nerve Agent")
      (play-from-hand state :runner "Sneakdoor Beta")
      (let [nerve (get-program state 0)
            sb (get-program state 1)]
        (card-ability state :runner sb 0)
        (run-continue state)
        (click-prompt state :runner "No action")
        (is (= 1 (get-counters (refresh nerve) :virus)))
        (card-ability state :runner sb 0)
        (run-continue state)
        (is (= 2 (get-counters (refresh nerve) :virus))))))

(deftest sneakdoor-beta-grant-security-testing-credits-on-hq
    ;; Grant Security Testing credits on HQ.
    (do-game
      (new-game {:runner {:deck ["Security Testing" "Sneakdoor Beta"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Sneakdoor Beta")
      (play-from-hand state :runner "Security Testing")
      (take-credits state :runner)
      (is (= 3 (:credit (get-runner))))
      (take-credits state :corp)
      (let [sb (get-program state 0)]
        (click-prompt state :runner "HQ")
        (card-ability state :runner sb 0)
        (run-continue state)
        (is (not (:run @state)) "Switched to HQ and ended the run from Security Testing")
        (is (= 5 (:credit (get-runner))) "Sneakdoor switched to HQ and earned Security Testing credits"))))

(deftest sneakdoor-beta-deflected-successful-run
  ;;sneakdoor beta should not access hq when deflected from archives
  (do-game
   (new-game {:corp {:hand ["Mind Game"] :deck ["NGO Front"]}
              :runner {:hand ["Sneakdoor Beta"]}})
   (play-from-hand state :corp "Mind Game" "Archives")
   (let [mindgame (get-ice state :archives 0)]
     (rez state :corp mindgame)
     (take-credits state :corp)
     (play-from-hand state :runner "Sneakdoor Beta")
     (let [sb (get-program state 0)]
       (card-ability state :runner sb 0)
       (run-continue state)
       (fire-subs state (refresh mindgame))
       (click-prompt state :corp "1 [Credits]")
       (click-prompt state :runner "0 [Credits]")
       (click-prompt state :corp "R&D")
       (click-prompt state :runner "No")
       (is (= :rd (get-in @state [:run :server 0])) "Run continues on R&D")
       (run-continue state)
       (is (= :rd (get-in @state [:run :server 0])) "Run continues on R&D (not HQ)")))))

(deftest sneakdoor-beta-sneakdoor-beta-trashed-during-run
    ;; Sneakdoor Beta trashed during run
    (do-game
      (new-game {:runner {:hand ["Sneakdoor Beta"]}
                 :corp {:hand ["Ichi 1.0" "Hedge Fund"]}})
      (play-from-hand state :corp "Ichi 1.0" "Archives")
      (let [ichi (get-ice state :archives 0)]
        (rez state :corp ichi)
        (take-credits state :corp)
        (play-from-hand state :runner "Sneakdoor Beta")
        (let [sb (get-program state 0)]
          (card-ability state :runner sb 0)
          (run-continue state)
          (fire-subs state (refresh ichi))
          (is (= :select (prompt-type :corp)) "Corp has a prompt to choose program to delete")
          (click-card state :corp "Sneakdoor Beta")
          (click-prompt state :corp "Done")
          (is (= "Sneakdoor Beta" (-> (get-runner) :discard first :title)) "Sneakdoor was trashed")
          (click-prompt state :corp "0")
          (click-prompt state :runner "1")
          (run-continue state)
          (run-continue state)
          (is (= :hq (get-in @state [:run :server 0])) "Run continues on HQ")))))

(deftest sneakdoor-beta-sneakdoor-beta-effect-ends-at-end-of-run
    ;; Sneakdoor Beta effect ends at end of run
    (do-game
      (new-game {:runner {:hand ["Sneakdoor Beta"]}
                 :corp {:hand ["Rototurret" "Hedge Fund"]
                        :discard ["Send a Message"]}})
      (play-from-hand state :corp "Rototurret" "Archives")
      (let [roto (get-ice state :archives 0)]
        (rez state :corp roto)
        (take-credits state :corp)
        (play-from-hand state :runner "Sneakdoor Beta")
        (let [sb (get-program state 0)]
          (card-ability state :runner sb 0)
          (run-continue state)
          (fire-subs state (refresh roto))
          (is (= :select (prompt-type :corp)) "Corp has a prompt to choose a program to trash")
          (click-card state :corp "Sneakdoor Beta")
          (is (= "Sneakdoor Beta" (-> (get-runner) :discard first :title)) "Sneakdoor was trashed")
          (run-on state "Archives")
          (run-continue state)
          (run-continue state :movement)
          (run-continue state :success)
          (is (= :archives (get-in @state [:run :server 0])) "Run continues on Archives")))))

(deftest sneakdoor-prime-a
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "PAD Campaign" 3)]}
               :runner {:hand ["Sneakdoor Prime A"]
                        :credits 10}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Sneakdoor Prime A")
    (card-ability state :runner (get-program state 0) 0)
    (is (= ["Server 1" "Server 2" "Cancel"] (prompt-buttons :runner)) "Only remotes available")
    (click-prompt state :runner "Server 1")
    (run-continue state)
    (is (= ["Archives" "R&D" "HQ"] (prompt-buttons :runner)) "Only centrals available")
    (click-prompt state :runner "HQ")
    (is (= :hq (get-in @state [:run :server 0])) "Run continues on HQ")
    (is (= ["Pay 4 [Credits] to trash" "No action"] (prompt-buttons :runner)) "Runner accessing card in HQ")))

(deftest sneakdoor-prime-b
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "PAD Campaign" 2)]}
               :runner {:hand ["Sneakdoor Prime B"]
                        :credits 10}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Sneakdoor Prime B")
    (card-ability state :runner (get-program state 0) 0)
    (is (= ["Archives" "R&D" "HQ" "Cancel"] (prompt-buttons :runner)) "Only centrals available")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (is (= ["Server 1" "Server 2" "Cancel"] (prompt-buttons :runner)) "Only remotes available")
    (click-prompt state :runner "Server 1")
    (is (= :remote1 (get-in @state [:run :server 0])) "Run continues on Server 1")
    (is (= ["Pay 4 [Credits] to trash" "No action"] (prompt-buttons :runner)) "Runner accessing card in Server 1")))

(deftest snitch-only-works-on-rezzed-ice
    ;; Only works on rezzed ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Quandary"]}
                 :runner {:deck ["Snitch"]}})
      (play-from-hand state :corp "Quandary" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Snitch")
      (let [snitch (get-program state 0)]
        (run-on state "R&D")
        (is (prompt-is-card? state :runner snitch) "Option to expose")
        (is (= "Expose approached piece of ice?" (:msg (prompt-map :runner))))
        (click-prompt state :runner "Yes")
        (is (= "Jack out?" (:msg (prompt-map :runner))))
        (click-prompt state :runner "Yes"))))

(deftest snitch-doesn-t-work-if-ice-is-rezzed
    ;; Doesn't work if ice is rezzed
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Quandary"]}
                 :runner {:deck ["Snitch"]}})
      (play-from-hand state :corp "Quandary" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Snitch")
      (run-on state "HQ")
      (is (no-prompt? state :runner) "No option to jack out")
      (run-continue-until state :movement)
      (run-jack-out state)))

(deftest snitch-doesn-t-work-if-there-is-no-ice
    ;; Doesn't work if there is no ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}
                 :runner {:deck ["Snitch"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Snitch")
      (run-on state "Archives")
      (is (no-prompt? state :runner) "No option to jack out")
      (run-continue state)))

(deftest snowball-strength-boost-until-end-of-run-when-used-to-break-a-subroutine
    ;; Strength boost until end of run when used to break a subroutine
    (do-game
      (new-game {:corp {:deck ["Spiderweb" "Fire Wall" "Hedge Fund"]
                        :credits 20}
                 :runner {:deck ["Snowball"]}})
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Fire Wall" "HQ")
      (play-from-hand state :corp "Spiderweb" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (core/gain state :corp :credit 1)
      (play-from-hand state :runner "Snowball")
      (let [sp (get-ice state :hq 1)
            fw (get-ice state :hq 0)
            snow (get-program state 0)]
        (run-on state "HQ")
        (rez state :corp sp)
        (run-continue state)
        (card-ability state :runner snow 1) ; match strength
        (is (= 2 (get-strength (refresh snow))))
        (card-ability state :runner snow 0) ; strength matched, break a sub
        (click-prompt state :runner "End the run")
        (click-prompt state :runner "End the run")
        (click-prompt state :runner "End the run")
        (is (= 5 (get-strength (refresh snow))) "Broke 3 subs, gained 3 more strength")
        (run-continue-until state :approach-ice fw)
        (rez state :corp fw)
        (run-continue state)
        (is (= 4 (get-strength (refresh snow))) "Has +3 strength until end of run; lost 1 per-encounter boost")
        (card-ability state :runner snow 1) ; match strength
        (is (= 5 (get-strength (refresh snow))) "Matched strength, gained 1")
        (card-ability state :runner snow 0) ; strength matched, break a sub
        (click-prompt state :runner "End the run")
        (is (= 6 (get-strength (refresh snow))) "Broke 1 sub, gained 1 more strength")
        (run-continue state)
        (is (= 5 (get-strength (refresh snow))) "+4 until-end-of-run strength")
        (run-jack-out state)
        (is (= 1 (get-strength (refresh snow))) "Back to default strength"))))

(deftest snowball-strength-boost-until-end-of-run-when-using-dynamic-auto-pump-and-break-ability
    ;; Strength boost until end of run when using dynamic auto-pump-and-break ability
    (do-game
      (new-game {:corp {:deck ["Spiderweb" (qty "Hedge Fund" 2)]}
                 :runner {:deck ["Snowball"]}})
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Spiderweb" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Snowball")
      (let [sp (get-ice state :hq 0)
            snow (get-program state 0)]
        (run-on state "HQ")
        (rez state :corp sp)
        (run-continue state)
        (is (= 1 (get-strength (refresh snow))) "Snowball starts at 1 strength")
        (auto-pump-and-break state (refresh snow))
        (is (= 5 (get-strength (refresh snow))) "Snowball was pumped once and gained 3 strength from breaking")
        (core/process-action "continue" state :corp nil)
        (is (= 4 (get-strength (refresh snow))) "+3 until-end-of-run strength"))))

(deftest stargate
  ;; Stargate
  (do-game
     (new-game {:corp {:deck [(qty "Ice Wall" 10)]
                       :hand ["Herald" "Troll"]}
                :runner {:deck ["Stargate"]}})
     (core/move state :corp (find-card "Herald" (:hand (get-corp))) :deck {:front true})
     (core/move state :corp (find-card "Troll" (:hand (get-corp))) :deck {:front true})
     (is (= "Troll" (-> (get-corp) :deck first :title)) "Troll on top of deck")
     (is (= "Herald" (-> (get-corp) :deck second :title)) "Herald 2nd")
     (take-credits state :corp)
     (play-from-hand state :runner "Stargate")
     (card-ability state :runner (get-program state 0) 0)
     (is (:run @state) "Run initiated")
     (run-continue state)
     (click-prompt state :runner "Troll")
     (is (no-prompt? state :runner) "Prompt closed")
     (is (not (:run @state)) "Run ended")
     (is (-> (get-corp) :discard first :seen) "Troll is faceup")
     (is (= "Troll" (-> (get-corp) :discard first :title)) "Troll was trashed")
     (is (= "Herald" (-> (get-corp) :deck first :title)) "Herald now on top of R&D")
     (card-ability state :runner (get-program state 0) 0)
     (is (not (:run @state)) "No run ended, as Stargate is once per turn")))

(deftest stargate-different-message-on-repeating-cards
    ;; Different message on repeating cards
    (testing "bottom card"
      (do-game
        (new-game {:corp {:deck [(qty "Troll" 10)]
                          :hand ["Herald" "Troll"]}
                   :runner {:deck ["Stargate"]}})
        (core/move state :corp (find-card "Herald" (:hand (get-corp))) :deck {:front true})
        (core/move state :corp (find-card "Troll" (:hand (get-corp))) :deck {:front true})
        (is (= "Troll" (-> (get-corp) :deck first :title)) "Troll 1st")
        (is (= "Herald" (-> (get-corp) :deck second :title)) "Herald 2nd")
        (is (= "Troll" (-> (get-corp) :deck (#(nth % 2)) :title)) "Another Troll 3rd")
        (take-credits state :corp)
        (play-from-hand state :runner "Stargate")
        (card-ability state :runner (get-program state 0) 0)
        (run-continue state)
        (click-prompt state :runner (nth (prompt-buttons :runner) 2))
        (is (last-log-contains? state "Runner uses Stargate to trash bottom Troll.") "Correct log")))
    (testing "middle card"
      (do-game
        (new-game {:corp {:deck [(qty "Troll" 10)]
                          :hand ["Herald" "Troll"]}
                   :runner {:deck ["Stargate"]}})
        (core/move state :corp (find-card "Troll" (:hand (get-corp))) :deck {:front true})
        (core/move state :corp (find-card "Herald" (:hand (get-corp))) :deck {:front true})
        (is (= "Herald" (-> (get-corp) :deck first :title)) "Herald 1st")
        (is (= "Troll" (-> (get-corp) :deck second :title)) "Troll 2nd")
        (is (= "Troll" (-> (get-corp) :deck (#(nth % 2)) :title)) "Another Troll 3rd")
        (take-credits state :corp)
        (play-from-hand state :runner "Stargate")
        (card-ability state :runner (get-program state 0) 0)
        (run-continue state)
        (click-prompt state :runner (second (prompt-buttons :runner)))
        (is (last-log-contains? state "Runner uses Stargate to trash middle Troll.") "Correct log")))
    (testing "top card"
      (do-game
        (new-game {:corp {:deck [(qty "Troll" 10)]
                          :hand ["Herald" "Troll"]}
                   :runner {:deck ["Stargate"]}})
        (core/move state :corp (find-card "Herald" (:hand (get-corp))) :deck {:front true})
        (core/move state :corp (find-card "Troll" (:hand (get-corp))) :deck {:front true})
        (is (= "Troll" (-> (get-corp) :deck first :title)) "Troll 1st")
        (is (= "Herald" (-> (get-corp) :deck second :title)) "Herald 2nd")
        (is (= "Troll" (-> (get-corp) :deck (#(nth % 2)) :title)) "Another Troll 3rd")
        (take-credits state :corp)
        (play-from-hand state :runner "Stargate")
        (card-ability state :runner (get-program state 0) 0)
        (run-continue state)
        (click-prompt state :runner (first (prompt-buttons :runner)))
        (is (last-log-contains? state "Runner uses Stargate to trash top Troll.") "Correct log"))))

(deftest stargate-no-position-indicator-if-non-duplicate-selected
    ;; No position indicator if non-duplicate selected
    (do-game
      (new-game {:corp {:deck [(qty "Troll" 10)]
                        :hand ["Herald" "Troll"]}
                 :runner {:deck ["Stargate"]}})
      (core/move state :corp (find-card "Herald" (:hand (get-corp))) :deck {:front true})
      (core/move state :corp (find-card "Troll" (:hand (get-corp))) :deck {:front true})
      (is (= "Troll" (-> (get-corp) :deck first :title)) "Troll 1st")
      (is (= "Herald" (-> (get-corp) :deck second :title)) "Herald 2nd")
      (is (= "Troll" (-> (get-corp) :deck (#(nth % 2)) :title)) "Another Troll 3rd")
      (take-credits state :corp)
      (play-from-hand state :runner "Stargate")
      (card-ability state :runner (get-program state 0) 0)
      (run-continue state)
      (click-prompt state :runner (second (prompt-buttons :runner)))
      (is (last-log-contains? state "Runner uses Stargate to trash Herald.") "Correct log")))

(deftest stargate-effect-persists-even-if-stargate-is-trashed
    ;; Effect persists even if Stargate is trashed
    (do-game
     (new-game {:corp {:deck [(qty "Ice Wall" 10)]
                       :hand ["Herald" "Troll" "Rototurret"]}
                :runner {:deck ["Stargate"]}})
     (core/move state :corp (find-card "Herald" (:hand (get-corp))) :deck {:front true})
     (core/move state :corp (find-card "Troll" (:hand (get-corp))) :deck {:front true})
     (is (= "Troll" (-> (get-corp) :deck first :title)) "Troll on top of deck")
     (is (= "Herald" (-> (get-corp) :deck second :title)) "Herald 2nd")
     (play-from-hand state :corp "Rototurret" "R&D")
     (take-credits state :corp)
     (play-from-hand state :runner "Stargate")
     (let [roto (get-ice state :rd 0)
           stargate (get-program state 0)]
       (card-ability state :runner stargate 0)
       (is (:run @state) "Run initiated")
       (rez state :corp roto)
       (run-continue state :encounter-ice)
       (card-subroutine state :corp roto 0)
       (click-card state :corp stargate)
       (is (not (installed? (refresh stargate))) "Stargate is trashed")
       (run-continue-until state :success)
       (click-prompt state :runner "Troll")
       (is (no-prompt? state :runner) "Prompt closed")
       (is (not (:run @state)) "Run ended")
       (is (-> (get-corp) :discard first :seen) "Troll is faceup")
       (is (= "Troll" (-> (get-corp) :discard first :title)) "Troll was trashed")
       (is (= "Herald" (-> (get-corp) :deck first :title)) "Herald now on top of R&D"))))

(deftest study-guide
  ;; Study Guide - 2c to add a power counter; +1 strength per counter
  (do-game
    (new-game {:runner {:deck ["Study Guide" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Study Guide")
    (let [sg (get-program state 0)]
      (card-ability state :runner sg 1)
      (is (= 4 (:credit (get-runner))) "Paid 2c")
      (is (= 1 (get-counters (refresh sg) :power)) "Has 1 power counter")
      (is (= 1 (get-strength (refresh sg))) "1 strength")
      (card-ability state :runner sg 1)
      (is (= 2 (:credit (get-runner))) "Paid 2c")
      (is (= 2 (get-counters (refresh sg) :power)) "Has 2 power counters")
      (is (= 2 (get-strength (refresh sg))) "2 strength"))))

(deftest sunya
  ;; Sūnya
  (do-game
      (new-game {:runner {:deck ["Sūnya"]}
                 :corp {:deck ["Rototurret"]}})
      (play-from-hand state :corp "Rototurret" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Sūnya")
      (let [sunya (get-program state 0)
            roto (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp (refresh roto))
        (run-continue state)
        (card-ability state :runner (refresh sunya) 0)
        (click-prompt state :runner "Trash a program")
        (click-prompt state :runner "End the run")
        (is (changed? [(get-counters (refresh sunya) :power) 1]
              (run-continue state))
            "Got 1 token"))))

(deftest surfer
  ;; Surfer - Swap position with ice before or after when encountering a piece of Barrier ice
  (do-game
      (new-game {:corp {:deck ["Ice Wall" "Quandary"]}
                 :runner {:deck ["Surfer"]}})
      (play-from-hand state :corp "Quandary" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Surfer")
      (is (= 3 (:credit (get-runner))) "Paid 2 credits to install Surfer")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 1))
      (run-continue state)
      (is (= 2 (get-in @state [:run :position])) "Starting run at position 2")
      (let [surf (get-program state 0)]
        (card-ability state :runner surf 0)
        (click-card state :runner (get-ice state :hq 0))
        (is (= 1 (:credit (get-runner))) "Paid 2 credits to use Surfer")
        (is (= 1 (get-in @state [:run :position])) "Now at next position (1)")
        (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall now at position 1"))))

(deftest surfer-swapping-twice-across-two-turns
    ;; Swapping twice across two turns
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "Vanilla"]}
                 :runner {:deck ["Surfer"]
                          :credits 10}})
      (play-from-hand state :corp "Vanilla" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Surfer")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 1))
      (run-continue state)
      (let [surf (get-program state 0)]
        (card-ability state :runner surf 0)
        (click-card state :runner (get-ice state :hq 0))
        (run-continue state :movement)
        (run-jack-out state)
        (run-on state "HQ")
        (rez state :corp (get-ice state :hq 1))
        (run-continue state)
        (card-ability state :runner surf 0)
        (click-card state :runner (get-ice state :hq 0))
        (is (= ["Vanilla" "Ice Wall"] (map :title (get-ice state :hq)))
            "Vanilla is innermost, Ice Wall is outermost again")
        (is (= [0 1] (map :index (get-ice state :hq)))))))

(deftest switchblade
  ;; Switchblade
  (do-game
    (new-game {:runner {:deck ["Cloak" "Switchblade"]
                        :credits 10}})
    (take-credits state :corp)
    (play-from-hand state :runner "Cloak")
    (play-from-hand state :runner "Switchblade")
    (let [cl (get-program state 0)
          sb (get-program state 1)]
      (is (= ["Break any number of Sentry subroutines (using at least 1 stealth [Credits])"
              "Add 7 strength (using at least 1 stealth [Credits])"]
             (mapv :label (:abilities sb))))
      (is (changed? [(:credit (get-runner)) 0
                     (get-counters (refresh cl) :recurring) -1
                     (get-strength (refresh sb)) 7]
                    (card-ability state :runner sb 1)
                    (click-card state :runner cl))
          "Used 1 credit from Cloak"))))

(deftest takobi-1-counter-when-breaking-all-subs
    ;; +1 counter when breaking all subs
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Enigma"]}
                 :runner {:hand ["Takobi" "Corroder" "Gordian Blade"]
                          :credits 20}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Takobi")
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Gordian Blade")
      (let [tako (get-program state 0)
            corr (get-program state 1)
            gord (get-program state 2)
            enig (get-ice state :hq 1)
            icew (get-ice state :hq 0)]
        (run-on state "HQ")
        (rez state :corp enig)
        (run-continue state)
        (card-ability state :runner gord 0)
        (click-prompt state :runner "End the run")
        (click-prompt state :runner "Done")
        (is (no-prompt? state :runner) "No prompt because not all subs were broken")
        (is (= 0 (get-counters (refresh tako) :power)) "No counter gained because not all subs were broken")
        (run-continue-until state :approach-ice icew)
        (rez state :corp icew)
        (run-continue state)
        (card-ability state :runner corr 0)
        (click-prompt state :runner "End the run")
        (click-prompt state :runner "Yes")
        (run-continue state)
        (is (= 1 (get-counters (refresh tako) :power)) "Counter gained from breaking all subs"))))

(deftest takobi-3-strength
    ;; +3 strength
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Enigma"]}
                 :runner {:hand ["Takobi" "Corroder" "Faust"]
                          :credits 10}})
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Takobi")
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Faust")
      (let [tako (get-program state 0)
            corr (get-program state 1)
            faus (get-program state 2)]
        (core/add-counter state :runner tako :power 3)
        (is (= 3 (get-counters (refresh tako) :power)) "3 counters on Takobi")
        (run-on state "HQ")
        (rez state :corp (get-ice state :hq 0))
        (run-continue state)
        (card-ability state :runner tako 0)
        (click-card state :runner (refresh faus))
        (is (not (no-prompt? state :runner)) "Can't choose AI breakers")
        (click-card state :runner (refresh corr))
        (is (no-prompt? state :runner) "Can choose non-AI breakers")
        (is (= 5 (get-strength (refresh corr))) "Corroder at +3 strength")
        (is (= 1 (get-counters (refresh tako) :power)) "1 counter on Takobi")
        (card-ability state :runner tako 0)
        (is (no-prompt? state :runner) "No prompt when too few power counters")
        (run-continue-until state :success)
        (is (= 2 (get-strength (refresh corr))) "Corroder returned to normal strength"))))

(deftest tranquilizer
  ;; Tranquilizer
  (do-game
      (new-game {:corp {:hand ["Ice Wall"]
                        :deck [(qty "Hedge Fund" 10)]}
                 :runner {:hand ["Tranquilizer"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Tranquilizer")
      (click-card state :runner (get-ice state :hq 0))
      (let [iw (get-ice state :hq 0)
            tranquilizer (first (:hosted (refresh iw)))]
        (is (= 1 (get-counters (refresh tranquilizer) :virus)))
        (take-credits state :runner)
        (rez state :corp iw)
        (take-credits state :corp)
        (is (= 2 (get-counters (refresh tranquilizer) :virus)))
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 3 (get-counters (refresh tranquilizer) :virus)))
        (is (not (rezzed? (refresh iw))) "Ice Wall derezzed")
        (take-credits state :runner)
        (rez state :corp iw)
        (take-credits state :corp)
        (is (= 4 (get-counters (refresh tranquilizer) :virus)))
        (is (not (rezzed? (refresh iw))) "Ice Wall derezzed again"))))

(deftest tranquilizer-derez-on-install
    ;; Derez on install
    (do-game
      (new-game {:corp {:hand ["Ice Wall"]
                        :deck [(qty "Hedge Fund" 10)]}
                 :runner {:hand ["Tranquilizer" "Hivemind" "Surge"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Hivemind")
      (let [iw (get-ice state :hq 0)
            hive (get-program state 0)]
        (is (= 1 (get-counters (refresh hive) :virus)) "Hivemind starts with 1 virus counter")
        (play-from-hand state :runner "Surge")
        (click-card state :runner (refresh hive))
        (is (= 3 (get-counters (refresh hive) :virus)) "Hivemind gains 2 virus counters")
        (rez state :corp iw)
        (is (rezzed? (refresh iw)) "Ice Wall rezzed")
        (play-from-hand state :runner "Tranquilizer")
        (click-card state :runner (get-ice state :hq 0))
        (let [tranquilizer (first (:hosted (refresh iw)))]
          (is (= 1 (get-counters (refresh tranquilizer) :virus)))
          (is (not (rezzed? (refresh iw))) "Ice Wall derezzed")))))

(deftest tremolo
  ;; Tremolo
  (do-game
    (new-game {:corp {:hand ["Battlement" "Meru Mati"]}
                :runner {:hand ["Tremolo" (qty "Severnius Stim Implant" 4)]
                        :credits 100}})
    (play-from-hand state :corp "Meru Mati" "HQ")
    (play-from-hand state :corp "Battlement" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :click 1)
    (play-from-hand state :runner "Tremolo")
    (let [tremolo (get-program state 0)
        meru (get-ice state :hq 0)
        battlement (get-ice state :hq 1)]
      (run-on state :hq)
      (rez state :corp battlement)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -3]
            (card-ability state :runner tremolo 0)
            (click-prompt state :runner "End the run")
            (click-prompt state :runner "End the run"))
          "Spent 3 credits to break subs")
      (run-continue-until state :approach-ice meru)
      (rez state :corp meru)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -2]
            (card-ability state :runner tremolo 1))
          "Spent 2 credits to match ice strength")
      (card-ability state :runner tremolo 0)
      (click-prompt state :runner "End the run")
      (run-continue state)
      (run-continue state)
      (is (not (:run @state)) "Run is finished")
      (play-from-hand state :runner "Severnius Stim Implant")
      (play-from-hand state :runner "Severnius Stim Implant")
      (run-on state :hq)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) -1]
            (card-ability state :runner tremolo 0)
            (click-prompt state :runner "End the run")
            (click-prompt state :runner "End the run"))
          "Spent only 1 credit to break subs"))))

(deftest trope-happy-path
    ;; Happy Path
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
                 :runner {:hand ["Trope" "Easy Mark"]
                          :discard ["Sure Gamble" "Easy Mark" "Dirty Laundry"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Trope")
      ;; wait 3 turns to make Trope have 3 counters
      (dotimes [_ 3]
        (take-credits state :runner)
        (take-credits state :corp))
      (is (= 3 (get-counters (refresh (get-program state 0)) :power)))
      (is (zero? (count (:deck (get-runner)))) "0 cards in deck")
      (is (= 3 (count (:discard (get-runner)))) "3 cards in discard")
      (card-ability state :runner (get-program state 0) 0)
      (is (not (no-prompt? state :runner)) "Shuffle prompt came up")
      (click-card state :runner (find-card "Easy Mark" (:discard (get-runner))))
      (click-card state :runner (find-card "Dirty Laundry" (:discard (get-runner))))
      (click-card state :runner (find-card "Sure Gamble" (:discard (get-runner))))
      (is (= 3 (count (:deck (get-runner)))) "3 cards in deck")
      (is (zero? (count (:discard (get-runner)))) "0 cards in discard")))

(deftest trope-heap-locked
    ;; Heap Locked
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Blacklist"]}
                 :runner {:hand ["Trope" "Easy Mark"]
                          :discard ["Sure Gamble" "Easy Mark" "Dirty Laundry"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (play-from-hand state :runner "Trope")
      ;; wait 3 turns to make Trope have 3 counters
      (dotimes [_ 3]
        (take-credits state :runner)
        (take-credits state :corp))
      (is (= 3 (get-counters (refresh (get-program state 0)) :power)))
      (is (zero? (count (:deck (get-runner)))) "0 cards in deck")
      (is (= 3 (count (:discard (get-runner)))) "3 cards in discard")
      (card-ability state :runner (get-program state 0) 0)
      (is (no-prompt? state :runner) "Shuffle prompt did not come up")))

(deftest trypano-hivemind-and-architect-interactions
    ;; Hivemind and Architect interactions
    (do-game
      (new-game {:corp {:deck [(qty "Architect" 2)]}
                 :runner {:deck [(qty "Trypano" 2) "Hivemind"]}})
      (play-from-hand state :corp "Architect" "HQ")
      (play-from-hand state :corp "Architect" "R&D")
      (let [architect-rezzed (get-ice state :hq 0)
            architect-unrezzed (get-ice state :rd 0)]
        (rez state :corp architect-rezzed)
        (take-credits state :corp)
        (play-from-hand state :runner "Trypano")
        (click-card state :runner (get-card state architect-rezzed))
        (play-from-hand state :runner "Trypano")
        (click-card state :runner architect-unrezzed)
        (is (= 2 (core/available-mu state)) "Trypano consumes 1 MU"))
      ;; wait 4 turns to make both Trypanos have 4 counters on them
      (dotimes [_ 4]
        (take-credits state :runner)
        (take-credits state :corp)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Yes"))
      (is (zero? (count (:discard (get-runner)))) "Trypano not in discard yet")
      (is (= 1 (count (get-in @state [:corp :servers :rd :ices]))) "Unrezzed Archiect is not trashed")
      (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "Rezzed Archiect is not trashed")
      (play-from-hand state :runner "Hivemind") ; now Hivemind makes both Trypanos have 5 counters
      (is (zero? (count (get-in @state [:corp :servers :rd :ices]))) "Unrezzed Archiect was trashed")
      (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "Rezzed Archiect was not trashed")
      (is (= 1 (count (:discard (get-runner)))) "Trypano went to discard")))

(deftest trypano-fire-when-hivemind-gains-counters
    ;; Fire when Hivemind gains counters
    (do-game
      (new-game {:corp {:deck ["Architect"]}
                 :runner {:deck ["Trypano" "Hivemind" (qty "Surge" 2)]}})
      (play-from-hand state :corp "Architect" "R&D")
      (let [architect-unrezzed (get-ice state :rd 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Trypano")
        (click-card state :runner architect-unrezzed)
        (is (zero? (count (:discard (get-runner)))) "Trypano not in discard yet")
        (is (= 1 (count (get-ice state :rd))) "Unrezzed Architect is not trashed")
        (play-from-hand state :runner "Hivemind")
        (let [hive (get-program state 0)]
          (is (= 1 (get-counters (refresh hive) :virus)) "Hivemind starts with 1 virus counter")
          (play-from-hand state :runner "Surge")
          (click-card state :runner (refresh hive))
          (is (= 3 (get-counters (refresh hive) :virus)) "Hivemind gains 2 virus counters")
          (play-from-hand state :runner "Surge")
          (click-card state :runner (refresh hive))
          (is (= 5 (get-counters (refresh hive) :virus)) "Hivemind gains 2 virus counters (now at 5)")
          (is (zero? (count (get-ice state :rd))) "Unrezzed Architect was trashed")
          (is (= 3 (count (:discard (get-runner)))) "Trypano went to discard")))))

(deftest tunnel-vision
    ;; Tunnel Vision
    (do-game
      (new-game {:corp {:hand ["Envelopment" "Ice Wall"]}
                 :runner {:hand ["Tunnel Vision"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Envelopment" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Tunnel Vision")
      (core/set-mark state :hq)
      (let [tv (get-program state 0)
            iw (get-ice state :rd 0)
            env (get-ice state :hq 0)]
        (run-on state :rd)
        (rez state :corp iw)
        (run-continue state)
        (card-ability state :runner tv 0)
        (is (no-prompt? state :runner) "Can only use ability when running marked server")
        (fire-subs state iw)
        (run-on state :hq)
        (rez state :corp env)
        (run-continue state)
        (is (changed? [(:credit (get-runner)) -4]
              (card-ability state :runner tv 1)
              (card-ability state :runner tv 1))
            "Spent 4 credits to match ice strength")
        (is (changed? [(:credit (get-runner)) -4]
              (card-ability state :runner tv 0)
              (click-prompt state :runner "End the run")
              (click-prompt state :runner "End the run")
              (click-prompt state :runner "End the run")
              (click-prompt state :runner "Done"))
            "Spent 4 credits to break 3 subs"))))

(deftest tycoon-tycoon-gives-2c-after-using-to-break-ice
    ;; Tycoon gives 2c after using to break ice
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Tycoon"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Tycoon")
      (let [tycoon (get-program state 0)
            credits (:credit (get-corp))]
        (run-on state "HQ")
        (run-continue state)
        (card-ability state :runner tycoon 0)
        (click-prompt state :runner "End the run")
        (card-ability state :runner tycoon 1)
        (is (= 4 (get-strength (refresh tycoon))) "Tycoon strength pumped to 4.")
        (is (= credits (:credit (get-corp))) "Corp doesn't gain credits until encounter is over")
        (run-continue state)
        (is (= (+ credits 2) (:credit (get-corp))) "Corp gains 2 credits from Tycoon being used")
        (is (= 1 (get-strength (refresh tycoon))) "Tycoon strength back down to 1."))))

(deftest tycoon-tycoon-gives-2c-even-if-ice-wasn-t-passed
    ;; Tycoon gives 2c even if ice wasn't passed
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "Nisei MK II"]}
                 :runner {:deck ["Tycoon"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (play-and-score state "Nisei MK II")
      (take-credits state :corp)
      (play-from-hand state :runner "Tycoon")
      (let [tycoon (get-program state 0)
            credits (:credit (get-corp))
            nisei (get-scored state :corp 0)]
        (run-on state "HQ")
        (run-continue state)
        (card-ability state :runner tycoon 0)
        (click-prompt state :runner "End the run")
        (card-ability state :runner tycoon 1)
        (is (= 4 (get-strength (refresh tycoon))) "Tycoon strength pumped to 4.")
        (is (= credits (:credit (get-corp))) "Corp doesn't gain credits until encounter is over")
        (card-ability state :corp (refresh nisei) 0)
        (is (= (+ credits 2) (:credit (get-corp))) "Corp gains 2 credits from Tycoon being used after Nisei MK II fires")
        (is (= 1 (get-strength (refresh tycoon))) "Tycoon strength back down to 1."))))

(deftest tycoon-tycoon-pays-out-on-auto-pump-and-break
    ;; Tycoon pays out on auto-pump-and-break
    (do-game
      (new-game {:corp {:deck ["Markus 1.0"]}
                 :runner {:deck ["Tycoon"]}})
      (play-from-hand state :corp "Markus 1.0" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Tycoon")
      (let [tycoon (get-program state 0)
            credits (:credit (get-corp))]
        (run-on state "HQ")
        (run-continue state)
        (auto-pump-and-break state (refresh tycoon))
        (is (= credits (:credit (get-corp))) "Corp doesn't gain credits until encounter is over")
        (core/continue state :corp nil)
        (is (= (+ credits 2) (:credit (get-corp))) "Corp gains 2 credits from Tycoon being used"))))

(deftest umbrella
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 2)]
                      :hand ["Fairchild 3.0" "Quandary"]
                        :credits 10}
                 :runner {:deck [(qty "Sure Gamble" 2)]
                          :hand ["Umbrella" "Hush"]
                          :credits 20}})
      (play-from-hand state :corp "Quandary" "HQ")
      (play-from-hand state :corp "Fairchild 3.0" "R&D")
      (take-credits state :corp)
      (core/gain state :runner :click 1)
      (play-from-hand state :runner "Umbrella")
      (play-from-hand state :runner "Hush")
      (click-card state :runner (get-ice state :rd 0))
      (let [umb (get-program state 0)
            quand (get-ice state :hq 0)
            fc3 (get-ice state :rd 0)]
        (run-on state "HQ")
        (rez state :corp quand)
        (run-continue state)
        (card-ability state :runner (refresh umb) 0)
        (is (no-prompt? state :runner) "Can't use Umbrella on ice not hosting Trojans")
        (fire-subs state (refresh quand))
        (run-on state "R&D")
        (rez state :corp fc3)
        (run-continue state)
        (auto-pump-and-break state (refresh umb))
        (is (changed? [(count (:hand (get-runner))) 1]
              (click-prompt state :runner "Yes"))
            "Runner drew 1 card")
        (is (changed? [(count (:hand (get-corp))) 1]
              (click-prompt state :corp "Yes"))
            "Corp drew 1 card")
        (core/continue state :corp nil)
        (run-jack-out state)
        (run-on state "R&D")
        (run-continue state)
        (auto-pump-and-break state (refresh umb))
        (is (changed? [(count (:hand (get-runner))) 0]
              (click-prompt state :runner "No"))
            "Runner declined to draw")
        (is (changed? [(count (:hand (get-corp))) 0]
              (click-prompt state :corp "No"))
            "Corp declined to draw"))))

(deftest unity
  ;; Unity
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Afshar"]
                        :credits 20}
                 :runner {:deck ["Unity"]
                          :credits 20}})
      (play-from-hand state :corp "Afshar" "R&D")
      (rez state :corp (get-ice state :rd 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Unity")
      (let [unity (get-program state 0)
            ice (get-ice state :rd 0)]
        (run-on state :rd)
        (run-continue state)
        (is (= 17 (:credit (get-runner))) "17 credits before breaking")
        (card-ability state :runner unity 1) ;;temp boost because EDN file
        (card-ability state :runner unity 0)
        (click-prompt state :runner "Make the Runner lose 2 [Credits]")
        (card-ability state :runner unity 0)
        (click-prompt state :runner "End the run")
        (is (= 14 (:credit (get-runner))) "14 credits after breaking")
        (is (zero? (count (remove :broken (:subroutines (refresh ice))))) "All subroutines have been broken"))))

(deftest unity-boost-test
    ;; Boost test
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
        (is (= 11 (:credit (get-runner))) "11 credits before breaking")
        (card-ability state :runner unity 1)
        (card-ability state :runner unity 1)
        (card-ability state :runner unity 0)
        (click-prompt state :runner "Do 1 net damage and make the Runner lose 2 [Credits]")
        (card-ability state :runner unity 0)
        (click-prompt state :runner "Do 1 net damage and make the Runner lose 2 [Credits]")
        (card-ability state :runner unity 0)
        (click-prompt state :runner "Do 1 net damage and make the Runner lose 2 [Credits]")
        (is (= 6 (:credit (get-runner))) "6 credits after breaking")
        (is (zero? (count (remove :broken (:subroutines (refresh ice))))) "All subroutines have been broken"))))

(deftest upya
  (do-game
    (new-game {:runner {:deck ["Upya"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Upya")
    (dotimes [_ 3]
      (run-empty-server state "R&D"))
    (is (= 3 (get-counters (get-program state 0) :power)) "3 counters on Upya")
    (take-credits state :corp)
    (dotimes [_ 3]
      (run-empty-server state "R&D"))
    (is (= 6 (get-counters (get-program state 0) :power)) "6 counters on Upya")
    (let [upya (get-program state 0)]
      (card-ability state :runner upya 0)
      (is (= 3 (get-counters (refresh upya) :power)) "3 counters spent")
      (is (= 2 (:click (get-runner))) "Gained 2 clicks")
      (card-ability state :runner upya 0)
      (is (= 3 (get-counters (refresh upya) :power)) "Upya not used more than once a turn")
      (is (= 2 (:click (get-runner))) "Still at 2 clicks"))
    (take-credits state :runner)
    (take-credits state :corp)
    (let [upya (get-program state 0)]
      (card-ability state :runner upya 0)
      (is (zero? (get-counters (refresh upya) :power)) "3 counters spent")
      (is (= 5 (:click (get-runner))) "Gained 2 clicks"))))

(deftest utae
  ;; Utae
  (do-game
    (new-game {:corp {:deck ["Enigma"]}
               :runner {:deck ["Utae" (qty "Logic Bomb" 3)]
                        :credits 10}})
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Utae")
    (let [utae (get-program state 0)]
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (let [credits (:credit (get-runner))]
        (card-ability state :runner utae 2)
        (is (= (dec credits) (:credit (get-runner))) "Spent 1 credit"))
      (let [credits (:credit (get-runner))]
        (card-ability state :runner utae 0)
        (click-prompt state :runner "2")
        (click-prompt state :runner "Force the Runner to lose [Click]")
        (click-prompt state :runner "End the run")
        (is (= (- credits 2) (:credit (get-runner))) "Spent 2 credits"))
      (let [credits (:credit (get-runner))]
        (card-ability state :runner utae 0)
        (is (no-prompt? state :runner) "Can only use ability once per run")
        (card-ability state :runner utae 1)
        (is (= credits (:credit (get-runner))) "Cannot use this ability without 3 installed virtual resources"))
      (run-continue state :movement)
      (run-jack-out state)
      (core/gain state :runner :click 2)
      (dotimes [_ 3]
        (play-from-hand state :runner "Logic Bomb"))
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :runner utae 2)
      (let [credits (:credit (get-runner))]
        (card-ability state :runner utae 1)
        (click-prompt state :runner "End the run")
        (click-prompt state :runner "Done")
        (is (= (dec credits) (:credit (get-runner)))))
      (is (= 3 (:credit (get-runner))) "Able to use ability now"))))

(deftest vamadeva-swap-ability
    ;; Swap ability
    (testing "Doesnt work if no Deva in hand"
      (do-game
        (new-game {:runner {:hand ["Vamadeva" "Corroder"]
                            :credits 10}})
        (take-credits state :corp)
        (play-from-hand state :runner "Vamadeva")
        (card-ability state :runner (get-program state 0) 2)
        (is (no-prompt? state :runner) "No select prompt as there's no Deva in hand")))
    (testing "Works with another deva in hand"
      (doseq [deva ["Aghora" "Sadyojata" "Vamadeva"]]
        (do-game
          (new-game {:runner {:hand ["Vamadeva" deva]
                              :credits 10}})
          (take-credits state :corp)
          (play-from-hand state :runner "Vamadeva")
          (let [installed-deva (get-program state 0)]
            (card-ability state :runner installed-deva 2)
            (click-card state :runner (first (:hand (get-runner))))
            (is (not (utils/same-card? installed-deva (get-program state 0)))
                "Installed deva is not same as previous deva")
            (is (= deva (:title (get-program state 0)))))))))

(deftest vamadeva-break-ability-targets-ice-with-1-subroutine
    ;; Break ability targets ice with 1 subroutine
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Enigma" "Ice Wall"]
                        :credits 10}
                 :runner {:hand ["Vamadeva"]
                          :credits 10}})
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Ice Wall" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Vamadeva")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (is (no-prompt? state :runner) "No break prompt")
      (run-continue state :movement)
      (run-jack-out state)
      (run-on state "R&D")
      (rez state :corp (get-ice state :rd 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (is (not (no-prompt? state :runner)) "Have a break prompt")
      (click-prompt state :runner "End the run")
      (is (:broken (first (:subroutines (get-ice state :rd 0))))
          "The break ability worked")))

(deftest wari
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Wari"]}})
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Wari")
    (run-empty-server state "HQ")
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "Barrier")
    (click-card state :runner (get-ice state :rd 0))
    (is (= 1 (count (:discard (get-runner)))) "Wari in heap")
    (is (not (no-prompt? state :runner)) "Runner is currently accessing Ice Wall")))

(deftest world-tree
  (do-game
    (new-game {:runner {:deck ["Sneakdoor Beta"]
                        :hand ["World Tree" "Paricia"]
                        :credits 20}})
    (take-credits state :corp)
    (play-from-hand state :runner "Paricia")
    (play-from-hand state :runner "World Tree")
    (run-empty-server state "Archives")
    (click-prompt state :runner "Done")
    (run-empty-server state "Archives")
    (is (no-prompt? state :runner) "No further prompts")
    (take-credits state :runner)
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (click-card state :runner (get-program state 1))
    (is (zero? (count (:discard (get-runner)))) "World Tree cannot trash itself")
    (click-card state :runner (get-program state 0))
    (is (= 1 (count (:discard (get-runner)))) "Paricia was trashed")
    (is (= 2 (count (:choices (prompt-map :runner)))) "Only Sneakdoor Beta (and No install) are available")
    (is (changed? [(:credit (get-runner)) -1]
          (click-prompt state :runner "Sneakdoor Beta"))
        "Sneakdoor Beta install cost lowered by 3")))

(deftest world-tree-no-other-cards-to-trash
  (do-game
    (new-game {:runner {:deck ["Sneakdoor Beta"]
                        :hand ["World Tree"]
                        :credits 10}})
    (take-credits state :corp)
    (play-from-hand state :runner "World Tree")
    (run-empty-server state "Archives")
    (is (no-prompt? state :runner) "No prompt since there's no other installed card to trash")))

(deftest world-tree-multiple-cards-in-server
  (do-game
    (new-game {:runner {:deck ["Sneakdoor Beta"]
                        :hand ["World Tree" "Paricia"]
                        :credits 10}
               :corp {:hand ["PAD Campaign" "Manegarm Skunkworks"]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Manegarm Skunkworks" "Server 1")
    (take-credits state :corp)
    (play-from-hand state :runner "Paricia")
    (play-from-hand state :runner "World Tree")
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Done")
    (click-card state :runner (get-content state :remote1 0))
    (click-prompt state :runner "No action")
    (click-prompt state :runner "No action")
    (is (no-prompt? state :runner))))

(deftest world-tree-no-available-install-targets
  (do-game
    (new-game {:runner {:deck ["Street Peddler" "Sure Gamble" "Boomerang"]
                        :hand ["World Tree" "Paricia"]
                        :credits 20}})
    (take-credits state :corp)
    (play-from-hand state :runner "Paricia")
    (play-from-hand state :runner "World Tree")
    (run-empty-server state "Archives")
    (click-prompt state :runner "Done")
    (run-empty-server state "Archives")
    (is (no-prompt? state :runner) "No further prompts")
    (take-credits state :runner)
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (click-card state :runner (get-program state 1))
    (is (zero? (count (:discard (get-runner)))) "World Tree cannot trash itself")
    (click-card state :runner (get-program state 0))
    (is (= 1 (count (:discard (get-runner)))) "Paricia was trashed")
    (is (= 1 (count (:choices (prompt-map :runner)))))
    (click-prompt state :runner "Done")))

(deftest world-tree-trashing-facedown-card
  (do-game
    (new-game {:runner {:deck ["Street Peddler" "Sure Gamble"]
                        :hand ["World Tree" "Harbinger"]
                        :credits 20}})
    (take-credits state :corp)
    (play-from-hand state :runner "Harbinger")
    (play-from-hand state :runner "World Tree")
    (run-empty-server state "Archives")
    (click-card state :runner (get-program state 0))
    (click-prompt state :runner "Done")
    (take-credits state :runner)
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (click-card state :runner (-> (get-runner) :rig :facedown first))
    (is (no-prompt? state :runner) "Select prompt not even shown")))

(deftest wyrm
  ;; Wyrm reduces strength of ice
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Wyrm"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Wyrm")
    (run-on state "HQ")
    (let [ice-wall (get-ice state :hq 0)
          wyrm (get-program state 0)]
      (rez state :corp ice-wall)
      (run-continue state)
      (card-ability state :runner wyrm 1)
      (is (zero? (get-strength (refresh ice-wall))) "Strength of Ice Wall reduced to 0")
      (card-ability state :runner wyrm 1)
      (is (= -1 (get-strength (refresh ice-wall))) "Strength of Ice Wall reduced to -1"))))

(deftest yusuf
  ;; Yusuf gains virus counters on successful runs and can spend virus counters from any installed card
  (do-game
      (new-game {:corp {:deck ["Fire Wall"]}
                 :runner {:deck ["Yusuf" "Cache"]}})
      (play-from-hand state :corp "Fire Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Yusuf")
      (play-from-hand state :runner "Cache")
      (let [fire-wall (get-ice state :hq 0)
            yusuf (get-program state 0)
            cache (get-program state 1)]
        (run-empty-server state "Archives")
        (is (= 1 (get-counters (refresh yusuf) :virus)) "Yusuf has 1 virus counter")
        (is (= 3 (get-strength (refresh yusuf))) "Initial Yusuf strength")
        (is (= 3 (get-counters (refresh cache) :virus)) "Initial Cache virus counters")
        (run-on state "HQ")
        (rez state :corp fire-wall)
        (run-continue state)
        (card-ability state :runner yusuf 1)
        (click-card state :runner cache)
        (card-ability state :runner yusuf 1)
        (click-card state :runner yusuf)
        (is (= 2 (get-counters (refresh cache) :virus)) "Cache lost 1 virus counter to pump")
        (is (= 5 (get-strength (refresh yusuf))) "Yusuf strength 5")
        (is (zero? (get-counters (refresh yusuf) :virus)) "Yusuf lost 1 virus counter to pump")
        (is (no-prompt? state :runner) "No prompt open")
        (card-ability state :runner yusuf 0)
        (click-prompt state :runner "End the run")
        (click-card state :runner cache)
        (is (= 1 (get-counters (refresh cache) :virus)) "Cache lost its final virus counter"))))
