(ns game-test.cards.icebreakers
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards)

(deftest adept-strength
  ;; Adept - +1 str for each unused MU
  (do-game
    (new-game (default-corp) (default-runner [(qty "Adept" 1) (qty "Box-E" 1)]))
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Adept")
    (let [ad (get-program state 0)]
      (is (= 2 (:memory (get-runner))))
      (is (= 4 (:current-strength (refresh ad))) "+2 strength for 2 unused MU")
      (play-from-hand state :runner "Box-E")
      (is (= 4 (:memory (get-runner))))
      (is (= 6 (:current-strength (refresh ad))) "+4 strength for 4 unused MU"))))

(deftest atman-install-0
  ;; Atman - Installing with 0 power counters
  (do-game
    (new-game (default-corp) (default-runner [(qty "Atman" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Atman")
    (prompt-choice :runner 0)
    (is (= 3 (:memory (get-runner))))
    (let [atman (get-in @state [:runner :rig :program 0])]
      (is (= 0 (get-counters atman :power)) "0 power counters")
      (is (= 0 (:current-strength atman)) "0 current strength"))))

(deftest atman-install-2
  ;; Atman - Installing with 2 power counters
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Atman" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Atman")
    (prompt-choice :runner 2)
    (is (= 3 (:memory (get-runner))))
    (let [atman (get-in @state [:runner :rig :program 0])]
      (is (= 2 (get-counters atman :power)) "2 power counters")
      (is (= 2 (:current-strength atman)) "2 current strength"))))

(deftest aumakua
  ;; Aumakua - Gain credit on no-trash
  (do-game
    (new-game (default-corp [(qty "PAD Campaign" 3)])
              (default-runner [(qty "Aumakua" 1)]))
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Aumakua")
    (run-empty-server state "Server 1")
    (prompt-choice :runner "No")
    (is (= 1 (get-counters (get-program state 0) :virus)) "Aumakua gains virus counter from no-trash")
    (core/gain state :runner :credit 5)
    (run-empty-server state "Server 1")
    (prompt-choice :runner "Yes")
    (is (= 1 (get-counters (get-program state 0) :virus)) "Aumakua does not gain virus counter from trash")))

(deftest aumakua-neutralize-all-threats
  ;; Aumakua - Neutralize All Threats interaction
  (do-game
    (new-game (default-corp [(qty "PAD Campaign" 3)])
              (default-runner [(qty "Aumakua" 1) (qty "Neutralize All Threats" 1)]))
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Aumakua")
    (play-from-hand state :runner "Neutralize All Threats")
    (core/gain state :runner :credit 5)
    (run-empty-server state "Server 1")
    (is (zero? (get-counters (get-program state 0) :virus)) "Aumakua does not gain virus counter from ABT-forced trash")))

(deftest baba-yaga
  (do-game
    (new-game
      (default-corp)
      (default-runner [(qty "Baba Yaga" 1) (qty "Faerie" 1) (qty "Yog.0" 1)(qty "Sharpshooter" 1)]))
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Baba Yaga")
    (play-from-hand state :runner "Sharpshooter")
    (let [baba (get-program state 0)
          base-abicount (count (:abilities baba))]
      (card-ability state :runner baba 0)
      (prompt-select :runner (find-card "Faerie" (:hand (get-runner))))
      (is (= (+ 2 base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga gained 2 subroutines from Faerie")
      (card-ability state :runner (refresh baba) 0)
      (prompt-select :runner (find-card "Yog.0" (:hand (get-runner))))
      (is (= (+ 3 base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga gained 1 subroutine from Yog.0")
      (core/trash state :runner (first (:hosted (refresh baba))))
      (is (= (inc base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga lost 2 subroutines from trashed Faerie")
      (card-ability state :runner baba 1)
      (prompt-select :runner (find-card "Sharpshooter" (:program (:rig (get-runner)))))
      (is (= 2 (count (:hosted (refresh baba)))) "Faerie and Sharpshooter hosted on Baba Yaga")
      (is (= 1 (:memory (get-runner))) "1 MU left with 2 breakers on Baba Yaga")
      (is (= 4 (:credit (get-runner))) "-5 from Baba, -1 from Sharpshooter played into Rig, -5 from Yog"))))

(deftest cerberus-rex
  ;; Cerberus "Rex" H2 - boost 1 for 1 cred, break for 1 counter
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "Cerberus \"Rex\" H2" 1)]))
   (take-credits state :corp)
   (play-from-hand state :runner "Cerberus \"Rex\" H2")
   (is (= 2 (:credit (get-runner))) "2 credits left after install")
   (let [rex (get-in @state [:runner :rig :program 0])]
     (is (= 4 (get-counters rex :power)) "Start with 4 counters")
     ;; boost strength
     (card-ability state :runner rex 1)
     (is (= 1 (:credit (get-runner))) "Spend 1 credit to boost")
     (is (= 2 (:current-strength (refresh rex))) "At strength 2 after boost")
     ;; break
     (card-ability state :runner rex 0)
     (is (= 1 (:credit (get-runner))) "No credits spent to break")
     (is (= 3 (get-counters (refresh rex) :power)) "One counter used to break"))))

(deftest chameleon-clonechip
  ;; Chameleon - Install on corp turn, only returns to hand at end of runner's turn
  (do-game
    (new-game (default-corp) (default-runner [(qty "Chameleon" 1) (qty "Clone Chip" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Clone Chip")
    (core/move state :runner (find-card "Chameleon" (:hand (get-runner))) :discard)
    (take-credits state :runner)
    (is (= 0 (count (:hand (get-runner)))))
    ;; Install Chameleon on corp turn
    (take-credits state :corp 1)
    (let [chip (get-in @state [:runner :rig :hardware 0])]
      (card-ability state :runner chip 0)
      (prompt-select :runner (find-card "Chameleon" (:discard (get-runner))))
      (prompt-choice :runner "Sentry"))
    (take-credits state :corp)
    (is (= 0 (count (:hand (get-runner)))) "Chameleon not returned to hand at end of corp turn")
    (take-credits state :runner)
    (is (= 1 (count (:hand (get-runner)))) "Chameleon returned to hand at end of runner's turn")))

(deftest chameleon-scheherazade
  ;; Chameleon - Returns to hand after hosting. #977
  (do-game
    (new-game (default-corp) (default-runner [(qty "Chameleon" 2) (qty "Scheherazade" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Chameleon")
    (prompt-choice :runner "Barrier")
    (is (= 3 (:credit (get-runner))) "-2 from playing Chameleon")
    ;; Host the Chameleon on Scheherazade that was just played (as in Personal Workshop/Hayley ability scenarios)
    (play-from-hand state :runner "Scheherazade")
    (let [scheherazade (get-in @state [:runner :rig :program 1])]
      (card-ability state :runner scheherazade 1) ; Host an installed program
      (prompt-select :runner (find-card "Chameleon" (:program (:rig (get-runner)))))
      (is (= 4 (:credit (get-runner))) "+1 from hosting onto Scheherazade")
      ;; Install another Chameleon directly onto Scheherazade
      (card-ability state :runner scheherazade 0) ; Install and host a program from Grip
      (prompt-select :runner (find-card "Chameleon" (:hand (get-runner))))
      (prompt-choice :runner "Code Gate")
      (is (= 2 (count (:hosted (refresh scheherazade)))) "2 Chameleons hosted on Scheherazade")
      (is (= 3 (:credit (get-runner))) "-2 from playing Chameleon, +1 from installing onto Scheherazade"))
    (is (= 0 (count (:hand (get-runner)))) "Both Chameleons in play - hand size 0")
    (take-credits state :runner)
    (is (= 2 (count (:hand (get-runner)))) "Both Chameleons returned to hand - hand size 2")))

(deftest crypsis
  ;; Crypsis - Loses a virus counter after encountering ice it broke
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 1)])
              (default-runner [(qty "Crypsis" 2)]))
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp)
    (core/gain state :runner :credit 100)
    (play-from-hand state :runner "Crypsis")
    (let [crypsis (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner crypsis 2)
      (is (= 1 (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has 1 virus counter")

      (run-on state "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (card-ability state :runner (refresh crypsis) 0) ; Match strength
      (card-ability state :runner (refresh crypsis) 1) ; Break
      (is (= 1 (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has 1 virus counter")
      (run-continue state)
      (is (= 0 (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has 0 virus counters")
      (run-jack-out state)
      (is (= 0 (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has 0 virus counters")

      (run-on state "Archives")
      (card-ability state :runner (refresh crypsis) 0) ; Match strength
      (card-ability state :runner (refresh crypsis) 1) ; Break
      (is (= 0 (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has 0 virus counters")
      (run-jack-out state)
      (is (= "Crypsis" (:title (first (:discard (get-runner)))))
          "Crypsis was trashed"))

    (take-credits state :runner)
    (take-credits state :corp)

    (play-from-hand state :runner "Crypsis")
    (let [crypsis (get-in @state [:runner :rig :program 0])]
      (run-on state "Archives")
      (card-ability state :runner (refresh crypsis) 0) ; Match strength
      (card-ability state :runner (refresh crypsis) 1) ; Break
      (is (nil? (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has nil virus counters")
      (run-jack-out state)
      (is (= "Crypsis" (:title (first (:discard (get-runner)))))
          "Crypsis was trashed"))))

(deftest deus-x-multiple-hostile-infrastructure
  ;; Multiple Hostile Infrastructure vs. Deus X
  (do-game
    (new-game
      (default-corp [(qty "Hostile Infrastructure" 3)])
      (default-runner [(qty "Deus X" 3) (qty "Sure Gamble" 2)]))
    (play-from-hand state :corp "Hostile Infrastructure" "New remote")
    (play-from-hand state :corp "Hostile Infrastructure" "New remote")
    (play-from-hand state :corp "Hostile Infrastructure" "New remote")
    (core/gain state :corp :credit 10)
    (core/rez state :corp (get-content state :remote1 0))
    (core/rez state :corp (get-content state :remote2 0))
    (core/rez state :corp (get-content state :remote3 0))
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Deus X")
    (run-empty-server state "Server 1")
    (prompt-choice :runner "Yes")
    (let [dx (get-program state 0)]
      (card-ability state :runner dx 1)
      (prompt-choice :runner "Done")
      (is (= 2 (count (:hand (get-runner)))) "Deus X prevented one Hostile net damage"))))

(deftest deus-x-fetal-jinteki-pe
  ;; Multiple sources of net damage vs. Deus X
  (do-game
    (new-game
      (make-deck "Jinteki: Personal Evolution" [(qty "Fetal AI" 6)])
      (default-runner [(qty "Deus X" 3) (qty "Sure Gamble" 2)]))
    (play-from-hand state :corp "Fetal AI" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Deus X")
    (run-empty-server state "Server 1")
    (prompt-choice :runner "Access")
    (let [dx (get-program state 0)]
      (card-ability state :runner dx 1)
      (prompt-choice :runner "Done")
      (prompt-choice :runner "Yes")
      (is (= 3 (count (:hand (get-runner)))) "Deus X prevented net damage from accessing Fetal AI, but not from Personal Evolution")
      (is (= 1 (count (:scored (get-runner)))) "Fetal AI stolen"))))

(deftest faerie-auto-trash
  ;; Faerie - trash after encounter is over, not before.
  (do-game
    (new-game
      (default-corp [(qty "Caduceus" 1)])
      (default-runner [(qty "Faerie" 1)]))
    (play-from-hand state :corp "Caduceus" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Faerie")
    (let [fae (get-program state 0)]
      (run-on state :archives)
      (core/rez state :corp (get-ice state :archives 0))
      (card-ability state :runner fae 0)
      (is (refresh fae) "Faerie not trashed until encounter over")
      (run-continue state)
      (is (find-card "Faerie" (:discard (get-runner))) "Faerie trashed"))))

(deftest faust-pump
  ;; Faust - Pump by discarding
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Faust" 1) (qty "Sure Gamble" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Faust")
    (let [faust (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner faust 1)
      (prompt-card :runner (first (:hand (get-runner))))
      (is (= 4 (:current-strength (refresh faust))) "4 current strength")
      (is (= 1 (count (:discard (get-runner)))) "1 card trashed"))))

(deftest faust-pump
  ;; Faust - Pump does not trigger trash prevention. #760
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Faust" 1)
                               (qty "Sacrificial Construct" 1)
                               (qty "Fall Guy" 1)
                               (qty "Astrolabe" 1)
                               (qty "Gordian Blade" 1 )
                               (qty "Armitage Codebusting" 1)]))
    (take-credits state :corp)
    (core/draw state :runner 1)
    (play-from-hand state :runner "Faust")
    (play-from-hand state :runner "Fall Guy")
    (play-from-hand state :runner "Sacrificial Construct")
    (is (= 2 (count (get-in @state [:runner :rig :resource]))) "Resources installed")
    (let [faust (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner faust 1)
      (prompt-card :runner (find-card "Astrolabe" (:hand (get-runner))))
      (is (empty? (:prompt (get-runner))) "No trash-prevention prompt for hardware")
      (card-ability state :runner faust 1)
      (prompt-card :runner (find-card "Gordian Blade" (:hand (get-runner))))
      (is (empty? (:prompt (get-runner))) "No trash-prevention prompt for program")
      (card-ability state :runner faust 1)
      (prompt-card :runner (find-card "Armitage Codebusting" (:hand (get-runner))))
      (is (empty? (:prompt (get-runner))) "No trash-prevention prompt for resource"))))

(deftest femme-counter
  ;; Femme Fatale counter test
  (do-game
   (new-game (default-corp [(qty "Ice Wall" 1)])
             (default-runner [(qty "Femme Fatale" 2)]))
   (play-from-hand state :corp "Ice Wall" "HQ")
   (take-credits state :corp)
   (core/gain state :runner :credit 18)
   (let [iw (get-ice state :hq 0)]
    (play-from-hand state :runner "Femme Fatale")
    (prompt-select :runner iw)
    (is (:icon (refresh iw)) "Ice Wall has an icon")
    (core/trash state :runner (get-in @state [:runner :rig :program 0]))
    (is (not (:icon (refresh iw))) "Ice Wall does not have an icon after Femme trashed")
    (play-from-hand state :runner "Femme Fatale")
    (prompt-select :runner iw)
    (is (:icon (refresh iw)) "Ice Wall has an icon")
    (core/trash state :corp iw)
    (is (not (:icon (refresh iw))) "Ice Wall does not have an icon after itself trashed"))))

(deftest god-of-war
  ;; God of War - Take 1 tag to place 2 virus counters
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "God of War" 1)]))
   (take-credits state :corp)
   (play-from-hand state :runner "God of War")
   (take-credits state :runner)
   (take-credits state :corp)
   (let [gow (get-program state 0)]
     (card-ability state :runner gow 2)
     (is (= 1 (:tag (get-runner))))
     (is (= 2 (get-counters (refresh gow) :virus)) "God of War has 2 virus counters"))))

(deftest mammon
  ;; Mammon - Pay to add X power counters at start of turn, all removed at end of turn
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "Mammon" 1)]))
   (take-credits state :corp)
   (play-from-hand state :runner "Mammon")
   (take-credits state :runner)
   (take-credits state :corp)
   (let [mam (get-program state 0)]
     (card-ability state :runner mam 0)
     (prompt-choice :runner 3)
     (is (= 2 (:credit (get-runner))) "Spent 3 credits")
     (is (= 3 (get-counters (refresh mam) :power)) "Mammon has 3 power counters")
     (take-credits state :runner)
     (is (= 0 (get-counters (refresh mam) :power)) "All power counters removed"))))

(deftest nanotk-install-ice-during-run
  ;; Na'Not'K - Strength adjusts accordingly when ice installed during run
  (do-game
    (new-game (default-corp [(qty "Architect" 1) (qty "Eli 1.0" 1)])
              (default-runner [(qty "Na'Not'K" 1)]))
    (play-from-hand state :corp "Architect" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Na'Not'K")
    (let [nanotk (get-program state 0)
          architect (get-ice state :hq 0)]
      (is (= 1 (:current-strength (refresh nanotk))) "Default strength")
      (run-on state "HQ")
      (core/rez state :corp architect)
      (is (= 2 (:current-strength (refresh nanotk))) "1 ice on HQ")
      (card-subroutine state :corp (refresh architect) 1)
      (prompt-select :corp (find-card "Eli 1.0" (:hand (get-corp))))
      (prompt-choice :corp "HQ")
      (is (= 3 (:current-strength (refresh nanotk))) "2 ice on HQ")
      (run-jack-out state)
      (is (= 1 (:current-strength (refresh nanotk))) "Back to default strength"))))

(deftest nanotk-redirect
  ;; Na'Not'K - Strength adjusts accordingly when run redirected to another server
  (do-game
    (new-game (default-corp [(qty "Susanoo-no-Mikoto" 1) (qty "Crick" 1) (qty "Cortex Lock" 1)])
              (default-runner [(qty "Na'Not'K" 1)]))
    (play-from-hand state :corp "Cortex Lock" "HQ")
    (play-from-hand state :corp "Susanoo-no-Mikoto" "HQ")
    (play-from-hand state :corp "Crick" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Na'Not'K")
    (let [nanotk (get-program state 0)
          susanoo (get-ice state :hq 1)]
      (is (= 1 (:current-strength (refresh nanotk))) "Default strength")
      (run-on state "HQ")
      (core/rez state :corp susanoo)
      (is (= 3 (:current-strength (refresh nanotk))) "2 ice on HQ")
      (card-subroutine state :corp (refresh susanoo) 0)
      (is (= 2 (:current-strength (refresh nanotk))) "1 ice on Archives")
      (run-jack-out state)
      (is (= 1 (:current-strength (refresh nanotk))) "Back to default strength"))))

(deftest overmind-counters
  ;; Overmind - Start with counters equal to unused MU
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Overmind" 1) (qty "Akamatsu Mem Chip" 2)]))
    (take-credits state :corp)
    (take-credits state :runner 1)
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (is (= 6 (:memory (get-runner))))
    (play-from-hand state :runner "Overmind")
    (is (= 5 (:memory (get-runner))))
    (let [ov (get-in @state [:runner :rig :program 0])]
      (is (= 5 (get-counters (refresh ov) :power)) "Overmind has 5 counters"))))

(deftest paperclip
  ;; Paperclip - prompt to install on encounter, but not if another is installed
  (do-game
    (new-game (default-corp [(qty "Vanilla" 1)])
              (default-runner [(qty "Paperclip" 2)]))
    (play-from-hand state :corp "Vanilla" "Archives")
    (take-credits state :corp)
    (trash-from-hand state :runner "Paperclip")
    (run-on state "Archives")
    (core/rez state :corp (get-ice state :archives 0))
    (prompt-choice :runner "Yes") ; install paperclip
    (run-continue state)
    (run-successful state)
    (is (not (:run @state)) "Run ended")
    (trash-from-hand state :runner "Paperclip")
    (run-on state "Archives")
    (is (empty? (:prompt (get-runner))) "No prompt to install second Paperclip")))

(deftest paperclip-multiple
  ;; Paperclip - do not show a second install prompt if user said No to first, when multiple are in heap
  (do-game
    (new-game (default-corp [(qty "Vanilla" 2)])
              (default-runner [(qty "Paperclip" 3)]))
    (play-from-hand state :corp "Vanilla" "Archives")
    (play-from-hand state :corp "Vanilla" "Archives")
    (take-credits state :corp)
    (trash-from-hand state :runner "Paperclip")
    (trash-from-hand state :runner "Paperclip")
    (trash-from-hand state :runner "Paperclip")
    (run-on state "Archives")
    (core/rez state :corp (get-ice state :archives 1))
    (prompt-choice :runner "No")
    (is (empty? (:prompt (get-runner))) "No additional prompts to rez other copies of Paperclip")
    (run-continue state)
    ;; we should get the prompt on a second ice even after denying the first.
    (core/rez state :corp (get-ice state :archives 0))
    (prompt-choice :runner "No")
    (is (empty? (:prompt (get-runner))) "No additional prompts to rez other copies of Paperclip")
    (core/jack-out state :runner)
    ;; Run again, make sure we get the prompt to install again.
    (run-on state "Archives")
    (prompt-choice :runner "No")
    (is (empty? (:prompt (get-runner))) "No additional prompts to rez other copies of Paperclip")))

(deftest peregrine
  ;; Peregrine - 2c to return to grip and derez an encountered code gate
  (do-game
    (new-game (default-corp [(qty "Paper Wall" 1) (qty "Bandwidth" 2)])
              (default-runner [(qty "Peregrine" 1)]))
    (play-from-hand state :corp "Bandwidth" "Archives")
    (play-from-hand state :corp "Bandwidth" "Archives")
    (play-from-hand state :corp "Paper Wall" "Archives")
    (take-credits state :corp)
    (core/gain state :runner :credit 2)
    (play-from-hand state :runner "Peregrine")
    (let [bw1 (get-ice state :archives 0)
          pw (get-ice state :archives 2)
          per (get-program state 0)]
      (run-on state "Archives")
      (core/rez state :corp pw)
      (core/rez state :corp bw1)
      (card-ability state :runner per 2)
      (is (and (= 2 (:credit (get-runner))) (empty? (:hand (get-runner)))) "Can't use Peregrine on a barrier")
      (run-continue state)
      (card-ability state :runner per 2)
      (is (and (= 2 (:credit (get-runner))) (empty? (:hand (get-runner)))) "Can't use Peregrine on unrezzed code gate")
      (run-continue state)
      (card-ability state :runner per 2)
      (is (= 0 (:credit (get-runner))) "Spent 2 credits")
      (is (= 1 (count (:hand (get-runner)))) "Peregrine returned to grip")
      (is (not (get-in (refresh bw1) [:rezzed])) "Bandwidth derezzed"))))

(deftest persephone
  ;; Persephone's ability trashes cards from R&D, triggering AR-Enhanced Security
  ;; See #3187
  (do-game
    (new-game
      (default-corp [(qty "Zed 1.0" 1) (qty "Zed 2.0" 3) (qty "AR-Enhanced Security" 1)])
      (default-runner [(qty "Persephone" 10)]))
    (core/move state :corp (find-card "Zed 2.0" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Zed 2.0" (:hand (get-corp))) :deck)
    (play-from-hand state :corp "AR-Enhanced Security" "New remote")
    (score-agenda state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Zed 1.0" "Archives")
    (core/rez state :corp (get-ice state :archives 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Persephone")
    (run-on state "Archives")
    (run-continue state)
    (prompt-choice :runner "Yes")
    (prompt-choice :runner 2)
    (is (= 1 (:tag (get-runner))) "Runner took 1 tag from using Persephone's ability while AR-Enhanced Security is scored")
    (take-credits state :runner)
    ;; Gotta move the discarded cards back to the deck
    (core/move state :corp (find-card "Zed 2.0" (:discard (get-corp))) :deck)
    (core/move state :corp (find-card "Zed 2.0" (:discard (get-corp))) :deck)
    (take-credits state :corp)
    (run-on state "Archives")
    (run-continue state)
    (prompt-choice :runner "Yes")
    (prompt-choice :runner 2)
    (is (= 2 (:tag (get-runner))) "Runner took 1 tag from using Persephone's ability while AR-Enhanced Security is scored")))

(deftest shiv
  ;; Shiv - Gain 1 strength for each installed breaker; no MU cost when 2+ link
  (do-game
    (new-game
      (default-corp)
      (make-deck "Nasir Meidan: Cyber Explorer" [(qty "Shiv" 1) (qty "Inti" 2)
                                                 (qty "Access to Globalsec" 1)]))
    (is (= 1 (:link (get-runner))) "1 link")
    (take-credits state :corp)
    (play-from-hand state :runner "Shiv")
    (let [shiv (get-program state 0)]
      (is (= 1 (:current-strength (refresh shiv))) "1 installed breaker; 1 strength")
      (play-from-hand state :runner "Inti")
      (is (= 2 (:current-strength (refresh shiv))) "2 installed breakers; 2 strength")
      (play-from-hand state :runner "Inti")
      (is (= 3 (:current-strength (refresh shiv))) "3 installed breakers; 3 strength")
      (is (= 1 (:memory (get-runner))) "3 MU consumed")
      (play-from-hand state :runner "Access to Globalsec")
      (is (= 2 (:link (get-runner))) "2 link")
      (is (= 2 (:memory (get-runner))) "Shiv stops using MU when 2+ link"))))

(deftest snowball
  ;; Snowball - Strength boost until end of run when used to break a subroutine
  (do-game
   (new-game (default-corp [(qty "Spiderweb" 1) (qty "Fire Wall" 1) (qty "Hedge Fund" 1)])
             (default-runner [(qty "Snowball" 1)]))
   (play-from-hand state :corp "Hedge Fund")
   (play-from-hand state :corp "Fire Wall" "HQ")
   (play-from-hand state :corp "Spiderweb" "HQ")
   (take-credits state :corp)
   (core/gain state :runner :credit 10)
   (play-from-hand state :runner "Snowball")
   (let [sp (get-ice state :hq 1)
         fw (get-ice state :hq 0)
         snow (get-program state 0)]
     (run-on state "HQ")
     (core/rez state :corp sp)
     (core/rez state :corp fw)
     (card-ability state :runner snow 1) ; match strength
     (is (= 2 (:current-strength (refresh snow))))
     (card-ability state :runner snow 0) ; strength matched, break a sub
     (card-ability state :runner snow 0) ; break a sub
     (is (= 4 (:current-strength (refresh snow))) "Broke 2 subs, gained 2 more strength")
     (run-continue state)
     (is (= 3 (:current-strength (refresh snow))) "Has +2 strength until end of run; lost 1 per-encounter boost")
     (card-ability state :runner snow 1)
     (card-ability state :runner snow 1) ; match strength
     (is (= 5 (:current-strength (refresh snow))) "Matched strength, gained 2")
     (card-ability state :runner snow 0) ; strength matched, break a sub
     (is (= 6 (:current-strength (refresh snow))) "Broke 1 sub, gained 1 more strength")
     (run-continue state)
     (is (= 4 (:current-strength (refresh snow))) "+3 until-end-of-run strength")
     (run-jack-out state)
     (is (= 1 (:current-strength (refresh snow))) "Back to default strength"))))

(deftest study-guide
  ;; Study Guide - 2c to add a power counter; +1 strength per counter
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "Study Guide" 1) (qty "Sure Gamble" 1)]))
   (take-credits state :corp)
   (play-from-hand state :runner "Sure Gamble")
   (play-from-hand state :runner "Study Guide")
   (let [sg (get-program state 0)]
     (card-ability state :runner sg 1)
     (is (= 4 (:credit (get-runner))) "Paid 2c")
     (is (= 1 (get-counters (refresh sg) :power)) "Has 1 power counter")
     (is (= 1 (:current-strength (refresh sg))) "1 strength")
     (card-ability state :runner sg 1)
     (is (= 2 (:credit (get-runner))) "Paid 2c")
     (is (= 2 (get-counters (refresh sg) :power)) "Has 2 power counters")
     (is (= 2 (:current-strength (refresh sg))) "2 strength"))))

(deftest wyrm
  ;; Wyrm reduces strength of ice
  (do-game
   (new-game (default-corp [(qty "Ice Wall" 1)])
             (default-runner [(qty "Wyrm" 1)]))
   (play-from-hand state :corp "Ice Wall" "HQ")
   (take-credits state :corp)
   (play-from-hand state :runner "Wyrm")
   (run-on state "HQ")
   (let [ice-wall (get-ice state :hq 0)
         wyrm (get-in @state [:runner :rig :program 0])]
     (core/rez state :corp ice-wall)
     (card-ability state :runner wyrm 1)
     (is (= 0 (:current-strength (refresh ice-wall))) "Strength of Ice Wall reduced to 0")
     (card-ability state :runner wyrm 1)
     (is (= -1 (:current-strength (refresh ice-wall))) "Strength of Ice Wall reduced to -1"))))

(deftest yusuf
  ;; Yusuf gains virus counters on successful runs and can spend virus counters from any installed card
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Yusuf" 1) (qty "Cache" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Yusuf")
    (play-from-hand state :runner "Cache")
    (let [yusuf (get-program state 0)
          cache (get-program state 1)]
      (run-empty-server state "Archives")
      (is (= 1 (get-in (refresh yusuf) [:counter :virus])) "Yusuf has 1 virus counter")
      (is (= 3 (:current-strength (refresh yusuf))) "Initial Yusuf strength")
      (is (= 3 (get-in (refresh cache) [:counter :virus])) "Initial Cache virus counters")
      (card-ability state :runner yusuf 0)
      (prompt-select :runner cache)
      (prompt-choice :runner 1)
      (is (= 2 (get-in (refresh cache) [:counter :virus])) "Cache lost a virus counter to pump")
      (is (= 4 (:current-strength (refresh yusuf))) "Yusuf strength 4")
      (is (= 1 (get-in (refresh yusuf) [:counter :virus])) "Initial Yusuf virus counters")
      (card-ability state :runner yusuf 0)
      (prompt-select :runner yusuf)
      (prompt-choice :runner 1)
      (is (= 5 (:current-strength (refresh yusuf))) "Yusuf strength 5")
      (is (= 0 (get-in (refresh yusuf) [:counter :virus])) "Yusuf lost a virus counter")
      (card-ability state :runner yusuf 1)
      (prompt-select :runner cache)
      (prompt-choice :runner 1)
      (is (= 1 (get-in (refresh cache) [:counter :virus])) "Cache lost a virus counter to break"))))
