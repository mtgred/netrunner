(ns game-test.cards.icebreakers
  (:require [game.core :as core]
            [game.utils :as utils]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [jinteki.utils :refer [count-tags]]
            [clojure.test :refer :all]))

(deftest adept
  ;; Adept - +1 str for each unused MU
  (do-game
    (new-game {:runner {:deck ["Adept" "Box-E"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Adept")
    (let [ad (get-program state 0)]
      (is (= 2 (core/available-mu state)))
      (is (= 4 (:current-strength (refresh ad))) "+2 strength for 2 unused MU")
      (play-from-hand state :runner "Box-E")
      (is (= 4 (core/available-mu state)))
      (is (= 6 (:current-strength (refresh ad))) "+4 strength for 4 unused MU"))))

(deftest atman
  ;; Atman
  (testing "Installing with 0 power counters"
    (do-game
      (new-game {:runner {:deck ["Atman"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Atman")
      (click-prompt state :runner "0")
      (is (= 3 (core/available-mu state)))
      (let [atman (get-program state 0)]
        (is (zero? (get-counters atman :power)) "0 power counters")
        (is (zero? (:current-strength atman)) "0 current strength"))))
  (testing "Installing with 2 power counters"
    (do-game
      (new-game {:runner {:deck ["Atman"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Atman")
      (click-prompt state :runner "2")
      (is (= 3 (core/available-mu state)))
      (let [atman (get-program state 0)]
        (is (= 2 (get-counters atman :power)) "2 power counters")
        (is (= 2 (:current-strength atman)) "2 current strength")))))

(deftest aumakua
  ;; Aumakua - Gain credit on no-trash
  (testing "Gain counter on no trash"
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
  (testing "Gain counters on empty archives"
    (do-game
      (new-game {:runner {:deck ["Aumakua"]}
                 :options {:start-as :runner}})
      (play-from-hand state :runner "Aumakua")
      (run-empty-server state :archives)
      (is (= 1 (get-counters (get-program state 0) :virus)) "Aumakua gains virus counter from accessing empty Archives")))
  (testing "Neutralize All Threats interaction"
    (do-game
      (new-game {:corp {:deck [(qty "PAD Campaign" 3)]}
                 :runner {:deck ["Aumakua" "Neutralize All Threats"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Aumakua")
      (play-from-hand state :runner "Neutralize All Threats")
      (core/gain state :runner :credit 5)
      (run-empty-server state "Server 1")
      (is (zero? (get-counters (get-program state 0) :virus)) "Aumakua does not gain virus counter from ABT-forced trash"))))

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
      (click-card state :runner (find-card "Faerie" (:hand (get-runner))))
      (is (= (+ 2 base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga gained 2 subroutines from Faerie")
      (card-ability state :runner (refresh baba) 0)
      (click-card state :runner (find-card "Yog.0" (:hand (get-runner))))
      (is (= (+ 3 base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga gained 1 subroutine from Yog.0")
      (core/trash state :runner (first (:hosted (refresh baba))))
      (is (= (inc base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga lost 2 subroutines from trashed Faerie")
      (card-ability state :runner baba 1)
      (click-card state :runner (find-card "Sharpshooter" (:program (:rig (get-runner)))))
      (is (= 2 (count (:hosted (refresh baba)))) "Faerie and Sharpshooter hosted on Baba Yaga")
      (is (= 1 (core/available-mu state)) "1 MU left with 2 breakers on Baba Yaga")
      (is (= 4 (:credit (get-runner))) "-5 from Baba, -1 from Sharpshooter played into Rig, -5 from Yog"))))

(deftest cerberus-rex-h2
  ;; Cerberus "Rex" H2 - boost 1 for 1 cred, break for 1 counter
  (do-game
    (new-game {:runner {:deck ["Cerberus \"Rex\" H2"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Cerberus \"Rex\" H2")
    (is (= 2 (:credit (get-runner))) "2 credits left after install")
    (let [rex (get-program state 0)]
      (is (= 4 (get-counters rex :power)) "Start with 4 counters")
      ;; boost strength
      (card-ability state :runner rex 1)
      (is (= 1 (:credit (get-runner))) "Spend 1 credit to boost")
      (is (= 2 (:current-strength (refresh rex))) "At strength 2 after boost")
      ;; break
      (card-ability state :runner rex 0)
      (is (= 1 (:credit (get-runner))) "No credits spent to break")
      (is (= 3 (get-counters (refresh rex) :power)) "One counter used to break"))))

(deftest chameleon
  ;; Chameleon - Install on corp turn, only returns to hand at end of runner's turn
  (testing "with Clone Chip"
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
  (testing "Returns to hand after hosting. #977"
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
      (is (= 2 (count (:hand (get-runner)))) "Both Chameleons returned to hand - hand size 2"))))

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
    (run-on state "HQ")
    (let [cradle (get-program state 0)
          strength (:strength (refresh cradle))]
      (dotimes [n 5]
        (when (pos? n)
          (core/draw state :runner n))
        (is (= (- strength n) (:current-strength (refresh cradle))) (str "Cradle should lose " n " strength"))
        (starting-hand state :runner [])
        (is (= strength (:current-strength (refresh cradle))) (str "Cradle should be back to original strength")))
      (core/draw state :runner 1)
      (is (= (dec strength) (:current-strength (refresh cradle))) "Cradle should lose 1 strength")
      (play-from-hand state :runner "Cache")
      (is (= strength (:current-strength (refresh cradle))) (str "Cradle should be back to original strength")))))

(deftest crypsis
  ;; Crypsis - Loses a virus counter after encountering ice it broke
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck [(qty "Crypsis" 2)]}})
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp)
    (core/gain state :runner :credit 100)
    (play-from-hand state :runner "Crypsis")
    (let [crypsis (get-program state 0)]
      (card-ability state :runner crypsis 2)
      (is (= 1 (get-counters (refresh crypsis) :virus))
          "Crypsis has 1 virus counter")
      (run-on state "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (card-ability state :runner (refresh crypsis) 0) ; Match strength
      (card-ability state :runner (refresh crypsis) 1) ; Break
      (is (= 1 (get-counters (refresh crypsis) :virus))
          "Crypsis has 1 virus counter")
      (run-continue state)
      (is (zero? (get-counters (refresh crypsis) :virus))
          "Crypsis has 0 virus counters")
      (run-jack-out state)
      (is (zero? (get-counters (refresh crypsis) :virus))
          "Crypsis has 0 virus counters")
      (run-on state "Archives")
      (card-ability state :runner (refresh crypsis) 0) ; Match strength
      (card-ability state :runner (refresh crypsis) 1) ; Break
      (is (zero? (get-counters (refresh crypsis) :virus))
          "Crypsis has 0 virus counters")
      (run-jack-out state)
      (is (= "Crypsis" (:title (first (:discard (get-runner)))))
          "Crypsis was trashed"))
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "Crypsis")
    (let [crypsis (get-program state 0)]
      (run-on state "Archives")
      (card-ability state :runner (refresh crypsis) 0) ; Match strength
      (card-ability state :runner (refresh crypsis) 1) ; Break
      (is (zero? (get-counters (refresh crypsis) :virus))
          "Crypsis has nil virus counters")
      (run-jack-out state)
      (is (= "Crypsis" (:title (first (:discard (get-runner)))))
          "Crypsis was trashed"))))

(deftest darwin
  ;; Darwin - starts at 0 strength
  (do-game
    (new-game {:runner {:deck ["Darwin"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Darwin")
    (let [darwin (get-program state 0)]
      (is (zero? (get-counters (refresh darwin) :virus)) "Darwin starts with 0 virus counters")
      (is (zero? (:current-strength (refresh darwin))) "Darwin starts at 0 strength")
      (take-credits state :runner)
      (take-credits state :corp)
      (card-ability state :runner (refresh darwin) 1) ; add counter
      (is (= 1 (get-counters (refresh darwin) :virus)) "Darwin gains 1 virus counter")
      (is (= 1 (:current-strength (refresh darwin))) "Darwin is at 1 strength"))))

(deftest deus-x
  (testing "vs Multiple Hostile Infrastructure"
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Infrastructure" 3)]}
                 :runner {:deck [(qty "Deus X" 3) (qty "Sure Gamble" 2)]}})
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
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (let [dx (get-program state 0)]
        (card-ability state :runner dx 1)
        (click-prompt state :runner "Done")
        (is (= 2 (count (:hand (get-runner)))) "Deus X prevented one Hostile net damage"))))
  (testing "vs Multiple sources of net damage"
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
        (click-prompt state :runner "Done")
        (click-prompt state :runner "Pay 2 [Credits] to steal")
        (is (= 3 (count (:hand (get-runner)))) "Deus X prevented net damage from accessing Fetal AI, but not from Personal Evolution")
        (is (= 1 (count (:scored (get-runner)))) "Fetal AI stolen")))))

(deftest faerie
  ;; Faerie - trash after encounter is over, not before.
  (do-game
    (new-game {:corp {:deck ["Caduceus"]}
               :runner {:deck ["Faerie"]}})
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

(deftest faust
  (testing "Basic test: Pump by discarding"
    (do-game
      (new-game {:runner {:deck ["Faust" (qty "Sure Gamble" 3)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Faust")
      (let [faust (get-program state 0)]
        (card-ability state :runner faust 1)
        (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
        (is (= 4 (:current-strength (refresh faust))) "4 current strength")
        (is (= 1 (count (:discard (get-runner)))) "1 card trashed"))))
  (testing "Pump does not trigger trash prevention. #760"
    (do-game
      (new-game {:runner {:deck ["Faust"
                                 "Sacrificial Construct"
                                 "Fall Guy"
                                 "Astrolabe"
                                 "Gordian Blade"
                                 "Armitage Codebusting"]}})
      (take-credits state :corp)
      (core/draw state :runner 1)
      (play-from-hand state :runner "Faust")
      (play-from-hand state :runner "Fall Guy")
      (play-from-hand state :runner "Sacrificial Construct")
      (is (= 2 (count (get-resource state))) "Resources installed")
      (let [faust (get-program state 0)]
        (card-ability state :runner faust 1)
        (click-card state :runner (find-card "Astrolabe" (:hand (get-runner))))
        (is (empty? (:prompt (get-runner))) "No trash-prevention prompt for hardware")
        (card-ability state :runner faust 1)
        (click-card state :runner (find-card "Gordian Blade" (:hand (get-runner))))
        (is (empty? (:prompt (get-runner))) "No trash-prevention prompt for program")
        (card-ability state :runner faust 1)
        (click-card state :runner (find-card "Armitage Codebusting" (:hand (get-runner))))
        (is (empty? (:prompt (get-runner))) "No trash-prevention prompt for resource")))))

(deftest femme-fatale
  ;; Femme Fatale counter test
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck [(qty "Femme Fatale" 2)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 18)
    (let [iw (get-ice state :hq 0)]
      (play-from-hand state :runner "Femme Fatale")
      (click-card state :runner iw)
      (is (:icon (refresh iw)) "Ice Wall has an icon")
      (core/trash state :runner (get-program state 0))
      (is (not (:icon (refresh iw))) "Ice Wall does not have an icon after Femme trashed")
      (play-from-hand state :runner "Femme Fatale")
      (click-card state :runner iw)
      (is (:icon (refresh iw)) "Ice Wall has an icon")
      (core/trash state :corp iw)
      (is (not (:icon (refresh iw))) "Ice Wall does not have an icon after itself trashed"))))

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

(deftest ika
  ;; Ika
  (testing "Can be hosted on both rezzed/unrezzed ice, respects no-host, is blanked by Magnet"
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
          (card-ability state :runner ika 2) ; host on a piece of ice
          (click-card state :runner tithonium)
          (is (utils/same-card? ika (first (:hosted (refresh tithonium)))) "Ika was rehosted")
          (is (= (- creds 2) (:credit (get-runner))) "Rehosting from rig cost 2 creds"))
        (run-on state :archives)
        (let [creds (:credit (get-runner))
              ika (first (:hosted (refresh tithonium)))]
          (card-ability state :runner ika 2)
          (click-card state :runner enigma)
          (is (utils/same-card? ika (first (:hosted (refresh enigma)))) "Ika was rehosted")
          (is (= (- creds 2) (:credit (get-runner))) "Rehosting from ice during run cost 2 creds"))
        (core/rez state :corp tithonium)
        (let [creds (:credit (get-runner))
              ika (first (:hosted (refresh enigma)))]
          (card-ability state :runner ika 2)
          (click-card state :runner tithonium)
          (is (zero?(count (:hosted (refresh tithonium)))) "Ika was not hosted on Tithonium")
          (is (= creds (:credit (get-runner))) "Clicking invalid targets is free")
          (click-prompt state :runner "Done")
          (core/rez state :corp magnet)
          (click-card state :corp ika)
          (is (zero?(count (:hosted (refresh enigma)))) "Ika was removed from Enigma")
          (is (= 1 (count (:hosted (refresh magnet)))) "Ika was hosted onto Magnet")
          (let [ika (first (:hosted (refresh magnet)))]
            (is (zero?(count (:abilities ika))) "Ika was blanked")))))))

(deftest inversificator
  ;; Inversificator shouldn't hook up events for unrezzed ice
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
      (run-on state :hq)
      (core/rez state :corp (refresh tur))
      (run-continue state)
      (card-ability state :runner (refresh inv) 0)
      (click-card state :runner (get-ice state :hq 1))
      (click-card state :runner (get-ice state :hq 0))
      (run-jack-out state)
      (is (= 1 (count (:hand (get-runner)))) "Runner still has 1 card in hand")
      (run-on state :hq)
      (run-continue state)
      (is (= 1 (count (:hand (get-runner)))) "Kakugo doesn't fire when unrezzed"))))

(deftest mammon
  ;; Mammon - Pay to add X power counters at start of turn, all removed at end of turn
  (do-game
    (new-game {:runner {:deck ["Mammon"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Mammon")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [mam (get-program state 0)]
      (card-ability state :runner mam 0)
      (click-prompt state :runner "3")
      (is (= 2 (:credit (get-runner))) "Spent 3 credits")
      (is (= 3 (get-counters (refresh mam) :power)) "Mammon has 3 power counters")
      (take-credits state :runner)
      (is (zero? (get-counters (refresh mam) :power)) "All power counters removed"))))

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
      (is (= 1 (:current-strength (refresh musaazi))) "Initial Musaazi strength")
      (is (= 2 (get-counters (refresh imp) :virus)) "Initial Imp virus counters")
      (run-on state "HQ")
      (core/rez state :corp lancelot)
      (card-ability state :runner musaazi 1) ; match strength
      (click-card state :runner imp)
      (is (= 1 (get-counters (refresh imp) :virus)) "Imp lost 1 virus counter to pump")
      (is (= 2 (:current-strength (refresh musaazi))) "Musaazi strength 2")
      (is (empty? (:prompt (get-runner))) "No prompt open")
      (card-ability state :runner musaazi 0)
      (click-card state :runner musaazi)
      (click-card state :runner imp)
      (click-prompt state :runner "Done")
      (is (zero?(get-counters (refresh imp) :virus)) "Imp lost its final virus counter")
      (is (zero?(get-counters (refresh imp) :virus)) "Musaazi lost its virus counter"))))

(deftest na-not-k
  ;; Na'Not'K - Strength adjusts accordingly when ice installed during run
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Architect" "Eli 1.0"]}
                 :runner {:deck ["Na'Not'K"]}})
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
        (click-card state :corp (find-card "Eli 1.0" (:hand (get-corp))))
        (click-prompt state :corp "HQ")
        (is (= 3 (:current-strength (refresh nanotk))) "2 ice on HQ")
        (run-jack-out state)
        (is (= 1 (:current-strength (refresh nanotk))) "Back to default strength"))))
  (testing "Strength adjusts accordingly when run redirected to another server"
    (do-game
      (new-game {:corp {:deck ["Susanoo-no-Mikoto" "Crick" "Cortex Lock"]}
                 :runner {:deck ["Na'Not'K"]}})
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
        (is (= 1 (:current-strength (refresh nanotk))) "Back to default strength")))))

(deftest overmind
  ;; Overmind - Start with counters equal to unused MU
  (do-game
    (new-game {:runner {:deck ["Overmind" (qty "Akamatsu Mem Chip" 2)]}})
    (take-credits state :corp)
    (take-credits state :runner 1)
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (is (= 6 (core/available-mu state)))
    (play-from-hand state :runner "Overmind")
    (is (= 5 (core/available-mu state)))
    (let [ov (get-program state 0)]
      (is (= 5 (get-counters (refresh ov) :power)) "Overmind has 5 counters"))))

(deftest paperclip
  ;; Paperclip - prompt to install on encounter, but not if another is installed
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Vanilla"]}
                 :runner {:deck [(qty "Paperclip" 2)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (take-credits state :corp)
      (trash-from-hand state :runner "Paperclip")
      (run-on state "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (click-prompt state :runner "Yes") ; install paperclip
      (run-continue state)
      (run-successful state)
      (is (not (:run @state)) "Run ended")
      (trash-from-hand state :runner "Paperclip")
      (run-on state "Archives")
      (is (empty? (:prompt (get-runner))) "No prompt to install second Paperclip")))
  (testing "firing on facedown ice shouldn't crash"
    (do-game
      (new-game {:corp {:deck ["Vanilla"]}
                 :runner {:deck ["Paperclip"]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Paperclip")
      (run-on state "Archives")
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "0")))
  (testing "do not show a second install prompt if user said No to first, when multiple are in heap"
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
      (core/rez state :corp (get-ice state :archives 1))
      (click-prompt state :runner "No")
      (is (empty? (:prompt (get-runner))) "No additional prompts to rez other copies of Paperclip")
      (run-continue state)
      ;; we should get the prompt on a second ice even after denying the first.
      (core/rez state :corp (get-ice state :archives 0))
      (click-prompt state :runner "No")
      (is (empty? (:prompt (get-runner))) "No additional prompts to rez other copies of Paperclip")
      (core/jack-out state :runner)
      ;; Run again, make sure we get the prompt to install again.
      (run-on state "Archives")
      (click-prompt state :runner "No")
      (is (empty? (:prompt (get-runner))) "No additional prompts to rez other copies of Paperclip"))))

(deftest peregrine
  ;; Peregrine - 2c to return to grip and derez an encountered code gate
  (do-game
    (new-game {:corp {:deck ["Paper Wall" (qty "Bandwidth" 2)]}
               :runner {:deck ["Peregrine"]}})
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
      (is (zero? (:credit (get-runner))) "Spent 2 credits")
      (is (= 1 (count (:hand (get-runner)))) "Peregrine returned to grip")
      (is (not (:rezzed (refresh bw1))) "Bandwidth derezzed"))))

(deftest persephone
  ;; Persephone's ability trashes cards from R&D, triggering AR-Enhanced Security
  ;; See #3187
  (do-game
    (new-game {:corp {:deck ["Zed 1.0" (qty "Zed 2.0" 3) "AR-Enhanced Security"]}
               :runner {:deck [(qty "Persephone" 10)]}})
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
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "2")
    (is (= 1 (count-tags state)) "Runner took 1 tag from using Persephone's ability while AR-Enhanced Security is scored")
    (take-credits state :runner)
    ;; Gotta move the discarded cards back to the deck
    (core/move state :corp (find-card "Zed 2.0" (:discard (get-corp))) :deck)
    (core/move state :corp (find-card "Zed 2.0" (:discard (get-corp))) :deck)
    (take-credits state :corp)
    (run-on state "Archives")
    (run-continue state)
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "2")
    (is (= 2 (count-tags state)) "Runner took 1 tag from using Persephone's ability while AR-Enhanced Security is scored")))

(deftest shiv
  ;; Shiv - Gain 1 strength for each installed breaker; no MU cost when 2+ link
  (do-game
    (new-game {:runner {:id "Nasir Meidan: Cyber Explorer"
                        :deck ["Shiv" (qty "Inti" 2)
                               "Access to Globalsec"]}})
    (is (= 1 (:link (get-runner))) "1 link")
    (take-credits state :corp)
    (play-from-hand state :runner "Shiv")
    (let [shiv (get-program state 0)]
      (is (= 1 (:current-strength (refresh shiv))) "1 installed breaker; 1 strength")
      (play-from-hand state :runner "Inti")
      (is (= 2 (:current-strength (refresh shiv))) "2 installed breakers; 2 strength")
      (play-from-hand state :runner "Inti")
      (is (= 3 (:current-strength (refresh shiv))) "3 installed breakers; 3 strength")
      (is (= 1 (core/available-mu state)) "3 MU consumed")
      (play-from-hand state :runner "Access to Globalsec")
      (is (= 2 (:link (get-runner))) "2 link")
      (is (= 2 (core/available-mu state)) "Shiv stops using MU when 2+ link"))))

(deftest snowball
  ;; Snowball - Strength boost until end of run when used to break a subroutine
  (do-game
    (new-game {:corp {:deck ["Spiderweb" "Fire Wall" "Hedge Fund"]}
               :runner {:deck ["Snowball"]}})
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
    (new-game {:runner {:deck ["Study Guide" "Sure Gamble"]}})
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

(deftest tycoon
  ;; Tycoon
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Tycoon"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (core/rez state state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Tycoon")
    (let [tycoon (get-program state 0)
          credits (:credit (get-corp))]
      (run-on state "HQ")
      (card-ability state :runner tycoon 0)
      (is (= credits (:credit (get-corp))) "Corp doesn't gain credits until encounter is over")
      (run-continue state)
      (is (= (+ credits 2) (:credit (get-corp))) "Corp gains 2 credits from Tycoon being used"))))

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
      (core/rez state :corp ice-wall)
      (card-ability state :runner wyrm 1)
      (is (zero? (:current-strength (refresh ice-wall))) "Strength of Ice Wall reduced to 0")
      (card-ability state :runner wyrm 1)
      (is (= -1 (:current-strength (refresh ice-wall))) "Strength of Ice Wall reduced to -1"))))

(deftest yusuf
  ;; Yusuf gains virus counters on successful runs and can spend virus counters from any installed card
  (testing "Yusuf basic tests"
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
        (is (= 3 (:current-strength (refresh yusuf))) "Initial Yusuf strength")
        (is (= 3 (get-counters (refresh cache) :virus)) "Initial Cache virus counters")
        (run-on state "HQ")
        (core/rez state :corp fire-wall)
        (card-ability state :runner yusuf 1) ; match strength
        (click-card state :runner cache)
        (click-card state :runner yusuf)
        (is (= 2 (get-counters (refresh cache) :virus)) "Cache lost 1 virus counter to pump")
        (is (= 5 (:current-strength (refresh yusuf))) "Yusuf strength 5")
        (is (zero?(get-counters (refresh yusuf) :virus)) "Yusuf lost 1 virus counter to pump")
        (is (empty? (:prompt (get-runner))) "No prompt open")
        (card-ability state :runner yusuf 0)
        (click-card state :runner cache)
        (click-prompt state :runner "Done")
        (is (= 1 (get-counters (refresh cache) :virus)) "Cache lost its final virus counter"))))
  (testing "Yusuf add strength test"
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
        (is (= 3 (:current-strength (refresh yusuf))) "Initial Yusuf strength")
        (is (= 3 (get-counters (refresh cache) :virus)) "Initial Cache virus counters")
        (run-on state "HQ")
        (core/rez state :corp fire-wall)
        (card-ability state :runner yusuf 2) ; add strength
        (click-card state :runner cache)
        (click-prompt state :runner "Done")
        (is (= 2 (get-counters (refresh cache) :virus)) "Cache lost 1 virus counter to pump")
        (is (= 4 (:current-strength (refresh yusuf))) "Yusuf strength 4")
        (is (empty? (:prompt (get-runner))) "No prompt open")))))
