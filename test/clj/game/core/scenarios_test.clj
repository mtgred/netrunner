(ns game.core.scenarios-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.test-framework :refer :all]))

(deftest degree-mill-cvs
  (testing "for issue #4515"
    (do-game
      (new-game {:corp {:hand ["Degree Mill"]
                        :discard ["Cyberdex Virus Suite"]}
                 :runner {:hand ["Aumakua" (qty "Clone Chip" 2)]
                          :credits 10}})
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (play-from-hand state :runner "Aumakua")
      (play-from-hand state :runner "Clone Chip")
      (play-from-hand state :runner "Clone Chip")
      (run-empty-server state :rd)
      (run-empty-server state :rd)
      (is (= 2 (get-counters (get-program state 0) :virus)) "Aumakua has 2 virus counters")
      (run-empty-server state :archives)
      (click-prompt state :corp "Yes")
      (is (= 1 (get-counters (get-program state 0) :virus)) "Aumakua has 1 virus counter after purge and no trash")
      (is (not (:run @state)) "Run has ended")
      (run-empty-server state :rd)
      (run-empty-server state :rd)
      (trash-from-hand state :corp "Degree Mill")
      (run-empty-server state :archives)
      (click-prompt state :runner "Degree Mill")
      (click-prompt state :runner "Pay to steal")
      (click-card state :runner (get-hardware state 0))
      (click-card state :runner (get-hardware state 1))
      (click-prompt state :corp "Yes")
      (is (zero? (get-counters (get-program state 0) :virus)) "Aumakua has 0 virus counter after purge and steal")
      (is (not (:run @state)) "Run has ended"))))

(deftest minigame-prevent-netdmg-resourcetrash
  (testing "Mini-game testing prevention of net damage and resource trashing, with hosted Fall Guy"
    (do-game
      (new-game {:corp {:deck ["Neural EMP" (qty "Hedge Fund" 3) "SEA Source"]}
                 :runner {:deck ["Fall Guy" "Off-Campus Apartment" "Net Shield"
                                 "Wireless Net Pavilion" "Sure Gamble"]}})
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :corp 1)
      (is (= 14 (:credit (get-corp))))
      (core/gain state :runner :click 2)
      (run-empty-server state "Archives") ; enable Corp play of Neural and SEA next turn
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Off-Campus Apartment")
      (play-from-hand state :runner "Wireless Net Pavilion")
      (play-from-hand state :runner "Net Shield")
      (let [apt (get-resource state 0)]
        (card-ability state :runner apt 0)
        (click-card state :runner (find-card "Fall Guy" (:hand (get-runner))))
        (take-credits state :runner)
        (is (= 6 (:credit (get-runner))))
        (play-from-hand state :corp "Neural EMP")
        (let [ns (get-program state 0)
              fg (first (:hosted (refresh apt)))]
          (card-ability state :runner ns 0)
          (is (= 5 (:credit (get-runner))) "Runner paid 1c to survive Neural EMP")
          (play-from-hand state :corp "SEA Source")
          (click-prompt state :corp "3") ; boost trace to 6
          (click-prompt state :runner "0")
          (is (= 1 (count-tags state)) "Runner took tag from SEA Source")
          (is (= 7 (:credit (get-corp))))
          (trash-resource state)
          (click-card state :corp "Off-Campus Apartment")
          (is (= 3 (:credit (get-corp))) "WNP increased cost to trash a resource by 2")
          (card-ability state :runner fg 0) ; Trash Fall Guy to save the Apartment!
          (is (= (:title (get-resource state 0)) "Off-Campus Apartment")
              "Apartment still standing")
          (is (= (:title (last (:discard (get-runner)))) "Fall Guy") "Fall Guy trashed"))))))

(deftest hb-glacier
  (testing "HB Glacier econ and server protection with upgrades - Ash, Caprice, Breaker Bay Grid, positional ice strength boost"
    (do-game
      (new-game {:corp {:id "Haas-Bioroid: Engineering the Future"
                        :deck ["Adonis Campaign"
                               "Global Food Initiative"
                               "Breaker Bay Grid"
                               "Caprice Nisei"
                               "Ash 2X3ZB9CY"
                               "Turing"
                               "Hedge Fund"]}
                 :runner {:deck ["Desperado"
                                 "Dirty Laundry"
                                 "Emergency Shutdown"
                                 "Lamprey"
                                 "Data Folding"
                                 "Career Fair"]}})
      (draw state :corp 1)
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (is (= 10 (:credit (get-corp))) "HB:EtF ability paid 1 credit")
      (play-from-hand state :corp "Breaker Bay Grid" "Server 1")
      (play-from-hand state :corp "Ash 2X3ZB9CY" "HQ")
      (let [adon (get-content state :remote1 0)
            bbg (get-content state :remote1 1)
            ash (get-content state :hq 0)]
        (rez state :corp bbg)
        (rez state :corp adon)
        (is (= 10 (:credit (get-corp))) "Breaker Bay Grid allowed rez of Adonis for free")
        (take-credits state :corp)
        (draw state :runner 1)
        (play-from-hand state :runner "Career Fair")
        (click-card state :runner (find-card "Data Folding" (:hand (get-runner))))
        (is (= 5 (:credit (get-runner))) "Data Folding installed for free by Career Fair")
        (play-from-hand state :runner "Lamprey")
        (play-from-hand state :runner "Desperado")
        (is (= 1 (:credit (get-runner))))
        (run-on state "HQ")
        (rez state :corp ash)
        (run-continue state)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (and (= 2 (:credit (get-runner))) (= 7 (:credit (get-corp))))
            "Desperado paid 1 to Runner, Lamprey took 1 from Corp")
        (click-prompt state :runner "No action") ; can't afford to trash Ash
        (take-credits state :runner)
        (play-from-hand state :corp "Caprice Nisei" "Server 1")
        (is (= 11 (:credit (get-corp))) "Gained 3 from Adonis and 1 from HB:EtF")
        (play-from-hand state :corp "Turing" "Server 1")
        (take-credits state :corp 1)
        (is (= 3 (:credit (get-runner))) "Gained 1 from Data Folding")
        (core/gain state :runner :click 2)
        (run-empty-server state "HQ")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (click-prompt state :runner "Pay 3 [Credits] to trash") ; trash Ash
        (is (and (= 1 (:credit (get-runner))) (= 11 (:credit (get-corp)))))
        (core/gain state :runner :credit 1)
        (play-from-hand state :runner "Dirty Laundry")
        (click-prompt state :runner "HQ")
        (run-continue state)
        (click-prompt state :runner "Steal")
        (is (= 2 (:agenda-point (get-runner))) "Stole Global Food Initiative")
        (is (and (= 6 (:credit (get-runner))) (= 10 (:credit (get-corp))))
            "Desperado plus Dirty Laundry, Lamprey took 1 from Corp")
        (run-on state "Server 1")
        (let [tur (get-ice state :remote1 0)
              cap (get-content state :remote1 2)]
          (rez state :corp tur)
          (run-continue state)
          (is (= 5 (get-strength (refresh tur))) "Turing +3 strength protecting a remote")
          (card-subroutine state :corp tur 0) ; end the run
          (click-prompt state :runner "End the run")
          (play-from-hand state :runner "Emergency Shutdown")
          (click-card state :runner tur)
          (is (not (:rezzed (refresh tur))) "Turing derezzed")
          (run-on state "Server 1") ; letting Runner in this time to use Caprice
          (rez state :corp cap)
          (run-continue state)
          ;; Caprice psi game started automatically
          (click-prompt state :corp "1 [Credits]")
          (click-prompt state :runner "2 [Credits]")
          (is (not (:run @state)) "Corp won Caprice psi game and ended the run"))))))

(deftest companions
  ;; Fencer Fueno, Mystic Maemi, Trickster Taka:
  ;; Gain 1c on start of turn or agenda steal
  (letfn [(companion-test [card]
            (do-game
              (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                                :hand ["Hostile Takeover"]}
                         :runner {:hand [card]}})
              (take-credits state :corp)
              (play-from-hand state :runner card)
              (let [cc (get-resource state 0)
                    counters (get-counters (refresh cc) :credit)]
                (is (zero? (get-counters (refresh cc) :credit)) "Companion starts with 0 credits")
                (run-empty-server state "HQ")
                (click-prompt state :runner "Steal")
                (is (= (inc counters) (get-counters (refresh cc) :credit)) "Companion gains 1c for stealing agenda")
                (run-empty-server state "Archives")
                (is (= (inc counters) (get-counters (refresh cc) :credit)) "Companion doesn't gain 1c when no agenda stolen"))))]
    (doall (map companion-test
                ["Fencer Fueno"
                 "Trickster Taka"
                 "Mystic Maemi"]))))

