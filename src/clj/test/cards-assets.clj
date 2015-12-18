(in-ns 'test.core)

(deftest adonis-campaign
  (do-game
    (new-game (default-corp [(qty "Adonis Campaign" 1)]) (default-runner))
    (play-from-hand state :corp "Adonis Campaign" "New remote")
    (let [ac (first (get-in @state [:corp :servers :remote1 :content]))]
      (core/rez state :corp ac)
      (is (= 1 (get-in @state [:corp :credit])))
      (is (= 12 (get-in (refresh ac) [:counter])))
      (take-credits state :corp 2)
      (take-credits state :runner)
      (is (= 6 (get-in @state [:corp :credit])))
      (is (= 9 (get-in (refresh ac) [:counter])))
      )))

(deftest aggressive-secretary
  (do-game
    (new-game
      (default-corp [(qty "Aggressive Secretary" 1)])
      (default-runner [(qty "Cache" 3)]))
    (play-from-hand state :corp "Aggressive Secretary" "New remote")
    (let [as (first (get-in @state [:corp :servers :remote1 :content]))]
      (core/advance state :corp as)
      (take-credits state :corp)
      ;Run on AggSec with 3 programs
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Cache")
      (core/run state :runner :remote1)
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (prompt-choice :corp "Yes")
      (is (= 3 (get-in @state [:corp :credit])))
      (prompt-select :corp (get-in @state [:runner :rig :program 1]))
      (prompt-choice :corp "Done")
      ;There should be one Cache left
      (is (= 3 (get-in @state [:corp :credit])))
      (is (=
            2
            (count (get-in @state [:runner :rig :program]))))
      )))

(deftest alix-t4lb07
  (do-game
    (new-game
      (default-corp [(qty "Alix T4LB07" 1) (qty "PAD Campaign" 3)])
      (default-runner))
    (play-from-hand state :corp "Alix T4LB07" "New remote")
    (let [alix (first (get-in @state [:corp :servers :remote1 :content]))]
      (core/rez state :corp alix)
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 2 (get-in (refresh alix) [:counter])))
      (is (= 4 (get-in @state [:corp :credit])))
      (card-ability state :corp alix 0)
      (is (= 8 (get-in @state [:corp :credit])))
      )))

(deftest eve-campaign
  (do-game
    (new-game (default-corp [(qty "Eve Campaign" 1)]) (default-runner))
    (play-from-hand state :corp "Eve Campaign" "New remote")
    (let [eve (first (get-in @state [:corp :servers :remote1 :content]))]
      (core/rez state :corp eve)
      (is (= 0 (get-in @state [:corp :credit])))
      (is (= 16 (get-in (refresh eve) [:counter])))
      (take-credits state :corp 2)
      (take-credits state :runner)
      (is (= 4 (get-in @state [:corp :credit])))
      (is (= 14 (get-in (refresh eve) [:counter])))
      )))

(deftest franchise-city
  (do-game
    (new-game (default-corp [(qty "Franchise City" 1) (qty "Accelerated Beta Test" 1)])
              (default-runner))
    (play-from-hand state :corp "Franchise City" "New remote")
    (play-from-hand state :corp "Accelerated Beta Test" "New remote")
    (core/rez state :corp (first (get-in @state [:corp :servers :remote1 :content])))
    (take-credits state :corp 1)
    (core/run state :runner :remote2)
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    (prompt-choice :runner "Steal")
    (is (= 0 (count (get-in @state [:corp :servers :server2 :content]))) "Agenda was stolen")
    (is (= 2 (:agenda-point (get-runner))) "Runner stole 2 points")
    (is (= 0 (count (get-in @state [:corp :servers :server1 :content]))) "Franchise City no longer installed")
    (is (find-card "Franchise City" (:scored (get-corp))) "Franchise City in corp scored area")
    (is (= 1 (:agenda-point (get-corp))) "Corp has 1 point")))

(deftest jackson-howard-draw
  "Jackson Howard - Draw 2 cards"
  (do-game
    (new-game (default-corp [(qty "Jackson Howard" 3) (qty "Hedge Fund" 3) (qty "Restructure" 2)])
              (default-runner))
    ; guaranteed to be at least 1 jhow in hand after draw, and 2 cards in R&D
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (let [jhow (first (get-in @state [:corp :servers :remote1 :content]))]
      (core/rez state :corp jhow)
      (is (= 5 (count (:hand (get-corp)))))
      (is (= 2 (:click (get-corp))))
      (card-ability state :corp jhow 0)
      (is (= 7 (count (:hand (get-corp)))) "Drew 2 cards")
      (is (= 1 (:click (get-corp)))))))

(deftest launch-campaign
  (do-game
    (new-game (default-corp [(qty "Launch Campaign" 1)]) (default-runner))
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (let [launch (first (get-in @state [:corp :servers :remote1 :content]))]
      (core/rez state :corp launch)
      (is (= 4 (get-in @state [:corp :credit])))
      (is (= 6 (get-in (refresh launch) [:counter])))
      (take-credits state :corp 2)
      (take-credits state :runner)
      (is (= 8 (get-in @state [:corp :credit])))
      (is (= 4 (get-in (refresh launch) [:counter])))
      )))

(deftest public-support
  "Public support scoring and trashing"
  ;TODO could also test for NOT triggering "when scored" events
  (do-game
    (new-game (default-corp [(qty "Public Support" 2)]) (default-runner))
    ; Corp turn 1, install and rez public supports
    (play-from-hand state :corp "Public Support" "New remote")
    (play-from-hand state :corp "Public Support" "New remote")
    (let [publics1 (first (get-in @state [:corp :servers :remote1 :content]))
          publics2 (first (get-in @state [:corp :servers :remote2 :content]))]
      (core/rez state :corp (refresh publics1))
      (core/rez state :corp (refresh publics2))
      (take-credits state :corp)
      ; Runner turn 1, creds
      (is (= 2 (:credit (get-corp))))
      (is (= 3 (get-in (refresh publics1) [:counter])))
      (take-credits state :runner)
      ; Corp turn 2, creds, check if supports are ticking
      (is (= 2 (get-in (refresh publics1) [:counter])))
      (is (= 0 (:agenda-point (get-corp))))
      (take-credits state :corp)
      ; Runner turn 2, run and trash publics2
      (core/click-run state :runner {:server :remote2})
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (prompt-choice :runner "Yes") ; pay to trash
      (is (= 5 (:credit (get-runner))))
      (take-credits state :runner)
      ; Corp turn 3, check how publics1 is doing
      (is (= 1 (get-in (refresh publics1) [:counter])))
      (is (= 0 (:agenda-point (get-corp))))
      (take-credits state :corp)
      ; Runner turn 3, boring
      (take-credits state :runner)
      ; Corp turn 4, check the delicious agenda points
      (is (= 1 (:agenda-point (get-corp))))
      (is (= (:zone (refresh publics1) :scored)))
      (is (= (:zone (refresh publics2) :discard)))
      (is (= "Public Support" (:title (first (get-in @state [:corp :scored])))))
      (is (= 1 (:agendapoints (first (get-in @state [:corp :scored])))))
      )))

(deftest sundew
  "Sundew"
  (do-game
    (new-game (default-corp [(qty "Sundew" 1)])
              (default-runner))
    (play-from-hand state :corp "Sundew" "New remote")
    (let [sund (first (get-in @state [:corp :servers :remote1 :content]))]
      (core/rez state :corp sund)
      (take-credits state :corp 2)
      (is (= 5 (:credit (get-corp))) "Cost 2cr to rez")
      ; spend a click not on a run
      (take-credits state :runner)
      (is (= 7 (:credit (get-corp))) "Corp gained 2cr from Sundew")
      (take-credits state :corp)
      (core/click-run state :runner {:server :remote1})
      (is (= 10 (:credit (get-corp))) "Corp did not gain 2cr from run on Sundew")
      (is (= 3 (:click (get-runner))) "Runner spent 1 click to start run"))))

;(deftest sundew-dirty-laundry
;  "Sundew - Dirty Laundry"
;  (do-game
;    (new-game (default-corp [(qty "Sundew" 1)])
;              (default-runner [(qty "Dirty Laundry" 1)]))
;    (play-from-hand state :corp "Sundew" "New remote")
;    (let [sund (first (get-in @state [:corp :servers :remote1 :content]))]
;      (core/rez state :corp sund)
;      (take-credits state :corp 2)
;      (is (= 5 (:credit (get-corp))) "Cost 2cr to rez")
;      ; spend a click on a run through a card, not through click-run.
;      (play-run-event state (find-card "Dirty Laundry" (:hand (get-runner))) :remote1)
;      (is (= 5 (:credit (get-corp))) "Corp did not gain 2cr from run on Sundew"))))

(deftest team-sponsorship-hq
  "Team Sponsorship - Install from HQ"
  (do-game
    (new-game (default-corp [(qty "Domestic Sleepers" 1) (qty "Team Sponsorship" 1) (qty "Adonis Campaign" 1)])
              (default-runner))
    (play-from-hand state :corp "Team Sponsorship" "New remote")
    (play-from-hand state :corp "Domestic Sleepers" "New remote")
    (let [ag1 (get-in @state [:corp :servers :remote2 :content 0])
          tsp (get-in @state [:corp :servers :remote1 :content 0])]
      (core/rez state :corp tsp)
      (core/gain state :corp :click 6 :credit 6)
      (core/advance state :corp {:card (refresh ag1)})
      (core/advance state :corp {:card (refresh ag1)})
      (core/score state :corp {:card (refresh ag1)})
      (is (= (:ts-active (refresh tsp)) true) "Team Sponsorship ability enabled")
      (card-ability state :corp tsp 0)
      (prompt-select :corp (find-card "Adonis Campaign" (:hand (get-corp))))
      (prompt-choice :corp "New remote")
      (is (= "Adonis Campaign" (get-in @state [:corp :servers :remote3 :content 0 :title])) "Adonis installed by Team Sponsorship")
      (is (nil? (find-card "Adonis Campaign" (:hand (get-corp)))) "No Adonis in hand")
      (is (nil? (:ts-active (refresh tsp))) "Team Sponsorship ability disabled"))))

(deftest team-sponsorship-archives
  "Team Sponsorship - Install from Archives"
  (do-game
    (new-game (default-corp [(qty "Domestic Sleepers" 1) (qty "Team Sponsorship" 1) (qty "Adonis Campaign" 1)])
              (default-runner))
    (play-from-hand state :corp "Team Sponsorship" "New remote")
    (play-from-hand state :corp "Domestic Sleepers" "New remote")
    (core/move state :corp (find-card "Adonis Campaign" (:hand (get-corp))) :discard)
    (let [ag1 (get-in @state [:corp :servers :remote2 :content 0])
          tsp (get-in @state [:corp :servers :remote1 :content 0])]
      (core/rez state :corp tsp)
      (core/gain state :corp :click 6 :credit 6)
      (core/advance state :corp {:card (refresh ag1)})
      (core/advance state :corp {:card (refresh ag1)})
      (core/score state :corp {:card (refresh ag1)})
      (is (= (:ts-active (refresh tsp)) true) "Team Sponsorship ability enabled")
      (card-ability state :corp tsp 0)
      (prompt-select :corp (find-card "Adonis Campaign" (:discard (get-corp))))
      (prompt-choice :corp "New remote")
      (is (= "Adonis Campaign" (get-in @state [:corp :servers :remote3 :content 0 :title])) "Adonis installed by Team Sponsorship")
      (is (nil? (find-card "Adonis Campaign" (:discard (get-corp)))) "No Adonis in discard")
      (is (nil? (:ts-active (refresh tsp))) "Team Sponsorship ability disabled"))))

(deftest team-sponsorship-multiple
  "Team Sponsorship - Multiple installed"
  (do-game
    (new-game (default-corp [(qty "Domestic Sleepers" 1) (qty "Team Sponsorship" 2) (qty "Adonis Campaign" 2)])
              (default-runner))
    (play-from-hand state :corp "Team Sponsorship" "New remote")
    (play-from-hand state :corp "Team Sponsorship" "New remote")
    (play-from-hand state :corp "Domestic Sleepers" "New remote")
    (core/move state :corp (find-card "Adonis Campaign" (:hand (get-corp))) :discard)
    (let [ag1 (get-in @state [:corp :servers :remote3 :content 0])
          tsp2 (get-in @state [:corp :servers :remote2 :content 0])
          tsp1 (get-in @state [:corp :servers :remote1 :content 0])]
      (core/rez state :corp tsp1)
      (core/rez state :corp tsp2)
      (core/gain state :corp :click 6 :credit 6)
      (core/advance state :corp {:card (refresh ag1)})
      (core/advance state :corp {:card (refresh ag1)})
      (core/score state :corp {:card (refresh ag1)})
      (is (= (:ts-active (refresh tsp1)) true) "Team Sponsorship 1 ability enabled")
      (is (= (:ts-active (refresh tsp2)) true) "Team Sponsorship 2 ability enabled")
      (card-ability state :corp tsp1 0)
      (prompt-select :corp (find-card "Adonis Campaign" (:discard (get-corp))))
      (prompt-choice :corp "New remote")
      (card-ability state :corp tsp2 0)
      (prompt-select :corp (find-card "Adonis Campaign" (:hand (get-corp))))
      (prompt-choice :corp "New remote")
      (is (= "Adonis Campaign" (get-in @state [:corp :servers :remote4 :content 0 :title])) "Adonis installed by Team Sponsorship")
      (is (= "Adonis Campaign" (get-in @state [:corp :servers :remote5 :content 0 :title])) "Adonis installed by Team Sponsorship")
      (is (nil? (:ts-active (refresh tsp1))) "Team Sponsorship 1 ability disabled")
      (is (nil? (:ts-active (refresh tsp2))) "Team Sponsorship 2 ability disabled"))))
