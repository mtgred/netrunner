(in-ns 'test.core)

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
      (is (= true (:ts-install (core/get-card state tsp)))) ; TSP ability enabled by score
      (card-ability state :corp tsp 0)
      ; TSP prompt should be active
      (is (= (:cid tsp) (get-in @state [:corp :prompt 0 :card :cid])))
      (prompt-choice :corp "HQ")
      (prompt-card :corp (find-card "Adonis Campaign" (:hand (get-corp))))
      (prompt-choice :corp "New remote")
      (is (= "Adonis Campaign" (get-in @state [:corp :servers :remote3 :content 0 :title])) "Adonis installed by Team Sponsorship")
      (is (nil? (find-card "Adonis Campaign" (:hand (get-corp)))) "No Adonis in hand")
      (is (nil? (:ts-install (core/get-card state tsp))) "Team Sponsorship ability disabled"))))

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
      (is (= true (:ts-install (core/get-card state tsp)))) ; TSP ability enabled by score
      (card-ability state :corp tsp 0)
      ; TSP prompt should be active
      (is (= (:cid tsp) (get-in @state [:corp :prompt 0 :card :cid])))
      (prompt-choice :corp "Archives")
      (prompt-card :corp (find-card "Adonis Campaign" (:discard (get-corp))))
      (prompt-choice :corp "New remote")
      (is (= "Adonis Campaign" (get-in @state [:corp :servers :remote3 :content 0 :title])) "Adonis installed by Team Sponsorship")
      (is (nil? (find-card "Adonis Campaign" (:discard (get-corp)))) "No Adonis in discard")
      (is (nil? (:ts-install (core/get-card state tsp))) "Team Sponsorship ability disabled"))))

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
          tsp1 (get-in @state [:corp :servers :remote2 :content 0])
          tsp2 (get-in @state [:corp :servers :remote1 :content 0])]
      (core/rez state :corp tsp1)
      (core/rez state :corp tsp2)
      (core/gain state :corp :click 6 :credit 6)
      (core/advance state :corp {:card (refresh ag1)})
      (core/advance state :corp {:card (refresh ag1)})
      (core/score state :corp {:card (refresh ag1)})
      (is (= true (:ts-install (core/get-card state tsp1)))) ; TSP1 ability enabled by score
      (is (= true (:ts-install (core/get-card state tsp2)))) ; TSP2 ability enabled by score
      (card-ability state :corp tsp1 0)
      ; TSP1 prompt should be active
      (is (= (:cid tsp1) (get-in @state [:corp :prompt 0 :card :cid])))
      (prompt-choice :corp "Archives")
      (prompt-card :corp (find-card "Adonis Campaign" (:discard (get-corp))))
      (prompt-choice :corp "New remote")
      (card-ability state :corp tsp2 0)
      ; TSP2 prompt should be active
      (is (= (:cid tsp2) (get-in @state [:corp :prompt 0 :card :cid])))
      (prompt-choice :corp "HQ")
      (prompt-card :corp (find-card "Adonis Campaign" (:hand (get-corp))))
      (prompt-choice :corp "New remote")
      (is (= "Adonis Campaign" (get-in @state [:corp :servers :remote4 :content 0 :title])) "Adonis installed by Team Sponsorship")
      (is (= "Adonis Campaign" (get-in @state [:corp :servers :remote5 :content 0 :title])) "Adonis installed by Team Sponsorship")
      (is (nil? (:ts-install (core/get-card state tsp1))) "Team Sponsorship 1 ability disabled")
      (is (nil? (:ts-install (core/get-card state tsp2))) "Team Sponsorship 2 ability disabled"))))
