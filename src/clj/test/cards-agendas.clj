(in-ns 'test.core)

(deftest fifteen-minutes
  "15 Minutes - check if it works correctly from both sides"
  (do-game
    (new-game (default-corp [(qty "15 Minutes" 1)]) (default-runner))
    (play-from-hand state :corp "15 Minutes" "New remote")
    (take-credits state :corp)
    ; use 15 minutes to take it away from runner
    (core/click-run state :runner {:server "Server 1"})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    (prompt-choice :runner "Steal")
    (take-credits state :runner)
    (is (= 1 (:agenda-point (get-runner))))
    (is (= 1 (count (:scored (get-runner)))))
    (let [fifm (first (:scored (get-runner)))]
      (is (= 3 (:click (get-corp))))
      (is (= 1 (count (:abilities (refresh fifm)))))
      (card-ability state :corp (refresh fifm) 0)
      (is (= 0 (:agenda-point (get-runner))))
      (is (= 0 (count (:scored (get-runner))))))
    (is (= "15 Minutes" (:title (first (:deck (get-corp))))))
    ; TODO: could also check for deck shuffle
    (is (= 2 (:click (get-corp))))
    ; use 15 minutes to take it away from corp (hey, maybe some obscure case happens where corp would want that)
    (core/click-draw state :corp 1)
    (play-from-hand state :corp "15 Minutes" "New remote")
    (take-credits state :runner)
    (score-agenda state :corp (get-in @state [:corp :servers :remote2 :content 0]))
    (is (= 1 (:agenda-point (get-corp))))
    (is (= 1 (count (:scored (get-corp)))))
    (let [fifm (first (:scored (get-corp)))]
      (is (= 1 (count (:abilities (refresh fifm)))))
      (card-ability state :corp (refresh fifm) 0)
      (is (= 0 (:agenda-point (get-corp))))
      (is (= 0 (count (:scored (get-corp))))))
    (is (= "15 Minutes" (:title (first (:deck (get-corp))))))))

(deftest ancestral-imager
  "Ancestral Imager - damage on jack out"
  (do-game
    (new-game (default-corp [(qty "Ancestral Imager" 3)]) (default-runner))
    (play-from-hand state :corp "Ancestral Imager" "New remote")
    (let [ai (get-in @state [:corp :servers :remote1 :content 0])]
      (score-agenda state :corp ai)
      (take-credits state :corp)
      (is (= 3 (count(get-in @state [:runner :hand]))))
      (core/click-run state :runner {:server :hq})
      (core/jack-out state :runner nil)
      (is (= 2 (count(get-in @state [:runner :hand])))))))

(deftest breaking-news
  "Test scoring breaking news"
  (do-game
    (new-game (default-corp [(qty "Breaking News" 3)]) (default-runner))
    (play-from-hand state :corp "Breaking News" "New remote")
    (score-agenda state :corp (get-in @state [:corp :servers :remote1 :content 0]))
    (is (= 2 (get-in @state [:runner :tag])))
    (take-credits state :corp)
    (is (= 0 (get-in @state [:runner :tag])))))

(deftest fetal-ai-damage
  "Fetal AI - damage on access"
  (do-game
    (new-game (default-corp [(qty "Fetal AI" 3)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Diesel" 3) (qty "Quality Time" 3)]))
    (play-from-hand state :corp "Fetal AI" "New remote")
    (take-credits state :corp 2)
    (core/click-run state :runner {:server :remote1})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    (prompt-choice :runner "Yes")
    (is (= 3 (count (:hand (get-runner)))) "Runner took 2 net damage from Fetal AI")
    (is (= 3 (:credit (get-runner))) "Runner paid 2cr to steal Fetal AI")
    (is (= 1 (count (:scored (get-runner)))) "Runner stole Fetal AI")))

(deftest fetal-ai-cant-afford
  "Fetal AI - can't afford to steal"
  (do-game
    (new-game (default-corp [(qty "Fetal AI" 3)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Diesel" 3) (qty "Quality Time" 3)]))
    (play-from-hand state :corp "Fetal AI" "New remote")
    (take-credits state :corp 2)
    (core/lose state :runner :credit 5)
    (core/click-run state :runner {:server :remote1})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    (prompt-choice :runner "Yes")
    (is (= 3 (count (:hand (get-runner)))) "Runner took 2 net damage from Fetal AI")
    (is (= 0 (count (:scored (get-runner)))) "Runner could not steal Fetal AI")))

(deftest napd-contract
  "NAPD Contract - Requires 4 credits to steal; scoring requirement increases with bad publicity"
  (do-game
    (new-game (default-corp [(qty "NAPD Contract" 1)])
              (default-runner))
    (play-from-hand state :corp "NAPD Contract" "New remote")
      (let [napd (get-in @state [:corp :servers :remote1 :content 0])]
        (core/advance state :corp {:card (refresh napd)})
        (core/advance state :corp {:card (refresh napd)})
        (take-credits state :corp)
        (core/lose state :runner :credit 2)
        (core/click-run state :runner {:server :remote1})
        (core/no-action state :corp nil)
        (core/successful-run state :runner nil)
        (prompt-choice :runner "Yes")
        (is (= 0 (count (:scored (get-runner)))) "Runner could not steal NAPD Contract")
        (is (= 3 (:credit (get-runner))) "Runner couldn't afford to steal, so no credits spent")
        (take-credits state :runner)
        (core/gain state :corp :bad-publicity 1)
        (core/advance state :corp {:card (refresh napd)})
        (core/advance state :corp {:card (refresh napd)})
        (core/score state :corp {:card (refresh napd)})
        (is (not (nil? (get-in @state [:corp :servers :remote1 :content 0]))) "Corp can't score with 4 advancements because of BP")
        (core/advance state :corp {:card (refresh napd)})
        (core/score state :corp {:card (refresh napd)})
        (is (= 2 (:agenda-point (get-corp))) "Scored NAPD for 2 points after 5 advancements"))))

(deftest oaktown-renovation
  "Oaktown Renovation - Installed face up, gain credits with each conventional advancement"
  (do-game
    (new-game (default-corp [(qty "Oaktown Renovation" 1) (qty "Shipment from SanSan" 1)])
              (default-runner))
    (core/gain state :corp :click 3)
    (play-from-hand state :corp "Oaktown Renovation" "New remote")
    (let [oak (get-in @state [:corp :servers :remote1 :content 0])]
      (is (get-in (refresh oak) [:rezzed]) "Oaktown installed face up")
      (core/advance state :corp {:card (refresh oak)})
      (is (= 6 (:credit (get-corp))) "Spent 1 credit to advance, gained 2 credits from Oaktown")
      (play-from-hand state :corp "Shipment from SanSan")
      (prompt-choice :corp "2")
      (prompt-select :corp oak)
      (is (= 3 (:advance-counter (refresh oak))))
      (is (= 6 (:credit (get-corp))) "No credits gained due to advancements being placed")
      (core/advance state :corp {:card (refresh oak)})
      (is (= 7 (:credit (get-corp))) "Spent 1 credit to advance, gained 2 credits from Oaktown")
      (core/advance state :corp {:card (refresh oak)})
      (is (= 5 (:advance-counter (refresh oak))))
      (is (= 9 (:credit (get-corp))) "Spent 1 credit to advance, gained 3 credits from Oaktown"))))

(deftest profiteering
  "Profiteering - Gain 5 credits per bad publicity taken"
  (do-game
    (new-game (default-corp [(qty "Profiteering" 1)])
              (default-runner))
    (play-from-hand state :corp "Profiteering" "New remote")
    (let [prof (get-in @state [:corp :servers :remote1 :content 0])]
      (score-agenda state :corp prof)
      (is (= 1 (:agenda-point (get-corp))))
      (prompt-choice :corp "3")
      (is (= 3 (:bad-publicity (get-corp))) "Took 3 bad publicity")
      (is (= 20 (:credit (get-corp))) "Gained 15 credits"))))

(deftest project-beale
  "Project Beale - Extra agenda points for over-advancing"
  (do-game
    (new-game (default-corp [(qty "Project Beale" 2)])
              (default-runner))
    (core/gain state :corp :click 8 :credit 8)
    (play-from-hand state :corp "Project Beale" "New remote")
    (let [pb1 (get-in @state [:corp :servers :remote1 :content 0])]
      (core/advance state :corp {:card (refresh pb1)})
      (core/advance state :corp {:card (refresh pb1)})
      (core/advance state :corp {:card (refresh pb1)})
      (core/advance state :corp {:card (refresh pb1)})
      (core/score state :corp {:card (refresh pb1)})
      (is (= 2 (:agenda-point (get-corp))) "Only 4 advancements: scored for standard 2 points")
      (play-from-hand state :corp "Project Beale" "New remote")
        (let [pb2 (get-in @state [:corp :servers :remote2 :content 0])]
          (core/advance state :corp {:card (refresh pb2)})
          (core/advance state :corp {:card (refresh pb2)})
          (core/advance state :corp {:card (refresh pb2)})
          (core/advance state :corp {:card (refresh pb2)})
          (core/advance state :corp {:card (refresh pb2)})
          (core/score state :corp {:card (refresh pb2)})
          (is (= 5 (:agenda-point (get-corp))) "5 advancements: scored for 3 points")))))

(deftest the-cleaners
  "The Cleaners - Bonus damage"
  (do-game
    (new-game (default-corp [(qty "The Cleaners" 1) (qty "Scorched Earth" 1)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Diesel" 3)]))
    (play-from-hand state :corp "The Cleaners" "New remote")
    (let [clean (first (get-in @state [:corp :servers :remote1 :content]))]
      (score-agenda state :corp clean)
      (core/gain state :runner :tag 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (= 0 (count (:hand (get-runner)))) "5 damage dealt to Runner"))))
