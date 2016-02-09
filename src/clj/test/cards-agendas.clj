(in-ns 'test.core)

(deftest fifteen-minutes
  "15 Minutes - check if it works correctly from both sides"
  (do-game
    (new-game (default-corp [(qty "15 Minutes" 1)]) (default-runner))
    (play-from-hand state :corp "15 Minutes" "New remote")
    (take-credits state :corp)
    ;; use 15 minutes to take it away from runner
    (run-empty-server state "Server 1")
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
    ;; TODO: could also check for deck shuffle
    (is (= 2 (:click (get-corp))))
    ;; use 15 minutes to take it away from corp (hey, maybe some obscure case happens where corp would want that)
    (core/click-draw state :corp 1)
    (play-from-hand state :corp "15 Minutes" "New remote")
    (take-credits state :runner)
    (score-agenda state :corp (get-content state :remote2 0))
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
    (new-game (default-corp [(qty "Ancestral Imager" 3)])
              (default-runner))
    (play-from-hand state :corp "Ancestral Imager" "New remote")
    (let [ai (get-content state :remote1 0)]
      (score-agenda state :corp ai)
      (take-credits state :corp)
      (is (= 3 (count(get-in @state [:runner :hand]))) "Runner has 3 cards in hand")
      (run-on state :hq)
      (run-jack-out state)
      (is (= 2 (count(get-in @state [:runner :hand]))) "Runner took 1 net damage"))))

(deftest astro-script-token
  "AstroScript token placement"
  (do-game
    (new-game (default-corp [(qty "AstroScript Pilot Program" 3) (qty "Ice Wall" 2)])
              (default-runner))
    (core/gain state :corp :click 3)
    (letfn [(try-place [from to]
              (card-ability state :corp (refresh from) 0)
              (prompt-select :corp (refresh to)))
            (should-not-place [from to msg]
              (try-place from to)
              (prompt-choice :corp "Done")
              (is (= 1 (:counter (refresh from)))
                  (str (:title from)" token was not used on " (:title to) msg))
              (is (or (= nil (:advance-counter (refresh to)))
                      (= 0 (:advance-counter (refresh to))))
                  (str "Advancement token not placed on " (:title to) msg)))
            (should-place [from to msg]
              (try-place from to)
              (is (= 0 (:counter (refresh from)))
                  (str (:title from) " token was used on " (:title to) msg))
              (is (= 1 (:advance-counter (refresh to)))
                  (str "Advancement token placed on " (:title to) msg)))]
      (play-from-hand state :corp "AstroScript Pilot Program" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (play-from-hand state :corp "AstroScript Pilot Program" "New remote")
      (let [scored-astro (get-in @state [:corp :scored 0])
            installed-astro (get-content state :remote2 0)
            hand-astro (find-card "AstroScript Pilot Program" (:hand get-corp))]
        (should-not-place scored-astro hand-astro " in hand")
        (should-place scored-astro installed-astro " that is installed")
        (core/advance state :corp {:card (refresh installed-astro)})
        (core/advance state :corp {:card (refresh installed-astro)})
        (core/score   state :corp {:card (refresh installed-astro)}))
      (play-from-hand state :corp "Ice Wall" "HQ")
      (let [no-token-astro (get-in @state [:corp :scored 0])
            token-astro (get-in @state [:corp :scored 1])
            hand-ice-wall (find-card "Ice Wall" (:hand get-corp))
            installed-ice-wall (get-ice state :hq 0)]
        (should-not-place token-astro no-token-astro " that is scored")
        (should-not-place token-astro hand-ice-wall " in hand")
        (should-place token-astro installed-ice-wall " that is installed")))))

(deftest breaking-news
  "Test scoring breaking news"
  (do-game
    (new-game (default-corp [(qty "Breaking News" 3)])
              (default-runner))
    (play-from-hand state :corp "Breaking News" "New remote")
    (score-agenda state :corp (get-content state :remote1 0))
    (is (= 2 (get-in @state [:runner :tag])) "Runner receives 2 tags from Breaking News")
    (take-credits state :corp)
    (is (= 0 (get-in @state [:runner :tag]))) "Two tags removed at the end of the turn"))

(deftest eden-fragment
  "Test that Eden Fragment ignores the install cost of the first ice"
  (do-game
    (new-game (default-corp [(qty "Eden Fragment" 3) (qty "Ice Wall" 3)])
              (default-runner))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Eden Fragment" "New remote")
    (score-agenda state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (take-credits state :runner)
    (take-credits state :runner)
    (take-credits state :runner)
    (take-credits state :runner)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (not (nil? (get-ice state :hq 1))) "Corp has two ice installed on HQ")
    (is (= 6 (get-in @state [:corp :credit])) "Corp does not pay for installing the first ICE of the turn")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (not (nil? (get-ice state :hq 2))) "Corp has three ice installed on HQ")
    (is (= 4 (get-in @state [:corp :credit])) "Corp pays for installing the second ICE of the turn")))

(deftest fetal-ai-damage
  "Fetal AI - damage on access"
  (do-game
    (new-game (default-corp [(qty "Fetal AI" 3)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Diesel" 3) (qty "Quality Time" 3)]))
    (play-from-hand state :corp "Fetal AI" "New remote")
    (take-credits state :corp 2)
    (run-empty-server state "Server 1")
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
    (run-empty-server state "Server 1")
    (prompt-choice :runner "Yes")
    (is (= 3 (count (:hand (get-runner)))) "Runner took 2 net damage from Fetal AI")
    (is (= 0 (count (:scored (get-runner)))) "Runner could not steal Fetal AI")))

(deftest genetic-resequencing
  "Genetic Resequencing - Place 1 agenda counter on a scored agenda"
  (do-game
    (new-game (default-corp [(qty "Genetic Resequencing" 1) (qty "Braintrust" 2)])
              (default-runner))
    (play-from-hand state :corp "Braintrust" "New remote")
    (play-from-hand state :corp "Braintrust" "New remote")
    (play-from-hand state :corp "Genetic Resequencing" "New remote")
    (let [bt1 (get-content state :remote1 0)
          bt2 (get-content state :remote2 0)
          gr (get-content state :remote3 0)]
      (score-agenda state :corp bt1)
      (let [btscored (get-in @state [:corp :scored 0])]
        (is (= 0 (:counter (refresh btscored))) "No agenda counters on scored Braintrust")
        (score-agenda state :corp gr)
        (prompt-select :corp bt2)
        (is (nil? (:counter (refresh bt2))) 
            "No agenda counters on installed Braintrust; not a valid target")
        (prompt-select :corp btscored)
        (is (= 1 (:counter (refresh btscored)))
            "1 agenda counter placed on scored Braintrust")))))

(deftest high-risk-investment
  "High-Risk Investment - Gain 1 agenda counter when scored; spend it to gain credits equal to Runner's credits"
  (do-game
    (new-game (default-corp [(qty "High-Risk Investment" 1)])
              (default-runner))
    (play-from-hand state :corp "High-Risk Investment" "New remote")
    (let [hri (get-content state :remote1 0)]
      (score-agenda state :corp hri)
      (let [hriscored (get-in @state [:corp :scored 0])]
        (is (= 1 (:counter (refresh hriscored))) "Has 1 agenda counter")
        (take-credits state :corp)
        (is (= 7 (:credit (get-corp))))
        (take-credits state :runner)
        (is (= 9 (:credit (get-runner))))
        (card-ability state :corp hriscored 0)
        (is (= 16 (:credit (get-corp))) "Gained 9 credits")
        (is (= 2 (:click (get-corp))) "Spent 1 click")
        (is (= 0 (:counter (refresh hriscored))) "Spent agenda counter")))))

(deftest hostile-takeover
  "Hostile Takeover - Gain 7 credits and take 1 bad publicity"
  (do-game
    (new-game (default-corp [(qty "Hostile Takeover" 1)])
              (default-runner))
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (let [ht (get-content state :remote1 0)]
      (score-agenda state :corp ht)
      (is (= 12 (:credit (get-corp))) "Gain 7 credits")
      (is (= 1 (:bad-publicity (get-corp))) "Take 1 bad publicity"))))

(deftest napd-contract
  "NAPD Contract - Requires 4 credits to steal; scoring requirement increases with bad publicity"
  (do-game
    (new-game (default-corp [(qty "NAPD Contract" 1)])
              (default-runner))
    (play-from-hand state :corp "NAPD Contract" "New remote")
      (let [napd (get-content state :remote1 0)]
        (core/advance state :corp {:card (refresh napd)})
        (core/advance state :corp {:card (refresh napd)})
        (take-credits state :corp)
        (core/lose state :runner :credit 2)
        (run-empty-server state "Server 1")
        (prompt-choice :runner "Yes")
        (is (= 0 (count (:scored (get-runner)))) "Runner could not steal NAPD Contract")
        (is (= 3 (:credit (get-runner))) "Runner couldn't afford to steal, so no credits spent")
        (take-credits state :runner)
        (core/gain state :corp :bad-publicity 1)
        (core/advance state :corp {:card (refresh napd)})
        (core/advance state :corp {:card (refresh napd)})
        (core/score state :corp {:card (refresh napd)})
        (is (not (nil? (get-content state :remote1 0)))
            "Corp can't score with 4 advancements because of BP")
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
    (let [oak (get-content state :remote1 0)]
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
      (is (= 9 (:credit (get-corp)))
          "Spent 1 credit to advance, gained 3 credits from Oaktown"))))

(deftest profiteering
  "Profiteering - Gain 5 credits per bad publicity taken"
  (do-game
    (new-game (default-corp [(qty "Profiteering" 1)])
              (default-runner))
    (play-from-hand state :corp "Profiteering" "New remote")
    (let [prof (get-content state :remote1 0)]
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
    (let [pb1 (get-content state :remote1 0)]
      (core/advance state :corp {:card (refresh pb1)})
      (core/advance state :corp {:card (refresh pb1)})
      (core/advance state :corp {:card (refresh pb1)})
      (core/advance state :corp {:card (refresh pb1)})
      (core/score state :corp {:card (refresh pb1)})
      (is (= 2 (:agenda-point (get-corp))) "Only 4 advancements: scored for standard 2 points")
      (play-from-hand state :corp "Project Beale" "New remote")
        (let [pb2 (get-content state :remote2 0)]
          (core/advance state :corp {:card (refresh pb2)})
          (core/advance state :corp {:card (refresh pb2)})
          (core/advance state :corp {:card (refresh pb2)})
          (core/advance state :corp {:card (refresh pb2)})
          (core/advance state :corp {:card (refresh pb2)})
          (core/score state :corp {:card (refresh pb2)})
          (is (= 5 (:agenda-point (get-corp))) "5 advancements: scored for 3 points")))))

(deftest tgtbt
  "TGTBT - Give the Runner 1 tag when they access"
  (do-game
    (new-game (default-corp [(qty "TGTBT" 2) (qty "Old Hollywood Grid" 1)])
              (default-runner))
    (play-from-hand state :corp "TGTBT" "New remote")
    (play-from-hand state :corp "Old Hollywood Grid" "Server 1")
    (play-from-hand state :corp "TGTBT" "New remote")
    (take-credits state :corp)
    (let [tg1 (get-content state :remote1 0)
          ohg (get-content state :remote1 1)]
      (run-on state "Server 1")
      (core/rez state :corp ohg)
      (run-successful state)
      (prompt-select :runner tg1)
      (prompt-choice :runner "OK") ; Accesses TGTBT but can't steal
      (is (= 1 (:tag (get-runner))) "Runner took 1 tag from accessing without stealing")
      (prompt-select :runner ohg))
    (prompt-choice :runner "Yes") ; Trashes OHG
    (run-empty-server state "Server 2")
    (prompt-choice :runner "Steal") ; Accesses TGTBT but can't steal

    (is (= 2 (:tag (get-runner))) "Runner took 1 tag from accessing and stealing")))

(deftest the-cleaners
  "The Cleaners - Bonus damage"
  (do-game
    (new-game (default-corp [(qty "The Cleaners" 1) (qty "Scorched Earth" 1)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Diesel" 3)]))
    (play-from-hand state :corp "The Cleaners" "New remote")
    (let [clean (get-content state :remote1 0)]
      (score-agenda state :corp clean)
      (core/gain state :runner :tag 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (= 0 (count (:hand (get-runner)))) "5 damage dealt to Runner"))))
