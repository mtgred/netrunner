(in-ns 'test.core)

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
      (is (= 2 (count(get-in @state [:runner :hand]))))
      )))

(deftest astro-script-token
  "AstroScript token placement"
  (do-game
    (new-game (default-corp [(qty "AstroScript Pilot Program" 3) (qty "Ice Wall" 2)])
              (default-runner))
    (core/gain state :corp :click 3)
    (let [try-place (fn [from to]
                      (card-ability state :corp (refresh from) 0)
                      (prompt-select :corp (refresh to)))
          should-not-place (fn [from to msg]
                             (try-place from to)
                             (prompt-choice :corp "Done")
                             (is (= 1 (:counter (refresh from))) (str (:title from) " token was not used on " (:title to) msg))
                             (is (or (= nil (:advance-counter (refresh to)))
                                     (= 0 (:advance-counter (refresh to)))) (str "Advancement token not placed on " (:title to) msg)))
          should-place (fn [from to msg]
                         (try-place from to)
                         (is (= 0 (:counter (refresh from))) (str (:title from) " token was used on " (:title to) msg))
                         (is (= 1 (:advance-counter (refresh to))) (str "Advancement token placed on " (:title to) msg)))]
      (play-from-hand state :corp "AstroScript Pilot Program" "New remote")
      (score-agenda state :corp (get-in @state [:corp :servers :remote1 :content 0]))
      (play-from-hand state :corp "AstroScript Pilot Program" "New remote")
      (let [scored-astro (get-in @state [:corp :scored 0])
            installed-astro (get-in @state [:corp :servers :remote2 :content 0])
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
            installed-ice-wall (get-in @state [:corp :servers :hq :ices 0])]
        (should-not-place token-astro no-token-astro " that is scored")
        (should-not-place token-astro hand-ice-wall " in hand")
        (should-place token-astro installed-ice-wall " that is installed")))))

(deftest breaking-news
  "Test scoring breaking news"
  (do-game
    (new-game (default-corp [(qty "Breaking News" 3)]) (default-runner))
    (play-from-hand state :corp "Breaking News" "New remote")
    (score-agenda state :corp (get-in @state [:corp :servers :remote1 :content 0]))
    (is (= 2 (get-in @state [:runner :tag])))
    (take-credits state :corp)
    (is (= 0 (get-in @state [:runner :tag])))
    ))

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
