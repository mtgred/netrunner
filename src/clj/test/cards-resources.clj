(in-ns 'test.core)

(deftest adjusted-chronotype
  "Ensure adjusted chronotype gains only 1 click when 2 clicks are lost"
  (do-game
   (new-game (default-corp) (default-runner [(qty "Adjusted Chronotype" 1) (qty "Beach Party" 2)]))
   (take-credits state :corp)
   (play-from-hand state :runner "Adjusted Chronotype")
   (play-from-hand state :runner "Beach Party")
   (take-credits state :runner)
   (take-credits state :corp)
   (is (= 4 (:click (get-runner))) "Should have lost 1 click and gained 1 click")
   (play-from-hand state :runner "Beach Party")
   (take-credits state :runner)
   (take-credits state :corp)
   (is (= 3 (:click (get-runner))) "Should have lost 2 clicks and gained 1 click")))

(deftest adjusted-chronotype-gcs
  "Ensure adjusted chronotype gains 2 clicks when 2 clicks are lost and GCS is installed"
  (do-game
   (new-game (default-corp) (default-runner [(qty "Adjusted Chronotype" 1) (qty "Beach Party" 3) (qty "Gene Conditioning Shoppe" 1)]))
   (take-credits state :corp)
   (play-from-hand state :runner "Adjusted Chronotype")
   (play-from-hand state :runner "Beach Party")
   (take-credits state :runner)
   (take-credits state :corp)
   (is (= 4 (:click (get-runner))) "Should have lost 1 click and gained 1 click")
   (play-from-hand state :runner "Beach Party")
   (play-from-hand state :runner "Gene Conditioning Shoppe")
   (take-credits state :runner)
   (take-credits state :corp)
   (is (= 4 (:click (get-runner))) "Should have lost 2 clicks and gained 2 clicks")
   (play-from-hand state :runner "Beach Party")
   (take-credits state :runner)
   (take-credits state :corp)
   (is (= 3 (:click (get-runner))) "Should have lost 3 clicks and gained 2 clicks")))

(deftest daily-casts
  "Play and tick through all turns of daily casts"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Daily Casts" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Daily Casts")
    (let [dc (get-in @state [:runner :rig :resource 0])]
      ;Number of credits
      (is (= 8 (get-in dc [:counter])))
      (is (= 2 (get-in @state [:runner :credit])))
      ;End turn
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 6 (get-in (refresh dc) [:counter])))
      (is (= 7 (get-in @state [:runner :credit])))
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 4 (get-in (refresh dc) [:counter])))
      (is (= 13 (get-in @state [:runner :credit])))
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 2 (get-in (refresh dc) [:counter])))
      (is (= 19 (get-in @state [:runner :credit])))
      (take-credits state :runner)
      (take-credits state :corp)
      (is (nil? (get-in @state [:runner :rig :resource 0]))))))

(deftest ddos
  "Prevent rezzing of outermost ice for the rest of the turn"
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 3)]) (default-runner [(qty "DDoS" 1)]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "DDoS")
    (let [ddos (get-in @state [:runner :rig :resource 0])
          iwall (get-in @state [:corp :servers :hq :ices 0])]
      (card-ability state :runner ddos 0)
      (is (= (:title ddos) (get-in @state [:runner :discard 0 :title]) ))
      (core/click-run state :runner {:server "HQ"})
      (core/rez state :corp iwall)
      (is (not (get-in (refresh iwall) [:rezzed])))
      (core/end-run state :runner)
      (core/click-run state :runner {:server "HQ"})
      (core/rez state :corp iwall)
      (is (not (get-in (refresh iwall) [:rezzed])))
      (core/end-run state :runner)
      (take-credits state :runner)
      (take-credits state :corp)
      (core/click-run state :runner {:server "HQ"})
      (core/rez state :corp iwall)
      (is (get-in (refresh iwall) [:rezzed])))))

(deftest film-critic-fetal-ai
  "Film Critic - Fetal AI interaction"
  (do-game
    (new-game (default-corp [(qty "Fetal AI" 3)])
              (default-runner [(qty "Film Critic" 1) (qty "Sure Gamble" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Film Critic")
    (let [fc (first (get-in @state [:runner :rig :resource]))]
      (core/click-run state :runner {:server :hq})
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      ; should not have taken damage yet
      (is (= 3 (count (:hand (get-runner)))) "No damage dealt yet")
      (card-ability state :runner fc 0)
      (is (= 3 (count (:hand (get-runner)))) "No damage dealt")
      (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
      (card-ability state :runner fc 1)
      (is (= 1 (count (:scored (get-runner)))) "Agenda added to runner scored")
      (is (= 3 (count (:hand (get-runner)))) "No damage dealt"))))

(deftest gene-conditioning-shoppe
  "Gene Conditioning Shoppe - set :genetics-trigger-twice flag"
  (do-game
   (new-game (default-corp [(qty "Hedge Fund" 3)])
             (default-runner [(qty "Gene Conditioning Shoppe" 1) (qty "Adjusted Chronotype" 1)]))
   (take-credits state :corp)
   (play-from-hand state :runner "Adjusted Chronotype")
   (let [adjusted-chronotype (get-in @state [:runner :rig :resource 0])]
     (is (not (core/persistent-flag? state :runner adjusted-chronotype :triggers-twice)))
     (play-from-hand state :runner "Gene Conditioning Shoppe")
     (is (core/persistent-flag? state :runner adjusted-chronotype :triggers-twice))
     (core/trash state :runner (get-in @state [:runner :rig :resource 1]))
     (is (not (core/persistent-flag? state :runner adjusted-chronotype :triggers-twice))))))

(deftest gene-conditioning-shoppe-redundancy
  "Gene Conditioning Shoppe - set :genetics-trigger-twice flag - ensure redundant copies work"
  (do-game
   (new-game (default-corp [(qty "Hedge Fund" 3)])
             (default-runner [(qty "Gene Conditioning Shoppe" 2) (qty "Adjusted Chronotype" 1)]))
   (take-credits state :corp)
   (take-credits state :runner)
   (take-credits state :corp)
   (play-from-hand state :runner "Adjusted Chronotype")
   (let [adjusted-chronotype (get-in @state [:runner :rig :resource 0])]
     (is (not (core/persistent-flag? state :runner adjusted-chronotype :triggers-twice)))
     (play-from-hand state :runner "Gene Conditioning Shoppe")
     (play-from-hand state :runner "Gene Conditioning Shoppe")
     (let [gcs1 (get-in @state [:runner :rig :resource 1])
           gcs2 (get-in @state [:runner :rig :resource 2])]
       (is (core/persistent-flag? state :runner adjusted-chronotype :triggers-twice))
       (core/trash state :runner gcs1)
       (is (core/persistent-flag? state :runner adjusted-chronotype :triggers-twice))
       (core/trash state :runner gcs2)
       (is (not (core/persistent-flag? state :runner adjusted-chronotype :triggers-twice)))))))

(deftest ice-carver
  "Ice Carver - lower ice strength on encounter"
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 1)])
              (default-runner [(qty "Ice Carver" 1)]))
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp 2)
    (let [iwall (first (get-in @state [:corp :servers :archives :ices]))]
      (core/rez state :corp iwall)
      (play-from-hand state :runner "Ice Carver")
      (core/click-run state :runner {:server "Archives"})
      (is (= 0 (:current-strength (refresh iwall))) "Ice Wall strength at 0 for encounter")
      (core/jack-out state :runner nil)
      (is (= 1 (:current-strength (refresh iwall))) "Ice Wall strength at 1 after encounter"))))

(deftest john-masanori
  "John Masanori - Draw 1 card on first successful run, take 1 tag on first unsuccessful run"
  (do-game
    (new-game (default-corp) (default-runner [(qty "John Masanori" 3) (qty "Sure Gamble" 3) (qty "Fall Guy" 1)]))
    (take-credits state :corp)
    (core/gain state :runner :click 1)
    (play-from-hand state :runner "John Masanori")
    (is (= 4 (count (:hand (get-runner)))))
    (core/click-run state :runner {:server "Archives"})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    (is (= 5 (count (:hand (get-runner)))) "1 card drawn from first successful run")
    (core/click-run state :runner {:server "Archives"})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    (is (= 5 (count (:hand (get-runner)))) "No card drawn from second successful run")
    (core/click-run state :runner {:server "HQ"})
    (core/jack-out state :runner nil)
    (is (= 1 (:tag (get-runner))) "1 tag taken from first unsuccessful run")
    (core/click-run state :runner {:server "HQ"})
    (core/jack-out state :runner nil)
    (is (= 1 (:tag (get-runner))) "No tag taken from second unsuccessful run")))

(deftest joshua-b
  "Joshua B. - Take 1 tag at turn end if you choose to gain the extra click"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Joshua B." 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Joshua B.")
    (take-credits state :runner)
    (take-credits state :corp)
    (prompt-choice :runner "Yes") ; gain the extra click
    (is (= 5 (:click (get-runner))) "Gained extra click")
    (take-credits state :runner)
    (is (= 1 (:tag (get-runner))) "Took 1 tag")))

(deftest kati-jones
  "Kati Jones - Click to store and take"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Kati Jones" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Kati Jones")
    (is (= 3 (:credit (get-runner))))
    (let [kati (get-in @state [:runner :rig :resource 0])]
      (card-ability state :runner kati 0)
      (is (= 2 (:click (get-runner))))
      (is (= 3 (:counter (refresh kati))) "Store 3cr on Kati")
      (card-ability state :runner kati 0)
      (is (= 2 (:click (get-runner))) "Second use of Kati should not be allowed")
      (is (= 3 (:counter (refresh kati))) "Second use of Kati should not be allowed")
      (take-credits state :runner 2)
      (is (= 5 (:credit (get-runner))) "Pass turn, take 2cr")
      (take-credits state :corp)
      (card-ability state :runner kati 0)
      (is (= 6 (:counter (refresh kati))) "Store 3cr more on Kati")
      (take-credits state :runner 3)
      (is (= 8 (:credit (get-runner))) "Pass turn, take 3cr")
      (take-credits state :corp)
      (card-ability state :runner (refresh kati) 1)
      (is (= 14 (:credit (get-runner))) "Take 6cr from Kati")
      (is (zero? (:counter (refresh kati))) "No counters left on Kati"))))

(deftest new-angeles-city-hall
  "New Angeles City Hall - Avoid tags; trash when agenda is stolen"
  (do-game
    (new-game (default-corp [(qty "SEA Source" 1) (qty "Breaking News" 1)])
              (default-runner [(qty "New Angeles City Hall" 1)]))
    (play-from-hand state :corp "Breaking News" "New remote")
    (take-credits state :corp 2)
    (play-from-hand state :runner "New Angeles City Hall")
    (let [nach (get-in @state [:runner :rig :resource 0])]
      (core/click-run state :runner {:server "Archives"})
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (take-credits state :runner)
      (is (= 6 (:credit (get-runner))))
      (play-from-hand state :corp "SEA Source")
      (prompt-choice :corp 0) ; default trace
      (prompt-choice :runner 0) ; Runner won't match
      (card-ability state :runner nach 0)
      (prompt-choice :runner "Done")
      (is (= 0 (:tag (get-runner))) "Avoided SEA Source tag")
      (is (= 4 (:credit (get-runner))) "Paid 2 credits")
      (take-credits state :corp)
      (core/click-run state :runner {:server "Server 1"})
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (prompt-choice :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))))
      (is (empty? (get-in @state [:runner :rig :resource])) "NACH trashed by agenda steal"))))

(deftest professional-contacts
  "Professional Contacts - Click to gain 1 credit and draw 1 card"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Professional Contacts" 3) (qty "Sure Gamble" 2) (qty "Shiv" 2)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Professional Contacts")
    (let [proco (get-in @state [:runner :rig :resource 0])]
      (card-ability state :runner proco 0)
      (is (= 2 (:click (get-runner))) "Spent 1 click")
      (is (= 1 (:credit (get-runner))) "Gained 1 credit")
      (is (= 5 (count (:hand (get-runner)))) "Drew 1 card")
      (card-ability state :runner proco 0)
      (is (= 1 (:click (get-runner))) "Spent 1 click")
      (is (= 2 (:credit (get-runner))) "Gained 1 credit")
      (is (= 6 (count (:hand (get-runner)))) "Drew 1 card"))))

(deftest security-testing
  "Security Testing - Ability"
  (do-game
    (new-game (default-corp [(qty "Jackson Howard" 1)]) (default-runner [(qty "Security Testing" 1)]))
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (take-credits state :corp 2)
    (play-from-hand state :runner "Security Testing")
    (let [st (get-in @state [:runner :rig :resource 0])]
      (take-credits state :runner 3)
      (take-credits state :corp)
      (prompt-choice :runner "Server 1")
      (core/run state :runner :remote1)
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (is (= 10 (:credit (get-runner))) "Gained 2 credits from Security Testing")
      (core/run state :runner :remote1)
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (prompt-choice :runner "No")
      (is (= 10 (:credit (get-runner))) "Did not gain credits on second run")
      (take-credits state :runner 2)

      (take-credits state :corp)
      (prompt-choice :runner "Server 1")
      (core/run state :runner :archives)
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (is (= 12 (:credit (get-runner))) "Did not gain credits when running other server"))))

(deftest spoilers
  "Spoilers - Mill the Corp when it scores an agenda"
  (do-game
    (new-game (default-corp [(qty "Hostile Takeover" 1) (qty "Hedge Fund" 1)])
              (default-runner [(qty "Spoilers" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Spoilers")
    (take-credits state :runner)
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
    (is (= 1 (count (:deck (get-corp)))))
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (let [ht (get-in @state [:corp :servers :remote1 :content 0])]
      (score-agenda state :corp ht)
      (is (= 1 (count (:discard (get-corp)))))
      (is (= 0 (count (:deck (get-corp)))) "Last card from R&D milled"))))

(deftest street-peddler-ability
  "Street Peddler - Ability"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Street Peddler" 1) (qty "Gordian Blade" 1) (qty "Torch" 1)
                                              (qty "Sure Gamble" 2)]))
    (take-credits state :corp)
    ; move Gordian back to deck
    (core/move state :runner (find-card "Gordian Blade" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Torch" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Street Peddler")
    (let [sp (get-in @state [:runner :rig :resource 0])]
      (is (= 3 (count (:hosted sp))) "Street Peddler is hosting 3 cards")
      (card-ability state :runner sp 0)
      (prompt-card :runner (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
      (is (= "Gordian Blade" (:title (get-in @state [:runner :rig :program 0]))) "Gordian Blade was installed")
      (is (= 3 (:memory (get-runner))) "Gordian cost 1 mu"))))

(deftest street-peddler-cant-afford
  "Street Peddler - Can't afford install"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Street Peddler" 1) (qty "Gordian Blade" 3)]))
    (take-credits state :corp)
    ; move Gordian back to deck
    (core/move state :runner (find-card "Gordian Blade" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Gordian Blade" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Gordian Blade" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Street Peddler")
    (let [sp (get-in @state [:runner :rig :resource 0])]
      (card-ability state :runner sp 0)
      (core/lose state :runner :credit 3)
      (is (= 2 (count (:choices (first (:prompt (get-runner)))))) "1 card and 1 cancel option on Street Peddler")
      (prompt-card :runner (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
      (is (zero? (count (get-in @state [:runner :rig :program]))) "Gordian Blade was not installed")
      (is (and (:installed (refresh sp)) (= 3 (count (:hosted (refresh sp))))
               "Street Peddler still installed with 3 hosted cards")))))

(deftest street-peddler-kate-discount
  "Street Peddler - Interaction with Kate discount"
  (do-game
    (new-game (default-corp) (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker"
                                        [(qty "Street Peddler" 1) (qty "Gordian Blade" 1) (qty "Sure Gamble" 2)]))
    (take-credits state :corp)
    ; move Gordian back to deck
    (core/move state :runner (find-card "Gordian Blade" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Street Peddler")
    (let [sp (get-in @state [:runner :rig :resource 0])]
      (core/lose state :runner :credit 3) ; should still be able to afford Gordian w/ Kate discount
      (card-ability state :runner sp 0)
      (is (= 2 (count (:choices (first (:prompt (get-runner)))))) "Only 1 choice (plus Cancel) to install off Peddler")
      (prompt-card :runner (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
      (is (= "Gordian Blade" (:title (get-in @state [:runner :rig :program 0]))) "Gordian Blade was installed")
      (is (= 3 (:memory (get-runner))) "Gordian cost 1 mu"))))

(deftest street-peddler-memory-units
  "Street Peddler - Programs Should Cost Memory. Issue #708"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Street Peddler" 1) (qty "Corroder" 3)]))
    (take-credits state :corp)
    ; move Corroders back to deck
    (core/move state :runner (find-card "Corroder" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Corroder" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Corroder" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Street Peddler")
    (is (= 4 (:memory (get-runner))) "No memory cost for hosting on Street Peddler")
    (let [sp (get-in @state [:runner :rig :resource 0])]
      (is (= "Corroder" (:title (first (:hosted sp)))) "Street Peddler is hosting Corroder")
      (card-ability state :runner sp 0)
      (prompt-card :runner (first (:hosted sp))) ; choose to install Gordian
      (is (= "Corroder" (:title (get-in @state [:runner :rig :program 0]))) "Corroder was installed")
      (is (= 3 (:memory (get-runner))) "Corroder cost 1 mu"))))

(deftest street-peddler-parasite-1cr
  "Street Peddler - Installing Parasite with only 1cr. Issue #491."
  (do-game
    (new-game (default-corp [(qty "Pop-up Window" 3)]) (default-runner [(qty "Street Peddler" 1) (qty "Parasite" 3)]))
    (play-from-hand state :corp "Pop-up Window" "HQ")
    (take-credits state :corp 2)
    ; move Parasites back to deck
    (core/move state :runner (find-card "Parasite" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Parasite" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Parasite" (:hand (get-runner))) :deck)
    (core/lose state :runner :credit 4) ; go down to 1 credit
    (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
    (play-from-hand state :runner "Street Peddler")
    (let [sp (get-in @state [:runner :rig :resource 0])
          pu (get-in @state [:corp :servers :hq :ices 0])]
      (core/rez state :corp pu)
      (card-ability state :runner sp 0)
      (prompt-card :runner (first (:hosted sp))) ; choose to install Parasite
      (is (= "Parasite" (:title (:card (first (get-in @state [:runner :prompt]))))) "Parasite target prompt")
      (prompt-select :runner pu)
      (is (= 4 (count (:discard (get-runner)))) "3 Parasite, 1 Street Peddler in heap")
      (is (= 1 (count (:discard (get-corp)))) "Pop-up Window in archives"))))

(deftest symmetrical-visage
  "Symmetrical Visage - Gain 1 credit the first time you click to draw each turn"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Symmetrical Visage" 3) (qty "Sure Gamble" 3) (qty "Fall Guy" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Symmetrical Visage")
    (is (= 3 (:credit (get-runner))))
    (core/click-draw state :runner nil)
    (is (= 4 (:credit (get-runner))) "Gained 1 credit from first click spent to draw")
    (core/click-draw state :runner nil)
    (is (= 4 (:credit (get-runner))) "No credit gained from second click spent to draw")))

(deftest symmetrical-visage-gcs
  "Symmetrical Visage - Gain 1 credit the first and second time you click to draw each turn when GCS is installed"
  (do-game
   (new-game (default-corp) (default-runner [(qty "Symmetrical Visage" 3) (qty "Gene Conditioning Shoppe" 3) (qty "Fall Guy" 1)]))
   (take-credits state :corp)
   (core/gain state :runner :click 1)
   (play-from-hand state :runner "Symmetrical Visage")
   (is (= 3 (:credit (get-runner))))
   (play-from-hand state :runner "Gene Conditioning Shoppe")
   (is (= 1 (:credit (get-runner))))
   (core/click-draw state :runner nil)
   (is (= 2 (:credit (get-runner))) "Gained 1 credit from first click spent to draw")
   (core/click-draw state :runner nil)
   (is (= 3 (:credit (get-runner))) "Gained 1 credit from second click spent to draw with Gene Conditioning Shoppe")
   ; Move Fall Guy back to deck
   (core/move state :runner (find-card "Fall Guy" (:hand (get-runner))) :deck)
   (core/click-draw state :runner nil)
   (is (= 3 (:credit (get-runner))) "No credit gained from third click spent to draw with Gene Conditioning Shoppe")))

(deftest synthetic-blood
  "Synthetic Blood - The first time you take damage each turn, draw one card"
  (do-game
   (new-game (default-corp [(qty "Data Mine" 3) (qty "Hedge Fund" 3)])
             (default-runner [(qty "Synthetic Blood" 3) (qty "Sure Gamble" 3) (qty "Fall Guy" 1)]))
   (play-from-hand state :corp "Data Mine" "HQ")
   (play-from-hand state :corp "Data Mine" "HQ")
   (take-credits state :corp)
   (let [first-dm (get-in @state [:corp :servers :hq :ices 1])
         second-dm (get-in @state [:corp :servers :hq :ices 0])]
     (play-from-hand state :runner "Synthetic Blood")
     (core/click-run state :runner {:server "HQ"})
     (core/rez state :corp first-dm)
     (card-ability state :corp first-dm 0)
     (is (= 4 (count (:hand (get-runner)))) "1 card drawn when receiving damage (1st time)")
     (core/no-action state :corp nil)
     (core/continue state :runner nil)
     (core/rez state :corp second-dm)
     (card-ability state :corp second-dm 0)
     (is (= 3 (count (:hand (get-runner)))) "no card drawn when receiving damage (2nd time)"))))

(deftest synthetic-blood-gcs
  "Synthetic Blood - The first and second time you take damage each turn (with GCS installed), draw one card"
  (do-game
   (new-game (default-corp [(qty "Data Mine" 3) (qty "Hedge Fund" 3)])
             (default-runner [(qty "Synthetic Blood" 3) (qty "Sure Gamble" 1) (qty "Gene Conditioning Shoppe" 3)]))
   (play-from-hand state :corp "Data Mine" "HQ")
   (play-from-hand state :corp "Data Mine" "HQ")
   (take-credits state :corp)
   (let [first-dm (get-in @state [:corp :servers :hq :ices 1])
         second-dm (get-in @state [:corp :servers :hq :ices 0])]
     (play-from-hand state :runner "Synthetic Blood")
     (play-from-hand state :runner "Gene Conditioning Shoppe")
     (core/click-run state :runner {:server "HQ"})
     (core/rez state :corp first-dm)
     (card-ability state :corp first-dm 0)
     (is (= 3 (count (:hand (get-runner)))) "1 card drawn when receiving damage (1st time)")
     (core/no-action state :corp nil)
     (core/continue state :runner nil)
     (core/rez state :corp second-dm)
     (card-ability state :corp second-dm 0)
     (is (= 3 (count (:hand (get-runner)))) "1 card drawn when receiving damage (2nd time)"))))

(deftest the-supplier-ability
  "The Supplier - Ability"
  (do-game
    (new-game (default-corp) (default-runner [(qty "The Supplier" 1) (qty "Plascrete Carapace" 1) (qty "Utopia Shard" 1)
                                              (qty "Hedge Fund" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "The Supplier")
    (let [ts (get-in @state [:runner :rig :resource 0])]
      (card-ability state :runner ts 0)
      (prompt-select :runner (find-card "Plascrete Carapace" (:hand (get-runner))))
      (card-ability state :runner ts 0)
      (is (= 1 (count (get-in @state [:runner :prompt 0 :choices]))))
      (prompt-select :runner (find-card "Utopia Shard" (:hand (get-runner))))
      (is (= 2 (count (:hosted (refresh ts)))) "The Supplier is hosting 2 cards")
      (core/end-turn state :runner nil)
      (take-credits state :corp)
      ; Utopia Shard cannot be afforded and should not be in the prompt
      (is (= 2 (count (get-in @state [:runner :prompt 0 :choices]))) "1 card and 'No install' choices for The Supplier")
      (prompt-card :runner (find-card "Plascrete Carapace" (:hosted (refresh ts))))
      (is (= 1 (:credit (get-runner))) "Runner charged 1 credit to install Plascrete off The Supplier")
      (take-credits state :runner)
      (is (= 5 (:credit (get-runner))))
      (take-credits state :corp)
      (prompt-choice :runner "No install")
      (is (= 0 (count (get-in @state [:runner :prompt]))) "Resolved The Supplier prompt")
      (is (= 1 (count (:hosted (refresh ts)))) "One card still on The Supplier"))))

(deftest the-supplier-kate-discount
  "The Supplier - Interaction with Kate discount. Issue #578."
  (do-game
    (new-game (default-corp) (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker"
                                        [(qty "The Supplier" 1) (qty "Plascrete Carapace" 1) (qty "Kati Jones" 1)
                                         (qty "Hedge Fund" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "The Supplier")
    (let [ts (get-in @state [:runner :rig :resource 0])]
      (card-ability state :runner ts 0)
      (prompt-select :runner (find-card "Plascrete Carapace" (:hand (get-runner))))
      (core/lose state :runner :credit (:credit (get-runner)))
      (core/end-turn state :runner nil)
      (take-credits state :corp)
      (is (= 2 (count (get-in @state [:runner :prompt 0 :choices]))) "Able to install Plascrete from The Supplier")
      (prompt-card :runner (find-card "Plascrete Carapace" (:hosted (refresh ts))))
      (is (= 0 (:credit (get-runner))) "Kate discount applied")
      (is (= 1 (count (get-in @state [:runner :rig :resource]))) "Plascrete installed"))))

(deftest virus-breeding-ground-gain
  "Virus Breeding Ground - Gain counters"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Virus Breeding Ground" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Virus Breeding Ground")
    (let [vbg (get-in @state [:runner :rig :resource 0])]
      (is (zero? (get vbg :counter 0)) "Virus Breeding Ground starts with 0 counters")
      (take-credits state :runner 3)
      (take-credits state :corp)
      (is (= 1 (get (refresh vbg) :counter 0)) "Virus Breeding Ground gains 1 counter per turn")
      (take-credits state :runner 3)
      (take-credits state :corp)
      (is (= 2 (get (refresh vbg) :counter 0)) "Virus Breeding Ground gains 1 counter per turn"))))

(deftest virus-breeding-ground-gain
  "Virus Breeding Ground - Move counters"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Virus Breeding Ground" 1) (qty "Hivemind" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Virus Breeding Ground")
    (play-from-hand state :runner "Hivemind")
    (let [hive (get-in @state [:runner :rig :program 0])
          vbg (get-in @state [:runner :rig :resource 0])]
      (is (= 1 (get hive :counter 0)) "Hivemind starts with 1 counter")
      (is (zero? (get vbg :counter 0)) "Virus Breeding Ground starts with 0 counters")
      (take-credits state :runner 3)
      (take-credits state :corp)
      (is (= 1 (get (refresh vbg) :counter 0)) "Virus Breeding Ground gains 1 counter per turn")
      (card-ability state :runner vbg 0)
      (prompt-select :runner hive)
      (is (= 2 (get (refresh hive) :counter 0)) "Hivemind gained 1 counter")
      (is (= 0 (get (refresh vbg) :counter 0)) "Virus Breeding Ground lost 1 counter"))))

(deftest xanadu
  "Xanadu - Increase all ICE rez cost by 1 credit"
  (do-game
    (new-game (default-corp [(qty "Paper Wall" 2) (qty "Launch Campaign" 1)])
              (default-runner [(qty "Xanadu" 1)]))
    (play-from-hand state :corp "Paper Wall" "HQ")
    (play-from-hand state :corp "Paper Wall" "R&D")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Xanadu")
    (let [pw1 (get-in @state [:corp :servers :hq :ices 0])
          pw2 (get-in @state [:corp :servers :rd :ices 0])
          lc (get-in @state [:corp :servers :remote1 :content 0])]
      (core/rez state :corp pw1)
      (is (= 4 (:credit (get-corp))) "Paid 1 instead of 0 to rez Paper Wall")
      (core/rez state :corp pw2)
      (is (= 3 (:credit (get-corp))) "Paid 1 instead of 0 to rez Paper Wall")
      (core/rez state :corp lc)
      (is (= 2 (:credit (get-corp))) "Paid 1 to rez Launch Campaign; no effect on non-ICE"))))
