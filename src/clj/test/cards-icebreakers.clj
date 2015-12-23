(in-ns 'test.core)

(deftest atman-install-2
  "Atman - Installing with 2 power counters"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Atman" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Atman")
    (prompt-choice :runner 2)
    (is (= 3 (:memory (get-runner))))
    (let [atman (get-in @state [:runner :rig :program 0])]
      (is (= 2 (:counter atman)) "2 power counters")
      (is (= 2 (:current-strength atman)) "2 current strength"))))

(deftest atman-install-0
  "Atman - Installing with 0 power counters"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Atman" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Atman")
    (prompt-choice :runner 0)
    (is (= 3 (:memory (get-runner))))
    (let [atman (get-in @state [:runner :rig :program 0])]
      (is (= 0 (:counter atman)) "0 power counters")
      (is (= 0 (:current-strength atman)) "0 current strength"))))

(deftest faust-pump
  "Faust - Pump by discarding"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Faust" 1) (qty "Sure Gamble" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Faust")
    (let [faust (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner faust 1)
      (prompt-card :runner (first (:hand (get-runner))))
      (is (= 4 (:current-strength (refresh faust))) "4 current strength")
      (is (= 1 (count (:discard (get-runner)))) "1 card trashed"))))

(deftest faust-pump
  "Faust - Pump does not trigger trash prevention. #760"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Faust" 1) (qty "Sacrificial Construct" 1) (qty "Fall Guy" 1)
                                              (qty "Astrolabe" 1) (qty "Gordian Blade" 1 )
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

(deftest overmind-counters
  "Overmind - Start with counters equal to unused MU"
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
      (is (= 5 (:counter (refresh ov))) "Overmind has 5 counters"))))
