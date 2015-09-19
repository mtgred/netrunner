(in-ns 'test.core)

(deftest astrolabe-memory
  "Astrolabe - Gain 1 memory"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Astrolabe" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Astrolabe")
    (is (= 5 (:memory (get-runner))) "Gain 1 memory")))

(deftest astrolabe-draw
  "Astrolabe - Draw on new server install"
  (do-game
    (new-game (default-corp [(qty "Snare!" 3) (qty "Shock!" 3) (qty "Project Junebug" 3)])
              (default-runner [(qty "Astrolabe" 3) (qty "Sure Gamble" 3) (qty "Cloak" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Astrolabe")
    (take-credits state :runner 3)
    ; corp's turn. install something from HQ to trigger Astrolabe draw
    (core/play state :corp {:card (first (:hand (get-corp))) :server "New remote"})
    (is (= 5 (count (:hand (get-runner)))) "Drew 1 card from server install")
    ; install over the old server; make sure nothing is drawn
    (core/play state :corp {:card (first (:hand (get-corp))) :server "Server 0"})
    (is (= 5 (count (:hand (get-runner)))) "Did not draw")
    (is (= 1 (count (:deck (get-runner)))) "1 card left in deck")))