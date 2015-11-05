(in-ns 'test.core)

(deftest hedge-fund
  (do-game
    (new-game (default-corp) (default-runner))
    (is (= 5 (:credit (get-corp))))
    (core/play state :corp {:card (first (:hand (get-corp)))})
    (is (= 9 (:credit (get-corp))))))

(deftest diversified-portfolio
  (do-game
    (new-game (default-corp [(qty "Diversified Portfolio" 1) (qty "Paper Wall" 1)
                             (qty "PAD Campaign" 3)])
              (default-runner))
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "Paper Wall" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Diversified Portfolio")
    (is (= 7 (:credit (get-corp))) "Ignored remote with ICE but no server contents")))
  
(deftest news-cycle
  (do-game
    (new-game (default-corp [(qty "Breaking News" 2) (qty "24/7 News Cycle" 3)])
              (default-runner))
    (play-from-hand state :corp "Breaking News" "New remote")
    (play-from-hand state :corp "Breaking News" "New remote")
    (let [ag1 (get-in @state [:corp :servers :remote1 :content 0])
          ag2 (get-in @state [:corp :servers :remote2 :content 0])]
      (core/gain state :corp :click 3)
      (core/advance state :corp {:card (refresh ag1)})
      (core/advance state :corp {:card (refresh ag1)})
      (core/score state :corp {:card (refresh ag1)})
      (core/advance state :corp {:card (refresh ag2)})
      (core/advance state :corp {:card (refresh ag2)})
      (core/score state :corp {:card (refresh ag2)})
      (take-credits state :corp)
      (is (= 0 (:tag (get-runner)))) ; tags cleared
      (take-credits state :runner)
      (play-from-hand state :corp "24/7 News Cycle")
      (prompt-card :corp (find-card "Breaking News" (:scored (get-corp))))
      (is (= 1 (:agenda-point (get-corp))) "Forfeited Breaking News")
      (prompt-card :corp (find-card "Breaking News" (:scored (get-corp))))
      (is (= 2 (:tag (get-runner))) "Runner given 2 tags"))))
