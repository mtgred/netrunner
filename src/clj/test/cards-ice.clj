(in-ns 'test.core)

(deftest end-the-run
  "Since all ETR ice share a common ability, we only need one test"
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 3) (qty "Hedge Fund" 3) (qty "Restructure" 2)])
              (default-runner))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp 2)
    (core/click-run state :runner {:server "HQ"})
    (is (= [:hq] (get-in @state [:run :server])))
    (let [iwall (get-in @state [:corp :servers :hq :ices 0])]
      (core/rez state :corp iwall)
      (card-ability state :corp (core/get-card state iwall) 0)
      (is (not (:run @state)) "Run is ended")
      (is (get-in @state [:runner :register :unsuccessful-run]) "Run was unsuccessful"))))
