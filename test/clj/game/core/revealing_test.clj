(ns game.core.revealing-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer [rezzed?]]
   [game.core.diffs :as diffs]
   [game.core.revealing :refer [set-last-played-or-rezzed]]
   [game.core.rezzing :as rezzing]
   [game.test-framework :refer :all]))

(deftest last-played-or-rezzed
  (testing "corp rez"
    (do-game
      (new-game {:corp {:hand ["Ice Wall"]}})
      (is (nil? (:last-played-or-rezzed @state)) "Nothing rezzed yet")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (is (nil? (:last-played-or-rezzed @state)) "Installing a face-down Corp card does not count")
      (rez state :corp (get-ice state :hq 0))
      (is (= "Ice Wall" (:title (:card (:last-played-or-rezzed @state)))) "Rezzed card is tracked")))

  (testing "operations and events"
    (do-game
      (new-game {:corp   {:hand ["Hedge Fund"] :credits 10}
                 :runner {:hand ["Sure Gamble"]}})
      (play-from-hand state :corp "Hedge Fund")
      (is (= "Hedge Fund" (:title (:card (:last-played-or-rezzed @state)))) "Played operation is tracked")
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (is (= "Sure Gamble" (:title (:card (:last-played-or-rezzed @state)))) "Played event is tracked")))

  (testing "runner install"
    (testing "faceup"
      (do-game
        (new-game {:runner {:hand ["Corroder" "Daily Casts" "Bookmark"] :credits 10}})
        (take-credits state :corp)
        (play-from-hand state :runner "Corroder")
        (is (= "Corroder" (:title (:card (:last-played-or-rezzed @state)))) "Installed program is tracked")
        (play-from-hand state :runner "Daily Casts")
        (is (= "Daily Casts" (:title (:card (:last-played-or-rezzed @state)))) "Installed resource is tracked")
        (play-from-hand state :runner "Bookmark")
        (is (= "Bookmark" (:title (:card (:last-played-or-rezzed @state)))) "Installed hardware is tracked")))

    (testing "facedown"
      ;; e.g. apex, harbinger, hunting grounds
      (do-game
        (new-game {:runner {:hand ["Corroder"] :credits 10}})
        (take-credits state :corp)
        (core/runner-install state :runner (core/make-eid state)
                             (find-card "Corroder" (:hand (get-runner)))
                             {:facedown true})
        (is (= "Corroder" (:title (get-runner-facedown state 0))) "Corroder is installed face-down")
        (is (nil? (:last-played-or-rezzed @state)) "Face-down install does not count"))))

  (testing "corp install"
    (testing "facedown"
      (do-game
        (new-game {:corp {:hand ["PAD Campaign" "Hostile Takeover"]}})
        (play-from-hand state :corp "PAD Campaign" "New remote")
        (is (nil? (:last-played-or-rezzed @state)) "Installing a face-down asset does not count")
        (play-from-hand state :corp "Hostile Takeover" "New remote")
        (is (nil? (:last-played-or-rezzed @state)) "Installing a face-down agenda does not count")))

    (testing "faceup agenda"
      (do-game
        (new-game {:corp {:hand ["Sacrifice Zone Expansion"]}})
        (play-from-hand state :corp "Sacrifice Zone Expansion" "New remote")
        (is (= "Sacrifice Zone Expansion" (:title (:card (:last-played-or-rezzed @state))))
            "Agenda installed face-up is tracked")))

    (testing "bangun faceup agenda"
      (do-game
        (new-game {:corp {:id "BANGUN: When Disaster Strikes" :hand ["Hostile Takeover"]}})
        (play-from-hand state :corp "Hostile Takeover" "New remote")
        (is (nil? (:last-played-or-rezzed @state)) "Face-down agenda install does not count")
        (click-prompt state :corp "Yes")
        (is (= "Hostile Takeover" (:title (:card (:last-played-or-rezzed @state))))
            "BANGUN turning the agenda faceup on install is tracked")))

    (testing "bangun manual faceup agenda"
      (do-game
        (new-game {:corp {:id "BANGUN: When Disaster Strikes" :hand ["Hostile Takeover"]}})
        (play-from-hand state :corp "Hostile Takeover" "New remote")
        (click-prompt state :corp "No")
        (is (nil? (:last-played-or-rezzed @state)) "Face-down agenda is not tracked")
        (card-ability state :corp (:identity (get-corp)) 0)
        (click-card state :corp (get-content state :remote1 0))
        (is (= "Hostile Takeover" (:title (:card (:last-played-or-rezzed @state))))
            "BANGUN manually turning the agenda faceup is tracked"))))

  (testing "only tracks printing info and an eid stamp"
    (do-game
      (new-game {:corp {:hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (let [tracked (:last-played-or-rezzed @state)]
        (is (= #{:card :eid} (set (keys tracked))))
        (is (= #{:title :printed-title :code :side} (set (keys (:card tracked))))
            "Only the printing info and side are tracked, no cid or other card state"))))

  (testing "playing the same card twice gives distinct values"
    (do-game
      (new-game {:corp {:hand [(qty "Hedge Fund" 2)]}})
      (play-from-hand state :corp "Hedge Fund")
      (let [first-play (:last-played-or-rezzed @state)]
        (play-from-hand state :corp "Hedge Fund")
        (let [second-play (:last-played-or-rezzed @state)]
          (is (= (:card first-play) (:card second-play)) "Same card both times")
          (is (not= (:eid first-play) (:eid second-play))
              "Different eid stamps, so clients see the second play as a change")))))

  (testing "respects no-msg"
    (do-game
      (new-game {:corp {:hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rezzing/rez state :corp (core/make-eid state) (get-ice state :hq 0) {:no-msg true})
      (is (rezzed? (get-ice state :hq 0)) "Ice Wall is rezzed")
      (is (nil? (:last-played-or-rezzed @state)) "A silent rez is not tracked")))

  (testing "ignores non-public"
    (do-game
      (new-game {:corp {:hand ["PAD Campaign"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (set-last-played-or-rezzed state (get-content state :remote1 0))
      (is (nil? (:last-played-or-rezzed @state))
          "The helper refuses to track a card that is not public")))

  (testing "syncs to all states"
    (do-game
      (new-game {:corp {:hand ["Ice Wall"]}})
      (let [{:keys [corp-state runner-state spect-state hist-state]} (diffs/public-states state)]
        (is (every? #(not (contains? % :last-played-or-rezzed))
                    [corp-state runner-state spect-state hist-state])
            "Absent from all views while nothing is tracked"))
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (let [{:keys [corp-state runner-state spect-state hist-state]} (diffs/public-states state)
            tracked                                                  (:last-played-or-rezzed @state)]
        (is (= "Ice Wall" (:title (:card tracked))))
        (is (= [tracked tracked tracked tracked]
               (map :last-played-or-rezzed [corp-state runner-state spect-state hist-state]))
            "Synced identically to every view")))))
