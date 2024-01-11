(ns game.core.engine-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.engine :as e]
   [game.test-framework :refer :all]))

(deftest first-trash
  (doseq [first-trash [:runner :corp]
          second-trash [:runner :corp]]
    (testing (str "Base test for " (name first-trash) " followed by " (name second-trash))
      (do-game
        (new-game {:corp {:hand (qty "Hedge Fund" 5)}})
        (is (not (core/first-trash? state)))
        (trash state first-trash (find-card "Hedge Fund" (:hand (get-corp))))
        (is (core/first-trash? state))
        (trash state second-trash (find-card "Hedge Fund" (:hand (get-corp))))
        (is (not (core/first-trash? state)))))))

(deftest queued-events
  (let [start (new-game)
        state (atom @start)
        event-1 {:event :test-event
                 :condition :test-condition
                 :effect (fn [state _ _ _ targets]
                           (swap! state update :order conj [:test-event targets]))}
        event-2 {:event :test-event
                 :condition :test-condition
                 :once-per-instance true
                 :effect (fn [state _ _ _ targets]
                           (swap! state update :order conj [:test-event targets]))}

        event-3 {:event :test-event-2
                 :condition :test-condition
                 :once-per-instance true
                 :effect (fn [state _ _ _ targets]
                           (swap! state update :order conj [:test-event-2 targets]))}
        event-4 {:event :test-event-2
                 :condition :test-condition
                 :effect (fn [state _ _ _ targets]
                           (swap! state update :order conj [:test-event-2 targets]))}]
    (testing "it works"
      (reset! state @start)
      (do-game
        state
        (e/register-pending-event state :test-event (first (:hand (get-corp))) event-1)
        (e/register-pending-event state :test-event-2 (first (:hand (get-corp))) event-3)
        (e/register-pending-event state :test-event (first (:hand (get-runner))) event-2)
        (e/register-pending-event state :test-event-2 (first (:hand (get-runner))) event-4)
        (e/queue-event state :test-event {:a 1 :b 2 :c 3})
        (e/queue-event state :test-event-2 {:a 4 :b 5 :c 6})
        (e/queue-event state :test-event {:a 'a :b 'b :c 'c})
        (e/queue-event state :test-event-2 {:a 'x :b 'y :c 'z})
        (e/checkpoint state nil (core/make-eid state) nil)
        (is (= '([:test-event-2 [{:a 4, :b 5, :c 6 :event :test-event-2}]]
                 [:test-event-2 [{:a x, :b y, :c z :event :test-event-2}]]
                 [:test-event [{:a a, :b b, :c c :event :test-event}
                               {:a 1, :b 2, :c 3 :event :test-event}]]
                 [:test-event-2 [{:a x, :b y, :c z :event :test-event-2}
                                 {:a 4, :b 5, :c 6 :event :test-event-2}]]
                 [:test-event [{:a 1, :b 2, :c 3 :event :test-event}]]
                 [:test-event [{:a a, :b b, :c c :event :test-event}]])
               (:order @state)))))))

(deftest merge-costs-paid
  (let [eid1 {:cost-paid {:click {:type :click
                                  :value 3}
                          :credit {:type :credit
                                   :value 1}
                          :forfeit {:type :forfeit
                                    :targets [{:title "NAPD Contract"}]
                                    :value 1}}}
        eid2 {:cost-paid {:click {:type :click
                                  :value 1}}}
        eid3 {:cost-paid {:trash {:type :trash
                                  :value 1
                                  :targets [{:title "C.I. Fund"}]}}}]
    (is (= {:click {:type :click
                    :targets nil
                    :value 3}
            :credit {:type :credit
                     :targets nil
                     :value 1}
            :forfeit {:type :forfeit
                      :targets [{:title "NAPD Contract"}]
                      :value 1}}
           (e/merge-costs-paid (:cost-paid eid1))))
    (is (= {:click {:type :click
                    :targets nil
                    :value 4}
            :credit {:type :credit
                     :targets nil
                     :value 1}
            :forfeit {:type :forfeit
                      :targets [{:title "NAPD Contract"}]
                      :value 1}}
           (e/merge-costs-paid (:cost-paid eid1) (:cost-paid eid2))))
    (is (= {:click {:type :click
                    :targets nil
                    :value 4}
            :credit {:type :credit
                     :targets nil
                     :value 1}
            :forfeit {:type :forfeit
                      :targets [{:title "NAPD Contract"}]
                      :value 1}
            :trash {:type :trash
                    :targets [{:title "C.I. Fund"}]
                    :value 1}}
           (e/merge-costs-paid (:cost-paid eid1) (:cost-paid eid2) (:cost-paid eid3))))))

(deftest trash-currents-test
  (before-each [state (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                                        :hand ["Hostile Takeover" "Cerebral Static" "Lag Time"]}
                                 :runner {:hand ["Scrubbed" "Interdiction"]}})
                _ (play-from-hand state :corp "Hostile Takeover" "New remote")]
    (testing "Corp current stays when agenda scored"
      (do-game state
        (play-from-hand state :corp "Cerebral Static")
        (score-agenda state :corp (get-content state :remote1 0))
        (is (find-card "Cerebral Static" (:current (get-corp))))))
    (testing "Corp current goes away when agenda stolen"
      (do-game state
        (play-from-hand state :corp "Cerebral Static")
        (take-credits state :corp)
        (run-empty-server state :remote1)
        (click-prompt state :runner "Steal")
        (is (not (find-card "Cerebral Static" (:current (get-corp)))))))
    (testing "Corp current goes away when corp plays another current"
      (do-game state
        (play-from-hand state :corp "Cerebral Static")
        (play-from-hand state :corp "Lag Time")
        (is (find-card "Lag Time" (:current (get-corp))))
        (is (not (find-card "Cerebral Static" (:current (get-corp)))))))
    (testing "Corp current goes away when runner plays a current"
      (do-game state
        (play-from-hand state :corp "Cerebral Static")
        (take-credits state :corp)
        (play-from-hand state :runner "Scrubbed")
        (is (not (find-card "Cerebral Static" (:current (get-corp)))))))
    (testing "Runner current stays when agenda stolen"
      (do-game state
        (take-credits state :corp)
        (play-from-hand state :runner "Scrubbed")
        (run-empty-server state :remote1)
        (click-prompt state :runner "Steal")
        (is (find-card "Scrubbed" (:current (get-runner))))))
    (testing "Runner current goes away when agenda scored"
      (do-game state
        (take-credits state :corp)
        (play-from-hand state :runner "Scrubbed")
        (take-credits state :runner)
        (score-agenda state :corp (get-content state :remote1 0))
        (is (not (find-card "Scrubbed" (:current (get-runner)))))))
    (testing "Runner current goes away when runner plays another current"
      (do-game state
        (take-credits state :corp)
        (play-from-hand state :runner "Scrubbed")
        (play-from-hand state :runner "Interdiction")
        (is (find-card "Interdiction" (:current (get-runner))))
        (is (not (find-card "Scrubbed" (:current (get-runner)))))))
    (testing "Runner current goes away when corp plays a current"
      (do-game state
        (take-credits state :corp)
        (play-from-hand state :runner "Scrubbed")
        (take-credits state :runner)
        (play-from-hand state :corp "Cerebral Static")
        (is (not (find-card "Scrubbed" (:current (get-runner)))))))))

(deftest trash-existing-programs-test
  (do-game
    (new-game {:corp {:hand ["Hedge Fund"]
                      :deck [(qty "Hedge Fund" 100)]}
               :runner {:hand ["Endless Hunger" "Corroder" "Akamatsu Mem Chip"]
                        :credits 100}})
    (take-credits state :corp)
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (play-from-hand state :runner "Endless Hunger")
    (play-from-hand state :runner "Corroder")
    (trash state :runner (get-hardware state 0))
    (click-card state :runner (get-program state 1))
    (click-prompt state :runner "Done")
    (is (= "Endless Hunger" (:title (get-program state 0))))
    (is (find-card "Corroder" (:discard (get-runner))))))

(deftest trash-existing-programs-not-enough-test
  (do-game
    (new-game {:corp {:hand ["Hedge Fund"]
                      :deck [(qty "Hedge Fund" 100)]}
               :runner {:hand ["Endless Hunger" "Corroder" "Dagger" "Box-E"]
                        :credits 100}})
    (take-credits state :corp)
    (core/gain state :runner :click 100)
    (play-from-hand state :runner "Box-E")
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Dagger")
    (play-from-hand state :runner "Endless Hunger")
    (trash state :runner (get-hardware state 0))
    (is (= "Insufficient MU. Trash 2 MU of installed programs."
           (:msg (prompt-map :runner))))
    (click-card state :runner (get-program state 0))
    (click-prompt state :runner "Done")
    (is (= "Insufficient MU. Trash 1 MU of installed programs."
           (:msg (prompt-map :runner))))
    (click-card state :runner (get-program state 0))
    (click-prompt state :runner "Done")
    (is (= "Endless Hunger" (:title (get-program state 0))))
    (is (find-card "Corroder" (:discard (get-runner))))
    (is (find-card "Dagger" (:discard (get-runner))))))

(deftest trash-previous-consoles-test
  (do-game
    (new-game {:corp {:hand ["Hedge Fund"]
                      :deck [(qty "Hedge Fund" 100)]}
               :runner {:hand ["Mirror" "Assimilator" "Hunting Grounds"]
                        :deck [(qty "Box-E" 5)]
                        :credits 100}})
    (take-credits state :corp)
    (core/gain state :runner :click 100)
    (play-from-hand state :runner "Mirror")
    (play-from-hand state :runner "Assimilator")
    (play-from-hand state :runner "Hunting Grounds")
    (card-ability state :runner (get-resource state 1) 1)
    (is (= 3 (count (get-runner-facedown state))))
    (card-ability state :runner (get-resource state 0) 0)
    (is (= "Choose a facedown installed card" (:msg (prompt-map :runner))))
    (click-card state :runner (get-runner-facedown state 0))
    (is (last-log-contains? state "Mirror is trashed."))
    (is (find-card "Mirror" (:discard (get-runner))))))

(deftest install-second-console-trashes-first
  (do-game
    (new-game {:corp {:hand ["Hedge Fund"]
                      :deck [(qty "Hedge Fund" 100)]}
               :runner {:hand ["Mirror" "Box-E"]
                        :credits 100}})
    (take-credits state :corp)
    (play-from-hand state :runner "Mirror")
    (play-from-hand state :runner "Box-E")
    (is (= "Box-E" (:title (get-hardware state 0))))
    (is (last-log-contains? state "Mirror is trashed."))))
