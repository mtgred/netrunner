(ns game.core.engine-test
  (:require [game.core :as core]
            [game.core.engine :as e]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

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
        side :corp
        corp-card {:cid 1
                   :side "Corp"
                   :type "Asset"
                   :title "Test Card 1"}
        runner-card {:cid 2
                     :side "Runner"
                     :type "Program"
                     :title "Test Card 2"}
        event-1 {:event :test-event
                 :effect (fn [state side eid card targets]
                           (swap! state update :order conj [:test-event targets]))}
        event-2 {:event :test-event
                 :once-per-instance true
                 :effect (fn [state side eid card targets]
                           (swap! state update :order conj [:test-event targets]))}

        event-3 {:event :test-event-2
                 :once-per-instance true
                 :effect (fn [state side eid card targets]
                           (swap! state update :order conj [:test-event-2 targets]))}
        event-4 {:event :test-event-2
                 :effect (fn [state side eid card targets]
                           (swap! state update :order conj [:test-event-2 targets]))}
        ]
    (testing "it works"
      (reset! state @start)
      (do-game
        state
        (e/make-pending-event state :test-event (first (:hand (get-corp))) event-1)
        (e/make-pending-event state :test-event-2 (first (:hand (get-corp))) event-3)
        (e/make-pending-event state :test-event (first (:hand (get-runner))) event-2)
        (e/make-pending-event state :test-event-2 (first (:hand (get-runner))) event-4)
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
                _ (do (play-from-hand state :corp "Hostile Takeover" "New remote"))]
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
        (is (not (find-card "Scrubbed" (:current (get-runner)))))))
    ))
