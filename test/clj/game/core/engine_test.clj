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
        (is (= '([:test-event-2 [{:a 4, :b 5, :c 6}]]
                 [:test-event-2 [{:a x, :b y, :c z}]]
                 [:test-event [{:a a, :b b, :c c} {:a 1, :b 2, :c 3}]]
                 [:test-event-2 [{:a x, :b y, :c z} {:a 4, :b 5, :c 6}]]
                 [:test-event [{:a 1, :b 2, :c 3}]]
                 [:test-event [{:a a, :b b, :c c}]])
               (:order @state)))))))
