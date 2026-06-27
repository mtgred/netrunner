(ns web.lobby-test
  (:require
   [clojure.test :refer :all]
   [web.app-state :as app-state]
   [web.lobby :as lobby]))

(defn- player
  ([side] (player side nil))
  ([side deck]
   (cond-> {:uid side :side side :user {:username side}}
     deck (assoc :deck deck))))

(def auto-opts
  {:auto-select-default-deck true
   :default-decks {:Corp {:standard "corp-standard"
                          :eternal "corp-eternal"}
                   :Runner {:standard "runner-standard"}}})

(deftest auto-select-deck-id-test
  (testing "returns the default deck id for the given side and format"
    (is (= "corp-standard" (lobby/auto-select-deck-id auto-opts "standard" "Corp")))
    (is (= "corp-eternal" (lobby/auto-select-deck-id auto-opts "eternal" "Corp")))
    (is (= "runner-standard" (lobby/auto-select-deck-id auto-opts "standard" "Runner"))))

  (testing "nil when auto-select is disabled"
    (is (nil? (lobby/auto-select-deck-id (assoc auto-opts :auto-select-default-deck false)
                                         "standard" "Corp"))))

  (testing "nil for Any Side"
    (is (nil? (lobby/auto-select-deck-id auto-opts "standard" "Any Side"))))

  (testing "nil when no default deck for that side+format"
    (is (nil? (lobby/auto-select-deck-id auto-opts "eternal" "Runner")))
    (is (nil? (lobby/auto-select-deck-id (assoc auto-opts :default-decks {})
                                         "standard" "Corp")))))

(deftest auto-select-decks-test
  (with-redefs [lobby/find-deck-for-user (fn [_db deck-id _user] {:_id deck-id})
                lobby/process-deck (fn [raw] (assoc raw :identity {:title "id"}))
                lobby/valid-deck-for-lobby? (fn [_lobby _deck] true)
                app-state/get-user (fn [_uid] {:options auto-opts})]
    (testing "selects each player's default deck for the lobby format"
      (let [lobby {:room "casual" :format "standard"
                   :players [(player "Corp") (player "Runner")]}
            result (lobby/auto-select-decks nil lobby)]
        (is (= "corp-standard" (get-in result [:players 0 :deck :_id])))
        (is (= "runner-standard" (get-in result [:players 1 :deck :_id])))))

    (testing "uses the side+format default"
      (let [lobby {:room "casual" :format "eternal"
                   :players [(player "Corp")]}
            result (lobby/auto-select-decks nil lobby)]
        (is (= "corp-eternal" (get-in result [:players 0 :deck :_id])))))

    (testing "leaves a player that already chose a deck untouched"
      (let [chosen {:_id "manual"}
            lobby {:room "casual" :format "standard"
                   :players [(player "Corp" chosen)]}
            result (lobby/auto-select-decks nil lobby)]
        (is (= chosen (get-in result [:players 0 :deck])))))

    (testing "no-op once the game has started"
      (let [lobby {:room "casual" :format "standard" :started true
                   :players [(player "Corp")]}
            result (lobby/auto-select-decks nil lobby)]
        (is (nil? (get-in result [:players 0 :deck])))))

    (testing "no-op outside the casual room"
      (doseq [room ["competitive" "angel-arena"]]
        (let [lobby {:room room :format "standard"
                     :players [(player "Corp")]}
              result (lobby/auto-select-decks nil lobby)]
          (is (nil? (get-in result [:players 0 :deck]))
              (str "should not auto-select in " room " room")))))))

(deftest auto-select-decks-skips-missing-deck-test
  (with-redefs [lobby/find-deck-for-user (fn [_db _deck-id _user] nil)
                app-state/get-user (fn [_uid] {:options auto-opts})]
    (testing "deleted default deck leaves the player without a selection"
      (let [lobby {:room "casual" :format "standard"
                   :players [(player "Corp")]}
            result (lobby/auto-select-decks nil lobby)]
        (is (nil? (get-in result [:players 0 :deck])))))))
