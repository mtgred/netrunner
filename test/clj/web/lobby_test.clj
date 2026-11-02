(ns web.lobby-test
  (:require
   [clojure.test :refer :all]
   [web.app-state :as app-state]
   [web.lobby :as lobby]))

(def auto-opts
  {:auto-select-default-deck-casual true
   :default-decks {:Corp {:standard "corp-standard"
                          :eternal "corp-eternal"}
                   :Runner {:standard "runner-standard"}}})

(deftest auto-select-deck-id-test
  (testing "returns the default deck id for the given side and format in casual"
    (is (= "corp-standard" (lobby/auto-select-deck-id auto-opts "casual" "standard" "Corp")))
    (is (= "corp-eternal" (lobby/auto-select-deck-id auto-opts "casual" "eternal" "Corp")))
    (is (= "runner-standard" (lobby/auto-select-deck-id auto-opts "casual" "standard" "Runner"))))

  (testing "nil when casual auto-select is disabled"
    (is (nil? (lobby/auto-select-deck-id (assoc auto-opts :auto-select-default-deck-casual false)
                                         "casual" "standard" "Corp"))))

  (testing "tournament uses its own setting, not the casual one"
    (is (nil? (lobby/auto-select-deck-id auto-opts "competitive" "standard" "Corp")))
    (is (= "corp-standard"
           (lobby/auto-select-deck-id (assoc auto-opts :auto-select-default-deck-tournament true)
                                      "competitive" "standard" "Corp"))))

  (testing "nil for Any Side"
    (is (nil? (lobby/auto-select-deck-id auto-opts "casual" "standard" "Any Side"))))

  (testing "nil when no default deck for that side+format"
    (is (nil? (lobby/auto-select-deck-id auto-opts "casual" "eternal" "Runner")))
    (is (nil? (lobby/auto-select-deck-id (assoc auto-opts :default-decks {})
                                         "casual" "standard" "Corp")))))

(defn- player
  ([side] (player side nil))
  ([side deck]
   (cond-> {:uid side :side side :user {:username side}}
     deck (assoc :deck deck))))

(defn- auto-select
  [lobby & {:keys [options deck-found?] :or {options auto-opts deck-found? true}}]
  (with-redefs [lobby/find-deck-for-user (fn [_db deck-id _user] (when deck-found? {:_id deck-id}))
                lobby/process-deck (fn [deck] (when deck (assoc deck :identity {:title "id"})))
                lobby/valid-deck-for-lobby? (fn [_lobby _deck] true)
                app-state/get-user (fn [_uid] {:options options})]
    (lobby/auto-select-decks nil lobby)))

(deftest auto-select-decks-test
  (testing "selects each deckless player's default deck for the lobby format"
    (let [result (auto-select {:room "casual" :format "standard"
                               :players [(player "Corp") (player "Runner")]})]
      (is (= "corp-standard" (get-in result [:players 0 :deck :_id])))
      (is (= "runner-standard" (get-in result [:players 1 :deck :_id])))))

  (testing "leaves a player that already chose a deck untouched"
    (let [chosen {:_id "manual"}
          result (auto-select {:room "casual" :format "standard"
                               :players [(player "Corp" chosen)]})]
      (is (= chosen (get-in result [:players 0 :deck])))))

  (testing "no-op once the game has started"
    (let [result (auto-select {:room "casual" :format "standard" :started true
                               :players [(player "Corp")]})]
      (is (nil? (get-in result [:players 0 :deck])))))

  (testing "no-op when the configured default deck no longer exists"
    (let [result (auto-select {:room "casual" :format "standard"
                               :players [(player "Corp")]}
                              :deck-found? false)]
      (is (nil? (get-in result [:players 0 :deck])))))

  (testing "no-op for non-constructed games"
    (let [opts {:auto-select-default-deck-casual true
                :default-decks {:Corp {:quick-draft "qd-deck"
                                       :chimera "ch-deck"}}}
          deck (fn [lobby] (get-in (auto-select (assoc lobby :players [(player "Corp")]) :options opts)
                                   [:players 0 :deck :_id]))]
      (is (nil? (deck {:room "casual" :format "quick-draft"})))
      (is (nil? (deck {:room "casual" :format "chimera"})))))

  (testing "no-op for precon gateway, but works for non-precon gateway"
    (let [opts {:auto-select-default-deck-casual true
                :default-decks {:Corp {:system-gateway "sg-deck"}}}
          deck (fn [lobby] (get-in (auto-select (assoc lobby :players [(player "Corp")]) :options opts)
                                   [:players 0 :deck :_id]))]
      (is (nil? (deck {:room "casual" :format "system-gateway" :precon :beginner})))
      (is (= "sg-deck" (deck {:room "casual" :format "system-gateway"}))))))

(deftest lobby-summary-deck-test
  (let [identity {:title "Az McCaffrey: Mechanical Prodigy" :faction "Criminal"}
        lobby {:gameid "g1"
               :room "casual"
               :format "standard"
               :title "game"
               :started true
               :allow-spectator true
               :players [(player "Corp" {:_id "deck-id" :name "My Deck" :identity identity})]}
        list-deck (fn [lobby] (get-in (lobby/lobby-summary lobby)
                                      [:players 0 :deck]))]
    (testing "public started games show decks in the lobby list"
      (let [deck (list-deck lobby)]
        (is (= "My Deck" (:name deck)))
        (is (= identity (:identity deck)))))
    (testing "password games hide decks from the lobby list"
      (is (nil? (list-deck (assoc lobby :password "password")))))
    (testing "games without spectators hide decks from the lobby list"
      (is (nil? (list-deck (assoc lobby :allow-spectator false)))))
    (testing "those in the game still see decks"
      (let [deck (get-in (lobby/lobby-summary (assoc lobby :password "password") true)
                         [:players 0 :deck])]
        (is (= "My Deck" (:name deck)))
        (is (= identity (:identity deck)))))))

(defn- rematch
  [old & {:keys [found-decks] :or {found-decks #{}}}]
  (with-redefs [lobby/find-deck-for-user (fn [_db deck-id _user]
                                           (when (found-decks deck-id) {:_id deck-id :name "fresh"}))
                lobby/process-deck (fn [deck] (assoc deck :identity {:title "id"}))
                lobby/valid-deck-for-lobby? (fn [_lobby deck] (not= "illegal" (:_id deck)))]
    (let [new-lobby (lobby/rematch-lobby nil old)]
      (lobby/leave-pool! (:pool new-lobby) (:gameid new-lobby))
      new-lobby)))

(def ^:private finished-game
  {:gameid "old" :started true :room "competitive" :format "standard" :title "t"
   :password "bcrypt-hash" :replay-id "r1" :replay-timestamp 3
   :time-extension 5 :excluded? true :rematch-requests #{"Corp" "Runner"}
   :spectators [{:uid "s" :user {:username "s"}}]
   :corp-spectators [{:uid "s" :user {:username "s"}}]
   :players [(player "Corp" {:_id "corp-deck" :name "stripped"})
             (player "Runner" {:_id "runner-deck" :name "stripped"})]})

(deftest rematch-lobby-test
  (testing "same players, sides, spectators and options in a fresh unstarted lobby"
    (let [r (rematch finished-game)]
      (is (not= "old" (:gameid r)))
      (is (not (:started r)))
      (is (= ["Corp" "Runner"] (mapv :side (:players r))))
      (is (= (:players r) (:original-players r)))
      (is (= (:spectators finished-game) (:spectators r)))
      (is (= (:corp-spectators finished-game) (:corp-spectators r)))
      (is (= ["competitive" "standard" "t" 5 true]
             ((juxt :room :format :title :time-extension :excluded?) r)))
      (is (nil? (:rematch-requests r)))))

  (testing "keeps the already hashed password instead of hashing it again"
    (is (= "bcrypt-hash" (:password (rematch finished-game)))))

  (testing "does not restore the old replay"
    (let [r (rematch finished-game)]
      (is (nil? (:replay-id r)))
      (is (nil? (:replay-timestamp r)))))

  (testing "reloads full decks; missing ones are left to pick again"
    (let [r (rematch finished-game :found-decks #{"corp-deck"})]
      (is (= "fresh" (get-in r [:players 0 :deck :name])))
      (is (nil? (get-in r [:players 1 :deck])))))

  (testing "decks that are no longer legal are left to pick again"
    (let [old (assoc-in finished-game [:players 0 :deck :_id] "illegal")
          r (rematch old :found-decks #{"illegal"})]
      (is (nil? (get-in r [:players 0 :deck])))))

  (testing "precon games keep their precon and skip the deck lookup"
    (let [r (rematch (assoc finished-game :format "system-gateway" :precon :beginner)
                     :found-decks #{"corp-deck" "runner-deck"})]
      (is (= :beginner (:precon r)))
      (is (every? (comp nil? :deck) (:players r))))))

(deftest leave-withdraws-rematch-request-test
  (let [lobbies {"g" {:gameid "g" :rematch-requests #{"Corp" "Runner"}
                      :players [(player "Corp") (player "Runner")]}}
        after (lobby/handle-leave-lobby lobbies "Runner" {:text "bye"})]
    (is (= #{"Corp"} (get-in after ["g" :rematch-requests])))))
