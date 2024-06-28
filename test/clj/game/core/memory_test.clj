(ns game.core.memory-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.memory :as memory]
   [game.test-framework :refer :all]))

(deftest mu+
  (testing "1 arity"
    (let [req (constantly :true)
          value (rand-int 10)]
      (with-redefs [constantly (fn [& _] req)]
        (is (= {:type :available-mu
                :req req
                :value [:regular value]}
               (memory/mu+ value)))
        (is (= {:type :available-mu
                :req req
                :value [:regular value]}
               (memory/mu+ [:regular value]))))))
  (testing "2 arity"
    (let [req (constantly :true)
          value (rand-int 10)]
      (is (= {:type :available-mu
              :req req
              :value [:regular value]}
             (memory/mu+ req value)))
      (is (= {:type :available-mu
              :req req
              :value [:regular value]}
             (memory/mu+ req [:regular value]))))))

(deftest virus-mu+
  (testing "1 arity"
    (let [value (rand-int 10)]
      (is (= {:type :available-mu
              :value [:virus value]}
             (select-keys (memory/virus-mu+ value) [:type :value])))))
  (testing "2 arity"
    (let [req (constantly :true)
          value (rand-int 10)]
      (is (= {:type :available-mu
              :req req
              :value [:virus value]}
             (memory/virus-mu+ req value))))))

(deftest caissa-mu+
  (testing "1 arity"
    (let [value (rand-int 10)]
      (is (= {:type :available-mu
              :value [:caissa value]}
             (select-keys (memory/caissa-mu+ value) [:type :value])))))
  (testing "2 arity"
    (let [req (constantly :true)
          value (rand-int 10)]
      (is (= {:type :available-mu
              :req req
              :value [:caissa value]}
             (memory/caissa-mu+ req value))))))

(deftest available-mu
  (let [state (atom {:runner {:memory {:available 0}}})]
    (is (= 0 (memory/available-mu state)) "defaults :used to 0"))
  (let [state (atom {:runner {:memory {:used 0}}})]
    (is (= 0 (memory/available-mu state)) "defaults :available to 0"))
  (let [state (atom {:runner {:memory {:available nil
                                       :used nil}}})]
    (is (= 0 (memory/available-mu state)) "handles nils"))
  (let [state (atom {:runner {:memory {:available 1
                                       :used 0}}})]
    (is (= 1 (memory/available-mu state)) "base value is :available"))
  (let [state (atom {:runner {:memory {:available 0
                                       :used 1}}})]
    (is (= -1 (memory/available-mu state)) "subtracts :used from :available")))

(deftest build-new-mu-init-test
  (do-game
    (new-game {:runner {:hand ["Corroder" "Sure Gamble"]}})
    (is (= {:only-for {:caissa {:available 0
                                :used 0}
                       :virus {:available 0
                               :used 0}}
            :available 4
            :used 0}
           (memory/build-new-mu state))
        "starting values should be 4 available, 0 used")))

(deftest build-new-mu-using-mu-test
  (do-game
    (new-game {:runner {:hand ["Corroder" "Sure Gamble"]}})
    (core/register-lingering-effect
      state :runner (find-card "Corroder" (:hand (get-runner)))
      {:type :used-mu
       :value 1})
    (is (= {:only-for {:caissa {:available 0
                                :used 0}
                       :virus {:available 0
                               :used 0}}
            :available 4
            :used 1}
           (memory/build-new-mu state))
        "should increase :used")))

(deftest build-new-mu-greater-than-available-test
  (do-game
    (new-game {:runner {:hand ["Corroder" "Sure Gamble"]}})
    (core/register-lingering-effect
      state :runner (find-card "Corroder" (:hand (get-runner)))
      {:type :used-mu
       :value 5})
    (is (= {:only-for {:caissa {:available 0
                                :used 0}
                       :virus {:available 0
                               :used 0}}
            :available 4
            :used 5}
           (memory/build-new-mu state))
        "should increase :used")))

(deftest build-new-mu-increasing-available-mu-test
  (do-game
    (new-game {:runner {:hand ["Corroder" "Sure Gamble"]}})
    (core/register-lingering-effect
      state :runner (find-card "Sure Gamble" (:hand (get-runner)))
      (memory/mu+ 2))
    (is (= {:only-for {:caissa {:available 0
                                :used 0}
                       :virus {:available 0
                               :used 0}}
            :available 6
            :used 0}
           (memory/build-new-mu state))
        "should increase :available")))

(deftest build-new-mu-virus-increasing-available-virus-mu-test
  (do-game
    (new-game {:runner {:hand ["Cache" "Sure Gamble"]}})
    (core/register-lingering-effect
      state :runner (find-card "Sure Gamble" (:hand (get-runner)))
      (memory/virus-mu+ 2))
    (is (= {:only-for {:caissa {:available 0
                                :used 0}
                       :virus {:available 2
                               :used 0}}
            :available 4
            :used 0}
           (memory/build-new-mu state))
        "should increase :available-virus, not :available")))

(deftest build-new-mu-virus-increasing-available-non-virus-mu-test
  (do-game
    (new-game {:runner {:hand ["Cache" "Sure Gamble"]}})
    (core/register-lingering-effect
      state :runner (find-card "Sure Gamble" (:hand (get-runner)))
      (memory/mu+ 2))
    (core/register-lingering-effect
      state :runner (find-card "Sure Gamble" (:hand (get-runner)))
      (memory/virus-mu+ 2))
    (is (= {:only-for {:caissa {:available 0
                                :used 0}
                       :virus {:available 2
                               :used 0}}
            :available 6
            :used 0}
           (memory/build-new-mu state))
        "should increase :available-virus, not :available")))

(deftest build-new-mu-virus-no-available-virus-mu-test
  (do-game
    (new-game {:runner {:hand ["Cache" "Sure Gamble"]}})
    (core/register-lingering-effect
      state :runner (find-card "Cache" (:hand (get-runner)))
      {:type :used-mu
       :value 2})
    (is (= {:only-for {:caissa {:available 0
                                :used 0}
                       :virus {:available 0
                               :used 2}}
            :available 4
            :used 2}
           (memory/build-new-mu state))
        "should increase :used")))

(deftest build-new-mu-virus-using-virus-mu-test
  (do-game
    (new-game {:runner {:hand ["Cache" "Sure Gamble"]}})
    (core/register-lingering-effect
      state :runner (find-card "Sure Gamble" (:hand (get-runner)))
      (memory/virus-mu+ 2))
    (core/register-lingering-effect
      state :runner (find-card "Cache" (:hand (get-runner)))
      {:type :used-mu
       :value 2})
    (is (= {:only-for {:caissa {:available 0
                                :used 0}
                       :virus {:available 2
                               :used 2}}
            :available 4
            :used 0}
           (memory/build-new-mu state))
        "should increase :used-virus")))

(deftest build-new-mu-virus-using-more-than-available-test
  (do-game
    (new-game {:runner {:hand ["Cache" "Sure Gamble"]}})
    (core/register-lingering-effect
      state :runner (find-card "Sure Gamble" (:hand (get-runner)))
      (memory/virus-mu+ 2))
    (core/register-lingering-effect
      state :runner (find-card "Cache" (:hand (get-runner)))
      {:type :used-mu
       :value 3})
    (is (= {:only-for {:caissa {:available 0
                                :used 0}
                       :virus {:available 2
                               :used 3}}
            :available 4
            :used 1}
           (memory/build-new-mu state))
        "should increase :used-virus")))

(deftest update-mu
  (do-game
    (new-game)
    (is (false? (memory/update-mu state)) "Returns false when no change occurs")
    (core/register-lingering-effect
      state :runner nil
      {:type :used-mu
       :value 1})
    (is (true? (memory/update-mu state)) "Returns true when a change has occured"))
  (do-game
    (new-game)
    (core/register-lingering-effect
      state :runner (find-card "Sure Gamble" (:hand (get-runner)))
      (memory/virus-mu+ 2))
    (is (true? (memory/update-mu state)) "Returns true when a change has occured")))

(deftest sufficient-mu?-test
  (do-game
    (new-game {:corp {:hand ["Hedge Fund"]
                      :deck [(qty "Hedge Fund" 100)]}
               :runner {:hand ["Endless Hunger" "Corroder" "Akamatsu Mem Chip"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Endless Hunger")
    ;; Results are calculated like this to avoid failure filling the screen with state
    (let [result (memory/sufficient-mu? state (find-card "Corroder" (:hand (get-runner))))]
      (is (not result) "Insufficient MU to install"))
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (let [result (memory/sufficient-mu? state (find-card "Corroder" (:hand (get-runner))))]
      (is result))))
