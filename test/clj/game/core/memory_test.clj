(ns game.core.memory-test
  (:require [game.core :as core]
            [game.core.memory :as memory]
            [game.core-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest mu+
  (testing "1 arity"
    (let [req (constantly :true)
          value (rand-int 10)]
      (with-redefs [constantly (fn [& _] req)]
        (is (= {:type :available-mu
                :req req
                :value [:regular value]}
               (core/mu+ value)))
        (is (= {:type :available-mu
                :req req
                :value [:regular value]}
               (core/mu+ [:regular value]))))))
  (testing "2 arity"
    (let [req (constantly :true)
          value (rand-int 10)]
      (is (= {:type :available-mu
              :req req
              :value [:regular value]}
             (core/mu+ req value)))
      (is (= {:type :available-mu
              :req req
              :value [:regular value]}
             (core/mu+ req [:regular value]))))))

(deftest virus-mu+
  (testing "1 arity"
    (let [value (rand-int 10)]
      (is (= {:type :available-mu
              :value [:virus value]}
             (select-keys (core/virus-mu+ value) [:type :value])))))
  (testing "2 arity"
    (let [req (constantly :true)
          value (rand-int 10)]
      (is (= {:type :available-mu
              :req req
              :value [:virus value]}
             (core/virus-mu+ req value))))))

(deftest caissa-mu+
  (testing "1 arity"
    (let [value (rand-int 10)]
      (is (= {:type :available-mu
              :value [:caissa value]}
             (select-keys (core/caissa-mu+ value) [:type :value])))))
  (testing "2 arity"
    (let [req (constantly :true)
          value (rand-int 10)]
      (is (= {:type :available-mu
              :req req
              :value [:caissa value]}
             (core/caissa-mu+ req value))))))

(deftest available-mu
  (let [state (atom {:runner {:memory {:available 0}}})]
    (is (= 0 (core/available-mu state)) "defaults :used to 0"))
  (let [state (atom {:runner {:memory {:used 0}}})]
    (is (= 0 (core/available-mu state)) "defaults :available to 0"))
  (let [state (atom {:runner {:memory {:available nil
                                       :used nil}}})]
    (is (= 0 (core/available-mu state)) "handles nils"))
  (let [state (atom {:runner {:memory {:available 1
                                       :used 0}}})]
    (is (= 1 (core/available-mu state)) "base value is :available"))
  (let [state (atom {:runner {:memory {:available 0
                                       :used 1}}})]
    (is (= -1 (core/available-mu state)) "subtracts :used from :available")))

(deftest build-new-mu
  (testing "non-virus card mu:"
    (do-game
      (new-game {:runner {:hand ["Corroder" "Sure Gamble"]}})
      (is (= {:only-for {:caissa {:available 0
                                  :used 0}
                         :virus {:available 0
                                 :used 0}}
              :available 4
              :used 0}
             (memory/build-new-mu state))
          "starting values should be 4 available, 0 used"))
    (testing "using mu"
      (do-game
        (new-game {:runner {:hand ["Corroder" "Sure Gamble"]}})
        (core/register-floating-effect
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
            "should increase :used"))
      (testing "when greater than :available"
        (do-game
          (new-game {:runner {:hand ["Corroder" "Sure Gamble"]}})
          (core/register-floating-effect
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
      (testing "increasing available mu"
        (do-game
          (new-game {:runner {:hand ["Corroder" "Sure Gamble"]}})
          (core/register-floating-effect
            state :runner (find-card "Sure Gamble" (:hand (get-runner)))
            (core/mu+ 2))
          (is (= {:only-for {:caissa {:available 0
                                      :used 0}
                             :virus {:available 0
                                     :used 0}}
                  :available 6
                  :used 0}
                 (memory/build-new-mu state))
              "should increase :available")))))
  (testing "virus mu:"
    (testing "available virus mu:"
      (testing "increasing available virus mu"
        (do-game
          (new-game {:runner {:hand ["Cache" "Sure Gamble"]}})
          (core/register-floating-effect
            state :runner (find-card "Sure Gamble" (:hand (get-runner)))
            (core/virus-mu+ 2))
          (is (= {:only-for {:caissa {:available 0
                                      :used 0}
                             :virus {:available 2
                                     :used 0}}
                  :available 4
                  :used 0}
                 (memory/build-new-mu state))
              "should increase :available-virus, not :available")))
      (testing "increasing available virus mu with available non-virus mu"
        (do-game
          (new-game {:runner {:hand ["Cache" "Sure Gamble"]}})
          (core/register-floating-effect
            state :runner (find-card "Sure Gamble" (:hand (get-runner)))
            (core/mu+ 2))
          (core/register-floating-effect
            state :runner (find-card "Sure Gamble" (:hand (get-runner)))
            (core/virus-mu+ 2))
          (is (= {:only-for {:caissa {:available 0
                                      :used 0}
                             :virus {:available 2
                                     :used 0}}
                  :available 6
                  :used 0}
                 (memory/build-new-mu state))
              "should increase :available-virus, not :available"))))
    (testing "virus card mu:"
      (testing "virus card mu:"
        (testing "with no available virus mu:"
          (testing "using virus card mu"
            (do-game
              (new-game {:runner {:hand ["Cache" "Sure Gamble"]}})
              (core/register-floating-effect
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
                  "should increase :used"))))
        (testing "with available virus mu:"
          (testing "using virus card mu"
            (do-game
              (new-game {:runner {:hand ["Cache" "Sure Gamble"]}})
              (core/register-floating-effect
                state :runner (find-card "Sure Gamble" (:hand (get-runner)))
                (core/virus-mu+ 2))
              (core/register-floating-effect
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
          (testing "using more virus card mu than available virus mu"
            (do-game
              (new-game {:runner {:hand ["Cache" "Sure Gamble"]}})
              (core/register-floating-effect
                state :runner (find-card "Sure Gamble" (:hand (get-runner)))
                (core/virus-mu+ 2))
              (core/register-floating-effect
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
                  "should increase :used-virus"))))))))

(deftest update-mu
  (do-game
    (new-game)
    (is (false? (core/update-mu state)) "Returns false when no change occurs")
    (core/register-floating-effect
      state :runner nil
      {:type :used-mu
       :value 1})
    (is (true? (core/update-mu state)) "Returns true when a change has occured"))
  (do-game
    (new-game)
    (core/register-floating-effect
      state :runner (find-card "Sure Gamble" (:hand (get-runner)))
      (core/virus-mu+ 2))
    (is (true? (core/update-mu state)) "Returns true when a change has occured")))
