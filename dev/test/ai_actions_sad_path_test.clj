(ns ai-actions-sad-path-test
  "Sad path tests for AI actions - focusing on error handling

   Priority: HIGHEST - These tests ensure friendly error messages
   and catch bugs before production."
  (:require [clojure.test :refer :all]
            [test-helpers :refer :all]
            [ai-actions]
            [ai-websocket-client-v2 :as ws]))

;; ============================================================================
;; Card Operations - Not Found Errors
;; ============================================================================

(deftest test-play-card-not-in-hand
  (testing "Playing card not in hand shows helpful error"
    (with-mock-state
      (mock-client-state
        :hand [{:cid 1 :title "Diesel" :cost 0}])
      (assert-error-message
        #(ai-actions/play-card! "Sure Gamble")
        "not found"))))

(deftest test-play-card-empty-hand
  (testing "Playing card from empty hand shows error"
    (with-mock-state
      (mock-client-state :hand [])
      (assert-error-message
        #(ai-actions/play-card! 0)
        "hand is empty"))))

(deftest test-play-card-invalid-index
  (testing "Playing card by out-of-bounds index shows error"
    (with-mock-state
      (mock-client-state
        :hand [{:cid 1 :title "Diesel"}])
      (assert-error-message
        #(ai-actions/play-card! 999)
        "not found"))))

(deftest test-install-card-not-found
  (testing "Installing non-existent card shows error"
    (with-mock-state
      (mock-client-state
        :hand [{:cid 1 :title "Daily Casts"}])
      (assert-error-message
        #(ai-actions/install-card! "Corroder")
        "not found"))))

;; ============================================================================
;; Wrong Side / Invalid Actions
;; ============================================================================

(deftest test-runner-cannot-rez
  (testing "Runner trying to rez shows side error"
    (with-mock-state
      (mock-client-state :side "runner")
      ;; This test assumes rez-card! checks for Corp side
      ;; May need adjustment based on actual implementation
      (is (thrown? Exception
                   (ai-actions/rez-card! "ICE Wall"))
          "Expected exception when Runner tries to rez"))))

(deftest test-corp-cannot-run
  (testing "Corp trying to run shows side error"
    (with-mock-state
      (mock-client-state :side "corp")
      ;; This test assumes run! checks for Runner side
      (is (thrown? Exception
                   (ai-actions/run! "HQ"))
          "Expected exception when Corp tries to run"))))

;; ============================================================================
;; State Validation
;; ============================================================================

(deftest test-action-when-not-connected
  (testing "Actions when not connected show helpful error"
    (with-mock-state
      (assoc (mock-client-state) :connected false)
      (assert-error-message
        #(ai-actions/play-card! "Sure Gamble")
        "not connected"))))

(deftest test-action-without-game-state
  (testing "Actions without game state show error"
    (with-mock-state
      (assoc (mock-client-state) :game-state nil)
      (assert-error-message
        #(ai-actions/play-card! "Sure Gamble")
        "game state"))))

(deftest test-choose-without-prompt
  (testing "Choosing when no prompt shows helpful error"
    (with-mock-state
      (mock-client-state :prompt nil)
      (assert-error-message
        #(ai-actions/choose 0)
        "No active prompt"))))

(deftest test-choose-invalid-option
  (testing "Choosing invalid option index shows error"
    (with-mock-state
      (mock-client-state
        :prompt (make-prompt
                 :msg "Choose option"
                 :choices [{:value "Option 1" :idx 0}
                          {:value "Option 2" :idx 1}]))
      (assert-error-message
        #(ai-actions/choose 999)
        "invalid"))))

;; ============================================================================
;; Edge Cases - Nil and Empty Inputs
;; ============================================================================

(deftest test-play-card-nil-input
  (testing "Playing card with nil input shows error"
    (with-mock-state
      (mock-client-state
        :hand [{:cid 1 :title "Sure Gamble"}])
      (assert-error-message
        #(ai-actions/play-card! nil)
        "invalid"))))

(deftest test-run-empty-server
  (testing "Running empty server name shows error"
    (with-mock-state
      (mock-client-state :side "runner")
      (assert-error-message
        #(ai-actions/run! "")
        "server"))))

(deftest test-run-invalid-server
  (testing "Running bogus server name shows helpful error"
    (with-mock-state
      (mock-client-state :side "runner")
      (assert-error-message
        #(ai-actions/run! "bogus-server-123")
        "invalid server"))))

;; ============================================================================
;; Resource Validation
;; ============================================================================

(deftest test-insufficient-credits-message
  (testing "Insufficient credits shows clear message (if implemented)"
    (with-mock-state
      (mock-client-state
        :side "runner"
        :credits 0
        :hand [{:cid 1 :title "Sure Gamble" :cost 5}])
      ;; This test checks if the action prevents playing expensive cards
      ;; May need adjustment based on whether client-side validation exists
      ;; If server-side only, this test might not apply
      (is true "TODO: Test insufficient credits if client validates"))))

(deftest test-insufficient-clicks-message
  (testing "Insufficient clicks shows clear message (if implemented)"
    (with-mock-state
      (mock-client-state :side "runner" :clicks 0)
      ;; Similar to credits test - depends on client-side validation
      (is true "TODO: Test insufficient clicks if client validates"))))

;; ============================================================================
;; Test Suite Summary
;; ============================================================================

(defn -main
  "Run sad path tests and report results"
  []
  (let [results (run-tests 'ai-actions-sad-path-test)]
    (println "\n========================================")
    (println "Sad Path Test Summary")
    (println "========================================")
    (println "Tests run:" (:test results))
    (println "Assertions:" (:pass results))
    (println "Failures:" (:fail results))
    (println "Errors:" (:error results))
    (println "========================================\n")
    (when (or (pos? (:fail results)) (pos? (:error results)))
      (System/exit 1))))

(comment
  ;; Run all sad path tests
  (run-tests 'ai-actions-sad-path-test)

  ;; Run specific test
  (test-play-card-not-in-hand)

  ;; Run from main
  (-main)
  )
