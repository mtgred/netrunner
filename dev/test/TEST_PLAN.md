# AI Client Unit Test Plan

## Overview

This plan defines a comprehensive unit testing strategy for the AI client codebase (~6120 lines), focusing on **sad path testing** (error handling) as the highest priority.

## Current State

### Existing Tests
- ✅ `ai_websocket_diff_test.clj` - Tests diff application (solid foundation!)
- ✅ `test_harness.clj` - Integration test framework for multi-client scenarios

### Test Coverage Gaps
- ❌ `ai_actions.clj` (2081 lines) - **0% unit test coverage**
- ❌ `ai_websocket_client_v2.clj` - Minimal coverage (diff only)
- ❌ `send_command` bash script - No tests

## Test Structure

```
dev/test/
├── ai_actions_test.clj          ← NEW (core action tests)
├── ai_actions_sad_path_test.clj ← NEW (error cases - PRIORITY 1)
├── ai_websocket_client_test.clj ← NEW (WebSocket tests)
├── ai_websocket_diff_test.clj   ← EXISTS (expand this)
├── test_helpers.clj              ← NEW (mock utilities)
└── test_harness.clj             ← EXISTS (integration tests)
```

## Test Categories & Priority

### Priority 1: Sad Path Tests (Error Handling)
**Goal:** Catch bugs before production, ensure friendly error messages

#### 1.1 Card Operations - Not Found Errors
- ❌ Card not in hand: `(play-card! "Nonexistent Card")` → should return error, not crash
- ❌ Invalid index: `(play-card! 999)` → out of bounds
- ❌ Not installed: `(use-ability! "Not Installed" 0)` → card not found
- ❌ Wrong card name (typo): `(play-card! "Sur Gamble")` → helpful "did you mean" message

#### 1.2 Insufficient Resources
- ❌ Not enough credits: `(play-card! "Expensive Agenda")` with 0 credits
- ❌ Not enough clicks: `(take-credit!)` with 0 clicks
- ❌ Empty hand: `(play-card! 0)` when hand is empty

#### 1.3 Wrong Side / Invalid Actions
- ❌ Runner trying to rez: `(rez-card! "ICE")` as Runner → Corp-only action
- ❌ Corp trying Runner actions: `(run! "HQ")` as Corp → Runner-only
- ❌ Score non-agenda: `(score-agenda! "Asset")` → must be agenda

#### 1.4 State Validation
- ❌ No game state: Calling actions before game starts
- ❌ No WebSocket connection: Actions when disconnected
- ❌ No active prompt: `(choose 0)` when no prompt exists
- ❌ Invalid prompt choice: `(choose 999)` when only 2 choices
- ❌ Wrong prompt type: `(choose-card! 0)` on non-select prompt

#### 1.5 Lobby/Connection Errors
- ❌ Join non-existent game: `(connect-game! "fake-uuid")`
- ❌ Create lobby while already in game
- ❌ Start game with <2 players
- ❌ Malformed game IDs: `(connect-game! "not-a-uuid")`

#### 1.6 Edge Cases
- ❌ Server normalization: `(run! "bogus-server")` → helpful error
- ❌ Null/nil inputs: `(play-card! nil)`
- ❌ Wrong type: `(play-card! {:not "a string"})`

### Priority 2: Happy Path Tests (Core Functionality)
**Goal:** Ensure core features work as expected

#### 2.1 Card Operations
- ✅ Play card by name: `(play-card! "Sure Gamble")`
- ✅ Play card by index: `(play-card! 0)`
- ✅ Install card (Runner): `(install-card! "Daily Casts")`
- ✅ Install card (Corp): `(install-card! "Palisade" "HQ")`
- ✅ Use ability: `(use-ability! "Smartware Distributor" 0)`
- ✅ Rez card: `(rez-card! "Prisec")`

#### 2.2 State Queries
- ✅ Read hand: `(show-hand)`
- ✅ Show credits: `(show-credits)`
- ✅ Show clicks: `(show-clicks)`
- ✅ Show board: `(show-board)`
- ✅ Show prompt: `(show-prompt-detailed)`
- ✅ List playables: `(list-playables)`

#### 2.3 Basic Actions
- ✅ Take credit: `(take-credit!)`
- ✅ Draw card: `(draw-card!)`
- ✅ End turn: `(end-turn!)`
- ✅ Start turn: `(start-turn!)`

#### 2.4 Mulligan
- ✅ Keep hand: `(keep-hand)`
- ✅ Mulligan: `(mulligan)`

#### 2.5 Prompts
- ✅ Choose by index: `(choose-option! 0)`
- ✅ Choose by value: `(choose-by-value! "keep")`
- ✅ Choose card: `(choose-card! 0)`

#### 2.6 Wait Helpers
- ✅ Wait for diff: `(wait-for-diff 60)`
- ✅ Wait for log: `(wait-for-log-past "marker text")`
- ✅ Timeout handling

### Priority 3: WebSocket & State Management
**Goal:** Ensure network layer is robust

#### 3.1 Message Parsing
- ✅ Batch events: `[[[event1] [event2]]]`
- ✅ Single events: `[[:event-type data]]`
- ❌ Malformed messages: Invalid EDN
- ❌ Unexpected message types

#### 3.2 Diff Application (expand existing test)
- ✅ Simple map updates
- ✅ Nested updates
- ✅ Array ops (`:+` for append)
- ✅ Removals
- ❌ Complex state changes (multiple nested diffs)
- ❌ Edge cases (empty diffs, null values)

#### 3.3 Connection Lifecycle
- ✅ Connect: `(connect! url)`
- ✅ Disconnect
- ❌ Reconnect after failure
- ❌ Handle stale connections
- ❌ WebSocket ping/pong

## Test Infrastructure

### Mock Utilities (`test/test_helpers.clj`)

```clojure
(ns test-helpers
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [ai-websocket-client-v2 :as ws]
            [ai-actions]))

(defn mock-client-state
  "Create a fake client state for testing

   Usage:
     (mock-client-state)  ; defaults
     (mock-client-state :side \"corp\" :credits 10)
     (mock-client-state :hand [{:cid 1 :title \"Sure Gamble\"}])"
  [& {:keys [side credits clicks hand installed prompt]
      :or {side "runner" credits 5 clicks 4 hand [] installed {}}}]
  {:connected true
   :uid "test-user"
   :gameid "test-game-id"
   :side side
   :game-state {:runner {:credit (if (= side "runner") credits 5)
                        :click (if (= side "runner") clicks 4)
                        :hand (if (= side "runner") hand [])
                        :rig installed
                        :prompt-state prompt}
                :corp {:credit (if (= side "corp") credits 5)
                       :click (if (= side "corp") clicks 3)
                       :hand (if (= side "corp") hand [])
                       :servers {}
                       :prompt-state prompt}}})

(defmacro with-mock-state
  "Run test with mocked client state

   Usage:
     (with-mock-state (mock-client-state :credits 10)
       (is (= 10 (ai-actions/show-credits))))"
  [state-map & body]
  `(let [original-state# @ws/client-state]
     (try
       (reset! ws/client-state ~state-map)
       ~@body
       (finally
         (reset! ws/client-state original-state#)))))

(defn assert-error-message
  "Assert that calling f prints expected error message

   Usage:
     (assert-error-message
       #(ai-actions/play-card! \"Fake Card\")
       \"not found in hand\")"
  [f expected-substring]
  (let [result (with-out-str (f))]
    (is (str/includes? result expected-substring)
        (str "Expected error containing: " expected-substring
             "\n  Actual output: " result))))

(defn assert-success-message
  "Assert that calling f prints expected success message"
  [f expected-substring]
  (let [result (with-out-str (f))]
    (is (str/includes? result expected-substring)
        (str "Expected success containing: " expected-substring
             "\n  Actual output: " result))))

(defn mock-websocket-send!
  "Mock WebSocket send function to capture sent messages

   Usage:
     (let [sent (atom [])]
       (with-redefs [ws/send-message! (mock-websocket-send! sent)]
         (ai-actions/take-credit!)
         (is (= 1 (count @sent)))))"
  [sent-atom]
  (fn [event-type data]
    (swap! sent-atom conj {:type event-type :data data})
    nil))
```

### Test Example Structure

```clojure
(ns ai-actions-sad-path-test
  (:require [clojure.test :refer :all]
            [test-helpers :refer :all]
            [ai-actions]))

(deftest test-play-card-not-in-hand
  (testing "Playing card not in hand shows helpful error"
    (with-mock-state
      (mock-client-state
        :hand [{:cid 1 :title "Diesel" :cost 0}])
      (assert-error-message
        #(ai-actions/play-card! "Sure Gamble")
        "not found in hand"))))

(deftest test-play-card-invalid-index
  (testing "Playing card by out-of-bounds index shows error"
    (with-mock-state
      (mock-client-state
        :hand [{:cid 1 :title "Diesel"}])
      (assert-error-message
        #(ai-actions/play-card! 999)
        "not found"))))

(deftest test-rez-card-as-runner
  (testing "Runner trying to rez shows side error"
    (with-mock-state
      (mock-client-state :side "runner")
      (assert-error-message
        #(ai-actions/rez-card! "ICE Block")
        "Only Corp can rez"))))

(deftest test-choose-no-prompt
  (testing "Choosing when no prompt shows helpful error"
    (with-mock-state
      (mock-client-state :prompt nil)
      (assert-error-message
        #(ai-actions/choose 0)
        "No active prompt"))))
```

## Running Tests

### Command Line

```bash
# Run all tests
lein test

# Run specific namespace
lein kaocha --focus ai-actions-sad-path-test

# Run specific test
lein kaocha --focus ai-actions-sad-path-test/test-play-card-not-in-hand

# Watch mode (auto-run on save)
lein kaocha --watch

# Run with coverage report
lein cloverage
```

### In REPL

```clojure
(require 'kaocha.repl)

;; Run all tests
(kaocha.repl/run)

;; Run specific namespace
(kaocha.repl/run 'ai-actions-sad-path-test)

;; Run specific test
(kaocha.repl/run 'ai-actions-sad-path-test/test-play-card-not-in-hand)
```

## Success Metrics

### Minimum Coverage
- **70%** of `ai_actions.clj` functions covered
- **100%** of error messages tested (every error path validated)
- **90%** of happy paths covered

### Test Performance
- **Unit tests:** < 5 seconds total
- **Integration tests:** Separate, slower tests in `test_harness.clj`

### Test Quality
- Every error message tested for clarity and helpfulness
- Edge cases covered (nil, empty, out-of-bounds)
- No test flakiness (100% reproducible results)

## Implementation Strategy

### Phase 1: Infrastructure (Day 1)
1. Create `test_helpers.clj` with mock utilities
2. Verify mocking works with simple test
3. Set up test runner in REPL

### Phase 2: Sad Paths (Days 1-2)
1. Card operations errors (not found, wrong side)
2. Resource validation (credits, clicks)
3. State validation (no game, no prompt)
4. Edge cases (nil, empty, wrong types)

### Phase 3: Happy Paths (Days 2-3)
1. Core actions (take credit, draw, end turn)
2. Card operations (play, install, use ability)
3. Prompts (choose, mulligan)
4. State queries (hand, board, status)

### Phase 4: WebSocket (Day 3)
1. Expand diff tests
2. Message parsing tests
3. Connection lifecycle tests

## Notes

- **Network Independence:** All tests mock WebSocket/HTTP calls - no actual network traffic
- **Fast Feedback:** Unit tests run in memory with fake game states
- **Integration Tests:** Keep slow, multi-client tests in `test_harness.clj`
- **Sandbox Compatible:** No network restrictions affect unit tests

## References

- Existing test: `dev/test/ai_websocket_diff_test.clj`
- Integration framework: `dev/test/test_harness.clj`
- Main code under test: `dev/src/clj/ai_actions.clj` (2081 lines)
- WebSocket client: `dev/src/clj/ai_websocket_client_v2.clj`

---

**Last Updated:** 2025-11-11
**Status:** Planning Phase
**Next Step:** Create `test_helpers.clj` infrastructure
