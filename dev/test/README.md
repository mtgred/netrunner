# AI Client Unit Tests

Comprehensive unit test suite for the Netrunner AI client, focusing on error handling (sad paths) and core functionality (happy paths).

## Test Structure

```
dev/test/
├── README.md                      ← You are here
├── TEST_PLAN.md                   ← Comprehensive testing strategy
├── test_helpers.clj               ← Mock utilities and helpers
├── ai_actions_test.clj            ← Happy path tests (core functionality)
├── ai_actions_sad_path_test.clj   ← Sad path tests (error handling)
├── ai_websocket_diff_test.clj     ← WebSocket diff tests (existing)
└── test_harness.clj               ← Integration tests (existing)
```

## Quick Start

### Running All Tests

```bash
# From project root
cd /home/user/netrunner
lein kaocha
```

### Running Specific Test Suites

```bash
# Run sad path tests (error handling)
lein kaocha --focus ai-actions-sad-path-test

# Run happy path tests (core functionality)
lein kaocha --focus ai-actions-test

# Run WebSocket diff tests
lein kaocha --focus ai-websocket-diff-test
```

### Running Individual Tests

```bash
# Run a specific test function
lein kaocha --focus ai-actions-sad-path-test/test-play-card-not-in-hand
```

### Watch Mode (Auto-run on Save)

```bash
# Automatically run tests when files change
lein kaocha --watch
```

## Test Categories

### 1. Sad Path Tests (`ai_actions_sad_path_test.clj`)

**Priority: HIGHEST** - These tests catch bugs before production.

Tests cover:
- ❌ Card not found in hand
- ❌ Invalid index (out of bounds)
- ❌ Wrong side (Runner trying Corp actions)
- ❌ No game state / not connected
- ❌ Invalid prompt choices
- ❌ Nil/empty inputs
- ❌ Invalid server names

Example:
```clojure
(deftest test-play-card-not-in-hand
  (testing "Playing card not in hand shows helpful error"
    (with-mock-state
      (mock-client-state
        :hand [{:cid 1 :title "Diesel" :cost 0}])
      (assert-error-message
        #(ai-actions/play-card! "Sure Gamble")
        "not found"))))
```

### 2. Happy Path Tests (`ai_actions_test.clj`)

Tests core functionality works correctly:
- ✅ Play card by name/index
- ✅ Install cards
- ✅ Run servers
- ✅ Take credit / draw card / end turn
- ✅ Choose prompt options
- ✅ Mulligan / keep hand
- ✅ Rez cards (Corp)
- ✅ Query game state

Example:
```clojure
(deftest test-show-hand
  (testing "show-hand returns current hand cards"
    (with-mock-state
      (mock-client-state
        :hand [{:cid 1 :title "Sure Gamble"}])
      (let [result (ai-actions/show-hand)]
        (is (= 1 (count result)))))))
```

### 3. WebSocket Tests (`ai_websocket_diff_test.clj`)

Tests diff application and state management (already exists).

### 4. Integration Tests (`test_harness.clj`)

Multi-client WebSocket tests for full game scenarios (already exists).

## Test Helpers API

### Mock State Creation

```clojure
(mock-client-state)  ; Default Runner with 5 credits, 4 clicks
(mock-client-state :side "corp" :credits 10)
(mock-client-state :hand [{:cid 1 :title "Sure Gamble"}])
```

### State Manipulation

```clojure
(with-mock-state (mock-client-state :credits 10)
  ;; Your test code here
  (is (= 10 (show-credits))))
```

### Assertions

```clojure
(assert-error-message
  #(play-card! "Fake Card")
  "not found in hand")

(assert-success-message
  #(play-card! "Sure Gamble")
  "played")

(assert-no-error
  #(show-hand))
```

### Card Creation

```clojure
(make-card :title "Sure Gamble" :cost 5 :type "Event")

(make-hand ["Sure Gamble" "Diesel" "Daily Casts"])

(make-prompt :msg "Choose" :choices ["Option 1" "Option 2"])
```

### WebSocket Mocking

```clojure
(let [sent (atom [])]
  (with-redefs [ws/send-message! (mock-websocket-send! sent)]
    (take-credit!)
    (is (= 1 (count @sent)))))
```

## REPL Development

```clojure
;; Load test namespace
(require '[ai-actions-sad-path-test :as sad])
(require '[ai-actions-test :as happy])
(require '[test-helpers :as h])

;; Run specific test
(sad/test-play-card-not-in-hand)

;; Run all tests in namespace
(clojure.test/run-tests 'ai-actions-sad-path-test)

;; Use Kaocha REPL
(require '[kaocha.repl :as k])
(k/run 'ai-actions-sad-path-test)
(k/run 'ai-actions-sad-path-test/test-play-card-not-in-hand)
```

## Test Coverage Goals

- **70%** of `ai_actions.clj` functions covered
- **100%** of error messages tested
- **90%** of happy paths covered

## Current Status

✅ Test infrastructure created
✅ Mock utilities implemented
✅ Sad path tests (initial set)
✅ Happy path tests (initial set)
🔄 Expanding coverage
⏳ Integration with CI/CD

## Adding New Tests

1. **Identify the function** to test in `ai_actions.clj`
2. **Decide test type**:
   - Sad path? → `ai_actions_sad_path_test.clj`
   - Happy path? → `ai_actions_test.clj`
3. **Use test helpers** from `test_helpers.clj`
4. **Write descriptive test names** and messages
5. **Run the test** to verify

Example template:
```clojure
(deftest test-new-function
  (testing "Description of what we're testing"
    (with-mock-state
      (mock-client-state :side "runner")
      (is (= expected-value (ai-actions/new-function))))))
```

## Troubleshooting

### Tests Won't Run

```bash
# Verify Kaocha is installed
lein deps

# Check tests.edn configuration
cat tests.edn
```

### Mock State Not Working

Ensure you're using `with-mock-state` and importing:
```clojure
(ns your-test
  (:require [test-helpers :refer :all]
            [ai-websocket-client-v2 :as ws]))
```

### WebSocket Send Not Mocked

Use `with-redefs` to mock WebSocket:
```clojure
(with-redefs [ws/send-message! (mock-websocket-send! sent-atom)]
  ;; Your test code
  )
```

## Performance

- **Unit tests** should complete in < 5 seconds
- **Integration tests** may take longer (separate suite)
- Use `--fail-fast` to stop on first failure

## Contributing

When adding tests:
1. Prioritize sad paths (error handling)
2. Write clear, descriptive test names
3. Include helpful assertion messages
4. Keep tests isolated (no shared state)
5. Mock external dependencies

## References

- [TEST_PLAN.md](./TEST_PLAN.md) - Comprehensive testing strategy
- [Kaocha Documentation](https://cljdoc.org/d/lambdaisland/kaocha)
- [ai_actions.clj](../src/clj/ai_actions.clj) - Code under test

---

**Last Updated:** 2025-11-11
**Status:** Initial implementation complete, expanding coverage
