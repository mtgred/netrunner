(ns test-helpers
  "Mock utilities and test helpers for AI client unit tests"
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

;; ============================================================================
;; Mock Client State Creation
;; ============================================================================

(defn mock-client-state
  "Create a fake client state for testing

   Usage:
     (mock-client-state)  ; defaults
     (mock-client-state :side \"corp\" :credits 10)
     (mock-client-state :hand [{:cid 1 :title \"Sure Gamble\"}])"
  [& {:keys [side credits clicks hand installed prompt servers active-player]
      :or {side "runner" credits 5 clicks 4 hand [] installed {} servers {} active-player "runner"}}]
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
                       :servers servers
                       :prompt-state prompt}
                :active-player active-player}})

;; ============================================================================
;; State Manipulation Macros
;; ============================================================================

(defmacro with-mock-state
  "Run test with mocked client state

   Usage:
     (with-mock-state (mock-client-state :credits 10)
       (is (= 10 (show-credits))))"
  [state-map & body]
  `(let [original-state# @ai-websocket-client-v2/client-state]
     (try
       (reset! ai-websocket-client-v2/client-state ~state-map)
       ~@body
       (finally
         (reset! ai-websocket-client-v2/client-state original-state#)))))

;; ============================================================================
;; Assertion Helpers
;; ============================================================================

(defn assert-error-message
  "Assert that calling f prints expected error message

   Usage:
     (assert-error-message
       #(play-card! \"Fake Card\")
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

(defn assert-no-error
  "Assert that calling f does not print error message"
  [f]
  (let [result (with-out-str (f))]
    (is (not (or (str/includes? result "ERROR")
                 (str/includes? result "Error")
                 (str/includes? result "❌")))
        (str "Expected no error but got: " result))))

;; ============================================================================
;; WebSocket Mocking
;; ============================================================================

(defn mock-websocket-send!
  "Mock WebSocket send function to capture sent messages

   Usage:
     (let [sent (atom [])]
       (with-redefs [ws/send-message! (mock-websocket-send! sent)]
         (take-credit!)
         (is (= 1 (count @sent)))))"
  [sent-atom]
  (fn [event-type data]
    (swap! sent-atom conj {:type event-type :data data})
    nil))

(defn mock-websocket-receive!
  "Simulate receiving a WebSocket message

   Usage:
     (mock-websocket-receive! [:game/diff [{:runner {:credit 6}} {}]])"
  [message]
  ;; This would simulate the message handler being called
  ;; Implementation depends on how the WebSocket client processes messages
  (println "Mock receive:" message))

;; ============================================================================
;; Card and Game State Builders
;; ============================================================================

(defn make-card
  "Create a card map for testing

   Usage:
     (make-card :cid 1 :title \"Sure Gamble\" :cost 5 :type \"Event\")"
  [& {:keys [cid title cost type subtype side faction]
      :or {cid 1 title "Test Card" cost 0 type "Event" subtype nil side "Runner" faction "Neutral"}}]
  (cond-> {:cid cid
           :title title
           :cost cost
           :type type
           :side side
           :faction faction}
    subtype (assoc :subtype subtype)))

(defn make-hand
  "Create a hand of cards for testing

   Usage:
     (make-hand [\"Sure Gamble\" \"Diesel\" \"Daily Casts\"])
     (make-hand [{:title \"Sure Gamble\" :cost 5} {:title \"Diesel\" :cost 0}])"
  [cards]
  (map-indexed
   (fn [idx card]
     (if (string? card)
       (make-card :cid (inc idx) :title card)
       (merge (make-card :cid (inc idx)) card)))
   cards))

(defn make-prompt
  "Create a prompt state for testing

   Usage:
     (make-prompt :msg \"Choose a card\" :choices [\"Option 1\" \"Option 2\"])
     (make-prompt :prompt-type \"select\" :choices [{:cid 1} {:cid 2}])"
  [& {:keys [msg choices prompt-type card]
      :or {msg "Choose" choices [] prompt-type "select"}}]
  {:msg msg
   :choices choices
   :prompt-type prompt-type
   :card card})

;; ============================================================================
;; Test Data Fixtures
;; ============================================================================

(def sample-runner-cards
  "Common runner cards for testing"
  [(make-card :title "Sure Gamble" :cost 5 :type "Event")
   (make-card :title "Diesel" :cost 0 :type "Event")
   (make-card :title "Daily Casts" :cost 3 :type "Resource")
   (make-card :title "Corroder" :cost 2 :type "Program" :subtype "Icebreaker")
   (make-card :title "Dirty Laundry" :cost 2 :type "Event")])

(def sample-corp-cards
  "Common corp cards for testing"
  [(make-card :title "Hedge Fund" :cost 5 :type "Operation" :side "Corp")
   (make-card :title "Ice Wall" :cost 1 :type "ICE" :subtype "Barrier" :side "Corp")
   (make-card :title "PAD Campaign" :cost 2 :type "Asset" :side "Corp")
   (make-card :title "Hostile Takeover" :cost 0 :type "Agenda" :side "Corp")
   (make-card :title "Enigma" :cost 3 :type "ICE" :subtype "Code Gate" :side "Corp")])

;; ============================================================================
;; Utility Functions
;; ============================================================================

(defn with-timeout
  "Run function with timeout, return ::timeout on timeout

   Usage:
     (with-timeout 1000 (long-running-fn))"
  [timeout-ms f]
  (let [fut (future (f))
        result (deref fut timeout-ms ::timeout)]
    (when (= result ::timeout)
      (future-cancel fut))
    result))

(defn capture-output
  "Capture stdout from running function

   Usage:
     (let [output (capture-output #(println \"Hello\"))]
       (is (= \"Hello\\n\" output)))"
  [f]
  (with-out-str (f)))

(defn simulate-game-state
  "Create a complete game state for integration testing

   Usage:
     (simulate-game-state
       :runner {:hand [{:title \"Sure Gamble\"}] :credit 5}
       :corp {:hand [{:title \"Hedge Fund\"}] :credit 5})"
  [& {:keys [runner corp active-player turn]
      :or {active-player "runner" turn 1}}]
  {:runner (merge {:credit 5 :click 4 :hand [] :rig {}} runner)
   :corp (merge {:credit 5 :click 3 :hand [] :servers {}} corp)
   :active-player active-player
   :turn turn
   :gameid "test-game-123"})

;; ============================================================================
;; Debugging Helpers
;; ============================================================================

(defn print-state
  "Pretty print current client state (useful for debugging tests)"
  [state]
  (clojure.pprint/pprint state))

(defn diff-states
  "Show differences between two states (useful for debugging)"
  [state1 state2]
  (let [keys1 (set (keys state1))
        keys2 (set (keys state2))
        common-keys (clojure.set/intersection keys1 keys2)
        only1 (clojure.set/difference keys1 keys2)
        only2 (clojure.set/difference keys2 keys1)]
    {:only-in-state1 only1
     :only-in-state2 only2
     :different (filter #(not= (get state1 %) (get state2 %)) common-keys)}))
