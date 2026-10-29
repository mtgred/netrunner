(ns game.core.prompts-test
  (:require
   [cheshire.core :as json]
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer [in-hand?]]
   [game.core.diffs :as diffs]
   [game.core.initializing :refer [make-card]]
   [game.macros :refer [effect]]
   [game.test-framework :refer :all]
   [jinteki.i18n :refer [->msg-map build-msg msg-type?]]))

(defn- resolve-test-ability
  [state ability]
  (core/resolve-ability state :corp ability (make-card {:title "test"}) nil))

(deftest msg-map-prompt-and-choices
  (let [spy (atom [])]
    (do-game (new-game)
      (resolve-test-ability state {:prompt (->msg-map :prompt_choose-one)
                                   :choices [(->msg-map :choice_yes) (->msg-map :choice_no)]
                                   :effect (effect (swap! spy conj target))})
      (is (= "Choose one" (:msg (prompt-map :corp))) "Prompt text renders")
      (is (= ["Yes" "No"] (prompt-buttons :corp)) "Buttons render")
      (click-prompt state :corp "No")
      (is (msg-type? (first @spy) :choice_no) "Clicking by text gives the msg map")
      (resolve-test-ability state {:prompt "Plain text"
                                   :choices [(->msg-map :choice_yes) "Maybe"]
                                   :effect (effect (swap! spy conj target))})
      (is (= "Plain text" (:msg (prompt-map :corp))) "Strings still work")
      (click-prompt state :corp (->msg-map :choice_yes))
      (is (msg-type? (second @spy) :choice_yes) "Clicking by msg map works"))))

(deftest msg-map-with-params
  (let [spy (atom [])
        draw-2 {:msg/type :choice_draw-cards
                :msg/count 2}]
    (do-game (new-game)
      (resolve-test-ability state {:prompt (effect {:msg/type :prompt_choose-one-choice-of
                                                    :msg/current 1
                                                    :msg/total 2})
                                   :choices [{:msg/type :choice_gain-credits
                                              :msg/credits 2}
                                             draw-2]
                                   :effect (effect (swap! spy conj target))})
      (is (= "Choose one. Choice 1 of 2" (:msg (prompt-map :corp))) "Prompt fn can return a msg map")
      (is (= ["Gain 2 [Credits]" "Draw 2 cards"] (prompt-buttons :corp)))
      (click-prompt state :corp "Draw 2 cards")
      (is (= [draw-2] @spy)))))

(deftest bare-keywords-throw
  (do-game (new-game)
    (is (thrown? Exception (resolve-test-ability state {:prompt :prompt_choose-one
                                                        :choices [(->msg-map :choice_done)]
                                                        :effect (fn [& _])})))
    (is (thrown? Exception (resolve-test-ability state {:prompt "Text"
                                                        :choices [:choice_done]
                                                        :effect (fn [& _])})))))

(deftest unregistered-ids-throw
  (is (thrown? Exception (->msg-map :not-a-registered-msg)))
  (do-game (new-game)
    (is (thrown? Exception (resolve-test-ability state {:prompt {:msg/type :not-a-registered-prompt}
                                                        :choices [(->msg-map :choice_done)]
                                                        :effect (fn [& _])})))
    (is (thrown? Exception (resolve-test-ability state {:prompt "Text"
                                                        :choices [{:msg/type :not-a-registered-choice}]
                                                        :effect (fn [& _])})))
    (is (thrown? Exception (resolve-test-ability state {:prompt "Text"
                                                        :choices [{:msg/type :choice_take-tags
                                                                   :msg/count "1"}]
                                                        :effect (fn [& _])}))
        "Params are checked against the registered schema")))

(deftest msg-maps-on-the-wire
  (do-game (new-game)
    (resolve-test-ability state {:prompt (->msg-map :prompt_choose-one)
                                 :choices [(->msg-map :choice_done) "Text"]
                                 :effect (fn [& _])})
    (let [corp-state (:corp-state (diffs/public-states state))
          parsed (json/parse-string (json/generate-string corp-state) true)]
      (is (= [{:msg/type "choice_done"} "Text"]
             (mapv :value (get-in parsed [:corp :prompt-state :choices])))
          "Msg maps survive json")
      (is (= {:msg/type "prompt_choose-one"} (get-in parsed [:corp :prompt-state :msg])))
      (is (= ["Done" "Choose one"]
             [(build-msg (get-in parsed [:corp :prompt-state :choices 0 :value]))
              (build-msg (get-in parsed [:corp :prompt-state :msg]))])
          "Msg types that arrive as strings still render"))))

(deftest default-select-prompt-text
  (doseq [[choices text] [[{} "Choose a target for test"]
                          [{:min 2} "Choose at least 2 targets for test"]
                          [{:max 1} "Choose up to 1 target for test"]
                          [{:max 2 :all true} "Choose 2 targets for test"]
                          [{:min 1 :max 2} "Choose at least 1 and up to 2 targets for test"]]]
    (do-game (new-game {:corp {:hand ["Hedge Fund" "Ice Wall"]}})
      (resolve-test-ability state {:choices (assoc choices :card in-hand?)
                                   :effect (fn [& _])})
      (is (= text (:msg (prompt-map :corp)))))))
