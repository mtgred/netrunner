(ns game-test.cards.resources.dj-fenris
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(let [;; Start id for dj-fenris
      sunny "Sunny Lebeau: Security Specialist"
      ;; Several G-mod identities
      geist "Armand \"Geist\" Walker: Tech Lord"
      hayley "Hayley Kaplan: Universal Scholar"
      kate "Kate \"Mac\" McCaffrey: Digital Tinker"
      kit "Rielle \"Kit\" Peddler: Transhuman"
      professor "The Professor: Keeper of Knowledge"
      jamie "Jamie \"Bzzz\" Micken: Techno Savant"
      chaos "Chaos Theory: Wünderkind"
      whizzard "Whizzard: Master Gamer"
      reina "Reina Roja: Freedom Fighter"
      maxx "MaxX: Maximum Punk Rock"]
  (deftest dj-fenris
    ;; DJ Fenris - host 1 g-mod id not in faction on DJ Fenris
    (testing "Hosting Chaos Theory"
      ;; Ensure +1 MU is handled correctly
      (do-game
        (new-game {:runner {:id sunny
                            :deck ["DJ Fenris"]}
                   :options {:start-as :runner}})
        (play-from-hand state :runner "DJ Fenris")
        (is (= (first (prompt-titles :runner)) geist) "List is sorted")
        (is (every? #(some #{%} (prompt-titles :runner))
                    [geist chaos reina maxx]))
        (is (not-any? #(some #{%} (prompt-titles :runner))
                      [professor whizzard jamie kate kit]))
        (click-prompt state :runner chaos)
        (is (= chaos (get-in (get-resource state 0) [:hosted 0 :title])) "Chaos Theory hosted on DJ Fenris")
        (is (= sunny (:title (:identity (get-runner)))) "Still Sunny, id not changed")
        (is (= 2 (:link (get-runner))) "2 link from Sunny")
        (is (= 5 (core/available-mu state)) "+1 MU from Chaos Theory")
        ;; Trash DJ Fenris
        (trash-resource state "DJ Fenris")
        (is (not= chaos (-> (get-runner) :rfg last :title)) "Chaos Theory not moved to rfg")
        (is (not= chaos (-> (get-runner) :discard last :title)) "Chaos Theory not moved to discard")
        (is (not= chaos (-> (get-runner) :hand last :title)) "Chaos Theory not moved to hand")
        (is (not= chaos (-> (get-runner) :identity :title)) "Chaos Theory not moved to identity")
        (is (= 1 (count (:discard (get-runner)))) "1 card in heap: DJ Fenris")
        (is (= 4 (core/available-mu state)) "+1 MU from Chaos Theory removed")
        ;; Recover DJ Fenris
        (core/move state :runner (get-in (get-runner) [:discard 0]) :hand)
        (core/gain state :runner :credit 3)
        ;; Re-play DJ Fenris
        (play-from-hand state :runner "DJ Fenris")
        (is (some #(= chaos (:title %)) (:choices (prompt-map :runner))) "Chaos Theory still available")
        ;; Try moving CT to hand
        (game.core/move state :runner (-> (get-resource state 0) :hosted first) :hand)
        (is (not= chaos (-> (get-runner) :rfg last :title)) "Chaos Theory not moved to rfg")
        (is (not= chaos (-> (get-runner) :discard last :title)) "Chaos Theory not moved to discard")
        (is (not= chaos (-> (get-runner) :hand last :title)) "Chaos Theory not moved to hand")
        (is (not= chaos (-> (get-runner) :identity :title)) "Chaos Theory not moved to identity")
        (is (= 4 (core/available-mu state)) "+1 MU from Chaos Theory removed")))
    (testing "Hosting Geist"
      ;; Ensure Geist effect triggers
      (do-game
        (new-game {:runner {:id sunny
                            :deck ["DJ Fenris" (qty "All-nighter" 3) (qty "Sure Gamble" 3)]}
                   :options {:start-as :runner}})
        (starting-hand state :runner ["DJ Fenris" "All-nighter" "All-nighter"])
        (play-from-hand state :runner "All-nighter")
        (play-from-hand state :runner "All-nighter")
        (play-from-hand state :runner "DJ Fenris")
        (is (= (first (prompt-titles :runner)) geist) "List is sorted")
        (is (every? #(some #{%} (prompt-titles :runner))
                    [geist chaos reina maxx]))
        (is (not-any? #(some #{%} (prompt-titles :runner))
                      [professor whizzard jamie kate kit]))
        (click-prompt state :runner geist)
        (is (= geist (get-in (get-resource state 2) [:hosted 0 :title])) "Geist hosted on DJ Fenris")
        (is (= sunny (:title (:identity (get-runner)))) "Still Sunny, id not changed")
        (is (= 2 (:link (get-runner))) "2 link from Sunny, no extra link from Geist")
        (let [hand-count (count (:hand (get-runner)))]
          (card-ability state :runner (get-resource state 0) 0) ; Use All-nighter
          (is (= (+ 1 hand-count) (count (:hand (get-runner))))
              "Drew one card with Geist when using All-nighter trash ability")
          (trash-resource state "DJ Fenris")
          (is (not= geist (-> (get-runner) :rfg last :title)) "Geist not moved to rfg")
          (is (not= geist (-> (get-runner) :discard last :title)) "Geist not moved to discard")
          (is (not= geist (-> (get-runner) :hand last :title)) "Geist not moved to hand")
          (is (not= geist (-> (get-runner) :identity :title)) "Geist not moved to identity")
          (is (= 2 (count (:discard (get-runner)))) "2 cards in heap: All-nighter and DJ Fenris")
          (card-ability state :runner (get-resource state 0) 0) ; Use All-nighter (again)
          (is (= (+ 1 hand-count) (count (:hand (get-runner))))
              "Did not draw another card - Geist ability removed when DJ Fenris was trashed"))))
    (testing "Geist does not trigger Laguna Velasco"
      ;; Regression test for #3759
      (do-game
        (new-game {:runner {:id sunny
                            :deck ["DJ Fenris" "Laguna Velasco District" (qty "All-nighter" 3) (qty "Sure Gamble" 3)]}
                   :options {:start-as :runner}})
        (starting-hand state :runner ["DJ Fenris" "Laguna Velasco District" "All-nighter"])
        (core/gain state :runner :credit 10)
        (play-from-hand state :runner "All-nighter")
        (play-from-hand state :runner "Laguna Velasco District")
        (play-from-hand state :runner "DJ Fenris")
        (is (= (first (prompt-titles :runner)) geist) "List is sorted")
        (is (every? #(some #{%} (prompt-titles :runner))
                    [geist reina maxx hayley chaos]))
        (is (not-any? #(some #{%} (prompt-titles :runner))
                      [professor whizzard jamie kate kit]))
        (click-prompt state :runner geist)
        (is (= geist (get-in (get-resource state 2) [:hosted 0 :title])) "Geist hosted on DJ Fenris")
        (is (= sunny (:title (:identity (get-runner)))) "Still Hayley, id not changed")
        (let [hand-count (count (:hand (get-runner)))]
          ;; Use All-nighter to trigger Geist
          (card-ability state :runner (get-resource state 0) 0)
          (is (= (+ 1 hand-count) (count (:hand (get-runner))))
              "Drew one card with Geist when using All-nighter trash ability, not two (from Laguna Velasco District)"))))))
