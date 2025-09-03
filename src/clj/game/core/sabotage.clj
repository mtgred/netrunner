(ns game.core.sabotage
  (:require
    [clojure.string :as string]
    [game.core.card :refer [corp? in-hand?]]
    [game.core.eid :refer [effect-completed]]
    [game.core.engine :refer [resolve-ability]]
    [game.core.moving :refer [trash-cards]]
    [game.core.say :refer [system-msg]]
    [game.macros :refer [req msg continue-ability]]
    [game.utils :refer [enumerate-str pluralize quantify]]))

(defn choosing-prompt-req
  [n]
  (req
    (let [cards-rd (count (get-in @state [:corp :deck]))
          forced-hq (- n cards-rd)]
      (str "Choose"
           (when (pos? forced-hq)
             (str " at least " forced-hq " " (pluralize "card" forced-hq) " and"))
           " up to " n " " (pluralize "card" n)
           " to trash from HQ. Remainder will be trashed from top of R&D."))))

(defn trash-selected-req
  [n]
  (req
    (let [targets (if (nil? target) [] targets) ; catch cancel-effect that gives [nil] as targets
          selected-hq (count targets)
          selected-rd (min (count (:deck corp))
                           (- n selected-hq))
          to-trash (concat targets (take selected-rd (:deck corp)))
          known-hq-cards (filter #(contains? (set (get-in @state [:breach :known-cids :hand] [])) (:cid %)) to-trash)
          known-rd-cards (filter #(contains? (set (get-in @state [:breach :known-cids :deck] [])) (:cid %)) to-trash)
          unknown-hq-cards (- selected-hq (count known-hq-cards))
          unknown-rd-cards (- selected-rd (count known-rd-cards))]
      (system-msg state side
                  (str
                    "trashes"
                    (when (pos? selected-hq)
                      (if (seq known-hq-cards)
                        (str " " (enumerate-str (concat (map :title known-hq-cards)
                                                        (when (pos? unknown-hq-cards)
                                                          [(quantify unknown-hq-cards "unknown card")])))
                             " from HQ")
                        (str " " (quantify selected-hq "card") " from HQ")))
                    (when (and (pos? selected-hq) (pos? selected-rd))
                      " and")
                    (when (pos? selected-rd)
                      (if (seq known-rd-cards)
                        (str " " (enumerate-str (concat (map :title known-rd-cards)
                                                        (when (pos? unknown-rd-cards)
                                                          [(quantify unknown-rd-cards "unknown card")])))
                             " from R&D")
                        (str " " (quantify selected-rd "card") " from the top of R&D")))))
      (trash-cards state side eid to-trash {:unpreventable true}))))

(defn sabotage-ability
  [n]
  (let [choosing-ab (fn [forced-hq]
                      ^:ignore-async-check
                      {:waiting-prompt true
                       :player :corp
                       :prompt (choosing-prompt-req n)
                       :choices {:min forced-hq
                                 :max n
                                 :card #(and (corp? %)
                                             (in-hand? %))}
                       :async true
                       :cancel-effect (trash-selected-req n)
                       :effect (trash-selected-req n)})
        check-forcing-ab {:async true
                          :effect (req
                                    (let [cards-rd (count (:deck corp))
                                          cards-hq (count (:hand corp))
                                          forced-hq (- n cards-rd)]
                                      (if (>= n (+ cards-rd cards-hq))
                                        ((trash-selected-req n) state :corp eid card (:hand corp))
                                        (continue-ability state side
                                                          (choosing-ab forced-hq)
                                                          card nil))))}]
    {:req (req (pos? n))
     :msg (msg "sabotage " n)
     :async true
     :effect (req
               (swap! state update-in [:stats :runner :cards-sabotaged] (fnil + 0) n)
               (continue-ability state side check-forcing-ab card targets))}))
