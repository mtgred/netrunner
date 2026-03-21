(ns game.core.sabotage
  (:require
    [clojure.string :as string]
    [game.core.card :refer [corp? in-hand?]]
    [game.core.eid :refer [effect-completed]]
    [game.core.engine :refer [resolve-ability]]
    [game.core.moving :refer [trash-cards]]
    [game.core.say :refer [multi-msg]]
    [game.macros :refer [req msg continue-ability]]
    [game.utils :refer [enumerate-str enumerate-cards pluralize quantify]]))

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

(defn- cards-str
  [known unknown from public]
  (let [unknown-str (str (quantify (count unknown) (if (seq known) "unknown card" "card"))
                         (when public (str " (" (enumerate-cards unknown)")")))]
    (if (seq known)
      (str " " (enumerate-str (concat (map :title known)
                                      (when (seq unknown)
                                        [unknown-str])))
           " from " from)
      (str unknown-str " from " from))))

(defn trash-selected-req
  [n]
  (req
    (let [targets (if (nil? target) [] targets) ; catch cancel-effect that gives [nil] as targets
          selected-hq (count targets)
          selected-rd (min (count (:deck corp))
                           (- n selected-hq))
          rnd-to-trash (take selected-rd (:deck corp))
          to-trash (concat targets rnd-to-trash)
          known-hq-cards (filter #(contains? (set (get-in @state [:breach :known-cids :hand] [])) (:cid %)) to-trash)
          known-rd-cards (filter #(contains? (set (get-in @state [:breach :known-cids :deck] [])) (:cid %)) to-trash)
          unknown-hq-cards (filter #(not (contains? (set (get-in @state [:breach :known-cids :hand] [])) (:cid %))) targets)
          unknown-rd-cards (filter #(not (contains? (set (get-in @state [:breach :known-cids :deck] [])) (:cid %))) rnd-to-trash)
          public-msg (str "trashes"
                          (when (pos? selected-hq) (cards-str known-hq-cards unknown-hq-cards "HQ" nil))
                          (when (and (pos? selected-hq) (pos? selected-rd)) " and ")
                          (when (pos? selected-rd) (cards-str known-rd-cards unknown-rd-cards "the top of R&D" nil)))
          private-msg (str "trashes"
                           (when (pos? selected-hq) (cards-str known-hq-cards unknown-hq-cards "hq" true))
                           (when (and (pos? selected-hq) (pos? selected-rd)) " and ")
                           (when (pos? selected-rd) (cards-str known-rd-cards unknown-rd-cards "the top of R&D" true)))]
      (multi-msg state side {:corp private-msg
                             :public public-msg})
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
                       :cancel {:async true
                                :effect (trash-selected-req n)}
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
