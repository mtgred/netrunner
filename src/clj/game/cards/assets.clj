(ns game.cards.assets
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [jinteki.utils :refer [str->int other-side]]
            [jinteki.cards :refer [all-cards]]))

;; Asset-specific helpers
(defn installed-access-trigger
  "Effect for triggering ambush on access.
  Ability is what happends upon access. If cost is specified Corp needs to pay that to trigger."
  ([cost ability]
   (let [ab (if (pos? cost) (assoc ability :cost [:credit cost]) ability)
         prompt (if (pos? cost)
                  (req (str "Pay " cost " [Credits] to use " (:title card) " ability?"))
                  (req (str "Use " (:title card) " ability?")))]
     (installed-access-trigger cost ab prompt)))
  ([cost ability prompt]
   {:access {:req (req (and installed (>= (:credit corp) cost)))
             :async true
             :effect (effect (show-wait-prompt :runner (str "Corp to use " (:title card)))
                             (continue-ability
                              {:optional
                               {:prompt prompt
                                :yes-ability ability
                                :end-effect (effect (clear-wait-prompt :runner))}}
                              card nil))}}))

(defn advance-ambush
  "Creates advanceable ambush structure with specified ability for specified cost"
  ([cost ability] (assoc (installed-access-trigger cost ability) :advanceable :always))
  ([cost ability prompt] (assoc (installed-access-trigger cost ability prompt) :advanceable :always)))

(defn campaign
  "Creates a Campaign with X counters draining Y per-turn.
  Trashes itself when out of counters"
  [counters per-turn]
  (let [ability {:msg (str "gain " per-turn " [Credits]")
                 :counter-cost [:credit per-turn]
                 :once :per-turn
                 :req (req (:corp-phase-12 @state))
                 :label (str "Gain " per-turn " [Credits] (start of turn)")
                 :effect (req (take-credits state :corp per-turn)
                              (when (zero? (get-counters card :credit))
                                (trash state :corp card)))}]
    {:effect (effect (add-counter card :credit counters))
     :derezzed-events {:runner-turn-ends corp-rez-toast}
     :events {:corp-turn-begins ability}
     :abilities [ability]}))

(defn as-trashed-agenda
  "Adds the given card to the given side's :scored area as an agenda worth n points after resolving the trash prompt."
  ([state side eid card n] (as-trashed-agenda state side eid card n nil))
  ([state side eid card n options]
  (or
    ; if the runner did not trash the card on access, then this will work
    (move state :runner (assoc (deactivate state side card) :agendapoints n) :scored options)
    ; allow force option in case of Blacklist/News Team
    (move state :runner (assoc (deactivate state side card) :agendapoints n :zone [:discard]) :scored options))
   (wait-for (trigger-event-sync state side :as-agenda (assoc card :as-agenda-side side :as-agenda-points n))
             (do (gain-agenda-point state side n)
                 (effect-completed state side eid)))))
