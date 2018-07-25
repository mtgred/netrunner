(ns game.cards.ice
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [jinteki.utils :refer [str->int other-side]]
            [jinteki.cards :refer [all-cards]]))

;;; Helper functions specific for ICE

;; Runner abilites for breaking subs
(defn runner-pay-or-break
  "Ability to break a subroutine by spending a resource (Bioroids, Negotiator, etc)"
  [cost subs label]
  (let [cost-str (build-cost-str [cost])
        subs-str (quantify subs "subroutine")]
    {:cost cost
     :label (str label " " subs-str)
     :effect (req (system-msg state :runner (str "spends " cost-str " to " label " " subs-str " on " (:title card))))}))

(defn runner-break
  "Ability to break a subroutine by spending a resource (Bioroids, Negotiator, etc)"
  [cost subs]
  (runner-pay-or-break cost subs "break"))

(defn runner-pay
  "Ability to pay to avoid a subroutine by spending a resource (Popup Window, Turing, etc)"
  [cost subs]
  (runner-pay-or-break cost subs "pay for"))

;; General subroutines
(def end-the-run
  "Basic ETR subroutine"
  {:label "End the run"
   :msg "end the run"
   :effect (effect (end-run))})

(def end-the-run-if-tagged
  "ETR subroutine if tagged"
  {:label "End the run if the Runner is tagged"
   :req (req tagged)
   :msg "end the run"
   :effect (effect (end-run))})

(defn give-tags
  "Basic give runner n tags subroutine."
  [n]
  {:label (str "Give the Runner " (quantify n "tag"))
   :msg (str "give the Runner " (quantify n "tag"))
   :async true
   :effect (effect (gain-tags :corp eid n))})

(def add-power-counter
  "Adds 1 power counter to the card."
  {:label "Add 1 power counter"
   :msg "add 1 power counter"
   :effect (effect (add-counter card :power 1))})

(defn trace-ability
  "Run a trace with specified base strength.
   If successful trigger specified ability"
  ([base {:keys [label] :as ability}]
   {:label (str "Trace " base " - " label)
    :trace {:base base
            :label label
            :successful ability}})
  ([base ability un-ability]
   (let [label (str (:label ability) " / " (:label un-ability))]
     {:label (str "Trace " base " - " label)
      :trace {:base base
              :label label
              :successful ability
              :unsuccessful un-ability}})))

(defn tag-trace
  "Trace ability for giving a tag, at specified base strength"
  ([base] (tag-trace base 1))
  ([base n] (trace-ability base (give-tags n))))

(defn gain-credits-sub
  "Gain specified amount of credits"
  [credits]
  {:label (str "Gain " credits " [Credits]")
   :msg (str "gain " credits " [Credits]")
   :effect (effect (gain-credits credits))})

(defn power-counter-ability
  "Does specified ability using a power counter."
  [{:keys [label message] :as ability}]
  (assoc ability :label (str "Hosted power counter: " label)
                 :msg (str message " using 1 power counter")
                 :counter-cost [:power 1]))

(defn do-psi
  "Start a psi game, if not equal do ability"
  ([{:keys [label] :as ability}]
  {:label (str "Psi Game - " label)
   :msg (str "start a psi game (" label ")")
   :psi {:not-equal ability}})
  ([{:keys [label-neq] :as neq-ability} {:keys [label-eq] :as eq-ability}]
   {:label (str "Psi Game - " label-neq " / " label-eq)
    :msg (str "start a psi game (" label-neq " / " label-eq ")")
    :psi {:not-equal neq-ability
          :equal     eq-ability}}))

(def take-bad-pub
  "Bad pub on rez effect."
  (effect (gain-bad-publicity :corp 1)
          (system-msg (str "takes 1 bad publicity from " (:title card)))))

(def runner-loses-click
  "Runner loses a click effect"
  (req (if (:runner-phase-12 @state)
    ; this handles Jak Sinclair losing clicks before they are given
    (do (swap! state update-in [:runner :extra-click-temp] (fnil dec 0))
        (toast state :runner "Runner loses a click at start of turn" "warning")
        (toast state :corp "Runner loses a click at start of turn" "warning"))
    (lose state :runner :click 1))))

;; For Advanceable ICE
(defn get-advance-counters
  [card]
  (+ (get-counters card :advancement) (:extra-advance-counter card 0)))

(def advance-counters
  "Number of advancement counters - for advanceable ICE."
  (req (get-advance-counters card)))

(def space-ice-rez-bonus
  "Amount of rez reduction for the Space ICE."
  (req (* -3 (get-advance-counters card))))

(defn space-ice
  "Creates data for Space ICE with specified abilities."
  [& abilities]
  {:advanceable :always
   :subroutines (vec abilities)
   :rez-cost-bonus space-ice-rez-bonus})


;; For Grail ICE
(defn grail-in-hand
  "Req that specified card is a Grail card in the Corp's hand."
  [card]
  (and (= (:side card) "Corp")
       (in-hand? card)
       (has-subtype? card "Grail")))

(def reveal-grail
  "Ability for revealing Grail ICE from HQ."
  {:label "Reveal up to 2 Grail ICE from HQ"
   :choices {:max 2
             :req grail-in-hand}
   :msg (let [sub-label #(:label (first (:subroutines (card-def %))))]
          (msg "reveal " (join ", " (map #(str (:title %) " (" (sub-label %) ")") targets))))})

(def resolve-grail
  "Ability for resolving a subroutine on a Grail ICE in HQ."
  {:label "Resolve a Grail ICE subroutine from HQ"
   :choices {:req grail-in-hand}
   :effect (req (doseq [ice targets]
                  (let [subroutine (first (:subroutines (card-def ice)))]
                    (resolve-ability state side subroutine card nil))))})

(defn grail-ice
  "Creates data for grail ICE"
  [ability]
  {:abilities [reveal-grail]
   :subroutines [ability resolve-grail]})


;; For NEXT ICE
(defn next-ice-count
  "Counts number of rezzed NEXT ICE - for use with NEXT Bronze and NEXT Gold"
  [corp]
  (let [servers (flatten (seq (:servers corp)))
        rezzed-next? #(and (rezzed? %) (has-subtype? % "NEXT"))]
    (reduce (fn [c server] (+ c (count (filter rezzed-next? (:ices server))))) 0 servers)))


;; For Morph ICE
(defn morph [state side card new old]
  (update! state side (assoc card
                        :subtype-target new
                        :subtype (combine-subtypes true
                                                   (remove-subtypes (:subtype card) old)
                                                   new)))
  (update-ice-strength state side card))

(defn morph-effect
  "Creates morph effect for ICE. Morphs from base type to other type"
  [base other]
  (req (if (odd? (get-counters (get-card state card) :advancement))
         (morph state side card other base)
         (morph state side card base other))))

(defn morph-ice
  "Creates the data for morph ICE with specified types and ability."
  [base other ability]
  (let [ab {:req (req (= (:cid card) (:cid target)))
            :effect (morph-effect base other)}]
    {:advanceable :always
     :effect (morph-effect base other)
     :subroutines [ability]
     :events {:advance ab :advancement-placed ab}}))


;; For Constellation ICE
(defn constellation-ice
  "Generates map for Constellation ICE with specified effect."
  [ability]
  {:subroutines [(assoc-in (trace-ability 2 ability) [:trace :kicker] (assoc ability :min 5))]})


; For 7 Wonders ICE
(defn wonder-sub
  "Checks total number of advancement counters on a piece of ice against number"
  [card number]
  (<= number (get-advance-counters card)))

;; Helper function for adding implementation notes to ICE defined with functions
(defn implementation-note
  "Adds an implementation note to the ice-definition"
  [note ice-def]
  (assoc ice-def :implementation note))
