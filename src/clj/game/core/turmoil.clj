(ns game.core.turmoil
  (:require
   [game.core.card :refer [agenda? asset? event? has-subtype? hardware? resource? program? upgrade? ice? operation? identity? corp? runner?]]
   [game.core.commands :refer [lobby-command]]
   [game.core.identities :refer [disable-identity disable-card]]
   [game.core.initializing :refer [card-init make-card]]
   [game.core.hosting :refer [host]]
   [game.core.moving :refer [move]]
   [game.core.payment :refer [->c]]
   [game.core.say :refer [system-msg]]
   [game.core.set-up :refer [build-card]]
   [game.utils :refer [same-card? server-cards server-card]]
   [clojure.string :as string]
   [taoensso.timbre :as timbre]))

;; store all this in memory once so we don't need to recalculate it a trillion times
;; everything should be a vec, so rand-nth will be O[1] instead of O[n/2]

(defonce agenda-by-points (atom {}))
(defonce identity-by-side (atom {}))
(defonce program-by-icebreaker (atom {}))
(defonce cards-by-type (atom {}))
(defonce has-been-set? (atom nil))

(defn- is-econ?
  "Is a card an economy card?
   Something like:
   * gain x credits
   * take x/all host/ed credits"
  [card]
  (re-find #".(ain|ake) (\d+|(.? host.*)).?.?.?redit" (or (:text (server-card (:title card))) "")))

(defonce filter-by-econ-types #{:asset :operation :resource :event})

(defn- set-cards! []
  (when-not @has-been-set?
    (timbre/info "assigning server cards for turmoil")
    (reset! agenda-by-points
            (->> (server-cards)
                 (filterv agenda?)
                 (group-by :agendapoints)))
    (reset! identity-by-side  {:corp   (->> (server-cards)
                                            (filterv identity?)
                                            (filterv corp?))
                               :runner (->> (server-cards)
                                            (filterv identity?)
                                            (filterv runner?))})
    (reset! program-by-icebreaker {:icebreaker (->> (server-cards)
                                                    (filterv program?)
                                                    (filterv #(has-subtype? % "Icebreaker")))
                                   :regular (->> (server-cards)
                                                 (filterv program?)
                                                 (filterv #(not (has-subtype? % "Icebreaker"))))})
    (reset! cards-by-type (let [types {:asset asset?
                                       :event event?
                                       :hardware hardware?
                                       :resource resource?
                                       :program program?
                                       :upgrade upgrade?
                                       :ice ice?
                                       :operation operation?}
                                keys-sorted (sort (keys types))]
                            (zipmap keys-sorted (mapv #(if (contains? filter-by-econ-types %)
                                                         {:economy (filterv (every-pred (types %) is-econ?) (server-cards))
                                                          :regular (filterv (every-pred (types %) (complement is-econ?)) (server-cards))}
                                                         (filterv (types %) (server-cards)))
                                                      keys-sorted))))
    (reset! has-been-set? true)))

(def replacement-factor
  "how often should we replace these cards?"
  {:hand 4 :deck 8  :discard 3 :id 4 :side 50 :card-type-cross-contam 10 :icebreaker-cross-contam 10 :econ-cross-contam 10})

(defn- should-replace?
  ([key] (-> (key replacement-factor 25) rand-int zero?))
  ([key ex] (-> (key replacement-factor 25) (min ex) (max 1) rand-int zero?)))

(def corp-card-types   #{:asset :upgrade :ice :operation})
(def runner-card-types #{:resource :hardware :program :event})

(defn- pick-replacement-card
  "given a card, pick a suitable replacement card at random
   agendas maintain point value,
   programs maintain if they are/aren't icebreakers,
   everything else is random"
  [card]
  (set-cards!)
  (let [c-type (-> card :type string/lower-case keyword)]
    (cond
      ;; agenda (x points) -> agenda (x points) should stop people gaming density
      (= c-type :agenda)
      (let [target-points (:agendapoints card)]
        (rand-nth (get @agenda-by-points target-points)))
      (= c-type :identity)
      (let [target-side (-> card :side string/lower-case keyword)]
        (rand-nth (get @identity-by-side target-side)))
      ;; icebreaker -> icebreaker should make it reasonable to not get completely locked out
      (= c-type :program)
      ;; allow 10% cross-contain for icebreakers
      (let [choice (should-replace? :icebreaker-cross-contam)
            choice (if (= 1 (count (filter identity [choice (has-subtype? card "Icebreaker")])))
                     :icebreaker
                     :regular)]
        (rand-nth (get @program-by-icebreaker choice)))
      (contains? filter-by-econ-types c-type)
      ;; allow 10% cross-conta for econ
      (let [choice (rand-nth
                     (if (is-econ? card)
                       [:economy :economy :economy :economy :economy :economy :economy :economy :economy :regular]
                       [:regular :regular :regular :regular :regular :regular :regular :regular :regular :economy]))]
        (rand-nth (get-in @cards-by-type [c-type choice])))
      :else (rand-nth (get @cards-by-type c-type)))))

(defn- replace-hand [state side]
  (let [new-hand (mapv #(if (should-replace? :hand (count (get-in @state [side :hand])))
                          (do (disable-card state side %)
                              (assoc (build-card (pick-replacement-card %)) :zone [:hand]))
                          %)
                       (get-in @state [side :hand]))]
    (swap! state assoc-in [side :hand] new-hand)))

(defn- replace-discard [state side]
  (let [new-discard (mapv #(if (should-replace? :discard)
                             (do (disable-card state side %)
                                 (assoc (build-card (pick-replacement-card %)) :zone [:discard] :seen (:seen %)))
                             %)
                       (get-in @state [side :discard]))]
    (swap! state assoc-in [side :discard] new-discard)))

(defn- replace-deck [state side]
  (let [new-deck (mapv #(if (should-replace? :deck)
                          (do (disable-card state side %)
                              (assoc (build-card (pick-replacement-card %)) :zone [:deck]))
                          %)
                       (get-in @state [side :deck]))]
    (swap! state assoc-in [side :deck] new-deck)))

(defn- replace-id [state side]
  ;; defuse any sillyness with 'replace-id'
  (when (should-replace? :id)
    (let [old-id (get-in @state [side :identity])
          new-id (pick-replacement-card {:type "Identity" :side (name side)})]
      ;; Handle hosted cards (Ayla) - Part 1
      (doseq [c (:hosted old-id)]
        (move state side c :temp-hosted))
      (disable-identity state side)
      ;; Move the selected ID to [:runner :identity] and set the zone
      (let [new-id (-> new-id make-card (assoc :zone [(->c :identity)]))
            num-old-blanks (:num-disabled old-id)]
        (swap! state assoc-in [side :identity] new-id)
        (card-init state side new-id)
        (when num-old-blanks
          (dotimes [_ num-old-blanks]
            (disable-identity state side))))
      ;; Handle hosted cards (Ayla) - Part 2
      (doseq [c (get-in @state [side :temp-hosted])]
        ;; Currently assumes all hosted cards are hosted facedown (Ayla)
        (host state side (get-in @state [side :identity]) c {:facedown true})))))

(defn- transpose-sides
  [state side]
  ;; this is kosher I promise -> Emphyrio, Jack Vance (it's a neat book, I recommend it)
  (system-msg state side "FINUKA TRANSPOSES")
  (lobby-command {:command :swap-sides
                  :gameid (:gameid @state)}))

(defn shuffle-cards-for-side
  [state side]
  (do (replace-hand state side)
      (replace-deck state side)
      (replace-discard state side)
      (replace-id state side)
      ;; 1 in 50 chance at the start of each turn to swap the sides you're playing on
      ;; realistically, this should happen on average about 2 games in 4
      ;; and 0.75 of those games will have 2+ swaps
      (when (should-replace? :side)
        (transpose-sides state side))))
