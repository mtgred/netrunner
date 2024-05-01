(ns game.core.card
  (:require
   [clojure.string :refer [lower-case]]
   [medley.core :refer [find-first]]))

(defrecord Card
  [abilities
   advance-counter
   advanceable
   advancementcost
   agendapoints
   art
   baselink
   card-target
   cid
   code
   corp-abilities
   cost
   counter
   current-advancement-requirement
   current-points
   current-strength
   cycle_code
   deck-limit
   disabled
   extra-advance-counter
   face
   facedown
   faces
   faction
   format
   host
   hosted
   icon
   images
   implementation
   index
   installed
   memoryunits
   minimumdecksize
   new
   normalizedtitle
   playable
   previous-versions
   previous-zone
   printed-title
   quantity
   rezzed
   rotated
   runner-abilities
   seen
   selected
   set_code
   side
   special
   strength
   subroutines
   subtype
   subtype-target
   subtypes
   title
   trash
   type
   uniqueness
   zone])

(defn get-cid
  "Gets the cid of a given card when wrapped in an effect-handler map"
  [card]
  (get-in card [:card :cid]))

(defn get-title
  "Title or printed title if the card is a counter or fake agenda"
  [card]
  (or (:title card) (:printed-title card)))

(defn get-nested-host
  "Recursively searches upward to find the 'root' card of a hosting chain."
  [card]
  (if (:host card) (recur (:host card)) card))

(defn get-zone
  "Returns the zone of the 'root' card of a hosting chain"
  [card]
  (:zone (get-nested-host card)))

(defn in-server?
  "Checks if the specified card is installed in -- and not PROTECTING -- a server"
  [card]
  (= (last (get-zone card)) #?(:clj :content
                               :cljs "content")))

(defn in-hand?
  "Checks if the specified card is in the hand."
  [card]
  (= (get-zone card) #?(:clj [:hand]
                        :cljs ["hand"])))

(defn in-discard?
  "Checks if the specified card is in the discard pile."
  [card]
  (= (get-zone card) #?(:clj [:discard]
                        :cljs ["discard"])))

(defn in-deck?
  "Checks if the specified card is in the draw deck."
  [card]
  (= (get-zone card) #?(:clj [:deck]
                        :cljs ["deck"])))

(defn in-archives-root?
  [card]
  (= (get-zone card) #?(:clj [:servers :archives :content]
                        :cljs ["servers" "archives" "content"])))

(defn in-hq-root?
  [card]
  (= (get-zone card) #?(:clj [:servers :hq :content]
                        :cljs ["servers" "hq" "content"])))

(defn in-rd-root?
  [card]
  (= (get-zone card) #?(:clj [:servers :rd :content]
                        :cljs ["servers" "rd" "content"])))

(defn in-root?
  [card]
  (or (in-archives-root? card)
      (in-hq-root? card)
      (in-rd-root? card)))

(defn protecting-archives?
  [card]
  (= (get-zone card) #?(:clj [:servers :archives :ices]
                        :cljs ["servers" "archives" "ices"])))

(defn protecting-hq?
  [card]
  (= (get-zone card) #?(:clj [:servers :hq :ices]
                        :cljs ["servers" "hq" "ices"])))

(defn protecting-rd?
  [card]
  (= (get-zone card) #?(:clj [:servers :rd :ices]
                        :cljs ["servers" "rd" "ices"])))

(defn protecting-a-central?
  [card]
  (or (protecting-archives? card)
      (protecting-hq? card)
      (protecting-rd? card)))

(defn in-play-area?
  "Checks if the specified card is in the play area."
  [card]
  (= (get-zone card) #?(:clj [:play-area]
                        :cljs ["play-area"])))

(defn in-set-aside?
  "Checks if the specific card is in a set-aside area."
  [card]
  (= (get-zone card) #?(:clj [:set-aside]
                        :cljs ["set-aside"])))

(defn set-aside-visible?
  "Checks if the specific card is in set aside and visible to this side"
  [card side]
  (and (in-set-aside? card)
       (if (= :corp side)
         (:corp-can-see (:set-aside-visibility card))
         (:runner-can-see (:set-aside-visibility card)))))

(defn in-current?
  "Checks if the specified card is in the 'current' zone."
  [card]
  (= (get-zone card) #?(:clj [:current]
                        :cljs ["current"])))

(defn in-scored?
  "Checks if the specified card is in _a_ score area (don't know which one)."
  [card]
  (= (get-zone card) #?(:clj [:scored]
                        :cljs ["scored"])))

(defn in-rfg?
  "Checks if the specified card is in the 'remove from game' zone"
  [card]
  (= (get-zone card) #?(:clj [:rfg]
                        :cljs ["rfg"])))

(defn- card-is?
  "Checks the property of the card to see if it is equal to the given value,
  as either a string or a keyword"
  [card property value]
  (let [cv (property card)]
    (cond
      (or (keyword? cv)
          (and (string? value)
               (string? cv)))
      (= value cv)
      (and (keyword? value)
           (string? cv))
      (= value (keyword (lower-case cv)))
      :else
      (= value cv))))

(defn runner?
  [card]
  (card-is? card :side "Runner"))

(defn corp?
  [card]
  (card-is? card :side "Corp"))

(defn is-type?
  "Checks if the card is of the specified type, where the type is a string."
  [card type]
  (card-is? card :type type))

(declare facedown?)

(defn agenda?
  [card]
  (is-type? card "Agenda"))

(defn asset?
  [card]
  (is-type? card "Asset"))

(defn event?
  [card]
  (and (not (facedown? card))
       (is-type? card "Event")))

(defn hardware?
  [card]
  (and (not (facedown? card))
       (is-type? card "Hardware")))

(defn ice?
  [card]
  (is-type? card "ICE"))

(defn fake-identity?
  [card]
  (is-type? card "Fake-Identity"))

(defn identity?
  [card]
  (or (is-type? card "Identity")
      (fake-identity? card)))

(defn operation?
  [card]
  (is-type? card "Operation"))

(defn program?
  [card]
  (and (not (facedown? card))
       (is-type? card "Program")))

(defn resource?
  [card]
  (and (not (facedown? card))
       (is-type? card "Resource")))

(defn upgrade?
  [card]
  (is-type? card "Upgrade"))

(defn condition-counter?
  [card]
  (is-type? card "Counter"))

(defn basic-action?
  [card]
  (is-type? card "Basic Action"))

(defn has-subtype?
  "Checks if the specified subtype is present in the card, ignoring case."
  [card subtype]
  (find-first #(= % subtype) (:subtypes card)))

(defn virus-program?
  [card]
  (and (program? card)
       (has-subtype? card "Virus")))

(defn console?
  [card]
  (and (hardware? card)
       (has-subtype? card "Console")))

(defn unique?
  [card]
  (:uniqueness card))

(defn corp-installable-type?
  "Is the card of an acceptable type to be installed in a server"
  [card]
  (or (asset? card)
      (agenda? card)
      (ice? card)
      (upgrade? card)))

(defn rezzed?
  [card]
  (:rezzed card))

(defn faceup?
  [card]
  (or (:seen card)
      (rezzed? card)))

(defn installed?
  [card]
  (or (:installed card)
      (= (first (get-zone card)) #?(:clj :servers
                                    :cljs "servers"))))

(defn facedown?
  "Checks if the specified card is facedown."
  [card]
  (or (when-not (condition-counter? card)
        (= (get-zone card) #?(:clj [:rig :facedown]
                              :cljs ["rig" "facedown"])))
      (:facedown card)))

(defn active?
  "Checks if the card is active and should receive game events/triggers."
  [card]
  (or (basic-action? card)
      (and (identity? card)
           (not (facedown? card)))
      (in-play-area? card)
      (in-current? card)
      (in-scored? card)
      (condition-counter? card)
      (and (corp? card)
           (installed? card)
           (rezzed? card))
      (and (runner? card)
           (installed? card)
           (not (facedown? card)))))

(defn get-advancement-requirement
  [card]
  (when (agenda? card)
    (or (:current-advancement-requirement card)
        (:advancementcost card))))

(defn get-agenda-points
  [card]
  (or (:current-points card)
      (:agendapoints card)
      0))

(defn- is-disabled-reg?
  [state card]
  (get (:disabled-card-reg @state) (:cid card)))

(defn can-be-advanced?
  "Returns true if the card can be advanced"
  ([card]
   (or (card-is? card :advanceable #?(:clj :always
                                      :cljs "always"))
       ;; e.g. Tyrant, Woodcutter
       (and (card-is? card :advanceable #?(:clj :while-rezzed
                                           :cljs "while-rezzed"))
            (rezzed? card))
       ;; e.g. Haas Arcology AI
       (and (card-is? card :advanceable #?(:clj :while-unrezzed
                                           :cljs "while-unrezzed"))
            (not (rezzed? card)))
       (and (is-type? card "Agenda")
            (installed? card))))
  ([state card]
   (and (can-be-advanced? card)
        ;; note - the text allowing a card to be advanceable is an ability,
        ;;    except for agendas, where it's implicit to the card type -nbk, apr '24
        (or (agenda? card)
            (not (is-disabled-reg? state card))))))

(defn get-counters
  "Get number of counters of specified type."
  [card counter]
  (if (= counter :advancement)
    ((fnil + 0 0) (:advance-counter card) (:extra-advance-counter card))
    (or (get-in card [:counter counter]) 0)))

(defn- to-keyword [string]
  (if (string? string)
    (keyword (lower-case string))
    string))

(defn same-card?
  "Checks if the two cards are the same by `:cid`. Returns false if both cards
  do not have `:cid`. Alternatively specify 1-function to use to check the card."
  ([card1 card2] (same-card? :cid card1 card2))
  ([func card1 card2]
   (let [r1 (func card1)
         r2 (func card2)]
     (and r1 r2 (= r1 r2)))))

#?(:clj
   (do
     (defn assoc-host-zones
       "Associates a new zone onto a card and its host(s)."
       [card]
       (let [card (update card :zone #(map keyword %))]
         (if (:host card)
           (update card :host assoc-host-zones)
           card)))

     (declare get-card-hosted)

     (defn get-card
       "Returns the most recent copy of the card from the current state, as identified
       by the argument's :zone and :cid."
       [state {:keys [cid zone side host type] :as card}]
       (when card
         (if (= type "Identity")
           (get-in @state [(to-keyword side) :identity])
           (if zone
             (if host
               (get-card-hosted state card)
               (some #(when (= cid (:cid %)) %)
                     (let [zones (map to-keyword zone)]
                       (if (= (first zones) :scored)
                         (into (get-in @state [:corp :scored]) (get-in @state [:runner :scored]))
                         (get-in @state (cons (to-keyword side) zones))))))
             card))))

     (defn get-card-hosted
       "Finds the current version of the given card by finding its host."
       [state card]
       (let [root-host (get-card state (get-nested-host card))
             helper (fn search [card target]
                      (when-not (nil? card)
                        (if-let [c (some #(when (same-card? % target) %) (:hosted card))]
                          c
                          (some #(when-let [s (search % target)] s) (:hosted card)))))]
         (helper root-host card)))))

(defn card-index
  "Get the zero-based index of the given card in its server's list of content"
  [state card]
  (or (:index card)
      (first (keep-indexed #(when (same-card? %2 card) %1) (get-in @state (cons :corp (get-zone card)))))))

(defn is-public?
  "Returns if a given card should be visible to the opponent"
  ([card] (is-public? (to-keyword (:side card))))
  ([card side]
   ;; public cards for both sides:
   ;; * basic action
   ;; * identity
   ;; * in a public zone: score area, current, play area, remove from game
   (or (basic-action? card)
       (identity? card)
       (in-scored? card)
       (in-current? card)
       (in-play-area? card)
       (in-rfg? card)
       (set-aside-visible? card side)
       (if (= side :corp)
         ;; public runner cards:
         ;; * installed/hosted and not facedown
         ;; * in heap
         (or (and (corp? card)
                  (not (in-set-aside? card)))
             (and (or (installed? card)
                      (:host card))
                  (or (faceup? card)
                      (not (facedown? card))))
             (in-discard? card))
         ;; public corp cards:
         ;; * installed and rezzed
         ;; * in archives and faceup
         (or (and (runner? card)
                  (not (in-set-aside? card)))
             (and (or (installed? card)
                      (:host card))
                  (or (operation? card)
                      (condition-counter? card)
                      (faceup? card)))
             (and (in-discard? card)
                  (faceup? card)))))))

;; CR 1.8
;; 10.1.3. Some abilities add a card to a player’s score area “as an agenda”. When this
;;    happens, the card loses all its previous properties and gains only those
;;    properties specified in the effect converting it. This conversion lasts until the
;;    card moves to a zone that is not a score area, at which point it returns to being
;;    its original printed card. If this happens in any way other than by agenda
;;    forfeit, the card is immediately trashed.

(defn convert-to-agenda
  [{:keys [cid code host hosted side title zone implementation]} n]
  (map->Card
    {:agendapoints n
     :cid cid
     :code code
     :host host
     :hosted hosted
     :implementation implementation
     :printed-title title
     :side side
     :type "Agenda"
     :zone zone}))

;; CR 1.8
;; 10.1.4. Some abilities can convert a card into a counter. When this happens, the card
;;    loses all its previous properties and gains only those properties specified in the
;;    effect converting it. This conversion lasts until the counter moves to another
;;    zone, at which point it reverts to being a card, regains its original printed
;;    characteristics, and is trashed.

(defn convert-to-condition-counter
  [{:keys [cid code side title zone implementation]}]
  (map->Card
    {:cid cid
     :code code
     :implementation implementation
     :printed-title title
     :side side
     :type "Counter"
     :zone zone}))
