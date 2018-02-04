(in-ns 'game.core)

(def trash-program {:prompt "Select a program to trash"
                    :label "Trash a program"
                    :msg (msg "trash " (:title target))
                    :choices {:req #(and (installed? %)
                                         (is-type? % "Program"))}
                    :effect (effect (trash target {:cause :subroutine})
                                    (clear-wait-prompt :runner))})

(def trash-hardware {:prompt "Select a piece of hardware to trash"
                     :label "Trash a piece of hardware"
                     :msg (msg "trash " (:title target))
                     :choices {:req #(and (installed? %)
                                          (is-type? % "Hardware"))}
                     :effect (effect (trash target {:cause :subroutine}))})

(def trash-resource-sub {:prompt "Select a resource to trash"
                         :label "Trash a resource"
                         :msg (msg "trash " (:title target))
                         :choices {:req #(and (installed? %)
                                              (is-type? % "Resource"))}
                         :effect (effect (trash target {:cause :subroutine}))})

(def trash-installed {:prompt "Select an installed card to trash"
                      :player :runner
                      :label "Force the Runner to trash an installed card"
                      :msg (msg "force the Runner to trash " (:title target))
                      :choices {:req #(and (installed? %)
                                           (= (:side %) "Runner"))}
                      :effect (effect (trash target {:cause :subroutine}))})

(def corp-rez-toast
  "Effect to be placed with `:runner-turn-ends` to remind players of 'when turn begins'
  triggers"
  {:effect (req (toast state :corp "Reminder: You have unrezzed cards with \"when turn begins\" abilities." "info"))})

(declare reorder-final) ; forward reference since reorder-choice and reorder-final are mutually recursive

(defn reorder-choice
  "Generates a recursive prompt structure for cards that do reordering (Indexing, Making an Entrance, etc.)

  reorder-side is the side to be reordered, i.e. :corp for Indexing and Precognition.
  wait-side is the side that has a wait prompt while ordering is in progress, i.e. :corp for Indexing and Spy Camera.

  This is part 1 - the player keeps choosing cards until there are no more available choices. A wait prompt should
  exist before calling this function. See Indexing and Making an Entrance for examples on how to call this function."

  ([reorder-side cards] (reorder-choice reorder-side (other-side reorder-side) cards `() (count cards) cards nil))
  ([reorder-side wait-side remaining chosen n original] (reorder-choice reorder-side wait-side remaining chosen n original nil))
  ([reorder-side wait-side remaining chosen n original dest]
  {:prompt (str "Select a card to move next "
                (if (= dest "bottom") "under " "onto ")
                (if (= reorder-side :corp) "R&D" "your Stack"))
   :choices remaining
   :delayed-completion true
   :effect (req (let [chosen (cons target chosen)]
                  (if (< (count chosen) n)
                    (continue-ability state side (reorder-choice reorder-side wait-side (remove-once #(not= target %) remaining)
                                                                 chosen n original dest) card nil)
                    (continue-ability state side (reorder-final reorder-side wait-side chosen original dest) card nil))))}))

(defn- reorder-final
  "Generates a recursive prompt structure for cards that do reordering (Indexing, Making an Entrance, etc.)
  This is part 2 - the player is asked for final confirmation of the reorder and is provided an opportunity to start over."

  ([reorder-side wait-side chosen original] (reorder-final reorder-side wait-side chosen original nil))
  ([reorder-side wait-side chosen original dest]
   {:prompt (if (= dest "bottom")
              (str "The bottom cards of " (if (= reorder-side :corp) "R&D" "your Stack")
                   " will be " (join  ", " (map :title (reverse chosen))) ".")
              (str "The top cards of " (if (= reorder-side :corp) "R&D" "your Stack")
                   " will be " (join  ", " (map :title chosen)) "."))
   :choices ["Done" "Start over"]
   :delayed-completion true
   :effect (req
             (cond
               (and (= dest "bottom") (= target "Done"))
               (do (swap! state update-in [reorder-side :deck]
                          #(vec (concat (drop (count chosen) %) (reverse chosen))))
                   (clear-wait-prompt state wait-side)
                   (effect-completed state side eid card))

               (= target "Done")
               (do (swap! state update-in [reorder-side :deck]
                          #(vec (concat chosen (drop (count chosen) %))))
                   (clear-wait-prompt state wait-side)
                   (effect-completed state side eid card))

               :else
               (continue-ability state side (reorder-choice reorder-side wait-side original '() (count original) original dest) card nil)))}))

(defn swap-ice
  "Swaps two pieces of ICE."
  [state side a b]
  (let [a-index (ice-index state a)
        b-index (ice-index state b)
        a-new (assoc a :zone (:zone b))
        b-new (assoc b :zone (:zone a))]
    (swap! state update-in (cons :corp (:zone a)) #(assoc % a-index b-new))
    (swap! state update-in (cons :corp (:zone b)) #(assoc % b-index a-new))
    (doseq [newcard [a-new b-new]]
      (doseq [h (:hosted newcard)]
        (let [newh (-> h
                       (assoc-in [:zone] '(:onhost))
                       (assoc-in [:host :zone] (:zone newcard)))]
          (update! state side newh)
          (unregister-events state side h)
          (register-events state side (:events (card-def newh)) newh))))
    (update-ice-strength state side a-new)
    (update-ice-strength state side b-new)))

(defn card-index
  "Get the zero-based index of the given card in its server's list of content. Same as ice-index"
  [state card]
  (first (keep-indexed #(when (= (:cid %2) (:cid card)) %1) (get-in @state (cons :corp (:zone card))))))

(defn swap-installed
  "Swaps two installed corp cards - like swap ICE except no strength update"
  [state side a b]
  (let [a-index (card-index state a)
        b-index (card-index state b)
        a-new (assoc a :zone (:zone b))
        b-new (assoc b :zone (:zone a))]
    (swap! state update-in (cons :corp (:zone a)) #(assoc % a-index b-new))
    (swap! state update-in (cons :corp (:zone b)) #(assoc % b-index a-new))
    (doseq [newcard [a-new b-new]]
      (doseq [h (:hosted newcard)]
        (let [newh (-> h
                       (assoc-in [:zone] '(:onhost))
                       (assoc-in [:host :zone] (:zone newcard)))]
          (update! state side newh)
          (unregister-events state side h)
          (register-events state side (:events (card-def newh)) newh))))))

;; Load all card definitions into the current namespace.
(load "cards/agendas")
(load "cards/assets")
(load "cards/events")
(load "cards/hardware")
(load "cards/ice")
(load "cards/icebreakers")
(load "cards/identities")
(load "cards/operations")
(load "cards/programs")
(load "cards/resources")
(load "cards/upgrades")

(def cards (merge cards-agendas cards-assets cards-events cards-hardware cards-ice cards-icebreakers cards-identities
                  cards-operations cards-programs cards-resources cards-upgrades))
