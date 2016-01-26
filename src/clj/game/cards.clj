(in-ns 'game.core)

(def trash-program {:prompt "Choose a program to trash"
                    :label "Trash a program"
                    :msg (msg "trash " (:title target))
                    :choices {:req #(and (installed? %)
                                         (is-type? % "Program"))}
                    :effect (effect (trash target {:cause :subroutine}))})

(def trash-hardware {:prompt "Choose a piece of hardware to trash"
                     :label "Trash a piece of hardware"
                     :msg (msg "trash " (:title target))
                     :choices {:req #(and (installed? %)
                                          (is-type? % "Hardware"))}
                     :effect (effect (trash target {:cause :subroutine}))})

(def trash-installed {:prompt "Choose an installed card to trash"
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

;; Load all card definitions into the current namespace.
(load "cards-agendas")
(load "cards-assets")
(load "cards-events")
(load "cards-hardware")
(load "cards-ice")
(load "cards-icebreakers")
(load "cards-identities")
(load "cards-operations")
(load "cards-programs")
(load "cards-resources")
(load "cards-upgrades")

(def cards (merge cards-agendas cards-assets cards-events cards-hardware cards-ice cards-icebreakers cards-identities
                  cards-operations cards-programs cards-resources cards-upgrades))
