(in-ns 'game.cards.resources)

(def card-definition-rosetta-2-0
  {"Rosetta 2.0"
   {:abilities [{:req (req (and (not (install-locked? state side))
                                (some #(is-type? % "Program") (all-active-installed state :runner))))
                 :cost [:click 1]
                 :prompt "Choose an installed program to remove from the game"
                 :choices {:req #(and (installed? %) (is-type? % "Program"))}
                 :effect (req (let [n (:cost target)
                                    t (:title target)]
                                (move state side target :rfg)
                                (resolve-ability state side
                                  {:prompt "Choose a non-virus program to install"
                                   :msg (req (if (not= target "No install")
                                               (str "remove " t
                                                    " from the game and install " (:title target)
                                                    ", lowering its cost by " n)
                                               (str "shuffle their Stack")))
                                   :priority true
                                   :choices (req (cancellable
                                                   (conj (vec (sort-by :title (filter #(and (is-type? % "Program")
                                                                                            (not (has-subtype? % "Virus")))
                                                                                      (:deck runner))))
                                                         "No install")))
                                   :effect (req (trigger-event state side :searched-stack nil)
                                                (shuffle! state side :deck)
                                                (when (not= target "No install")
                                                  (install-cost-bonus state side [:credit (- n)])
                                                  (runner-install state side target)))} card nil)))}]}})
