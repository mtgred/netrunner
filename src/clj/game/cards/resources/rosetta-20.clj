(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-rosetta-20
  {"Rosetta 2.0"
   {:abilities [{:req (req (and (not (install-locked? state side))
                                (some #(is-type? % "Program") (all-active-installed state :runner))))
                 :cost [:click 1]
                 :prompt "Choose an installed program to remove from the game"
                 :choices {:req #(and installed? (is-type? % "Program"))}
                 :effect (req (let [n (:cost target)
                                    t (:title target)]
                                (move state side target :rfg)
                                (gain state side :memory (:memoryunits target))
                                (resolve-ability state side
                                  {:prompt "Choose a non-virus program to install"
                                   :msg (req (if (not= target "No install")
                                               (str "remove " t " from the game and install " (:title target) ", lowering its cost by " n)
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
                                                  (runner-install state side target)))} card nil)))}]}
   "Rogue Trading"
   {:data {:counter {:credit 18}}
    :abilities [{:cost [:click 2]
                 :counter-cost [:credit 6]
                 :msg "gain 6 [Credits] and take 1 tag"
                 :effect (req (gain state :runner :credit 6)
                              (when (zero? (get-in card [:counter :credit] 0))
                                (trash state :runner card {:unpreventable true}))
                              (tag-runner state :runner eid 1))}]}})
