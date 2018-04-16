(in-ns 'game.core)

(declare can-host?)

(def card-programs-self-modifying-code
  {"Self-modifying Code"
   {:abilities [{:req (req (not (install-locked? state side)))
                 :effect (req (when-completed (trash state side card {:cause :ability-cost})
                                              (continue-ability state side
                                                {:prompt "Choose a program to install"
                                                 :msg (req (if (not= target "No install")
                                                             (str "install " (:title target))
                                                             (str "shuffle their Stack")))
                                                 :priority true
                                                 :choices (req (cancellable
                                                                 (conj (vec (sort-by :title (filter #(is-type? % "Program")
                                                                                                    (:deck runner))))
                                                                       "No install")))
                                                 :cost [:credit 2]
                                                 :effect (req (trigger-event state side :searched-stack nil)
                                                              (shuffle! state side :deck)
                                                              (when (not= target "No install")
                                                                (runner-install state side target)))} card nil)))}]}})