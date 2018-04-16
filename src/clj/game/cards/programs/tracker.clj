(in-ns 'game.core)

(declare can-host?)

(def card-programs-tracker
  {"Tracker"
   (let [ability {:prompt "Choose a server for Tracker" :choices (req servers)
                  :msg (msg "target " target)
                  :req (req (not (:server-target card)))
                  :effect (effect (update! (assoc card :server-target target)))}]
     {:abilities [{:label "Make a run on targeted server" :cost [:click 1 :credit 2]
                   :req (req (some #(= (:server-target card) %) runnable-servers))
                   :msg (msg "make a run on " (:server-target card) ". Prevent the first subroutine that would resolve from resolving")
                   :effect (effect (run (:server-target card) nil card))}]
      :events {:runner-turn-begins ability
               :runner-turn-ends {:effect (effect (update! (dissoc card :server-target)))}}})})