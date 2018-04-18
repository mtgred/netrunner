(in-ns 'game.core)

(def card-definitions-agendas-armored-servers
  {"Armored Servers"
   {:implementation "Runner must trash cards manually when required"
    :effect (effect (add-counter card :agenda 1))
    :silent (req true)
    :abilities [{:counter-cost [:agenda 1]
                 :req (req (:run @state))
                 :msg "make the Runner trash a card from their grip to jack out or break subroutines for the remainder of the run"}]}})
