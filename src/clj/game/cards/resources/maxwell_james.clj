(in-ns 'game.cards.resources)

(def card-definition-maxwell-james
  {"Maxwell James"
   {:in-play [:link 1]
    :abilities [{:req (req (some #{:hq} (:successful-run runner-reg)))
                 :prompt "Choose a piece of ICE protecting a remote server"
                 :choices {:req #(and (ice? %) (rezzed? %) (is-remote? (second (:zone %))))}
                 :msg "derez a piece of ICE protecting a remote server"
                 :effect (effect (derez target)
                                 (trash card {:cause :ability-cost}))}]}})
