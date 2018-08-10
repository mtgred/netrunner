(in-ns 'game.cards.assets)

(def card-definition-amani-senai
  {"Amani Senai"
   (letfn [(get-last-stolen-pts [state]
             (advancement-cost state :corp (last (get-in @state [:runner :scored]))))
           (get-last-scored-pts [state]
             (advancement-cost state :corp (last (get-in @state [:corp :scored]))))
           (senai-ability [trace-base-func]
             {:interactive (req true)
              :optional {:prompt "Trace with Amani Senai?"
                         :player :corp
                         :yes-ability {:trace {:base (req (trace-base-func state))
                                               :successful
                                               {:choices {:req #(and (installed? %)
                                                                     (card-is? % :side :runner))}
                                                :label "add an installed card to the Grip"
                                                :msg (msg "add " (:title target) " to the Runner's Grip")
                                                :effect (effect (move :runner target :hand true))}}}}})]
    {:events {:agenda-scored (senai-ability get-last-scored-pts)
              :agenda-stolen (senai-ability get-last-stolen-pts)}})})
