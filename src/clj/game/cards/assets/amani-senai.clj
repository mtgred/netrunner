(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-amani-senai
  {"Amani Senai"
   (let [get-last-stolen-pts (fn [state] (advancement-cost state :corp (last (get-in @state [:runner :scored]))))
         get-last-scored-pts (fn [state] (advancement-cost state :corp (last (get-in @state [:corp :scored]))))
         senai-ability (fn [trace-base-func]
                         {:interactive (req true)
                          :optional {:prompt "Trace with Amani Senai?" :player :corp
                                     :yes-ability {:trace {:base (req (trace-base-func state))
                                                           :choices {:req #(and (installed? %)
                                                                                (card-is? % :side :runner))}
                                                           :msg "add an installed Runner card to the grip"
                                                           :effect (effect (move :runner target :hand true))}}}})]
     {:events {:agenda-scored (senai-ability get-last-scored-pts)
               :agenda-stolen (senai-ability get-last-stolen-pts)}})})
