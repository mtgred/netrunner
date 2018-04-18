(in-ns 'game.core)

(def card-definitions-operations-the-all-seeing-i
  {"The All-Seeing I"
   (let [trash-all-resources {:player :runner
                              :effect (req (trash-cards state side (get-in runner  [:rig :resource])))
                              :msg (msg "trash all resources")}]
       {:req (req tagged)
        :delayed-completion true
        :effect (effect
                 (continue-ability
                   (if-not (zero? (:bad-publicity corp)) ;; If corp's bad-pub is 0
                     {:optional {:player :runner
                                 :prompt "Remove 1 bad publicity from the corp to prevent all resources from being trashed?"
                                 :yes-ability {:effect (effect (lose :corp :bad-publicity 1))
                                               :player :corp
                                               :msg (msg "lose 1 bad publicity, preventing all resources from being trashed")}
                                 :no-ability trash-all-resources}}
                    trash-all-resources)
                  card targets))})})
