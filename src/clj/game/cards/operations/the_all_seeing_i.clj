(in-ns 'game.cards.operations)

(def card-definition-the-all-seeing-i
  {"The All-Seeing I"
   (let [trash-all-resources {:player :runner
                              :effect (req (trash-cards state side (filter #(is-type? % "Resource") (all-active-installed state :runner))))
                              :msg (msg "trash all resources")}]
       {:req (req tagged)
        :async true
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
