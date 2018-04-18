(in-ns 'game.core)

(def card-definitions-operations-archived-memories
  {"Archived Memories"
   {:effect (req (let [cid (:cid card)]
                   (resolve-ability state side
                     {:prompt "Select a card from Archives to add to HQ"
                      :show-discard true
                      :choices {:req #(and (not= (:cid %) cid)
                                           (= (:side %) "Corp")
                                           (= (:zone %) [:discard]))}
                      :effect (final-effect (move target :hand)
                                            (system-msg (str "adds " (if (:seen target) (:title target) "an unseen card") " to HQ")))}
                    card nil)))}})
