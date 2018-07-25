(in-ns 'game.cards.operations)

(def card-definition-archived-memories
  {"Archived Memories"
   {:async true
    :effect (req (let [cid (:cid card)]
                   (continue-ability
                     state side
                     {:prompt "Select a card from Archives to add to HQ"
                      :show-discard true
                      :choices {:req #(and (not= (:cid %) cid)
                                           (= (:side %) "Corp")
                                           (= (:zone %) [:discard]))}
                      :effect (effect (move target :hand)
                                      (system-msg (str "adds " (if (:seen target) (:title target) "an unseen card") " to HQ")))}
                     card nil)))}})
