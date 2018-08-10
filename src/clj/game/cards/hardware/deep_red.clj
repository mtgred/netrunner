(in-ns 'game.cards.hardware)

(def card-definition-deep-red
  {"Deep Red"
   {:implementation "MU use restriction not enforced"
    :in-play [:memory 3]
    :events {:runner-install
             {:optional
              {:req (req (has-subtype? target "Caïssa"))
               :prompt "Use Deep Red?" :priority 1
               :yes-ability {:async true
                             :effect (req (let [cid (:cid target)]
                                            (continue-ability state side
                                              {:async true
                                               :prompt "Choose the just-installed Caïssa to have Deep Red trigger its [Click] ability"
                                               :choices {:req #(= cid (:cid %))}
                                               :msg (msg "trigger the [Click] ability of " (:title target)
                                                         " without spending [Click]")
                                               :effect (req (gain state :runner :click 1)
                                                            (play-ability state side {:card target :ability 0})
                                                            (effect-completed state side eid))}
                                             card nil)))}}}}}})
