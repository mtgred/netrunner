(in-ns 'game.core)

(def card-hardware-deep-red
  {"Deep Red"
   {:implementation "MU use restriction not enforced"
    :in-play [:memory 3]
    :events {:runner-install
             {:optional
              {:delayed-completion true
               :req (req (has-subtype? target "Caïssa"))
               :prompt "Use Deep Red?" :priority 1
               :yes-ability {:delayed-completion true
                             :effect (req (let [cid (:cid target)]
                                            (continue-ability state side
                                              {:delayed-completion true
                                               :prompt "Choose the just-installed Caïssa to have Deep Red trigger its [Click] ability"
                                               :choices {:req #(= cid (:cid %))}
                                               :msg (msg "trigger the [Click] ability of " (:title target)
                                                         " without spending [Click]")
                                               :effect (req (gain state :runner :click 1)
                                                            (play-ability state side {:card target :ability 0})
                                                            (effect-completed state side eid))}
                                             card nil)))}
               :no-ability {:effect (req (effect-completed state side eid))}}}}}})
