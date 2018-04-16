(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-seidr-laboratories-destiny-defined
  {"Seidr Laboratories: Destiny Defined"
   {:implementation "Manually triggered"
    :abilities [{:req (req (:run @state))
                 :once :per-turn
                 :prompt "Select a card to add to the top of R&D"
                 :show-discard true
                 :choices {:req #(and (= (:side %) "Corp") (in-discard? %))}
                 :effect (effect (move target :deck {:front true}))
                 :msg (msg "add " (if (:seen target) (:title target) "a card") " to the top of R&D")}]}})
