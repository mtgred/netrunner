(in-ns 'game.cards.ice)

(def card-definition-owl
  {"Owl"
   {:subroutines [{:choices {:req #(and (installed? %)
                                        (is-type? % "Program"))}
                   :label "Add installed program to the top of the Runner's Stack"
                   :msg "add an installed program to the top of the Runner's Stack"
                   :effect (effect (move :runner target :deck {:front true})
                                   (system-msg (str "adds " (:title target) " to the top of the Runner's Stack")))}]}})
