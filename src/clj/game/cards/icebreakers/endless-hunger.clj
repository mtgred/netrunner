(in-ns 'game.core)

(def card-definitions-icebreakers-endless-hunger
  {"Endless Hunger"
   {:abilities [{:label "Trash 1 installed card to break 1 \"End the run.\" subroutine"
                 :prompt "Select a card to trash for Endless Hunger"
                 :choices {:req #(and (= (:side %) "Runner") (:installed %))}
                 :msg (msg "trash " (:title target)
                           " and break 1 \"[Subroutine] End the run.\" subroutine")
                 :effect (effect (trash target {:unpreventable true}))}]}})
