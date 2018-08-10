(in-ns 'game.cards.ice)

(def card-definition-jua
  {"Jua"
   {:implementation "Encounter effect is manual"
    :abilities [{:msg "prevent the Runner from installing cards for the rest of the turn"
                 :effect (effect (register-turn-flag! card :runner-lock-install (constantly true)))}]
    :subroutines [{:label "Choose 2 installed Runner cards, if able. The Runner must add 1 of those to the top of the Stack."
                   :req (req (>= (count (all-installed state :runner)) 2))
                   :async true
                   :prompt "Select 2 installed Runner cards"
                   :choices {:req #(and (= (:side %) "Runner")
                                        (installed? %))
                             :max 2
                             :all true}
                   :msg (msg "add either " (card-str state (first targets)) " or " (card-str state (second targets)) " to the Stack")
                   :effect (req (when (= (count targets) 2)
                                     (show-wait-prompt state :corp "Runner to decide which card to move")
                                     (continue-ability
                                       state :runner
                                        {:player :runner
                                         :priority 1
                                         :prompt "Select a card to move to the Stack"
                                         :choices targets ;{:req (fn [x] (some #(= % x) targets))} - Alternative version
                                         :effect (req (let [c target]
                                                        (clear-wait-prompt state :corp)
                                                        (move state :runner c :deck {:front true})
                                                        (system-msg state :runner (str "selected " (card-str state c) " to move to the Stack"))))}
                                         card nil)))}]}})
