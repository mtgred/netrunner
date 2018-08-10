(in-ns 'game.cards.hardware)

(def card-definition-hippo
  {"Hippo"
   {:implementation "Subroutine and first encounter requirements not enforced"
    :abilities [{:label "Remove Hippo from the game: trash outermost piece of ICE if all subroutines were broken"
                 :req (req (and run
                                (pos? (count run-ices))))
                 :async true
                 :effect (req (let [ice (last run-ices)]
                                (system-msg
                                  state :runner
                                  (str "removes Hippo from the game to trash " (card-str state ice)))
                                (move state :runner card :rfg)
                                (trash state :runner eid ice nil)))}]}})
