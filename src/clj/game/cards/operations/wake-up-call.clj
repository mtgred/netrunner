(in-ns 'game.core)

(def card-definitions-operations-wake-up-call
  {"Wake Up Call"
   {:req (req (last-turn? state :runner :trashed-card))
    :prompt "Select a piece of hardware or non-virtual resource"
    :choices {:req #(or (hardware? %)
                        (and (resource? %) (not (has-subtype? % "Virtual"))))}
    :delayed-completion true
    :effect (req (let [chosen target
                       wake card]
                   (show-wait-prompt state side "Runner to resolve Wake Up Call")
                   (continue-ability state :runner
                                     {:prompt (str "Trash " (:title chosen) " or suffer 4 meat damage?")
                                      :choices [(str "Trash " (:title chosen))
                                                "4 meat damage"]
                                      :delayed-completion true
                                      :effect (req (clear-wait-prompt state :corp)
                                                   (move state :corp (last (:discard corp)) :rfg)
                                                   (if (.startsWith target "Trash")
                                                     (do (system-msg state side (str "chooses to trash " (:title chosen)))
                                                         (trash state side eid chosen nil))
                                                     (do (system-msg state side "chooses to suffer meat damage")
                                                         (damage state side eid :meat 4 {:card wake
                                                                                         :unboostable true}))))}
                                     card nil)))}})
