(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-muertos-gang-member
  {"Muertos Gang Member"
   {:effect (req (resolve-ability
                   state :corp
                   {:prompt "Select a card to derez"
                    :choices {:req #(and (= (:side %) "Corp")
                                         (not (is-type? % "Agenda"))
                                         (:rezzed %))}
                    :effect (req (derez state side target))}
                  card nil))
    :leave-play (req (resolve-ability
                       state :corp
                       {:prompt "Select a card to rez, ignoring the rez cost"
                        :choices {:req #(not (:rezzed %))}
                        :effect (req (rez state side target {:ignore-cost :rez-cost})
                                     (system-msg state side (str "rezzes " (:title target) " at no cost")))}
                      card nil))
    :abilities [{:msg "draw 1 card"
                 :effect (effect (trash card {:cause :ability-cost}) (draw))}]}})
