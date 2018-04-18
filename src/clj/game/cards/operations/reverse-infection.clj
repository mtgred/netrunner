(in-ns 'game.core)

(def card-definitions-operations-reverse-infection
  {"Reverse Infection"
   {:prompt "Choose One:"
    :choices ["Purge virus counters."
              "Gain 2 [Credits]"]
    :effect (req (if (= target "Gain 2 [Credits]")
                   (do (gain state side :credit 2)
                       (system-msg state side "uses Reverse Infection to gain 2 [Credits]"))
                   (let [pre-purge-virus (number-of-virus-counters state)]
                     (purge state side)
                     (let [post-purge-virus (number-of-virus-counters state)
                           num-virus-purged (- pre-purge-virus post-purge-virus)
                           num-to-trash (quot num-virus-purged 3)]
                       (mill state :corp :runner num-to-trash)
                       (system-msg state side (str "uses Reverse Infection to purge "
                                                   num-virus-purged (pluralize " virus counter" num-virus-purged)
                                                   " and trash "
                                                   num-to-trash (pluralize " card" num-to-trash)
                                                   " from the top of the stack"))))))}})
