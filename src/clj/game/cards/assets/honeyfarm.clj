(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-honeyfarm
  {"Honeyfarm"
   {:flags {:rd-reveal (req true)}
    :access {:msg "force the Runner to lose 1 [Credits]"
             :effect (effect (lose :runner :credit 1))}}})