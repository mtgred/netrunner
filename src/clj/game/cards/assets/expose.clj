(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-expose
  {"Exposé"
   {:advanceable :always
    :abilities [{:label "Remove 1 bad publicity for each advancement token on Exposé"
                 :msg (msg "remove " (:advance-counter card) " bad publicity")
                 :effect (effect (trash card) (lose :bad-publicity (:advance-counter card)))}]}})