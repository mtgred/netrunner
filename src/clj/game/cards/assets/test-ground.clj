(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-test-ground
  {"Test Ground"
   {:implementation "Derez is manual"
    :advanceable :always
    :abilities [{:label "Derez 1 card for each advancement token"
                 :msg (msg "derez " (:advance-counter card)) :effect (effect (trash card))}]}})
