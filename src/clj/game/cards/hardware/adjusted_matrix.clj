(in-ns 'game.cards.hardware)

(def card-definition-adjusted-matrix
  {"Adjusted Matrix"
   {:implementation "Click Adjusted Matrix to use ability."
    :req (req (not-empty (filter #(has-subtype? % "Icebreaker") (all-active-installed state :runner))))
    :prompt "Choose Icebreaker on which to install Adjusted Matrix"
    :choices {:req #(and (= (:side %) "Runner") (has-subtype? % "Icebreaker") (installed? %))}
    :msg (msg "host it on " (card-str state target))
    :effect (effect (update! (assoc target :subtype (combine-subtypes false (-> target :subtype) "AI")))
                    (host (get-card state target) (get-card state card)))
    :abilities [{:cost [:click 1]
                 :req (req run)
                 :msg "break ice subroutine"}]
    :events {:pre-card-moved {:req (req (= (:cid target) (:cid card)))
                              :effect (effect (update! (assoc (-> card :host) :subtype (-> card :host :subtype (remove-subtypes-once ["AI"])))))}}}})
