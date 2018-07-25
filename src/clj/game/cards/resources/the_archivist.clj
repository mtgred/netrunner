(in-ns 'game.cards.resources)

(def card-definition-the-archivist
  {"The Archivist"
   {:in-play [:link 1]
    :events {:agenda-scored {:req (req (or (has-subtype? target "Initiative")
                                           (has-subtype? target "Security")))
                             :interactive (req true)
                             :async true
                             :msg "force the Corp to initiate a trace"
                             :label "Trace 1 - If unsuccessful, take 1 bad publicity"
                             :trace {:base 1
                                     :unsuccessful
                                     {:effect (effect (gain-bad-publicity :corp 1)
                                                      (system-msg :corp (str "takes 1 bad publicity")))}}}}}})
