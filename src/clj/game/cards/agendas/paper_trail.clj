(in-ns 'game.cards.agendas)

(def card-definition-paper-trail
  {"Paper Trail"
   {:trace {:base 6
            :successful {:msg "trash all connection and job resources"
                         :effect (req (doseq [resource (filter #(or (has-subtype? % "Job")
                                                                    (has-subtype? % "Connection"))
                                                               (all-active-installed state :runner))]
                                        (trash state side resource)))}}}})
