(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-paper-trail
  {"Paper Trail"
   {:trace {:base 6
            :msg "trash all connection and job resources"
            :effect (req (doseq [resource (filter #(or (has-subtype? % "Job")
                                                       (has-subtype? % "Connection"))
                                                  (all-active-installed state :runner))]
                                   (trash state side resource)))}}})