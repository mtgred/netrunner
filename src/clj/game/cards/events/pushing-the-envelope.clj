(in-ns 'game.core)

(declare run-event)

(def card-events-pushing-the-envelope
  {"Pushing the Envelope"
   (letfn [(hsize [s] (count (get-in s [:runner :hand])))]
   {:msg (msg (if (<= (hsize @state) 2)
           "make a run, and adds +2 strength to installed icebreakers"
           "make a run"))
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :delayed-completion true
    :effect (req (when (<= (hsize @state) 2)
                   (let [breakers (filter #(has-subtype? % "Icebreaker") (all-active-installed state :runner))]
                     (doseq [t breakers] (pump state side t 2 :all-run))))
                 (game.core/run state side (make-eid state) target))})})
