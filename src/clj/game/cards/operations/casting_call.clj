(in-ns 'game.cards.operations)

(def card-definition-casting-call
  {"Casting Call"
   {:choices {:req #(and (is-type? % "Agenda")
                         (in-hand? %))}
    :async true
    :effect (req (let [agenda target]
                   (continue-ability
                     state side {:prompt (str "Choose a server to install " (:title agenda))
                                 :choices (installable-servers state agenda)
                                 :effect (req (corp-install state side agenda target {:install-state :face-up})
                                              ; find where the agenda ended up and host on it
                                              (let [agenda (some #(when (= (:cid %) (:cid agenda)) %)
                                                                 (all-installed state :corp))]
                                                ; the operation ends up in :discard when it is played; to host it,
                                                ; we need (host) to look for it in discard.
                                                (host state side agenda (assoc card :zone [:discard]
                                                                                    :seen true :installed true))
                                                (system-msg state side (str "hosts Casting Call on " (:title agenda)))))}
                     card nil)))
    :events {:access {:req (req (= (:cid target) (:cid (:host card))))
                      :async true
                      :effect (effect (gain-tags :runner eid 2)) :msg "give the Runner 2 tags"}}}})
