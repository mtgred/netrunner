(in-ns 'game.core)

(def card-definitions-operations-recruiting-trip
  {"Recruiting Trip"
   (let [rthelp (fn rt [total left selected]
                  (if (pos? left)
                    {:prompt (str "Choose a Sysop (" (inc (- total left)) "/" total ")")
                     :choices (req (cancellable (filter #(and (has-subtype? % "Sysop")
                                                              (not (some #{(:title %)} selected))) (:deck corp)) :sorted))
                     :msg (msg "put " (:title target) " into HQ")
                     :delayed-completion true
                     :effect (req (move state side target :hand)
                                  (continue-ability
                                    state side
                                    (rt total (dec left) (cons (:title target) selected))
                                    card nil))}
                    {:effect (effect (shuffle! :corp :deck))
                     :msg (msg "shuffle R&D")}))]
   {:prompt "How many Sysops?"
    :delayed-completion true
    :choices :credit
    :msg (msg "search for " target " Sysops")
    :effect (effect (continue-ability (rthelp target target []) card nil))})})
