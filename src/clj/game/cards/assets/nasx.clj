(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-nasx
  {"NASX"
   (let [ability {:msg "gain 1 [Credits]"
                  :label "Gain 1 [Credits] (start of turn)"
                  :once :per-turn
                  :effect (effect (gain :credit 1))}]
     {:implementation "Manual - click NASX to add power counters"
      :derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins ability}
      :abilities [ability
                  {:label "Place 1 power counter" :cost [:credit 1]
                   :effect (effect (add-counter card :power 1)
                                   (system-msg (str "places 1 power counter on NASX")))}
                  {:label "Place 2 power counters" :cost [:credit 2]
                   :effect (effect (add-counter card :power 2)
                                   (system-msg (str "places 2 power counters on NASX")))}
                  {:label "[Trash] and gain 2 [Credits] for each power counter" :cost [:click 1]
                   :msg (msg "gain " (* 2 (get-in card [:counter :power])) " [Credits]")
                   :effect (effect (gain :credit (* 2 (get-in card [:counter :power])))
                                   (trash card))}]})})
