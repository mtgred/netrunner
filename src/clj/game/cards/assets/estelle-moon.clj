(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-estelle-moon
  {"Estelle Moon"
   {:events {:corp-install {:req (req (and (#{"Asset" "Agenda" "Upgrade"} (:type target))
                                           (is-remote? (second (:zone target)))))
                            :effect (effect (add-counter card :power 1)
                                            (system-msg (str "places 1 power counter on Estelle Moon")))}}
    :abilities [{:label "Draw 1 card and gain 2 [Credits] for each power counter"
                 :effect (req (let [n (get-in card [:counter :power] 0)]
                                (draw state side n)
                                (gain state side :credit (* 2 n))
                                (system-msg state side (str "uses Estelle Moon to draw " n " cards and gain " (* 2 n) " [Credits]"))
                                (trash state side card {:cause :ability-cost})))}]}})
