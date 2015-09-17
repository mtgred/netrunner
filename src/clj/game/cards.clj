(in-ns 'game.core)

(defn ice? [card] 
  (= (:type card) "ICE"))

(defn rezzed? [card] 
  (:rezzed card))

(def trash-program {:prompt "Choose a program to trash" :label "Trash a program"
                    :msg (msg "trash " (:title target))
                    :choices {:req #(and (:installed %) (= (:type %) "Program"))}
                    :effect (effect (trash target {:cause :subroutine}))})

(def trash-hardware {:prompt "Choose a piece of hardware to trash" :label "Trash a piece of hardware"
                     :msg (msg "trash " (:title target))
                     :choices {:req #(and (:installed %) (= (:type %) "Hardware"))}
                     :effect (effect (trash target {:cause :subroutine}))})

(def trash-installed {:prompt "Choose an installed card to trash" :player :runner
                      :label "Force the Runner to trash an installed card"
                      :msg (msg "force the Runner to trash " (:title target))
                      :choices {:req #(and (:installed %) (= (:side %) "Runner"))}
                      :effect (effect (trash target {:cause :subroutine}))})

(load "cards-agendas")
(load "cards-assets")
(load "cards-events")
(load "cards-hardware")
(load "cards-ice")
(load "cards-icebreakers")
(load "cards-identities")
(load "cards-operations")
(load "cards-programs")
(load "cards-resources")
(load "cards-upgrades")

(def cards (merge cards-agendas cards-assets cards-events cards-hardware cards-ice cards-icebreakers cards-identities
                  cards-operations cards-programs cards-resources cards-upgrades))
