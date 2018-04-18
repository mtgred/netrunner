(in-ns 'game.core)

(def card-definitions-ice-mother-goddess
  {"Mother Goddess"
   (let [ab (effect (update! (let [subtype (->> (mapcat :ices (flatten (seq (:servers corp))))
                                                (filter #(and (rezzed? %) (not= (:cid card) (:cid %))))
                                                (mapcat #(split (:subtype %) #" - "))
                                                (cons "Mythic")
                                                distinct
                                                (join " - "))]
                               (assoc card :subtype-target (remove-subtypes subtype "Mythic")
                                           :subtype subtype))))
         mg {:req (req (ice? target))
             :effect ab}]
     {:effect ab
      :subroutines [end-the-run]
      :events {:rez mg :card-moved mg :derez mg :ice-subtype-changed mg}})})
