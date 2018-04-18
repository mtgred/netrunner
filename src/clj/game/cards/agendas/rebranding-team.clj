(in-ns 'game.core)

(def card-definitions-agendas-rebranding-team
  {"Rebranding Team"
   (letfn [(get-assets [state corp]
             (filter #(is-type? % "Asset") (concat (all-installed state :corp)
                                                   (:deck corp)
                                                   (:hand corp)
                                                   (:discard corp))))
           (add-ad [state side c]
             (update! state side (assoc-in c [:persistent :subtype] "Advertisement")))]
     {:interactive (req true)
      :msg "make all assets gain Advertisement"
      :effect (req (doseq [c (get-assets state corp)] (add-ad state side c)))
      :swapped {:msg "make all assets gain Advertisement"
                :effect (req (doseq [c (get-assets state corp)] (add-ad state side c)))}
      :leave-play (req (doseq [c (get-assets state corp)]
                         (update! state side (assoc-in c [:persistent :subtype]
                                                      (->> (split (or (-> c :persistent :subtype) "") #" - ")
                                                           (drop 1) ;so that all actual ads remain ads if agenda leaves play
                                                           (join " - "))))))})})
