(in-ns 'game.core)

(def card-definitions-ice-susanoo-no-mikoto
  {"Susanoo-no-Mikoto"
   {:subroutines [{:req (req (not= (:server run) [:discard]))
                   :msg "make the Runner continue the run on Archives"
                   :effect (req (swap! state update-in [:run]
                                       #(assoc % :position (count (get-in corp [:servers :archives :ices]))
                                                 :server [:archives])))}]}})
