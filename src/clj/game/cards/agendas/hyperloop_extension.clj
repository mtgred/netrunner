(in-ns 'game.cards.agendas)

(def card-definition-hyperloop-extension
  {"Hyperloop Extension"
   (let [he (req (gain-credits state :corp 3)
                 (system-msg state side (str "uses Hyperloop Extension to gain 3 [Credits]")))]
     {:effect he
      :stolen {:effect he}})})
