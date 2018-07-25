(in-ns 'game.cards.ice)

(def card-definition-envelope
  {"Envelope"
   {:subroutines [(do-net-damage 1)
                  end-the-run]}})
