(in-ns 'game.cards.icebreakers)

(def card-definition-brahman
  {"Brahman"
   (auto-icebreaker ["All"]
                    {:implementation "Adding non-virus program to top of Stack is manual"
                     :abilities [(break-sub 1 2 "ICE")
                                 (strength-pump 2 1)]})})
