(in-ns 'game.core)

(def card-definitions-assets-public-support
  {"Public Support"
   {:effect (effect (add-counter card :power 3))
    :derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins
             {:effect (req (add-counter state side card :power -1)
                           (when (<= (get-in card [:counter :power]) 1)
                             (system-msg state :corp "uses Public Support to add it to their score area as an agenda worth 1 agenda point")
                             (as-agenda state :corp (dissoc card :counter) 1)))} }}})
