(in-ns 'game.cards.resources)

(def card-definition-eden-shard
  {"Eden Shard"
   (shard-constructor :rd "force the Corp to draw 2 cards" (req (draw state :corp 2)))})
