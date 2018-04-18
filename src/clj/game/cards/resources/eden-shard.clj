(in-ns 'game.core)

(def card-definitions-resources-eden-shard
  {"Eden Shard"
   (shard-constructor :rd "force the Corp to draw 2 cards" (req (draw state :corp 2)))})
