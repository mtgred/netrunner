(in-ns 'game.core)

(def card-definitions-resources-utopia-shard
  {"Utopia Shard"
   (shard-constructor :hq "force the Corp to discard 2 cards from HQ at random"
                      (effect (trash-cards :corp (take 2 (shuffle (:hand corp))))))})
