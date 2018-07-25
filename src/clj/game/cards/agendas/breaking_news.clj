(in-ns 'game.cards.agendas)

(def card-definition-breaking-news
  {"Breaking News"
   {:async true
    :effect (effect (gain-tags :corp eid 2))
    :silent (req true)
    :msg "give the Runner 2 tags"
    :end-turn {:effect (effect (lose :runner :tag 2))
               :msg "make the Runner lose 2 tags"}}})
