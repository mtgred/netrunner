class ManabaseRouter extends Backbone.Router
  routes:
    "": -> $('.carousel').carousel(0)
    "play": -> $('.carousel').carousel(1)
    "deckbuilder": -> $('.carousel').carousel(2)
    "cards": -> $('.carousel').carousel(3)
    "news": -> $('.carousel').carousel(4)

$ ->
  mr = new ManabaseRouter()
  Backbone.history.start(pushState: true)

  $('.carousel-indicator li').click (e) ->
    mr.navigate $(e.delegateTarget).data("url")
