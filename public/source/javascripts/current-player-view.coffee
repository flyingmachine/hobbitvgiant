App.views.currentPlayer = Ember.View.create
  templateName: 'current-player'
  playerBinding: 'App.controllers.player.currentPlayer'

$ ->
  App.views.currentPlayer.appendTo('#player')