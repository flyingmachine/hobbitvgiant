App.views.playersView = Ember.CollectionView.create
  classNames: ['players']
  contentBinding: 'App.controllers.player.players'
  itemViewClass: Ember.View.extend
    classNames: ['player']
    templateName: 'player'

$ ->
  App.views.playersView.appendTo("#players")