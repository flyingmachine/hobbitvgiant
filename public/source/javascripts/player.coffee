gameSocket = undefined

App.models.Player = Ember.Object.extend
  currentHealth: null
  maxHealth: null
  name: null
  id: null
  
  healthPercentage: (->
    (@currentHealth / @maxHealth) * 100
  ).property("currentHealth", "maxHealth")