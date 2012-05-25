App.gameSocket = null
  
$ ->
  addObject = (object) ->
    if object.body?
      if App.playerController.currentPlayer
        App.playerController.players.push(App.models.Player.create object.body)
      else
        App.playerController.set "currentPlayer", App.models.Player.create(object.body)
  
  processMessage = (message) ->
    if message.add?
      addObject object for object in message.add
    if message.destroy?
      destroyObject object for object in message.destroy
  
  $("#connector").click ->
    unless App.gameSocket
      gameSocket = new WebSocket("ws://localhost:12345/game?#{$(@).val()}")
      gameSocket.onopen = (event) ->
        console.log "Opened!"

      gameSocket.onmessage = (event) ->
        console.log event.data
        data = JSON.parse(event.data)
        processMessage data

  $("#new-health").change ->
    gameSocket.send $(this).val()