App.gameSocket = null
$ ->
  addObject = (object) ->
    console.log object
    if object.currentPlayer?
      console.log "Adding current player"
      App.controllers.player.set "currentPlayer", App.Player.create(object.currentPlayer)
    else
      console.log "Adding player"
      App.controllers.player.players.pushObject(App.Player.create object.player)

  
  processMessage = (message) ->
    if message.add?
      addObject object for object in message.add
    if message.destroy?
      destroyObject object for object in message.destroy
  
  $("#login-form").submit ->
    unless App.gameSocket
      App.gameSocket = new WebSocket("ws://localhost:12345/game?#{$("#name").val()}")
      App.gameSocket.onopen = (event) ->
        console.log "Opened!"
        $("#login").hide()
        $("#update-health").show()

      App.gameSocket.onmessage = (event) ->
        data = JSON.parse(event.data)
        processMessage data
        
    false

  $("#new-health").change ->
    App.gameSocket.send $(this).val()