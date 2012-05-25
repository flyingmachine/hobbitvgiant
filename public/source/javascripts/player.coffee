gameSocket = undefined

class Player
  currentHealth: "1"
  maxHealth: "1"
  name: ""
  healthPercentage: () =>
    parseFloat(@currentHealth) / parseFloat(@maxHealth) * 100

player = new Player

$ ->
  $("#connector").click ->
    unless gameSocket
      gameSocket = new WebSocket("ws://localhost:12345/game?#{$(@).val()}")
      gameSocket.onopen = (event) ->
        console.log "Opened!"

      gameSocket.onmessage = (event) ->
        console.log 'Got message'
        console.log event.data
        playerData = JSON.parse(event.data)
        player.currentHealth = playerData.currentHealth
        player.maxHealth = playerData.maxHealth
        player.name = playerData.name
        $("#name").text player.name
        $("#health .text").text player.healthPercentage()

  $("#new-health").change ->
    gameSocket.send $(this).val()

window.App.models.player = player