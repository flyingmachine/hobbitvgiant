gameSocket = undefined

class Hobbit
  currentHealth: "1"
  maxHealth: "1"
  name: "blah"
  healthPercentage: () =>
    parseFloat(this.currentHealth) / parseFloat(this.maxHealth)

hobbit = new Hobbit

$ ->
  $("#connector").click ->
    unless gameSocket
      gameSocket = new WebSocket("ws://localhost:12345/game")
      gameSocket.onopen = (event) ->
        console.log "Opened!"

      gameSocket.onmessage = (event) ->
        console.log event.data
        hobbitData = JSON.parse(event.data)
        hobbit.currentHealth = hobbitData.currentHealth
        hobbit.maxHealth = hobbitData.maxHealth
        $("#name").text hobbit.name
        $("#id").text hobbit.id
        $("#health").text parseFloat(hobbit.currentHealth) / parseFloat(hobbit.maxHealth)

  $("#new-health").change ->
    gameSocket.send $(this).val()

window.hobbit = hobbit