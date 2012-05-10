var gameSocket;
var hobbit;
$(function(){
  $("#connector").click(function() {
    if (!gameSocket) {
      gameSocket = new WebSocket("ws://localhost:12345/game");
      gameSocket.onopen = function (event) {
        console.log("Opened!");
      }

      gameSocket.onmessage = function (event) {
        console.log(event.data);
        hobbit = JSON.parse(event.data);
        
        $("#name").text(hobbit.name);
        $("#id").text(hobbit.id);
        $("#health").text(parseFloat(hobbit.currentHealth) / parseFloat(hobbit.maxHealth));
      }
    }
  })

  $("#new-health").change(function() {
    gameSocket.send($(this).val())
  })
})
