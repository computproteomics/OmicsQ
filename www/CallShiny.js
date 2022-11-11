// listener to check whether message was sent successfully
window.addEventListener("message", displayMessage, false);
 function displayMessage(evt) { 
 console.log(evt.data)
 Shiny.setInputValue("app_log", evt.data);
}


// open app and send message with data
shinyjs.send_message = function(params)
{
  
  //params = shinyjs.getParams(params)
  console.log(params.url);
  console.log(params.dat);

  var tool = params.tool;
  var message = params.dat;
  var response = "";

  var extWindow = window.open(params.url, "_blank");
  // Try 5 times 
  var timer = 1; 
  while(timer < 6 && response.localeCompare(tool + " data received") == 0){
    setTimeout(sendData, 2000);
    window.addEventListener("message", updateResponse, false);
    timer = timer + 1; 
    console.log(response !== tool + " data received")
    function updateResponse(evt) {
      response = evt.data;
    };
  };

  function sendData() {
    extWindow.postMessage(JSON.stringify(message),'*');
    console.log("message sent");
  }
};   
    
    
         
