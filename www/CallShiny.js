// listener to check whether message was sent successfully
window.addEventListener("message", displayMessage, false);
 function displayMessage(evt) { 
 console.log(evt.data)
 Shiny.setInputValue("app_log", evt.data);
};

// Manipulate buttons for example file
shinyjs.run_button = function(params)
{
  Shiny.setInputValue(params.button, params.number);
};

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
  window[tool] = extWindow;
  let ninterv = setInterval(sendData, 2000);
  window.addEventListener("message", updateResponse, false);

  function updateResponse(evt) {
    response = evt.data;
    if (evt.data.expr_matrix == null) {
      if (response.localeCompare(tool + " data received") != 0) {
        clearInterval(ninterv);
      }
    } else {
      //console.log(evt.data); 
      console.log("PolysTest: got data back");
      Shiny.setInputValue("polystest_results", evt.data);
    }
  }
  setTimeout(function() {
    clearInterval(ninterv);
  }, 20000)



  function sendData() {
    extWindow.postMessage(JSON.stringify(message), '*');
    console.log(message);
    console.log("message sent");
  }
};


// check ext window and retrieve results
shinyjs.retrieve_results = function(params)
{
  
  //params = shinyjs.getParams(params)
  console.log(params.url);
  console.log(params.dat);

  var tool = params.tool;
  var message = params.dat;

  var extWindow = window[tool];
  console.log(extWindow);
  extWindow.postMessage(JSON.stringify(message), '*');
  setTimeout(sendData, 5000);
  setTimeout(sendData, 10000);

  function sendData() {
    extWindow.postMessage(JSON.stringify(message), '*');
    console.log(message)
    console.log("message sent");
  };
};   

