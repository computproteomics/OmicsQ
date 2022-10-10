var exprmatr = [];
function handleFiles(files) {
      // Check for the various File API support.
      if (window.FileReader) {
          // FileReader are supported.
      } else {
          alert('FileReader are not supported in this browser.');
      }
        getAsText(files[0]);
        
    }


function getAsText(fileToRead) {
      var reader = new FileReader();
      // Read file into memory as UTF-8      
      reader.readAsText(fileToRead);
      // Handle errors load
      reader.onload = loadHandler;
      reader.onerror = errorHandler;
    }


  function loadHandler(event) {
      var csv = event.target.result;
      processData(csv);
    }

    function processData(csv) {
        var allTextLines = csv.split(/\r\n|\n/);
        var lines = [];
        for (var i=0; i<allTextLines.length; i++) {
            var data = allTextLines[i].split(',');
                for (var j=0; j<data.length; j++) {
                var str = data[j].replace(/['"]+/g, '');
                    lines.push(str);
                }
        }
	exprmatr = lines;
}

function errorHandler(evt) {
      if(evt.target.error.name == "NotReadableError") {
          alert("Canno't read file !");
      }
    }

function send_message()
{
alert(1);
var message = {"numrep":3, "numcond":4, "exprmatr":[1,1,12,4,2,4,2,4,25,23]};
if (exprmatr.length > 0) {
  message.exprmatr = exprmatr;
}
	var extWindow = window.open("http://computproteomics.bmb.sdu.dk/Apps/TestAPI/TestAPI/TestAPI/", "_blank");
  setTimeout(checkIfOpen, 15000);
  
  function checkIfOpen() {
      //if(!trackedWindows[windowName])
      
//      console.log(JSON.parse(JSON.stringify(message)));
        extWindow.postMessage(JSON.stringify(message),'*');
        console.log("message sent");
    }
  

}
