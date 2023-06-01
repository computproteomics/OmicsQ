
         var exprmatr = {};
function handleFiles(files) {
      // Check for the various File API support.
      if (window.FileReader) {
          // FileReader are supported.
      } else {
          alert('FileReader are not supported in this browser.');
      }
        getAsText(files[0]);
        
    };


function getAsText(fileToRead) {
      var reader = new FileReader();
      // Read file into memory as UTF-8      
      reader.readAsText(fileToRead);
      // Handle errors load
      reader.onload = loadHandler;
      reader.onerror = errorHandler;
    };


  function loadHandler(event) {
      var csv = event.target.result;
      processData(csv);
    };

    function processData(csv) {
        var allTextLines = csv.split(/\r\n|\n/);
        var lines = {};
        // skipping first and last line
        var data = allTextLines[0].split(',');
        header = [];
        for (var j=0; j<data.length; j++) {
             var str = data[j].replace(/['"]+/g, '');
             header.push(str);
             lines[str] = [];
        }  
        //console.log(lines);
        for (var i=1; i<(allTextLines.length-1); i++) {
            data = allTextLines[i].split(',');
                for (j=0; j<data.length; j++) {
                var str2= data[j].replace(/['"]+/g, '');
                    (lines[header[j]]).push(str2);
                }
        }
exprmatr = lines;
}

function errorHandler(evt) {
      if(evt.target.error.name == "NotReadableError") {
          alert("Canno't read file !");
      }
    }

function send_message(url,dat)
{
//alert("press button to accept opening a new tab that loads VSClust with your data");

// Alternatively I would suggest this kind of format (with bio/tech replicates description)
var message = dat;

console.log(exprmatr.length);
if (Object.keys(exprmatr).length > 0) {
message = {};
message.expr_matrix = exprmatr;
message.numrep = 3;
message.numcond = 6;
}
 
 //console.log(message);
//var extWindow = window.open("http://computproteomics.bmb.sdu.dk/Apps/TestAPI/TestAPI/TestAPI/", "_blank");
//var extWindow = window.open("http://computproteomics.bmb.sdu.dk/Apps/VSClustTmp/", "_blank");
var extWindow = window.open(url, "_blank");
    
  setTimeout(checkIfOpen, 5000);
  
  function checkIfOpen() {
      //if(!trackedWindows[windowName])
//      console.log(JSON.parse(JSON.stringify(message)));
        extWindow.postMessage(JSON.stringify(message),'*');
        console.log("message sent");
    }
    }
    
    
         
