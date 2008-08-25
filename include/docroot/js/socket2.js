/////////////////////////////////////////////////////////////////////////////
// JsSocket - flash bridge to open a socket connection
/////////////////////////////////////////////////////////////////////////////
var _jssocket = null;

function jssocket_init()
{
    // This is the id/name of the flash object
    _jssocket = document.getElementById("mymovie");;

    // Set callbacks for flash to call
    _jssocket.setCallBack("connect","soc_connect");
    _jssocket.setCallBack("disconnect","soc_closed");
    _jssocket.setCallBack("recieve","soc_msg");
    _jssocket.setCallBack("ioerror","soc_error");

    _jssocket.connect(document.location.hostname,1935);
}

// Called when socket connects
function soc_connect()
{
  console.log($("#footer img"));
    $("#footer span").text("connected");
    $("#footer img").attr("src","/img/tick.png");
    console.log("bodge for expenses page");
    console.log(document.location.href);
    var path;
    if ((document.location.href == "http://127.0.0.1:9000/html/expenses.html") ||
	(document.location.href == "http://127.0.0.1:9000/html/expenses_printable.html"))
      {
        console.log("picked up the hooky location stuff in socket.js");
	path = "http://127.0.0.1:9000/expenses/claim/1/";
      }
      else
      {  
        console.log("using the real page location in socket.js");
        path=document.location.href;
      }
     console.log(path);
    _jssocket.write("register "+path+"a1:j30");
}

function soc_error()
{
    $("#footer span").text("error");
    $("#footer img").attr("src","/img/cross.png");
}
            
// Called when socket is closed
function soc_closed()
{
  console.log("disconnecting");
    $("#footer span").text("disconnected");
    $("#footer img").attr("src","/img/cross.png");
}
            
// Called when socket receives message
function soc_msg(msg)
{
    var x = msg.split("\n");
    $.each(x,function(i)
    {
        var arr = x[i].split(" ");

        if(arr.shift() == "change")
        {
            handle_attr(arr.join(" "));
        }        
    });
}
