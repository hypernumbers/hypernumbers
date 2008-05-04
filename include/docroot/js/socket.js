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
    $("#footer span").text("connected");
    $("#footer img").attr("src","/img/tick.png");
    _jssocket.write("register "+document.location.href+"a1:j30");
}

function soc_error()
{
    $("#footer span").text("error");
    $("#footer img").attr("src","/img/cross.png");
}
            
// Called when socket is closed
function soc_closed()
{
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
