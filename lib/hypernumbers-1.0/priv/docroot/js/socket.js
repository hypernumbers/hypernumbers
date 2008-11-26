/////////////////////////////////////////////////////////////////////////////
// JsSocket - flash bridge to open a socket connection
/////////////////////////////////////////////////////////////////////////////
var _jssocket = null;

function jssocket_init()
{
    // This is the id/name of the flash object
    _jssocket = document.getElementById("mymovie");

    // Set callbacks for flash to call
    _jssocket.setCallBack("connect","soc_connect");
    _jssocket.setCallBack("disconnect","soc_closed");
    _jssocket.setCallBack("recieve","soc_msg");
    _jssocket.setCallBack("ioerror","soc_error");

    var get_port = function(data)
    {
      var port = $(data).find("port").text();
      _jssocket.connect(document.location.hostname,1935);
    };
    $.get("/?port",get_port);
}

// Called when socket connects
function soc_connect()
{
    $("#footer span").text("connected");
    $("#footer img").attr("src","/img/tick.png");
    var url = document.location.protocol + "//" + document.location.host
      + document.location.pathname;
    _jssocket.write("register "+url+"a1:j50");
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
	var f = arr.shift();

        if(f == "change")
        {
            handle_attr(arr.join(" "));
        }
	else if(f == "delete")
	{
	    var val = arr.shift();
	    var ref = arr.shift();

	    if(val == "value")
	    {
		$("#hn").spreadsheet("setValue",ref,"");
	    }else if (val == "formula")
	    {
		formulae[ref] = "";
	    }
	}
    });
}
