var _view = "A1:J30";

var selectedRow = 0;
var selectedCol = 0;

$(function()
{
    var so = new SWFObject("/swf/JsSocket.swf", "jssocket","400", "50", "8");
    so.addParam("allowScriptAccess", "always");
    so.write("jssocket");

    // Populate the functions menu
    $.get("/funs.xml", function(data) { 
        addMenuTree(data.firstChild,$("#menu > li > ul")); });
    
    // Debug Panel 
    $("#toggle_dbg").change( function(){
        $("#dbg").toggle(); });

    // File Menu
    $("a.file").click( function() {
        $("#filemenu ul").toggle(); });

    // Import SpreadSheet
    $("#m_import").click( function()
    {
        $("#filemenu ul").toggle();
        var x = $("#import").modal();
        $("#Filedata").change(function()
        {
            $.ajaxFileUpload
            ({
                url:'?import', 
                secureuri:false,
                datatype: "json",
                fileElementId:'Filedata'
            })
            x.close();
        });
    });

    $("#formula").blur(function()  { 
        var cellurl = document.location.href+to_b26((selectedCol-1))
            + (selectedRow-1);
        if(this.value != "=")
            $.post(cellurl,{action:"create",value:this.value});
    });

    setupRange(_view);
    loadRange(_view);

    $('#hn tr > td > input').contextMenu('myMenu1', {
      bindings: {
        'hnumber': function(t) {
          copy(document.getElementById("texttocopy"));
        }
     },
      menuStyle: {
        width: '150px'
      },
      itemStyle: {
        font: '8px',
        border: 'none',
        padding: '3px'
      },
      itemHoverStyle: {
        background: '#EEE',
        border: 'none'
      }
    });
});

function addMenuTree(el,root)
{
    $.each($(el).children(),function() 
    {
        var menuitem = $(this);
        var el = $("<li><a>"+$(menuitem).attr("label")+"</a></li>").appendTo(root);
         
        if($(this).children().size() > 0)
        {
            $(el).children("a").addClass("menuparent");
            addMenuTree(this,$("<ul></ul>").appendTo(el));
        }
        else
        {
            $(el).children("a").click(function() {
                $("#formula").val($("#formula").val()+$(menuitem).attr("data"));
                $("#menu li ul").css({ display:"none"});
                $("#formula").focus();
            });   
        }
    });
}

$.fn.hoverClass = function(c) {
    return this.each(function(){
        $(this).hover( 
            function() { $(this).addClass(c);  },
            function() { $(this).removeClass(c); }
        );
    });
}; 

function loadRange(range)
{
    $.get(document.location.href+_view+"?format=xml", 
        function(data) {
            $.each($(data).find("row"),function(x) {
                $.each($(this).find("column"),function(y) {
                    setCell(x+2,y+2,$(this).text());
                });
            });
        });

}

function setupRange(range)
{
    var arr = range.split(":");
    var cell2 = cellRef(arr[1]);

    for(var z = 0; z < cell2[1]+1; z++)
    {
        var row =  $("<tr/>").appendTo("#hn");

        for(var n = 0; n < cell2[0]+1; n++) 
        {
            var col = String.fromCharCode(n+64);

            if(z == 0)
            {
                row.append("<th>"+ ((n ==0 ) ? "" : col)+"</th>");
            }
            else
            {
                row.append(((n ==0 ) 
                    ? "<td class='rowindex'>"+z+"</td>" 
                    : "<td><div class='"+(col+z)+"' /></td>"));
            }
        }
    }

    $("#hn tr:nth-child(odd)").addClass("odd");


    $("#hn tr > td > div").each(function() 
    {

        $(this).mousedown(function()
        {
            
        });

        $(this).mouseup(function() 
        { 
            $(this).unbind('blur');

            //$(".sel").removeClass("sel");
            //$(this).addClass("sel");

            //var _val = $(this).text();
            var _col = this.parentNode.cellIndex;
            var _row = this.parentNode.parentNode.rowIndex;
            var _url = document.location.href+to_b26(_col)+_row;

            $(this).empty();
            $(this).append("<input type='text' value='"+_val+"' />")

            $("#texttocopy").text("=hn(\""+_url+"\"?hypernumber)");

            selectedCol = _col+1;
            selectedRow = _row+1;

            highlight(selectedRow,selectedCol);
    
            $.get(_url+"?toolbar&format=xml",function(data) {
                $("#formula").val("="+$(data).find("formula").text());
            });

            $(this).children("input").blur(function()  
            {
                var newval = $(this).children("input").val();

                $(this).empty();
                $(this).text(newval);

                if(_val != newval)
                    $.post(_url,{action:"create",value:newval});
            });
        });

        $(this).keypress(function (e) 
        { 
            if (e.keyCode == 13 || e.keyCode == 40)
                moveSelection('DOWN');
            else if(e.keyCode == 39)
                moveSelection('RIGHT');
            else if (e.keyCode == 38)
                moveSelection('UP');
            else if (e.keyCode == 37)
                moveSelection('LEFT');
        });
    });
}

function setCell(x,y,val)
{
    $("#hn tr:nth-child("+x+") "
        +"td:nth-child("+y+") div").text(val);
}

// Parses a cell reference(a1) into x y indexes
function cellRef(cell)
{
    var x = from_b26((cell.match(/[a-z]+/i)[0]).toLowerCase());
    var y = parseInt(cell.match(/[0-9]+/)[0]);

    return [x,y];
}

function to_b26(cell)
{
    return String.fromCharCode(cell+96);
}

function from_b26(cell)
{
    return cell.charCodeAt(0)-96;
}

function dbg(msg)
{
    $("#dbg").append(msg+"\n");
}

function highlight(row,col)
{
    $("#hn tr .rowindex, #hn tr th").removeClass("highlight");
    $("#hn tr:nth-child("+row+") .rowindex").addClass("highlight");
    $("#hn tr th:nth-child("+col+")").addClass("highlight");
}

function moveSelection(direction)
{
    var ind = get_xy(direction);

    if(ind !== false)
    {
    
        $("#hn tr:nth-child("+selectedRow+") "
            +"td:nth-child("+selectedCol+") input").blur();
    
        $("#hn tr:nth-child("+(ind[0])+") "
            +"td:nth-child("+ind[1]+") input").focus();
    }
}

function get_xy(direction)
{
    var arr = _view.split(":");
    var cell = cellRef(arr[1]);

    if(direction == 'DOWN' && selectedRow < cell[1]+1)
        return [(selectedRow+1),selectedCol];

    else if(direction == 'UP' && selectedRow > 2)
        return [(selectedRow-1),selectedCol];

    else if(direction == 'LEFT' && selectedCol > 2)
        return [selectedRow,(selectedCol-1)];

    else if(direction == 'RIGHT' && selectedCol < cell[0]+1)
        return [selectedRow,(selectedCol+1)];

    else 
        return false;
}

/////////////////////////////////////////////////////////////////////////////
// JsSocket - flash bridge to open a socket connection
/////////////////////////////////////////////////////////////////////////////
var _jssocket = null;

// Called from the swf once it has loaded
function jssocket_init()
{
    dbg("Socket Init");

    // This is the id/name of the flash object
    _jssocket = $("#jssocket")[0];

    // Set callbacks for flash to call
    _jssocket.setCallBack("connect","soc_connect");
    _jssocket.setCallBack("disconnect","soc_closed");
    _jssocket.setCallBack("recieve","soc_msg");

    // Connect
    _jssocket.connect(document.location.hostname,1935);
    $("#debug img").attr("src","/images/loading.gif");
}

// Called when socket connects
function soc_connect()
{
    dbg("Socket Connected");

    $("#debug img").attr("src","/images/connect.png");
    _jssocket.write("register "+document.location.href+_view);
}

// Called when socket is closed
function soc_closed()
{
    $("#debug img").attr("src","/images/disconnect.png");
    dbg("Socket Closed");
}

// Called when socket receives message
function soc_msg(msg)
{
    dbg("Receieved: "+msg);

    var arr = msg.split(" ");

    if(arr[0] == "change")
    {
        var urlarr = arr[1].split("/");
        var cell = cellRef(urlarr[urlarr.length-1]);

        setCell(cell[1]+1,cell[0]+1,arr[2]);
    }
}

function copy(inElement) {

  if (inElement.createTextRange) {
    
    var range = inElement.createTextRange();
    
    if (range && BodyLoaded==1)
     range.execCommand('Copy');

  } else {
    
    var flashcopier = 'flashcopier';
    
    if(!document.getElementById(flashcopier)) {
    
      var divholder = document.createElement('div');
      divholder.id = flashcopier;
      document.body.appendChild(divholder);
    }
    
    document.getElementById(flashcopier).innerHTML = '';

    var divinfo = '<embed src="/swf/_clipboard.swf" FlashVars="clipboard='+escape(inElement.value)+'" width="0" height="0" type="application/x-shockwave-flash"></embed>';
    
    document.getElementById(flashcopier).innerHTML = divinfo;
  }
}