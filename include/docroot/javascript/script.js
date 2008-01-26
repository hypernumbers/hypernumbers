    var _view = "A1:J30";
    
    var selectedRow = 0;
    var selectedCol = 0;
    
    var views = { INIT:0, VIEW:1, SELECT:2, DRAG:3 };
    
    var selection  = new Object();
    var dragselection = new Object();

$(function()
{
    var currentview = views.INIT;

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

    currentview = views.VIEW;
}

function resetSelection()
{
    $(".dragselected").removeClass("dragselected");  

    dragselection.startx = null;
    dragselection.starty = null;
    dragselection.endx = null;
    dragselection.endy = null;
    dragselection.initx = null;
    dragselection.inity = null;

    selection.startx = null;
    selection.starty = null;
    selection.endx = null;
    selection.endy = null;
    selection.initx = null;
    selection.inity = null;
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
               row.append("<th "+((n ==0 ) ? "class='rowindex'" : "")+
                    ">"+ ((n ==0 ) ? "&nbsp;" : col)+"</th>");
            }
            else
            {
                row.append(((n ==0 ) 
                    ? "<td class='rowindex'>"+z+"</td>" 
                    : "<td><div class='"+(col+z)+" cell'></div></td>"));
            }
        }
    }

    $("#hn tr:nth-child(odd)").addClass("odd");


    $("#hn tr > td > div.cell").each(function() 
    {
        $(this).mousedown(function() 
        { 
            if(currentview != views.DRAG)
            {
                resetSelection();
                startSelection(this.parentNode.cellIndex,
                    this.parentNode.parentNode.rowIndex);
            }
        });

        $(this).mouseover(function() 
        { 
            if(currentview == views.SELECT)
            {
                addSelection(this.parentNode.cellIndex,
                    this.parentNode.parentNode.rowIndex);
            }
            else if(currentview == views.DRAG)
            {
                addDrag(this.parentNode.cellIndex,
                    this.parentNode.parentNode.rowIndex);
            }
        });

        $(this).mouseup(function() 
        {
            if(currentview != views.DRAG)
            {
                selectCell(this);
            }
            currentview = views.VIEW;
        });
    });
}

function startDragSelection(x,y)
{
    $(".dragselected").removeClass("dragselected");   
    currentview = views.DRAG;
     
    dragselection.initx = x;
    dragselection.inity = y;

    highlightDragSelection();
}

function addDrag(x,y)
{
    $(".dragselected").removeClass("dragselected"); 

    if(x >= selection.startx && x <= selection.endx && y > dragselection.inity)
    {
        dragselection.startx = selection.startx; 
        dragselection.starty = selection.endy + 1;
        dragselection.endx = selection.endx; 
        dragselection.endy = y;
    }
    else if(x >= selection.startx && x <= selection.endx && y < dragselection.inity)
    {
        dragselection.endx = selection.endx; 
        dragselection.endy = selection.starty - 1;
        dragselection.startx = selection.startx; 
        dragselection.starty = y;
    }
    else if(y >= selection.starty && y <= selection.endy && x < dragselection.initx)
    {
        dragselection.endy = selection.endy; 
        dragselection.endx = selection.startx - 1;
        dragselection.starty = selection.starty; 
        dragselection.startx = x;
    }
    else if(y >= selection.starty && y <= selection.endy && x > dragselection.initx)
    {
        dragselection.starty = selection.starty; 
        dragselection.startx = selection.endx + 1;
        dragselection.endy = selection.endy; 
        dragselection.endx = x;
    }


    highlightDragSelection();
}

function highlightDragSelection()
{
    $("table tr:lt("+(dragselection.endy+1)+"):gt("+(dragselection.starty-1)+")"
        ).find("td:lt("+(dragselection.endx+1)+"):gt("+(dragselection.startx-1)+") div").addClass("dragselected");
}

function startSelection(x,y)
{
    $(".selected").removeClass("selected");   
    currentview = views.SELECT;
     
    selection.initx = selection.startx = selection.endx = x;
    selection.inity = selection.starty = selection.endy = y;

    highlightSelection();
}

function addSelection(x,y)
{
    $(".selected").removeClass("selected");
    $(".singleselected").removeClass("singleselected");

    if(x >= selection.initx && y >= selection.inity)
    {
        selection.startx = selection.initx;
        selection.starty = selection.inity;
        selection.endx = x;
        selection.endy = y;
    }
    else if(x >= selection.initx && y <= selection.inity)
    {
        selection.endx = x;  
        selection.endy = selection.inity;
        selection.starty = y;
    }
    else if(x <= selection.initx && y >= selection.inity)
    {
        selection.endy = y;  
        selection.endx = selection.initx;
        selection.startx = x;
    }
    else if(x <= selection.initx && y <= selection.inity)
    {
        selection.endy = selection.inity;
        selection.starty = y;
        selection.endx = selection.initx;
        selection.startx = x;
    }

    highlightSelection();
}

function selectCell(cell)
{
    currentview = views.VIEW;
    //resetSelection();

    $(cell).addClass("singleselected");

    var _val = $(cell).text();
    var _col = cell.parentNode.cellIndex;
    var _row = cell.parentNode.parentNode.rowIndex;
    var _url = document.location.href+to_b26(_col)+_row;

    $(cell).empty();
    selectedCol = _col+1;
    selectedRow = _row+1;

    highlight();

    $("<input type='text' value='"+_val+"' />").keypress(function (e) { 
        if (e.keyCode == 13 || e.keyCode == 40)
                    moveSelection('DOWN');
        else if(e.keyCode == 39)
                   moveSelection('RIGHT');
        else if (e.keyCode == 38)
                    moveSelection('UP');
        else if (e.keyCode == 37)
                    moveSelection('LEFT'); }).blur(function()  
        {
            var newval = $(this).val();

            $(this).parent().empty().text(newval);

            if(_val != newval)
                $.post(_url,{action:"create",value:newval});

        }).appendTo(cell).focus();

    $("<div class='drag' />").css("left",$(cell).width()-5
        ).css("bottom",4).mousedown(function() 
    {
        startDragSelection(cell.parentNode.cellIndex,
            cell.parentNode.parentNode.rowIndex);

    }).appendTo(cell);
            

    $("#texttocopy").text("=hn(\""+_url+"\"?hypernumber)");

    $.get(_url+"?toolbar&format=xml",function(data) {
        $("#formula").val("="+$(data).find("formula").text());
    });
}

function highlightSelection()
{
    $("table tr:lt("+(selection.endy+1)+"):gt("+(selection.starty-1)+")"
        ).find("td:lt("+(selection.endx+1)+"):gt("+(selection.startx-1)+") div").addClass("selected");
}

function setCell(x,y,val)
{
    $("#hn tr:nth-child("+x+") "
        +"td:nth-child("+y+") div").html(val);
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

function highlight()
{
    $(".highlight").removeClass("highlight");
    $("table tr td.rowindex:lt("+(selection.endy)+"):gt("+(selection.starty-2)+")").addClass("highlight");
    $("table tr th:lt("+(selection.endx+1)+"):gt("+(selection.startx-1)+")").addClass("highlight");
}

function moveSelection(direction)
{
    var ind = get_xy(direction);

    if(ind !== false)
    {
    
        $("#hn tr:nth-child("+selectedRow+") "
            +"td:nth-child("+selectedCol+") input").blur();
    
        selectCell(
            $("#hn tr:nth-child("+(ind[0])+") "
                +"td:nth-child("+ind[1]+") div")[0]);
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
