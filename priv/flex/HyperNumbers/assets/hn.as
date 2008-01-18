// ActionScript file
import mx.controls.*;
import mx.rpc.events.ResultEvent;
import mx.events.*;
import mx.rpc.http.HTTPService;
import mx.events.DropdownEvent;
import mx.controls.Alert;
import mx.collections.*;
import hypernumbers.remoting.*;
import hypernumbers.http.*;
import mx.events.MenuEvent;

[Bindable] private var cm:ContextMenu = new ContextMenu();

private var _socket:RemotingSocket;
private var _url:Object  = new Object();
private var _view:Object = new Object();
private var _user:Object = new Object();

private var store_links_to:XML;
private var store_links_from:XML;

private var _RowIndex:int;
private var _ColIndex:int;

private var connected:Boolean = false;

[Bindable] private var _menu:XMLList = <>
    <menuitem label='File' data='top'>
        <menuitem label='Import Spreadsheet' data='?import' />
    </menuitem>
    <menuitem label='Browse' data='top'>
        <menuitem label='Local Websheets' data='top' />
        <menuitem type="separator"/>
        <menuitem label='Incoming Links' data='top' />
        <menuitem label='Outgoing Links' data='top' /> 
    </menuitem>
    </>;


[Bindable] private var g_formula:String = "";
[Bindable] private var gridXMLList:XMLList = new XMLList();

[Embed(source="connect.png")]
[Bindable] public var socketOn:Class;    

[Embed(source="disconnect.png")]
[Bindable] public var socketOff:Class;

/////////////////////////////////////////////////////////////////////////////
//
// Initialise the page 
//
/////////////////////////////////////////////////////////////////////////////
private function init():void
{
    // We are currently looking at this range of the spreadsheet
    _view.range = "a1:o20";

    _url = parseURL((Application.application.url).replace("/@swf","/"));

    // Listen for DataGrid events
    dg.addEventListener("itemEditEnd",itemEditEndHandler);
    dg.addEventListener("itemFocusIn",itemFocusInHandler);

    // Populate the function menu bar
    XMLHttp.get("http://"+_url.domain+":"+_url.port+"/funs.xml",function(e:ResultEvent):void
    { 
        functions.dataProvider = new XMLListCollection(new XMLList(e.result)); 
    });
    
    cm.hideBuiltInItems();
    cm.addEventListener(ContextMenuEvent.MENU_SELECT, contextMenu_menuSelect);

    Security.loadPolicyFile(_url.domain)
    connect();  
    load();
}

// Sticking to a 'dumb' gui so just reload all the information at once
private function load():void
{
    XMLHttp.get("http://"+_url.domain+":"+_url.port+"/?pages&format=xml",loadbrowse);
    XMLHttp.get(_url.fullpath+"?incoming&format=xml",loadLinksFrom);    
    XMLHttp.get(_url.fullpath+"?outgoing&format=xml",loadLinksTo);    
    XMLHttp.get(_url.fullpath+escape(_view.range)+"?format=xml",loadRange);    

    XMLHttp.get(_url.fullpath+"?loggedin&format=xml",function(e:ResultEvent):void 
    { 
        currentState = (e.result.toString() == "true") 
            ? 'LoggedIn' : 'Login'; 
    });    
}


/////////////////////////////////////////////////////////////////////////////
//
// Context Menu Stuff
//
////////////////////////////////////////////////////////////////////////////
// When the context menu is clicked, this checks all the links to
// and from a page and attaches any links relating to the cell
// to the context menu
private function contextMenu_menuSelect(evt:ContextMenuEvent):void
{
    dg.selectedIndex = _RowIndex;
    cm.customItems = new Array();

    if(store_links_from != null)
    {
        for each (var x:XML in store_links_from.link)
        {
            if(x.from.row == _RowIndex && x.from.column == _ColIndex)
                addCMI("outgoing: ",x.to.site+""+x.to.path);
        }
    }

    if(store_links_to != null)
    {
        for each (var y:XML in store_links_to.link)
        {
            if(y.to.row == _RowIndex && y.to.column == _ColIndex)
                addCMI("incoming: ",y.from.site+""+y.from.path);
        }
    }
}

private function addCMI(pre:String,label:String):void
{
    var cmi:ContextMenuItem = new ContextMenuItem(pre+label, true);
    cmi.addEventListener(ContextMenuEvent.MENU_ITEM_SELECT, 
        function(evt:ContextMenuEvent):void { go(label); });
    cm.customItems.push(cmi);;
}

private function menuHandler2(event:MenuEvent):void
{
    if (event.item.@data != "top")
        g_formula+=event.item.@data;
}

/////////////////////////////////////////////////////////////////////////////
//
// Call handlers for http requests
//
/////////////////////////////////////////////////////////////////////////////
private function loadbrowse(event:ResultEvent):void
{
    for each (var page:XML in event.result.page)
    {
        _menu.menuitem[1].appendChild(
            new XML("<menuitem label='"+page.toString()+"'"
                +"data='"+page.toString()+"'/>"));
    }
}

private function loadLinksFrom(event:ResultEvent):void
{
    //store_links_to = event.result as XML;
    //link_to.dataProvider = new Array();
    //for each (var linksto:XML in event.result.link)
    //{
    //    var lab:String = linksto.from.site.toString()
    //        + linksto.from.path.toString();
    //    link_to.dataProvider.addItem({label:lab});
    //}
}

private function loadLinksTo(event:ResultEvent):void
{
    //store_links_from = event.result as XML;
    //link_from.dataProvider = new Array();
    ///for each (var linksfrom:XML in event.result.link)
    //{
    //    var lab:String = linksfrom.to.site.toString()
    //        + linksfrom.to.path.toString();
    //    link_from.dataProvider.addItem({label:lab});
    //}
}

private function loadRange(event:ResultEvent):void
{
    if(event.result.toString() == "Error: unauthorised")
        warn.text = "* You must be logged in to view this page";

    else
    {
        gridXMLList = new XMLList();
        var y:int = 1;
        for each (var item:XML in event.result.row)
        {
            var tmp:XML = new XML("<row><row_name>"
                +y+"</row_name></row>");

            for(var i:int = 0; i < item.column.length(); i++)
            {
                tmp.appendChild(new XML("<column"+(i+1)
                    + ">"+item.column[i]+"</column"+(i+1)+">"));
            }
            gridXMLList += tmp;
            y++;
        }
    }
}

/////////////////////////////////////////////////////////////////////////////
//
// Login functions
//
/////////////////////////////////////////////////////////////////////////////
public function login():void
{
    var xml_str:String = "<post><action>login</action><user>"+user.text+"</user>"
        +"<password>"+pass.text+"</password></post>";

    XMLHttp.post(_url.fullpath+"?login&format=xml",xml_str,function(e:ResultEvent):void
    {
        if(e.result == "invalid_user")
            Alert.show("Invalid Login!");

        else if(e.result == "success")
        {
            load();
            currentState='LoggedIn';
        }
    });
}

public function logout():void
{
    var xml_str:String = "<post><action>logout</action></post>";

    XMLHttp.post(_url.fullpath+"?login&format=xml",xml_str,
        function(e:ResultEvent):void
    {
        if(e.result == "success") 
            currentState='Login'
        else 
            Alert.show("Error Logging Out");       
    });
}

/////////////////////////////////////////////////////////////////////////////
//
// Socket functions
//
/////////////////////////////////////////////////////////////////////////////
private function connect():void
{
    if(_socket == null || !_socket.connected)
    {
        log("Connecting");
        
        _socket = new RemotingSocket(_url.domain,1935);
        _socket.addEventListener("newRemotingMsg",remotingSocketHandler);

        _socket.addEventListener(Event.CONNECT, function(event:Event):void
        {   
            log("Socket Connected");
            socketimage.source = socketOn;  
        });
        _socket.addEventListener(Event.CLOSE, function(event:Event):void
        { 
            log("Socket Disconnected");
            socketimage.source = socketOff; 
        });
        
        _socket.write("register " + _url.fullpath + _view.range);
    }
}

private function remotingSocketHandler(event:RemotingEvent):void
{
    log(event.message);

    var message:Array = event.message.split(" ");

    if (message[0] == "change")
    {
        var changeurl:Object = parseURL(message[1]);
        var ref:String = changeurl.path.pop();

        var col:String = "";
        var row:String = "";

        for (var i:int; i < ref.length; i++)
        {
            var char:int = ref.charCodeAt(i);

            if (char > 47 && char < 58)
                row += ref.charAt(i);
            else
                col += ref.charAt(i);
        }

        var colindex:int = col.charCodeAt(0)-96;
        var rowindex:int = int(row);

        message.shift();
        message.shift();

        gridXMLList[rowindex-1].replace(colindex,
            new XML("<column"+colindex+">"+message.join(" ")+"</column"+colindex+">"));
    }
}

/////////////////////////////////////////////////////////////////////////////
//
// DataGrid
//
////////////////////////////////////////////////////////////////////////////
private function itemEditEndHandler(e:DataGridEvent):void
{
    var new_t:String = TextInput(dg.itemEditorInstance).text;
    var old_t:String = gridXMLList[e.rowIndex].child("column" + e.columnIndex);

    if (new_t != old_t)
        sendformula(new_t);
}

private function itemFocusInHandler(event:DataGridEvent):void
{
    var index:String = (int(event.rowIndex) +1 ).toString();
    _view.cell = make_alpha(event.columnIndex) + index;

    var tmppath:String = "/"+_url.path.join("/");

    _selection.dataProvider = new Array(
        {"label": "="+tmppath+_view.cell},
        {"label": "=hypernumber(\""+_url.fullpath+_view.cell+"?hypernumber\")"});

    XMLHttp.get(_url.fullpath+_view.cell+"?toolbar&format=xml",
        function(e:ResultEvent):void 
    {   
        g_formula= "="+e.result.formula; 
    });
}

public function sendformula(formula:String):void
{
    XMLHttp.post(_url.fullpath+_view.cell+"?format=xml","<post><action>create</action>"
        +"<value>"+formula+"</value></post>", function(e:ResultEvent):void {} );
}

/////////////////////////////////////////////////////////////////////////////
//
// Utility Functions
//
/////////////////////////////////////////////////////////////////////////////
// this function takes a positive integer and turns it into a base 26 number
// where all values are represented by a lower case letter
private function make_alpha(nInteger:int):String
{
    var Ret:String;
    var Div:Number;
    var DivInt:int;
    var Alpha:int;
    Ret="";
    do
    {
        Div=nInteger/26;
        DivInt=int(Div);
        Alpha=nInteger-26*DivInt+96;
        Ret=String.fromCharCode(Alpha)+Ret;
        nInteger=DivInt;
    } while (DivInt > 0);
    return Ret;
}

private function go(url:String):void
{
    navigateToURL(new URLRequest(url), '_self');
}

private function parseURL(orig:String):Object
{
    var tmp:Object = new Object();

    tmp.fullpath = orig;
    tmp.path = (tmp.fullpath.replace("http://","").split("/"));

    var tmp_domain:Array = (tmp.path[0]).split(":");
    tmp.path.shift();

    tmp.domain = tmp_domain[0];
    tmp.port   = (tmp_domain[1] == undefined) ? 80 : tmp_domain[1];

    return tmp;
}

private function showDebug():void
{
    output.visible = debug.selected;
    output.includeInLayout = debug.selected;
}

private function log(log:String):void
{
    output.text += log+"\n";
}
