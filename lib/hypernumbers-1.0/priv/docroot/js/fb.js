///////////////////////////////////////////////////////////////////
//                                                               //
// Colophon                          
//                                                               //
///////////////////////////////////////////////////////////////////
        
// "Let us change our traditional attitude to the construction of
//  programs. Instead of imagining that our main task is to 
//  instruct a computer what to do let us concentrate rather on
//  explaining to human beings what we want a computer to do"
//
// Donald Knuth - Literate Programming - The Computer Journal May 1984

// Official Reason For The Structure Of This Code
// * make it easier for the users of the How To to understand what we are doing
//
// Real Reason For The Structure Of This Code
// * my javascript is pure boggin 'cos I don't speak it :(

var Dom = YAHOO.util.Dom;
var Event = YAHOO.util.Event;
var DD = YAHOO.util.DD;
var DDTarget = YAHOO.util.DDTarget;
var ContextMenu = YAHOO.widget.ContextMenu;
var Resize = YAHOO.util.Resize;

// Define the 'get new form component ID'
// which gives you the div id that will be used to
// identify a new component
// a CSS rule bound to this id will be used to place the object on the screen
function getNewFormCompId() {
  var returnVal = _GLOBAL_formComponentId;
  _GLOBAL_formComponentId=_GLOBAL_formComponentId+1;
  return returnVal;
};

// Set up some utilities
function setDialog(item,val) {
  var el = document.getElementById(item);

  switch (item) {
		
  case "objId": 
    el.textContent=val;
    oFormFocus.changeFocus(val);
    break;

  default:
    el.textContent= val;
    break;
			
  };
};

// Define an onDoubleClick handler for the new components
function editFormComp(e, objId) {
  var elem=document.getElementById(objId);
  text = e.target.textContent;
  setDialog("text",text);
  setDialog("objId",objId);
};

// Define the 'create new object function'
// This actually adds a control to the form
function createNewObj(p_sType, p_aArgs, objId) {
  xy=Dom.getXY(Dom.get(this.id));
  X=xy[0];
  Y=xy[1];
  var insertPoint = document.getElementById("dropzone");
  var formCompId = getNewFormCompId();
  var formCompIdStr = "form_comp_id_"+formCompId;
  var formCompResizeStr = "resize_"+formCompId;
  // first set up some general stuff
  var newNode = document.createElement("div");
  newNode.style.position = "absolute";
  newNode.style.left = X+"px";
  newNode.style.top  = Y+"px";
  newNode.className = "dragNdrop";
  newNode.id=formCompIdStr;
  // now do the 'per insert type' stuff
  if(objId == "headline") {
    newNode.innerHTML="<div id=\"dd-handle-"+formCompId+"\" class=\"dd-handle\">"+
      "<img src=\"/img/move.jpg\" height=\"8\" width=\"8\"></div>"+
      "<h3 class=\"dragNdropElement\" id=\""+formCompResizeStr+"\">"+
      "</h3>"
    insertPoint.appendChild(newNode);
    var resize = new Resize(formCompResizeStr);
   } 
  else if(objId == "generaltext") {
    newNode.innerHTML="<div id=\"dd-handle-"+formCompId+"\" class=\"dd-handle\">"+
      "<img src=\"/img/move.jpg\" height=\"8\" width=\"8\"></div>"+
      "<p class=\"dragNdropElement\" id=\""+formCompResizeStr+"\"></p>";
    insertPoint.appendChild(newNode);
    var resize = new Resize(formCompResizeStr);
  } 
  else if(objId == "button") {
    newNode.innerHTML="<div id=\"dd-handle-"+formCompId+"\" class=\"dd-handle\">"+
      "<img src=\"/img/move.jpg\" height=\"8\" width=\"8\"></div>"+
      "<input class=\"dragNdropElement\" type=\"button\" value=\"Configure\"></input>";
    insertPoint.appendChild(newNode);
  } 
  else if(objId == "checkbox") {
    newNode.innerHTML="<div id=\"dd-handle-"+formCompId+"\" class=\"dd-handle\">"+
      "<img src=\"/img/move.jpg\" height=\"8\" width=\"8\"></div>"+
      "Option 1<input class=\"dragNdropElement\" type=\"checkbox\" "+
      "value=\"Configure\"></input>";
    insertPoint.appendChild(newNode);
  } 
  else if(objId == "radiobox") {
    newNode.innerHTML="<div id=\"dd-handle-"+formCompId+"\" class=\"dd-handle\">"+
      "<img src=\"/img/move.jpg\" height=\"8\" width=\"8\"></div>"+
      "Option 1<input class=\"dragNdropElement\" type=\"radio\" "+
      "value=\"Configure\"></input>";
    insertPoint.appendChild(newNode);
  } 
  else if(objId == "menu") {
    newNode.innerHTML="<div id=\"dd-handle-"+formCompId+"\" class=\"dd-handle\">"+
      "<img src=\"/img/move.jpg\" height=\"8\" width=\"8\"></div>"+
      "<select class=\"dragNdropElement\" name=\"menu\"><option>"+
      "Option 1</option><option>Option 2</option></select>";
    insertPoint.appendChild(newNode);
  }
  else if(objId == "textinput") {
    newNode.innerHTML="<div id=\"dd-handle-"+formCompId+"\" class=\"dd-handle\">"+
      "<img src=\"/img/move.jpg\" height=\"8\" width=\"8\"></div>"+
      "<input class=\"dragNdropElement\" id=\""+formCompResizeStr+"\" "+
      "type=\"textarea\"></input>";
    insertPoint.appendChild(newNode);
    var resize = new Resize(formCompResizeStr);
  } 
  else if(objId == "textarea") {
    newNode.innerHTML="<div id=\"dd-handle-"+formCompId+"\" class=\"dd-handle\">"+
      "<img src=\"/img/move.jpg\" height=\"8\" width=\"8\"></div>"+
      "<textarea class=\"dragNdropElement\" id=\""+formCompResizeStr+"\"></textarea>";
    insertPoint.appendChild(newNode)	;
    var resize = new Resize(formCompResizeStr);
  } 
  else if(objId == "cellblock") {
    newNode.innerHTML="<div id=\"dd-handle-"+formCompId+"\" class=\"dd-handle\">"+
      "<img src=\"/img/move.jpg\" height=\"8\" width=\"8\"></div>"+
      "<table class=\"dragNdropElement\" id=\""+formCompResizeStr+"\" cols=\"3\" border=\"1\">"+
      "<tr><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td></tr>"+
      "<tr><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td></tr></table>";
    insertPoint.appendChild(newNode);
    var resize = new Resize(formCompResizeStr);
  };
  // finally do the global stuff
  // register the new object as drag and drop
  compForm[formCompIdStr] = new DD(formCompIdStr);
  compForm[formCompIdStr].setHandleElId("dd-handle-"+formCompId);
  // set up the double click handler
  Event.addListener(formCompResizeStr,'dblclick', editFormComp, formCompResizeStr, true);
};
			  
/*
  Initialize the ContextMenu instances when the the elements
  that trigger their display are ready to be scripted.
*/
Event.onContentReady("dropzone", function () {
                       var oDropzone = this;
                       // "render" event handler for the canvas context menu
                       function onCanvasMenuRender(p_sType, p_aArgs) {
                         if (this.parent) {  // submenu
                           this.checkedItem = this.getItem(0);
                         }
                       }
                       /*
                         Array of object literals - each containing configuration
                         properties for the items for the context menu.
                       */
                       var oCnvsCtxtMenuItemData = [
                         { text: "Headline",     onclick: { fn: createNewObj, obj: "headline"} },
                         { text: "General Text", onclick: { fn: createNewObj, obj: "generaltext" } },
                         { text: "Button",       onclick: { fn: createNewObj, obj: "button" } },
                         { text: "Check Box",    onclick: { fn: createNewObj, obj: "checkbox" } },
                         { text: "Radio Box",    onclick: { fn: createNewObj, obj: "radiobox" } },
                         { text: "Menu",         onclick: { fn: createNewObj, obj: "menu" } },
                         { text: "Text Input",   onclick: { fn: createNewObj, obj: "textinput" } },
                         { text: "Text Area",    onclick: { fn: createNewObj, obj: "textarea" } },
                         { text: "Cell Block",   onclick: { fn: createNewObj, obj: "cellblock" } }	
                         ];
                       /*
                         Instantiate a ContextMenu:  The first argument passed to
                         the constructor is the id of the element to be created; the
                         second is an object literal of configuration properties.
                       */
                       var oCnvsCtxtMenu = new ContextMenu(
                                                           "canvascontextmenu",
                                                           {
                                                           trigger: "dropzone",
                                                               itemdata: oCnvsCtxtMenuItemData,
                                                               lazyload: true
                                                               }
                                                           );
                       // Add a "render" event handler to the canvas context menu
                       oCnvsCtxtMenu.subscribe("render", onCanvasMenuRender);
                       
                });

(function() {
  Event.onDOMReady(function() {
                     // Create the drop and no-drop zones
                     var dropzone = new DDTarget("dropzone");
                     var nodropzone = new DDTarget("nodropzone");
                     nodropzone.isTarget=false;
                     setup_langs();
                   });    
 })();
