HN.Tools.Util = {};

HN.Tools.Util.handle_drop = function(ev, ui)
{
  var drag;
  var new_elem;
  drag = HN.Tools.Util.clean_drag_name(ui.draggable[0].className);
  console.log(drag + " dropped on " + ev.target.className);
  new_elem = HN.Tools.make_new_elem(drag[0]);
  console.log(new_elem);
  if (HN.Gui.Util.is_member("htmltag", ev.target.className.split(" ")))
  {
    console.log("handle me properly...");
    console.log(ev.target.parentNode);
    console.log(new_elem);
    console.log(ev.target);
    ev.target.parentNode.insertBefore(new_elem, ev.target);
    //console.log(ev.target.previousSibling);
    // If there is a previous sibling make the drop a new sibling
    // but if there isn't make the drop the first node of the inner HTML...
    //if (ev.target.previousSibling) {
    //  }
  } else {
    ev.target.appendChild(new_elem);
  };
  // Now refresh the droppables
  $(".container").droppable({drop: HN.Tools.Util.handle_drop, greedy: true});
  $(".container_td").droppable({drop: HN.Tools.Util.handle_drop, greedy: true});
  $(".htmltag").droppable({drop: HN.Tools.Util.handle_drop, greedy: true});
  // Make the container htmltag's draggable too...
  $(".container").draggable();
  $(".htmltag").draggable();

  // First upbind all onclick elements
  $(".htmltag").unbind("click", HN.Tools.Util.html_onclick);
  $(".htmltag").bind("click", HN.Tools.Util.html_onclick);
  // and do the same hack for mouse clicks
  $(".htmltag").unbind("mousedown", HN.Tools.Util.mouse_click_block);
  $(".htmltag").bind("mousedown", HN.Tools.Util.mouse_click_block);
  $(".container").unbind("mousedown", HN.Tools.Util.mouse_click_block);
  $(".container").bind("mousedown", HN.Tools.Util.mouse_click_block);
};

HN.Tools.Util.html_onclick = function(e) {
                      console.log("in on click for htmltab");
                      console.log(e);
                      console.log(e.target);
                      e.stopPropagation();
                      return false;
};

// Hack because of my use of jquery.contextmenu
HN.Tools.Util.mouse_click_block = function(e) {
  e.stopPropagation();
};

//HN.Tools.Util.enumerate_positions = function() {
//  var element;
//  var i;
//  var top;
//  var left;
//  var bottom;
//  var right;
//  var type;
//  console.log("enumerate child nodes of canvas...");
//  element = HN.Util.id("canvas2");
//  for (i = 0; i < element.childNodes.length; i += 1) {
//    top    = element.childNodes[i].offsetTop;
//    left   = element.childNodes[i].offsetLeft;
//    bottom = top  + element.childNodes[i].clientHeight;
//    right  = left + element.childNodes[i].clientWidth;
//    type = element.childNodes[i].tagName;
//    if (type === "DIV") {
//      console.log(element.childNodes[i] + " is of " + type
//                  + " and has coords " + top + ", " + left
//                  +", " + bottom + ", " + right);
//    } else if (type === "TABLE") {
//      table = element.childNodes[i];
//        HN.Tools.Util.enumerate_children(element.childNodes[i]);
//    } else {
//      console.log("type is " + type);
//    }
//  }
//};

//HN.Tools.Util.enumerate_children = function(node)
//{
//  var i;
//  if (node) {
//    for (i = 0; i < node.childNodes.length; i += 1) {
//      console.log(node.childNodes[i]);
//      HN.Tools.Util.enumerate_children(node.childNodes[i]);
//    }
//  }
//};

HN.Tools.Util.clean_drag_name = function(string) {
   var oldnamearray;
   var newnamearray =[];
   var i;
   oldnamearray= string.split(" ");
   for (i = 0; i < oldnamearray.length; i += 1) {
     if (oldnamearray[i] !== "templatesitem"
         && oldnamearray[i] !== "toolsitem"
         && oldnamearray[i] !== "ui-draggable") {
           newnamearray[newnamearray.length] = oldnamearray[i];
         }
   };
   return newnamearray;
};

HN.Tools.Util.close_help_dialog = function()
{
  HN.Tools.Util.dialog_close("#helpdialog");
};

HN.Tools.Util.dialog_close = function (dialog)
{
  $("#cover").fadeOut("fast");
  $(dialog).fadeOut("fast");
};

HN.Tools.Util.dialog_open = function(dialog) {
  $(dialog).css("margin-left", -($(dialog).width()/2));
  $(dialog).css("margin-top", -($(dialog).height()/2));
  $(dialog).fadeIn("fast");
  $("#cover").fadeIn("fast");
};

HN.Tools.Util.show_dialog = function(dialog) {
  HN.Tools.Util.dialog_open(HN.Util.id(dialog));
};

HN.Tools.Util.show = function(id)
{
  var element;
  element = HN.Util.id(id);
  element.style.display ="block";
};

HN.Tools.Util.hide = function(id)
{
  var element;
  element = HN.Util.id(id);
  element.style.display ="none";
};
