HN.Tools.Util = {};

HN.Tools.Util.Keys = {};
HN.Tools.Util.Keys.DELETE = 46;
HN.Tools.Util.Keys.BACKSPACE = 8;

HN.Tools.Util.get_new_id = function()
{
  return 'id_' + new Date().getTime();
};

HN.Tools.Util.does_it_contain = function(val, classname) {
  var i;
  var array;
  array = classname.split(" ");
  for (i = 0; i < array.length; i += 1) {
    if (array[i] === val) {
      return true;
    };
  };
  return false;
};

HN.Tools.Util.keypress = function(ev) {
  if (ev.originalEvent.keyCode === HN.Tools.Util.Keys.DELETE ||
      ev.originalEvent.keyCode === HN.Tools.Util.Keys.BACKSPACE) {
      if (Register.selected) {
        $("#"+Register.selected).remove();
      }
  }
};

HN.Tools.Util.clean_drag_name = function(string) {
   var oldnamearray;
   var newnamearray =[];
   var i;
   oldnamearray= string.split(" ");
   for (i = 0; i < oldnamearray.length; i += 1) {
     if (oldnamearray[i] !== "layoutsitem"
         && oldnamearray[i] !== "elementsitem"
         && oldnamearray[i] !== "widgetssitem"
         && oldnamearray[i] !== "ui-draggable") {
           newnamearray[newnamearray.length] = oldnamearray[i];
         }
   };
   return newnamearray[0];
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

HN.Tools.Util.offset_open = function(dialog) {
  $(dialog).css("margin-left", -($(dialog).width()/2));
  $(dialog).css("margin-top", -($(dialog).height()/2) + 205);
  $(dialog).fadeIn("fast");
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

HN.Tools.Util.drop = function(e, ui) {
  var classname;
  classname = ui.draggable[0].className;
  if (HN.Tools.Util.does_it_contain("layoutsitem", classname)) {
    HN.Tools.Util.drop_layouts(e, ui);
  } else if (HN.Tools.Util.does_it_contain("elementsitem", classname)) {
    HN.Tools.Util.drop_elements(e, ui);
  } else if (HN.Tools.Util.does_it_contain("widgetsitem", classname)) {
    HN.Tools.Util.drop_widgetss(e, ui);
  } else if (HN.Tools.Util.does_it_contain("layout", classname)
             || HN.Tools.Util.does_it_contain("element", classname)
             || HN.Tools.Util.does_it_contain("widget", classname)) {
    HN.Tools.Util.drop_dropped(e, ui);
  } else {
    console.log("Erk...");
  };
};

HN.Tools.Util.drop_dropped = function(e, ui){
  var elem;
  e.target.appendChild(ui.draggable[0]);
  elem = HN.Util.id(ui.draggable[0].id);
  elem.style.left="";
  elem.style.top="";
};

HN.Tools.Util.drop_elements = function(e, ui) {
  var drag;
  var new_elem;
  var new_id;
  var html;
  var dialogid;
  var fn;
  drag = HN.Tools.Util.clean_drag_name(ui.draggable[0].className);
  new_elem = Register.elements[drag]['htmlfn']();
  new_id = HN.Tools.Util.get_new_id();
  new_elem.setAttribute("id", new_id);
  e.target.appendChild(new_elem);
  $("#" + new_id).draggable();
  dialogid = Register.elements[drag]['dialogid'];
  fn = function (ev) {Register.currentelement = new_id;
                      Register.selected = "";
                      $("#" + dialogid).trigger("focus");
                      HN.Tools.Util.dialog_open(HN.Util.id(dialogid));
                      ev.stopPropagation();};
  $("#"+new_id).bind("dblclick", fn);
  $("#"+new_id).bind("click", HN.Tools.Register.selected);
};

HN.Tools.Util.drop_widgets = function(e, ui) {
  console.log("drop widgets...");
};

HN.Tools.Util.drop_layouts = function(e, ui) {
  var drag;
  var new_elem;
  var new_id;
  var html;
  var dialogid;
  var fn;
  drag = HN.Tools.Util.clean_drag_name(ui.draggable[0].className);
  new_elem = Register.layouts[drag]['htmlfn']();
  new_id = HN.Tools.Util.get_new_id();
  new_elem.setAttribute("id", new_id);
  e.target.appendChild(new_elem);
  $("#" + new_id).draggable();
  $(".layout_target").droppable({drop: HN.Tools.Util.drop, greedy: true});

  // get another reference to the new element 'in situ'
  dialogid = Register.layouts[drag]['dialogid'];
  fn = function (ev) {Register.currentlayout = new_id;
                      Register.selected = "";
                      $("#" + dialogid).trigger("focus");
                      HN.Tools.Util.dialog_open(HN.Util.id(dialogid));
                      ev.stopPropagation();};
  $("#"+new_id).bind("dblclick", fn);
  $("#"+new_id).bind("click", HN.Tools.Register.selected);
};

HN.Tools.Util.new_layout = function(layout) {
  Register.reg("layouts", layout);
};

HN.Tools.Util.new_element = function(element) {
  Register.reg("elements", element);
};

HN.Tools.Util.new_widget = function(widget) {
  Register.reg("widgets", widget);
};

HN.Tools.Util.draw = function() {
  Register.draw("layouts");
  Register.draw("elements");
  Register.draw("widgets");
};
