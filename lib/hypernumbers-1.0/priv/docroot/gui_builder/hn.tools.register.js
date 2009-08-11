/*
 * @class HN.Tools.Register
 * this class holds the register for layouts, elements and widgets
 */

HN.Tools.Register = function() {
  this.layouts  = [];
  this.elements = [];
  this.widgets  = [];
  this.currentlayout = "";
  this.currentelement = "";
  this.currentwidget = "";
  this.selected = "";
  return this;
};

HN.Tools.Register.prototype.reg = function(type, reg) {
  var length;
  var elem;
  var regobjs;
  switch (type) {
  case "layouts":
    regobjs = this.layouts;
    break;
  case "elements":
    regobjs = this.elements;
    break;
  case "widgets":
    regobjs = this.widgets;
    break;
  }
  // Add the dialog box to the html
  elem = HN.Util.id("dialog_boxes");
  $("#dialog_boxes").append(reg.dialogbox);

  // Now configure it
  reg.dialogfn();

  // Now set up the rest of the register
  regobjs[reg.classname] = {toolboxlable: reg.toolboxlable,
                            htmlfn:       reg.htmlfn,
                            dialogid:     elem.lastChild.id};
};

HN.Tools.Register.selected = function(ev) {
  var selected;
  selected = ev.currentTarget.id;
  Register.selected = selected;
  // removed the selected class
  $(".selected").removeClass("selected");
  $("#"+selected).addClass("selected");
  ev.stopPropagation();
};

HN.Tools.Register.prototype.draw = function(type) {
  var name;
  var elem;
  var html = "";
  var id;
  var regobjs;
  var classname;
  switch (type) {
  case "layouts":
    regobjs = this.layouts;
    id = HN.Util.id('layoutslist');
    classname = "layoutsitem";
    break;
  case "elements":
    regobjs = this.elements;
    id = HN.Util.id('elementslist');
    classname = "elementsitem";
    break;
  case "widgets":
    regobjs = this.widgets;
    id = HN.Util.id('widgetslist');
    classname = "widgetssitem";
    break;
  }
  for (name in regobjs) {
    if (typeof regobjs[name] !== 'function') {
    elem = "<li><div class=\"" + classname + " " + name + "\">"
      + regobjs[name]['toolboxlable'] + "</div></li>";
    html += elem;
    }
  }
  id.innerHTML = html;
};