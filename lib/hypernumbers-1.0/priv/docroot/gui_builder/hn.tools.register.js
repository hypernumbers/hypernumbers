/*
 * @class HN.Tools.Register
 * this class holds the register for layouts, elements and widgets
 */

HN.Tools.Register = function() {
  var currentpage;
  var fn;
  this.layouts  = [];
  this.elements = [];
  this.widgets  = [];
  this.currentlayout = "";
  this.currentelement = "";
  this.currentwidget = "";
  this.currentconfig = "";
  this.selected = "";
  this.currentbindings = "";
  this.currentpage = document.location.pathname;
  this.currentrange = "a1:j10";
  HN.Tools.Util.gen_bindings_table(this.currentpage,
                                   this.currentrange);
  this.tabset = "";
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
  // Add the config box to the html
  elem = HN.Util.id("config_boxes");
  $("#config_boxes").append(reg.configbox);

  // Now configure it
  reg.configboxfn();

  // Now set up the rest of the register
  regobjs[reg.classname] = {toolboxlable: reg.toolboxlable,
                            htmlfn:       reg.htmlfn,
                            configid:     elem.lastChild.id};
};

HN.Tools.Register.selected = function(selected) {
  Register.selected = selected;
  // removed the selected class
  $(".selected").removeClass("selected");
  $("#"+selected).addClass("selected");
};

HN.Tools.Register.show_config = function(configid) {
  var oldconfigid;
  oldconfigid = Register.currentconfig;
  Register.currentconfig = configid;
  if (oldconfigid !== "") {
    $("#" + oldconfigid).css({'display': "none"});
  }
  $("#" + configid).trigger("focus");
  $("#" + configid).css({'display': "block"});
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