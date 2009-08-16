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
  this.bindings_table = [];
  this.currentlayout = "";
  this.currentelement = "";
  this.currentwidget = "";
  this.currentconfig = "";
  this.selected = "";
  this.currentbindings = "";
  this.currentpage = document.location.pathname;
  this.currentrange = "a1:j10";
  // now register the bindings table
  this.add_bindings_table(this.currentpage);
  this.tabset = "";
  return this;
};

HN.Tools.Register.prototype.gen_bindings_table = function(page, range)
{
  var size;
  var table_id;
  var table;
  var row;
  var cell;
  var i;
  var j;
  var elem;
  var oldtable;
  size = HN.Util.parse_range(range.toUpperCase());
  table_id = HN.Tools.Util.get_new_id();
  table = document.createElement("table"),
  table.setAttribute("id", table_id);
  table.setAttribute("class", "bindings_table");
  row = table.insertRow(-1);
  row.insertCell(-1);
  for (i = size.x1; i <= size.x2; i += 1) {
    cell = row.insertCell(-1);
    cell.setAttribute("id", "head_" + i);
    cell.setAttribute("class", "bindings_header");
    cell.innerHTML = HN.Util.to_b26(i);
  };
  for (j = size.y1; j <= size.y2; j += 1) {
    row = table.insertRow(-1);
    cell = row.insertCell(-1);
    cell.setAttribute("class", "bindings_header");
    cell.innerHTML = j;
    for (i = size.x1; i <= size.x2; i += 1) {
      cell = row.insertCell(-1);
      cell.setAttribute("id", "binding_" +HN.Util.to_b26(i) + j);
      cell.innerHTML = "<div id=\"" + HN.Tools.Util.get_new_id()
        + "\" class=\"hn\" data-gui-element-type=\"inputtext\""
        + " data-render-type=\"both\" data-binding=\""
        + page + HN.Util.to_b26(i) + j + " />";
  };
  }
  elem = HN.Util.id("insert_bindings");
  elem.appendChild(table);
  return table_id;
};

HN.Tools.Register.prototype.add_bindings_table = function(page) {
  console.log("in add_bindings_table");
  console.log(this.bindings_table);
  var table_id;
  this.currentpage = page;
  table_id = this.gen_bindings_table(this.currentpage, this.currentrange);
  this.bindings_table[this.currentpage] = table_id;
  console.log(this.bindings_table);
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