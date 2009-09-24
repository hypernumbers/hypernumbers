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
  return this;
};

HN.Tools.Register.prototype.load_guis = function() {
  var that = this;
  $.ajax({url: document.location.pathname + "?guis",
          dataType: "json",
          success: function(data) {
            that.guis = data;
            that.make_menus();
            }
         });
};

HN.Tools.Register.prototype.make_menus = function() {
  var opts;
  // First up make the various menus
  this.make_file_menus();
    opts = {
    callback : function(el) {
      return Toolbar.menuItemPicked(el);
    }
  };
  $("#menu").filemenu(opts);

};

HN.Tools.Register.prototype.make_file_menus = function() {
  var elem1;
  var elem2;
  var html1;
  var html2;
  var arrow;
  var i;
  elem1 = HN.Util.id("gui_open");
  elem2 = HN.Util.id("gui_view");
  if (this.guis.length !== 0) {
    html1 = "<ul class=\"parent\">";
    html2 = html1;
    for (index in this.guis) {
      html1 += "<li><a class=\"parent hover\" name=\"open\">"
        + this.guis[index].file + "</a></li>";
      html2 += "<li><a href=\"./?gui=" + Register.guis[index].path
        + this.guis[index].file
        + "\"class=\"parent hover\" name=\"view\" target=\"_blank\">"
        + this.guis[index].file + "</a></li>";
    }
    html1 += "</ul>";
    html2 += "</ul>";
    arrow = "<div class=\"arrow\"></div>";
    elem1.innerHTML = "<a name=\"ignore\">" + elem1.childNodes[0].textContent
      + arrow + "</a>" + html1;
    elem2.innerHTML = "<a name=\"ignore\">" + elem2.childNodes[0].textContent
      + arrow + "</a>" + html2;
  } else {
    elem1.innerHTML = "<a name=\"ignore\" class=\"disabled\">("
      + elem1.childNodes[0].text + ")</a>";
    elem2.innerHTML = "<a name=\"ignore\" class=\"disabled\">("
      + elem2.childNodes[0].text + ")</a>";
  }
};

HN.Tools.Register.prototype.gen_bindings_table = function(page, range)
{
  var size;
  var table_id;
  var table;
  var row;
  var cell;
  var width;
  var i;
  var j;
  var elem;
  var oldtable;
  size = HN.Util.parse_range(range.toUpperCase());
  table_id = HN.Util.get_new_id();
  table = document.createElement("table"),
  table.setAttribute("id", table_id);
  table.setAttribute("class", "bindings_table");
  row = table.insertRow(-1);
  row.insertCell(-1);
  width = 100/(size.x2 - size.x1 + 1);
  for (i = size.x1; i <= size.x2; i += 1) {
    cell = row.insertCell(-1);
    cell.setAttribute("id", "head_" + i);
    cell.setAttribute("class", "ui-widget-header bindings_header");
    cell.setAttribute("width", width + "%");
    cell.innerHTML = "<h6>" + HN.Util.to_b26(i) + "</h6>";
  };
  for (j = size.y1; j <= size.y2; j += 1) {
    row = table.insertRow(-1);
    cell = row.insertCell(-1);
    cell.setAttribute("class", "ui-widget-header bindings_header");
    cell.innerHTML = "<h6>" + j + "</h6>";
    for (i = size.x1; i <= size.x2; i += 1) {
      cell = row.insertCell(-1);
      cell.setAttribute("id", "binding_" +HN.Util.to_b26(i) + j);
      cell.setAttribute("class", "bindings_cell");
      cell.innerHTML = "<div id=\"" + HN.Util.get_new_id()
        + "\" class=\"hn\" data-gui-element-type=\"dblclick_block\""
        + " data-render-type=\"both\" data-binding=\""
        + page + HN.Util.to_b26(i) + j + " />";
  };
  }
  elem = HN.Util.id("insert_bindings");
  elem.appendChild(table);
  // now bind the selection functions to it
  $(".bindings_cell").bind("mousedown", HN.Tools.Register.bindings_select_start);
  $(".bindings_cell").bind("mouseup", HN.Tools.Register.bindings_select_stop);
  return table_id;
};

HN.Tools.Register.bindings_select_start = function(ev) {
  Register.startselection = HN.Tools.Register.snip_binding(ev.currentTarget.id);
  // Switch on the mousemove stuff
  $(".bindings_cell").bind("mouseover", HN.Tools.Register.bindings_select_move);
  HN.Tools.Register.show_selection(ev);
};

HN.Tools.Register.bindings_select_move = function(ev) {
  HN.Tools.Register.show_selection(ev);
};

HN.Tools.Register.bindings_select_stop = function(ev) {
  HN.Tools.Register.show_selection(ev);
  // Switch off the mousemove stuff
  $(".bindings_cell").unbind("mouseover", HN.Tools.Register.bindings_select_move);
};

HN.Tools.Register.show_selection = function(ev) {
  var elem1;
  var elem2;
  var path;
  Register.endselection = HN.Tools.Register.snip_binding(ev.currentTarget.id);
  elem1 = HN.Util.id(Register.currentbindings);
  elem2 = HN.Util.id("binding_pages");
  if (elem2.value === "./") {
    path = "";
  } else {
    path = elem2.value;
  }
  if (Register.startselection === Register.endselection) {
    elem1.value = path + Register.startselection;
  } else {
    elem1.value = path + HN.Tools.Register.get_rectified_selected();
  }
  // now fire the onchange event on the element
  $("#" + Register.currentbindings).trigger("change");
  HN.Tools.Register.mark_selection();
};

HN.Tools.Register.get_rectified_selected = function() {
  var start;
  var end;
  var newstart = {};
  var newend = {};
  start = HN.Util.parse_ref(Register.startselection);
  end = HN.Util.parse_ref(Register.endselection);
  if (start.x > end.x) {
    newstart["x"] = end.x;
    newend["x"] = start.x;
  } else {
    newend["x"] = end.x;
    newstart["x"] = start.x;
  }
  if (start.y > end.y) {
    newstart["y"] = end.y;
    newend["y"] = start.y;
  } else {
    newend["y"] = end.y;
    newstart["y"] = start.y;
  }
  return HN.Util.coord_to_ref(newstart) + ":" + HN.Util.coord_to_ref(newend);
};

HN.Tools.Register.clear_selection = function() {
  $(".bindings_selected").removeClass("bindings_selected");
};

HN.Tools.Register.mark_selection = function() {
  HN.Tools.Register.clear_selection();
  var start;
  var end;
  var mini;
  var maxi;
  var minj;
  var maxj;
  var i;
  var j;
  var id;
  var elem;
  start = HN.Util.parse_ref(Register.startselection);
  end = HN.Util.parse_ref(Register.endselection);
  if (start.y > end.y) {
    mini = end.y;
    maxi = start.y;
  } else {
    mini = start.y;
    maxi = end.y;
  }
  if (start.x > end.x) {
    minj = end.x;
    maxj = start.x;
  } else {
    minj = start.x;
    maxj = end.x;
  }
  for (i = mini; i <= maxi; i++) {
    for (j = minj; j <= maxj; j++) {
      id = "binding_" + HN.Util.coord_to_ref({x: j, y: i});
      HN.Util.id(id).setAttribute("class", "bindings_selected");
    }
  }
};

HN.Tools.Register.snip_binding = function(binding) {
  var bits;
  bits = binding.split("_");
  return bits[1];
};

HN.Tools.Register.prototype.add_bindings_table = function(page) {
  var table_id;
  this.currentpage = page;
  table_id = this.gen_bindings_table(this.currentpage, this.currentrange);
  this.bindings_table[this.currentpage] = table_id;
};

HN.Tools.Register.prototype.configure_for_gui_builder = function() {
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
  this.startselection = "";
  this.endselection = "";
  this.guis = [];
  this.load_guis();
  HN.Tools.Register.configure1(this.layouts);
  HN.Tools.Register.configure1(this.elements);
  HN.Tools.Register.configure1(this.widgets);
};

HN.Tools.Register.configure1 = function(objects){
  var thing;
  var elem;
  for (thing in objects) {
    // Add the config box to the html
    elem = HN.Util.id("config_boxes");
    $("#config_boxes").append(objects[thing].configbox);

    // Now configure it
    objects[thing].configboxfn();

    // Now set up the rest of the register
    objects[thing].configid = elem.lastChild.id;
  }
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

  // Now set up the rest of the register
  regobjs[reg.classname] = {toolboxlable:  reg.toolboxlable,
                            htmlfn:        reg.htmlfn,
                            helptext:      reg.helptext,
                            aftercreatefn: reg.aftercreatefn,
                            configbox:     reg.configbox,
                            configboxfn:   reg.configboxfn};
};

HN.Tools.Register.selected = function(selected) {
  Register.selected = selected;
  // removed the selected classes
  // There is one class for the theme roller
  // and one for the gui builder
  $(".ui-state-highlight").removeClass("ui-state-highlight");
  $(".selected").removeClass("selected");
  $("#"+selected).addClass("ui-state-highlight");
  $("#"+selected).addClass("selected");
};

HN.Tools.Register.show_config = function(configid) {
  var oldconfigid;
  oldconfigid = Register.currentconfig;
  Register.currentconfig = configid;
  if (oldconfigid !== "") {
    $("#" + oldconfigid).css({'display': "none"});
  }
  if (configid !== "") {
    $("#" + configid).trigger("focus");
    $("#" + configid).css({'display': "block"});
  };
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
    classname = "widgetsitem";
    break;
  }
  for (name in regobjs) {
    if (typeof regobjs[name] !== 'function') {
    elem = "<li><table><tr><td><div class=\"" + classname + " " + name + "\">"
      + regobjs[name]['toolboxlable'] +"</div></td>"
        + "<td width=\"10%\"><span class=\"help\" onClick=\"HN.Tools.Register.pop_help(\'" + regobjs[name]['helptext'] +"\');\">(?)</span></td></tr>"
      + "</table></li>";
    html += elem;
    }
  }
  id.innerHTML = html;
};

HN.Tools.Register.pop_help = function(string) {
  var elem1;
  var elem2
  elem1 = HN.Util.id("floating_help_contents");
  elem1.innerHTML = string;
  elem2 = HN.Util.id("floatinghelpbox");
  elem2.style.display = "block";
};

HN.Tools.Register.close_help = function() {
  var elem;
  elem = HN.Util.id("floatinghelpbox");
  elem.style.display = "none";
}