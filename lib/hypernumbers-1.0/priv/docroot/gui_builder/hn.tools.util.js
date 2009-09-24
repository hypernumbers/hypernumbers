HN.Tools.Util = {};

HN.Tools.Util.Keys = {};
HN.Tools.Util.Keys.DELETE = 46;
HN.Tools.Util.Keys.BACKSPACE = 8;

HN.Tools.Util.does_it_contain = function(val, classname) {
  var i;
  if (classname) {
    var array;
    array = classname.split(" ");
    for (i = 0; i < array.length; i += 1) {
      if (array[i] === val) {
        return true;
      };
    };
  };
  return false;
};

HN.Tools.Util.deselect_everything = function(ev) {
    Register.selected="";
    // Double highlighting of selected classes
    $(".ui-state-highlight").removeClass("ui-state-highlight");
    $(".selected").removeClass("selected");
    // this just makes the tabbed dialog box show hee-haw...
    HN.Tools.Register.show_config("");
};

HN.Tools.Util.keypress = function(ev) {
  if (ev.originalEvent.keyCode === HN.Tools.Util.Keys.DELETE ||
      ev.originalEvent.keyCode === HN.Tools.Util.Keys.BACKSPACE) {
      if (Register.selected
          && ev.originalTarget.tagName !== "INPUT"
          && Register.selected !== "canvas") {
        $("#" + Register.selected).remove();
        $("#" + Register.currentconfig).hide();
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
         && oldnamearray[i] !== "widgetsitem"
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

HN.Tools.Util.show_dialog = function(dialog) {
  HN.Tools.Util.dialog_open(HN.Util.id(dialog));
};

HN.Tools.Util.show = function(id)
{
  var element;
  element = HN.Util.id(id);
  element.style.display ="block";
};

HN.Tools.Util.drop = function(e, ui) {
  var classname;
  classname = ui.draggable[0].className;
  if (HN.Tools.Util.does_it_contain("layoutsitem", classname)) {
    HN.Tools.Util.drop_layouts(e, ui);
  } else if (HN.Tools.Util.does_it_contain("elementsitem", classname)) {
    HN.Tools.Util.drop_elements(e, ui);
  } else if (HN.Tools.Util.does_it_contain("widgetsitem", classname)) {
    HN.Tools.Util.drop_widgets(e, ui);
  } else if (HN.Tools.Util.does_it_contain("layout", classname)
             || HN.Tools.Util.does_it_contain("element", classname)
             || HN.Tools.Util.does_it_contain("widget", classname)) {
    HN.Tools.Util.drop_dropped(e, ui);
  } else {
    console.log("classname is " + ui.draggable[0].className);
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
  drag = HN.Tools.Util.clean_drag_name(ui.draggable[0].className);
  new_elem = Register.elements[drag]['htmlfn']();
  new_id = HN.Util.get_new_id();
  new_elem.setAttribute("id", new_id);
  new_elem.setAttribute("configid", drag);
  e.target.appendChild(new_elem);
  HN.Tools.Util.set_up_element(new_id, drag);
  HN.Tools.Util.set_gridlines(new_id);
};

HN.Tools.Util.set_up_element = function(id, name) {
  var config_id;
  $("#" + id).draggable();
  config_id = Register.elements[name]['configid'];
  $("#" + id).bind("click", HN.Tools.Util.click_on_element(config_id));
  $("#" + id).bind("dblclick", HN.Tools.Util.pop_config_tab);
  HN.Tools.Register.selected(id);
  // Now call the after create fn on it
  Register.elements[name]['aftercreatefn'](id);
};

HN.Tools.Util.drop_widgets = function(e, ui) {
  console.log("needs work...");
  var drag;
  var new_elem;
  var new_id;
  var html;
  drag = HN.Tools.Util.clean_drag_name(ui.draggable[0].className);
  new_elem = Register.widgets[drag]['htmlfn']();
  new_id = HN.Util.get_new_id();
  new_elem.setAttribute("id", new_id);
  e.target.appendChild(new_elem);
  new_elem.setAttribute("configid", drag);
  $(".layout_target").droppable({drop: HN.Tools.Util.drop, greedy: true});
  HN.Tools.Util.set_up_widget(new_id, drag);
  HN.Tools.Util.set_gridlines(new_id);
};

HN.Tools.Util.set_up_widget = function(id, name) {
  var config_id;
  $("#" + id).draggable();
  // get another reference to the new element 'in situ'
  config_id = Register.widgets[name]['configid'];
  $("#" + id).bind("click", HN.Tools.Util.click_on_widget(config_id));
  $("#" + id).bind("dblclick", HN.Tools.Util.pop_config_tab);
  HN.Tools.Register.selected(id);
  // Now call the after create fn on it
  Register.widgets[name]['aftercreatefn'](id);
};

HN.Tools.Util.drop_layouts = function(e, ui) {
  var drag;
  var new_elem;
  var new_id;
  var html;
  drag = HN.Tools.Util.clean_drag_name(ui.draggable[0].className);
  new_elem = Register.layouts[drag]['htmlfn']();
  new_id = HN.Util.get_new_id();
  new_elem.setAttribute("id", new_id);
  e.target.appendChild(new_elem);
  new_elem.setAttribute("configid", drag);
  $(".layout_target").droppable({drop: HN.Tools.Util.drop, greedy: true});
  HN.Tools.Util.set_up_layout(new_id, drag);
  HN.Tools.Util.set_gridlines(new_id);
};

HN.Tools.Util.set_up_layout = function(id, name) {
  var config_id;
  $("#" + id).draggable();
  // get another reference to the new element 'in situ'
  config_id = Register.layouts[name]['configid'];
  $("#" + id).bind("click", HN.Tools.Util.click_on_layout(config_id));
  $("#" + id).bind("dblclick", HN.Tools.Util.pop_config_tab);
  HN.Tools.Register.selected(id);
  // Now call the after create fn on it
  Register.layouts[name]['aftercreatefn'](id);
};

HN.Tools.Util.new_layout = function(layout) {
  Register.reg("layouts", layout);
};

HN.Tools.Util.new_widget = function(widget) {
  Register.reg("widgets", widget);
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

HN.Tools.Util.set_selection = function(select, value) {
  var i;
  for (i = 0; i < select.length; i += 1) {
    if (select[i].value === value) {
      select[i].selected = true;
    }
  }
};

HN.Tools.Util.set_thickness = function(id, new_thickness) {
  $("#" + id).removeClass("hn_" + HN.Tools.Util.get_thickness(id));
  $("#" + id).addClass("hn_" + new_thickness);
};


HN.Tools.Util.get_thickness = function(id) {
  var bits;
  var i;
  bits = HN.Util.id(id).className.split(" ");
  for (i = 0; i < bits.length; i += 1) {
    switch (bits[i])
    {
      case "hn_verythin":
        return "verythin";
      case "hn_thin":
        return "thin";
      case "hn_normal":
        return "normal";
      case "hn_thick":
        return "thick";
      case "hn_verythick":
        return "verythick";
    }
  };
  return "normal";
};

HN.Tools.Util.set_size = function(id, new_size) {
  $("#" + id).removeClass("hn_" + HN.Tools.Util.get_size(id));
  $("#" + id).addClass("hn_" + new_size);
};

HN.Tools.Util.get_size = function(id) {
  return HN.Tools.Util.extract_size(HN.Util.id(id).className);
};

HN.Tools.Util.extract_size = function(classname) {
  var bits;
  var i;
  bits = classname.split(" ");
  for (i = 0; i < bits.length; i += 1) {
    switch (bits[i])
    {
      case "hn_smallest":
        return "smallest";
      case "hn_small":
        return "small";
      case "hn_medium":
        return "medium";
      case "hn_large":
        return "large";
      case "hn_largest":
        return "largest";
    }
  }
  return "medium";
};

HN.Tools.Util.get_path_from_url = function(url) {
  var bits;
  var patharray;
  var i;
  var path;
  bits = url.split("/");
  patharray = bits.slice(3, bits.length -1);
  path = "/";
  for (i = 0; i <= patharray.length - 1; i += 1) {
    path += patharray[i] + "/";
  }
  return path;
};

HN.Tools.Util.new_bindings_page = function() {
  var page;
  var value;
  var full_path;
  var new_full_path;
  var new_page;
  var options;
  page = HN.Util.id("newpage");
  value = page.value;
  new_full_path = HN.Util.id("newpage_full_path");
  // synchronise the full_path check box on the preview
  full_path = HN.Util.id("bindings_full_path");
  full_path.checked = new_full_path.checked;
  // all paths should end in a slash - make it so
  if (value[value.length - 1] !== "/") {
    value += "/";
  }
  if (new_full_path.checked) {
    // a full path should start with a '/' - if not make it so
    if (value[0] === "/") {
      new_page = document.location.protocol
                   + "//" + document.location.host
                   + value;
    } else {
      new_page = document.location.protocol
                   + "//" + document.location.host
                   + "/" + value;
    }
  } else {
    // a relative path should start with "./" or "../"
    // make it so
    if (value[0] === ".") {
      new_page = HN.Gui.Register.compress_url(value.split("/"));
    } else {
      value = document.location.protocol
        + "//" + document.location.host
        + "./" + value;
      new_page = HN.Gui.Register.compress_url(value.split("/"));
    }
  }
  // Now create a new bindings table
  Register.add_bindings_table(HN.Tools.Util.get_path_from_url(new_page));
  // force a reregistration
  HNGui.reregister();
  HN.Tools.Util.dialog_close("#newpagedialog");
  // now force a redraw of the bindings dialog box
  // but first change the current page
  Register.currentpage = HN.Tools.Util.get_path_from_url(new_page);
  HN.Tools.Util.configure_paths();
  // finally clean out the dialog box
  page.value = "";
};

HN.Tools.Util.configure_paths = function() {
  var pageelem;
  var bindings_page;
  var bindings_full_path;
  var new_full_path;
  var path1;
  var path2;
  var path2a;
  var html;
  var selectedtext;
  var fn;
  pageelem = HN.Util.id("page_selector");
  bindings_page = HN.Util.id("bindings_page");
  bindings_full_path = HN.Util.id("bindings_full_path");
  new_full_path = HN.Util.id("newpage_full_path");
  // synchronise the 2 full path checkboxes
  new_full_path.checked = bindings_full_path.checked;
  html = "<select id=\"binding_pages\" class=\"ui-state-default\">";
  for (page in HNGui.register.pages) {
    if (typeof page !== 'function') {
      path1 = HN.Tools.Util.get_path_from_url(page);
      if (HN.Tools.Util.get_path_from_url(page) === Register.currentpage) {
        selectedtext = " selected ";
      } else {
        selectedtext ="";
      }
      if (bindings_full_path.checked) {
        html += "<option value=\"" + path1 + "\"" + selectedtext + ">"
          + path1 + "</option>";
      } else {
        path2=HN.Tools.Util.make_relative(path1);
        path2a = HN.Tools.Util.pad(path2);
        html += "<option value=\"" + path2 + "\"" + selectedtext + ">"
          + path2a + "</option>";
      }
    }
  }
  html += "<option value=\"9999\">or bind another page...</option>";
  html += "</select>";
  pageelem.innerHTML = html;
  // now set up the functions on it
  fn = function(ev) {
    // the add additional pages value is the last one
    // so check for that one (rembering that the index
    // starts at 0 not 1)
    if ((ev.target.selectedIndex + 1) === ev.target.options.length) {
      HN.Tools.Util.show_dialog("newpagedialog");
    } else {
      Register.currentpage = ev.target.options[ev.target.selectedIndex].value;
      $("#binding_pages").bind("change", fn);
      // hide all bindings pages
      $(".bindings_table").hide();
      // show the bindings page for the current page
      $("#" + Register.bindings_table[Register.currentpage]).show();
    }
  };
  $("#binding_pages").bind("change", fn);
  // hide all bindings pages
  $(".bindings_table").hide();
  // show the bindings page for the current page
  $("#" + Register.bindings_table[Register.currentpage]).show();
};

HN.Tools.Util.make_relative = function(page) {
  var currentpage;
  var c_bits;
  var c_len;
  var p_bits;
  var p_len;
  var i;
  var j;
  var ret = "";
  var prefix;
  currentpage = document.location.pathname;
  if (currentpage === page) {
    return "./";
  } else {
    // first break up the paths
    c_bits = currentpage.split("/");
    p_bits = page.split("/");
    // each array starts and ends with a ghost blank element
    // trim them off
    c_bits = c_bits.slice(1, c_bits.length - 1);
    p_bits = p_bits.slice(1, p_bits.length - 1);
    c_len = c_bits.length;
    p_len = p_bits.length;
    for (i = 0; i <= p_len; i += 1) {
      if (!c_bits[i]) {
        return "./" + p_bits.slice(i, p_len);
        } else if (c_bits[i] !== p_bits[i]) {
          for (j = 0; j < p_len; j++) {
            ret += "/" + p_bits[j];
          }
          prefix = HN.Tools.Util.repeat_string("../", c_len - i);
          ret = prefix + ret.slice(1, ret.length) + "/";
          return ret;
      }
    }
  }
};

HN.Tools.Util.pad = function(string) {
  var length;
  var diff;
  var pad;
  var padding;
  length = string.length;
  if (length < 10) {
    diff = parseInt(10 - length);
    pad = "&nbsp;";
    padding = HN.Tools.Util.repeat_string(pad, diff);
    return string + padding;
    } else {
      return string;
  }
};

HN.Tools.Util.repeat_string = function(s, n) {
  var ret = "";
  for (var i = 0; i < n; i++)
    ret += s;
  return ret;
};

HN.Tools.Util.parse_path_and_ref = function(value) {
  var bits;
  var path;
  var ref;
  bits = value.split("/");
  if (bits.length === 1) {
    return {'path': "./", 'ref': bits[0]};
  } else {
    path = bits.slice(0, bits.length - 2).join("/");
    ref = bits[bits.length - 1];
    return {'path': path, 'ref': ref};
  }
};

HN.Tools.Util.make_page = function() {
  var page;
  var cover;
  var head;
  var headhtml = "";
  var length;
  var i;
  var scripts1;
  var scripts2;
  var ret;
  page = document.createElement("div");
  cover = document.createElement("div");
  cover.setAttribute("id", "cover");
  // bodge because of round tripping via XML...
  // Must force the div not to be self-closing...
  // (ie <div id="blah" /> is not valid...
  cover.innerHTML = " ";
  page.appendChild(HN.Util.id("canvas").cloneNode(true));
  page.insertBefore(cover, page.firstChild);
  HN.Tools.Util.clean_up(page);
  head = document.getElementsByTagName("head")[0];
  length = head.childNodes.length;
  var copy = document.location.protocol + "//"
    + document.location.host
    + "/gui_builder/gui_builder.css";
  for (i = 0; i < length; i++) {
    if (head.childNodes[i].tagName === "META") {
      headhtml += (new XMLSerializer()).serializeToString(head.childNodes[i]);
    } else if (head.childNodes[i].tagName === "LINK") {
      // skip the gui builder css
      if (head.childNodes[i].href !== copy) {
        headhtml += (new XMLSerializer()).serializeToString(head.childNodes[i]);
      }
    }
  }
  scripts1 = document.createElement("div");
  scripts1.innerHTML = HN.Util.id("scripts1").innerHTML;
  scripts2 = document.createElement("div");
  scripts2.innerHTML = HN.Util.id("scripts2").innerHTML;
  ret = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
    + "<html>\n"
    + "  <head>"
    + headhtml
    + "  </head>\n"
    + "  <body>\n"
    // + page.innerHTML + "\n"
    + (new XMLSerializer()).serializeToString(page) + "\n"
    + "    <div id=\"scripts\">\n"
    + scripts1.innerHTML
    + "    </div>\n"
    + "  <script src=\"/gui_builder/main.js\"></script>"
    + scripts2.innerHTML
    + "  <script src=\"/gui_builder/maindraw.js\"></script>"
    + "  </body>\n"
    + "</html>";
  return ret;
};

HN.Tools.Util.clean_up = function(node) {
  // If the node is a hypernumbers element then
  // * delete its content (dont worry these will be put back in
  //                       dynamically by the libraries at run time)
  // strip out its id
  if (HN.Tools.Util.is_hn_element(node)) {
    node.innerHTML = "";
    node.removeAttribute("id");
  };
  // If the node is of class layout also strip it of its id
  if (HN.Tools.Util.does_it_contain("layout", node.className)) {
    node.removeAttribute("id");
  };
  // ditto widgets
  if (HN.Tools.Util.does_it_contain("widget", node.className)) {
    node.removeAttribute("id");
  };
  // delete any inline styles
  if (node.style) {
    node.removeAttribute("style");
  };
  HN.Tools.Util.clean_class_name(node);
  var i;
  for(i = 0; i < node.childNodes.length; i++)
  {
    HN.Tools.Util.clean_up(node.childNodes[i]);
  }
};

HN.Tools.Util.is_hn_element = function(node) {
  if (node.getAttribute && node.getAttribute("data-gui-element-type")) {
    return true;
  } else {
    return false;
  }
};

HN.Tools.Util.clean_class_name = function(node) {
  var bits;
  var new_class = [];
  var i;
  var len;
  var new_class2;
  if (node.className) {
    bits = node.className.split(" ");
    for (i = 0; i < bits.length; i++) {
      if (bits[i] !== "ui-draggable"
          && bits[i] !== "ui-droppable"
          && bits[i] !== "ui-state-highlight"
          && bits[i] !== "gridlines"
          && bits[i] !== "selected") {
          // && bits[i] !== "layout"
          // && bits[i] !== "layout_target") {
        len = new_class.length;
        new_class[len] =  bits[i];
      };
    }
    if (new_class.length === 0) {
      node.removeAttribute("class");
    } else {
      new_class2 = new_class.join(" ");
      node.setAttribute("class", new_class2);
    }
  }
};

HN.Tools.Util.save_page = function() {
  var name;
  var newpage;
  var json;
  var page;
  // close the dialog box
  HN.Tools.Util.dialog_close("#savedialog");
  name = HN.Util.id("save_name").value;
  newpage = HN.Tools.Util.make_page();
  json = {save_gui: {name: name,
                     form: newpage}};
  page = document.location.protocol + "//"
    + document.location.host
    + document.location.pathname;
  $.post(page, JSON.stringify(json));
  // Now re-make the menus
  // (might need a small delay...)
  Register.load_guis();
};

HN.Tools.Util.pop_config_tab = function(ev) {
  // Jump to the config tab set
  Register.tabset.tabs('select', 2);
  ev.stopPropagation();
};

HN.Tools.Util.click_on_element = function(config_id) {
  return function(ev) {
    Register.currentelement = ev.currentTarget.id;
    HN.Tools.Register.selected(ev.currentTarget.id);
    HN.Tools.Register.show_config(config_id);
    $("#" + config_id).css({'display': "block"});
    ev.stopPropagation();
  };
};

HN.Tools.Util.click_on_layout = function(config_id) {
  return function(ev) {
    Register.currentlayout = ev.currentTarget.id;
    HN.Tools.Register.selected(ev.currentTarget.id);
    HN.Tools.Register.show_config(config_id);
    ev.stopPropagation();
  };
};

HN.Tools.Util.click_on_widget = function(config_id) {
  return function(ev) {
    Register.currentwidget = ev.currentTarget.id;
    HN.Tools.Register.selected(ev.currentTarget.id);
    HN.Tools.Register.show_config(config_id);
    ev.stopPropagation();
  };
};

HN.Tools.Util.set_gridlines = function(new_id) {
  var elem;
  elem = HN.Util.id("grid_lines");
  if (elem.checked) {
    $("#" + new_id).addClass("gridlines");
  } else {
    $("#" + new_id).removeClass("gridlines");
  }
};

HN.Tools.Util.toggle_gridlines = function(ev) {
  var elem;
  elem = HN.Util.id("grid_lines");
  if (elem.checked) {
    $(".layout").addClass("gridlines");
    $(".element").addClass("gridlines");
    $(".widget").addClass("gridlines");
  } else {
    $(".layout").removeClass("gridlines");
    $(".element").removeClass("gridlines");
    $(".widget").removeClass("gridlines");
  }
};

HN.Tools.Util.set_up_canvas = function() {
  var fn1;
  $(".canvas").droppable({drop: HN.Tools.Util.drop, greedy: true});
  // Configure the page on click
  // bind the page config box to the canvas item
  fn1 = function(ev) {
    Register.tabset.tabs('select', 2);
    HN.Tools.Register.selected("canvas");
    HN.Tools.Register.show_config("page_sizes");
    ev.stopPropagation();
  };
  $("#canvas").bind("dblclick", fn1);
};

HN.Tools.Util.get_user = function() {
  var page;
  // sort this out for later...
  page = document.location.protocol + "//"
    + document.location.host
    + document.location.pathname;
  return HNGui.register.pages[page].data.user;
};

HN.Tools.Util.runafterconfigs = function() {
  var elements;
  var configid1;
  var layouts;
  var new_id1;
  var configid2;
  var widgets;
  var new_id2;
  var configid3;
  var i;

  elements = document.getElementsByClassName("element");
  for (i = 0; i < elements.length; i++) {
    configid1 = elements[i].getAttribute("configid");
    Register.elements[configid1]['aftercreatefn'](Register.elements[configid1].id);
  };

  layouts = document.getElementsByClassName("layout");
  for (i = 0; i < layouts.length; i++) {
    configid2 = layouts[i].getAttribute("configid");
    Register.layouts[configid2]['aftercreatefn'](Register.layouts[configid2].id);
  };

  widgets = document.getElementsByClassName("widget");
  for (i = 0; i < widgets.length; i++) {
    configid3 = widgets[i].getAttribute("configid");
    Register.widgets[configid3]['aftercreatefn'](Register.widgets[configid3].id);
  };

};

HN.Tools.Util.is_integer = function(string) {
  var i;
  if (HN.Tools.Util.is_empty(string)) {
    if (HN.Tools.Util.is_integer.arguments.length == 1) {
      return 0;
    } else {
      return (HN.Tools.Util.is_integer.arguments[1] == true);
    };
  };
  for (i = 0; i < string.length; i++) {
    var c = string.charAt(i);
    if (!HN.Tools.Util.is_digit(c)) {
      return false;
    };
  };
  return true;
};

HN.Tools.Util.is_empty = function(string) {
  return ((string == null) || (string.length == 0));
};

HN.Tools.Util.is_digit =function(c) {
  return ((c >= "0") && (c <= "9"));
};

HN.Tools.Util.change_bindings_range = function(ev) {
  Register.currentrange = ev.target.value;
  HN.Tools.Util.gen_bindings_table(Register.currentpage,
                                   ev.target.value);
  HN.Gui.Register.reregister("gui_builder");
};

HN.Tools.Util.show_bindings_table = function(ev) {
  // Unhighlight the old one
  if (Register.currentbindings !== "") {
      $("#" + Register.currentbindings).removeClass("highlight");
  };
  Register.currentbindings = ev.currentTarget.id;
  // Highlight the new one
  $("#" + Register.currentbindings).addClass("highlight");
  $("#bindings").show();
  $("#" + Register.bindings_table[Register.currentpage]).show();
  ev.stopPropagation();
};

HN.Tools.Util.hide_bindings_table = function(ev) {
  $("#bindings").hide();
};

HN.Tools.Util.minimize_bindings_table = function(ev) {
  $("#insert_bindings").hide();
  $(".bindings_minimize").unbind("click",
                                 HN.Tools.Elements.Core.minimize_bindings_table);
  $(".bindings_minimize").addClass("bindings_maximize");
  $(".bindings_maximize").bind("click",
                               HN.Tools.Elements.Core.maximize_bindings_table);
  $(".bindings_minimze").removeClass("bindings_minimize");
};

HN.Tools.Util.maximize_bindings_table = function(ev) {
  $("#insert_bindings").show();
  $(".bindings_maximize").unbind("click",
                                 HN.Tools.Elements.Core.maximize_bindings_table);
  $(".bindings_maximize").addClass("bindings_minimize");
  $(".bindings_minimize").bind("click",
                               HN.Tools.Elements.Core.minimize_bindings_table);
  $(".bindings_maximze").removeClass("bindings_maximize");
};

HN.Tools.Util.expand_ref = function(binding) {
  var type;
  var ref;
  var i;
  var j;
  var ret = [];
  type = HN.Gui.Register.get_type(binding);
  switch (type) {
    case "cell":
      ret[0] = binding;
      return ret;
    case "range":
      ref = HN.Util.parse_range(binding.toUpperCase());
      for (i = ref.x1; i < ref.x2 + 1; i++) {
        for (j = ref.y1; j < ref.y2 + 1; j++) {
          ret[ret.length] = HN.Util.to_b26(i) + j;
        }
      }
      return ret;
    default:
      return [];
  }
};