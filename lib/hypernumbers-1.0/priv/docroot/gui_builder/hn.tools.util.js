HN.Tools.Util = {};

HN.Tools.Util.max_id = -1;

HN.Tools.Util.Keys = {};
HN.Tools.Util.Keys.DELETE = 46;
HN.Tools.Util.Keys.BACKSPACE = 8;

HN.Tools.Util.find_max = function() {
  var max;
  var tags;
  var i;
  var bits;
  max = 0;
  tags = document.getElementsByTagName("*");
  for (i = 1; i < tags.length; i += 1) {
    if (tags[i].id) {
      bits=tags[i].id.split("_");
      if (bits[0] === "id") {
        if (bits[1] && parseInt(bits[1]) > max) {
          max = parseInt(bits[1]);
        }
      }
    }
  }
  HN.Tools.Util.max_id = max + 1;
};

HN.Tools.Util.get_new_id = function()
{
  HN.Tools.Util.max_id += 1;
  return "id_" + HN.Tools.Util.max_id;
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
      if (Register.selected && ev.originalTarget.tagName !== "INPUT") {
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
  console.log("HN.Tools.Util.hide is deprecated...");
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
  var configid;
  var fn;
  drag = HN.Tools.Util.clean_drag_name(ui.draggable[0].className);
  new_elem = Register.elements[drag]['htmlfn']();
  new_id = HN.Tools.Util.get_new_id();
  new_elem.setAttribute("id", new_id);
  e.target.appendChild(new_elem);
  $("#" + new_id).draggable();
  configid = Register.elements[drag]['configid'];
  fn = function (ev) {Register.currentelement = new_id;
                      HN.Tools.Register.selected(ev.currentTarget.id);
                      HN.Tools.Register.show_config(configid);
                      $("#" + configid).css({'display': "block"});
                      // Now jump to the config tab set
                      Register.tabset.tabs('select', 1);
                      ev.stopPropagation();};
  $("#"+new_id).bind("click", fn);
};

HN.Tools.Util.drop_widgets = function(e, ui) {
  console.log("drop widgets...");
};

HN.Tools.Util.drop_layouts = function(e, ui) {
  var drag;
  var new_elem;
  var new_id;
  var html;
  var configid;
  var fn;
  drag = HN.Tools.Util.clean_drag_name(ui.draggable[0].className);
  new_elem = Register.layouts[drag]['htmlfn']();
  new_id = HN.Tools.Util.get_new_id();
  new_elem.setAttribute("id", new_id);
  e.target.appendChild(new_elem);
  $("#" + new_id).draggable();
  $(".layout_target").droppable({drop: HN.Tools.Util.drop, greedy: true});

  // get another reference to the new element 'in situ'
  configid = Register.layouts[drag]['configid'];
  fn = function (ev) {Register.currentlayout = new_id;
                      HN.Tools.Register.selected(ev.currentTarget.id);
                      HN.Tools.Register.show_config(configid);
                      // Now jump to the config tab set
                      Register.tabset.tabs('select', 1);
                      ev.stopPropagation();};
  $("#"+new_id).bind("click", fn);
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

HN.Tools.Util.set_selection = function(select, value) {
  var i;
  for (i = 0; i < select.length; i += 1) {
    if (select[i].value === value) {
      select[i].selected = true;
    }
  }
};

HN.Tools.Util.set_size = function(id, new_size) {
  $("#" + id).removeClass(HN.Tools.Util.get_size(id));
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
      default:
        return "medium";
    }
  }
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
  //if (!HNGui.register.pages[new_page]) {
  //  options = new HN.Gui.DataOptions([]);
  //  HNGui.register.pages[new_page] = new HN.Data(new_page, options);
  //};
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
  html = "<select id=\"binding_pages\">";
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
  console.log("in make_page");
  var page;
  var head;
  var scripts;
  var ret;
  page = document.createElement("div");
  console.log(HN.Util.id("canvas"));
  //console.log(HN.Util.id("canvas").innerHTML);
  //page.innerHTML = HN.Util.id("canvas").innerHTML;
  page.appendChild(HN.Util.id("canvas").cloneNode(true));
  console.log("page is...");
  console.log(page);
  HN.Tools.Util.clean_up(page);
  head = document.createElement("head");
  head.innerHTML = document.getElementsByTagName("head")[0].innerHTML;
  scripts = document.createElement("div");
  scripts.innerHTML = HN.Util.id("scripts").innerHTML;
  // console.log(page);
  // console.log(head);
  // console.log(scripts);
  ret = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n"
    + "<html>\n"
    + "  <head>\n"
    + head.innerHTML
    + "  </head>\n"
    + "  <body>\n"
    + "    <div id=\"canvas\">\n"
    + page.innerHTML + "\n"
    + "    </div>\n"
    + "    <div id=\"scripts\">\n"
    + scripts.innerHTML
    + "    </div>\n"
    + "  </body>\n"
    + "</html>";
  console.log(ret);
  return ret;
};

HN.Tools.Util.clean_up = function(node) {
  // If the node is a hypernumbers element then delete its contents
  // (dont worry these will be put back in dynamically by the
  // libraries at run  time)
  if (HN.Tools.Util.is_hn_element(node))
    {
      //console.log("is an hn element");
      //console.log(node);
      node.innerHTML = "";
    } else {
      //console.log("not hn element...");
      //console.log(node);
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
  console.log(node);
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
  if (node.className) {
    bits = node.className.split(" ");
    for (i = 0; i < bits.length - 1; i++) {
      if (!(bits[i] !== "ui-draggable"
            || bits[i] !== "ui-droppable"
            || bits[i] !== "selected"))
        new_class[new_class.length] = bits[i];
    }
    if (new_class.length === 0) {
      node.removeAttribute("class");
    } else  {
      node.setAttribute("class", new_class.join(" "));
    }
  }
};