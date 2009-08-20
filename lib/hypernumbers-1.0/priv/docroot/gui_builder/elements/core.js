/*
 * @class HN.Tools.Elements.Core is the basic elements class
 *
 * These elements work by first initialising themselves and then,
 * finally, loading themselves into the toolbox register
 */
HN.Tools.Elements = {};

HN.Tools.Elements.Core = function() {
  var elements = {};
  var b1;
  var c1;

  // First define the config boxes
  b1 = "<div id=\"elementsconfigbox\" class=\"standardconfig\">"
    + "<p class=\"toolstitle\">Configure The Element</p><hr />"
    + "<table>"
    + "<tr id=\"source_td\">"
    + "<td>Source Binding:</td>"
    + "<td><input id=\"element_sourcebinding\" class=\"input\" type=\"textarea\" /></td>"
    + "</tr>"
    + "<tr>"
    + "<td>Bind To:</td>"
    + "<td><input id=\"element_binding\" class=\"input\" type=\"textarea\" /></td>"
    + "</tr>"
    + "<tr>"
    + "<td>Type:</td>"
    + "<td><select id=\"element_type\">"
    + "<option value = \"image\">Image</option>"
    + "<option value = \"lable\" selected>Lable</option>"
    + "<option value = \"inputtext\">Input Text</option>"
    + "<option value = \"inputradio\">Radio Box</option>"
    + "<option value = \"select\">Drop Down Box</option>"
    + "</select></td>"
    + "</tr>"
    + "<tr>"
    + "<td>Size:</td>"
    + "<td><select id=\"element_size\">"
    + "<option value = \"smallest\">Smallest</option>"
    + "<option value = \"small\">Small</option>"
    + "<option value = \"medium\">Medium</option>"
    + "<option value = \"large\">Large</option>"
    + "<option value = \"largest\">Largest</option>"
    + "</select></td>"
    + "</tr>"
    + "<tr>"
    + "<td>Binding Type:</td>"
    + "<td><select id=\"element_binding_type\">"
    + "<option value = \"both\">Both</option>"
    + "<option value = \"dynamic\">Dynamic</option>"
    + "<option value = \"static\">Static</option>"
    + "</select></td>"
    + "</tr>"
    + "<tr>"
    + "<td colspan=\"2\"><form id=\"elementform\"/>"
    + "<input id=\"elementok\" type=\"button\" class=\"action\" value=\"Save Configuration\" style=\"display: none\" /></td>"
    + "</tr>"
    + "</table>"
    + "</div>";

  c1 = {toolboxlable: "Element",
        classname:    "element",
        htmlfn:       HN.Tools.Elements.Core.make_element,
        configbox:    b1,
        configboxfn:  this.configure1
       };

  // first up define the layouts
  elements =
    {
      core1: c1
    };
  return elements;
};

HN.Tools.Elements.Core.make_element = function() {
  var elem;
  elem = document.createElement('p');
  elem.setAttribute("class", "element unconfigured");
  elem.innerHTML = "Unconfigured Element";
  return elem;
};

//
// Config Box functions
//

HN.Tools.Elements.Core.prototype.configure1 = function()
{
  var core = HN.Tools.Elements.Core;
  var configure = HN.Tools.Util.configure_paths;
  // Set up the mini-spreadsheet
  // Functions that hide and show the mini-spreadsheet
  $("#element_binding").bind("focus", core.show_bindings_table);
  $("#element_binding").bind("change", core.is_valid);
  $("#element_sourcebinding").bind("focus",core.show_bindings_table);
  $("#element_sourcebinding").bind("change",core.is_valid);
  // Functions that drive the dialog box
  $("#elementok").bind("click", core.make_configured_element);
  $("#elementsconfigbox").bind("focus", core.configure_box);
  $("#element_type").bind("change", core.change_type);
  $("#bindings_full_path").bind("change", configure);
  $("#bindings_range").bind("change", core.change_bindings_range);
};

/* HN.Tools.Elements.Core.config_focus = function(ev) {
  console.log("In config_focus...");
  console.log(Register.selected);
  console.log(HN.Util.id(Register.selected));
  var elem;
  var element_type;
  var render_type;
  var binding;
  var source_binding;
  elem = HN.Util.id(Register.selected);
  element_type = elem.getAttribute("data-gui-element-type");
  render_type = elem.getAttribute("data-render-type");
  binding = elem.getAttribute("data-binding");
  source_binding = elem.getAttribute("data-source-binding");
  if (element_type) {
    console.log(element_type);
  } else {
    console.log("no data-gui-element-type attribute...");
  }
  if (render_type) {
    console.log(render_type);
  } else {
    console.log("no data-gui-render-type attribute...");
  }
  if (binding) {
    console.log(binding);
  } else {
    console.log("no data-binding attribute...");
  }
  if (source_binding) {
    console.log(source_binding);
  } else {
    console.log("no data-source-binding attribute...");
  }
}; */

HN.Tools.Elements.Core.make_configured_element = function() {
  var elem;
  var type;
  var binding;
  var source;
  var bindingtype;
  var size;
  var html;
  elem = HN.Util.id(Register.selected);
  type = HN.Util.id("element_type").value;
  binding = HN.Util.id("element_binding").value;
  bindingtype = HN.Util.id("element_binding_type").value;
  elem.setAttribute("data-gui-element-type", type);
  elem.setAttribute("data-render-type", bindingtype);
  elem.setAttribute("data-binding", binding);
  if (type !== "lable" && type !== "image" & type !== "inputtext") {
    source = HN.Util.id("element_sourcebinding").value;
    elem.setAttribute("data-source-binding", source);
  }
  size = HN.Util.id("element_size").value;
  HN.Tools.Util.set_size(Register.selected, size);
  elem.value = "";
  $("#" + Register.selected).removeClass("unconfigured");
  $("#" + Register.selected).addClass("hn");
  HNGui.reregister();
  Register.selected = "";
  // Now jump to the layout/elements tab set
  Register.tabset.tabs('select', 0);
};

HN.Tools.Elements.Core.is_valid = function() {
  var type;
  var binding;
  var source;
  var ok;
  type = HN.Util.id("element_type").value;
  binding = HN.Util.id("element_binding").value;
  source = HN.Util.id("element_sourcebinding").value;
  ok = HN.Util.id("elementok");
  if (type === "lable" || type === "image" || type === "inputtext") {
    if (binding && binding !== "") {
      ok.style.display = "block";
    } else {
      ok.style.display = "none";
    }
  } else if (type === "inputradio" || type === "select") {
    if ((binding && binding !== "")
        && (source && source !== "")) {
      ok.style.display = "block";
    } else {
      ok.style.display = "none";
    }
  }
};

HN.Tools.Elements.Core.change_bindings_range = function(ev) {
  Register.currentrange = ev.target.value;
  HN.Tools.Util.gen_bindings_table(Register.currentpage,
                                   ev.target.value);
  HN.Gui.Register.reregister("gui_builder");
};

HN.Tools.Elements.Core.configure_bindings = function(type) {
  var row;
  row = HN.Util.id("source_td");
  if (type === "lable"
    || type === "image"
    || type === "inputtext") {
      row.style.display="none";
  } else if (type === "inputradio"
    || type === "select")
  {
    row.style.display="";
  }
};

//
// Think this is irrelevant
//
/* HN.Tools.Elements.Core.make_relative = function(page) {
  console.log(page);
  var currentpage;
  var c_bits;
  var c_len;
  var p_bits;
  var p_len;
  currentpage = document.location.pathname;
  if (currentpage === page) {
    return "./";
  } else {
    c_bits = currentpage.split("/");
    c_len=c_bits.length;
    p_bits = page.split("/");
    p_len = p_bits.length;
    console.log(c_bits + " " + c_len + " : " + p_bits + " " + p_len);
    return "fix me...";
  }
}; */

HN.Tools.Elements.Core.show_bindings_table = function(ev) {
  Register.currentbindings = ev.currentTarget.id;
  $("#bindings").show();
  $("#" + Register.bindings_table[Register.currentpage]).show();
  ev.stopPropagation();
};

HN.Tools.Elements.Core.hide_bindings_table = function(ev) {
  $("#bindings").hide();
};

HN.Tools.Elements.Core.minimize_bindings_table = function(ev) {
  $("#insert_bindings").hide();
  $(".bindings_minimize").unbind("click",
                                 HN.Tools.Elements.Core.minimize_bindings_table);
  $(".bindings_minimize").addClass("bindings_maximize");
  $(".bindings_maximize").bind("click",
                               HN.Tools.Elements.Core.maximize_bindings_table);
  $(".bindings_minimze").removeClass("bindings_minimize");
};

HN.Tools.Elements.Core.maximize_bindings_table = function(ev) {
  $("#insert_bindings").show();
  $(".bindings_maximize").unbind("click",
                                 HN.Tools.Elements.Core.maximize_bindings_table);
  $(".bindings_maximize").addClass("bindings_minimize");
  $(".bindings_minimize").bind("click",
                               HN.Tools.Elements.Core.minimize_bindings_table);
  $(".bindings_maximze").removeClass("bindings_maxnimize");
};

HN.Tools.Elements.Core.configure_box = function() {
  var bindings;
  var source;
  var elem;
  var type;
  var select1;
  var size;
  var select2;
  var bindings_range;
  // clear the bindings boxes
  bindings = HN.Util.id("element_binding");
  bindings.value = "";
  source = HN.Util.id("element_sourcebinding");
  source.value = "";
  // zap the seclection
  Register.startselection = "";
  Register.endselection = "";
  elem = HN.Util.id(Register.currentelement);
  // set the right value of the type drop-down
  type = elem.getAttribute("data-gui-element-type");
  // if type is not set force it to the default 'lable'
  if (!type) {
    type = "lable";
  }
  select1 = HN.Util.id("element_type");
  HN.Tools.Util.set_selection(select1, type);
  // populate the size drop-down
  size = HN.Tools.Util.get_size(Register.currentelement);
  // set the right value of the size drop down
  select2 = HN.Util.id("element_size");
  HN.Tools.Util.set_selection(select2, size);
  // show the appropriate bindings boxes
  HN.Tools.Elements.Core.configure_bindings(type);
  // Show the page being looked at
  HN.Tools.Util.configure_paths();
  bindings_range = HN.Util.id("bindings_range");
  bindings_range.value = Register.currentrange;
  };

HN.Tools.Elements.Core.change_type = function(e) {
  HN.Tools.Elements.Core.configure_bindings(e.target.value);
  HN.Tools.Elements.Core.is_valid();
};

//
// Finally make the calls to register the core elements...
//
var core = new HN.Tools.Elements.Core();
HN.Tools.Util.new_element(core['core1']);
