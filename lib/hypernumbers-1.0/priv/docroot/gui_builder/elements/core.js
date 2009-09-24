/*
 * @class HN.Tools.Elements.Core is the basic elements class
 *
 * These elements work by first initialising themselves and then,
 * finally, loading themselves into the toolbox register
 */

HN.Tools.Elements.Core = function() {
  var elements = {};
  var b1;
  var c1;

  // First define the config boxes
  b1 = "<div id=\"elementsconfigbox\" class=\"standardconfig\">"
    + "<p class=\"toolstitle\">Configure The Element</p><hr />"
    + "<table>"
    + "<tbody>"
    + "<tr id=\"source_td\">"
    + "<td>Source Binding:</td>"
    + "<td><input id=\"element_sourcebinding\" class=\"input ui-state-default\" type=\"textarea\" /></td>"
    + "</tr>"
    + "<tr>"
    + "<td>Bind To:</td>"
    + "<td><input id=\"element_binding\" class=\"input ui-state-default\" type=\"textarea\" /></td>"
    + "</tr>"
    + "<tr>"
    + "<td>Type:</td>"
    + "<td><select id=\"element_type\" class=\"ui-state-default\">"
    + "<option value = \"image\">Image</option>"
    + "<option value = \"block\" selected>Block</option>"
    + "<option value = \"inputtext\">Input Text</option>"
    + "<option value = \"inputtextarea\">Input Text Area</option>"
    + "<option value = \"checkbox\">Checkbox</option>"
    + "<option value = \"radio\">Radio Box</option>"
    + "<option value = \"select\">Drop Down Box</option>"
    + "</select></td>"
    + "</tr>"
    + "<tr>"
    + "<td>Size:</td>"
    + "<td><select id=\"element_size\" class=\"ui-state-default\">"
    + "<option value = \"smallest\">Smallest</option>"
    + "<option value = \"small\">Small</option>"
    + "<option value = \"medium\">Medium</option>"
    + "<option value = \"large\">Large</option>"
    + "<option value = \"largest\">Largest</option>"
    + "</select></td>"
    + "</tr>"
    + "<tr style=\"display: none;\">"
    + "<td>Binding Type:</td>"
    + "<td><select id=\"element_binding_type\" class=\"ui-state-default\">"
    + "<option value = \"both\">Both</option>"
    + "<option value = \"dynamic\">Dynamic</option>"
    + "<option value = \"static\">Static</option>"
    + "</select></td>"
    + "</tr>"
    + "<tr><td colspan=\"2\"><br />"
    + "<input id=\"elementok\" type=\"button\" class=\"action ui-state-default\" value=\"Save Configuration\" style=\"display: none\" /></td>"
    + "</tr>"
    + "</tbody>"
    + "</table>"
    + "</div>";

  c1 = {toolboxlable:  "Element",
        classname:     "basic_element",
        htmlfn:        HN.Tools.Elements.Core.make_element,
        configbox:     b1,
        configboxfn:   this.configure1,
        aftercreatefn: function(id) {return HN.Tools.Elements.Core.aftercreate(id);},
        helptext:     "An element is a bit of a web page - a text field, or "
          + "an input field, etc, etc. Drop one on the page then double-click "
          + "it to configure it."
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
  // display the save button if the current configuration is valid
  core.is_valid();
  // Set up the mini-spreadsheet
  // Functions that hide and show the mini-spreadsheet
  $("#element_binding").bind("focus", HN.Tools.Util.show_bindings_table);
  $("#element_binding").bind("change", core.is_valid);
  $("#element_sourcebinding").bind("focus", HN.Tools.Util.show_bindings_table);
  $("#element_sourcebinding").bind("change",core.is_valid);
  // Functions that drive the dialog box
  $("#elementok").bind("click", core.make_configured_element);
  $("#elementsconfigbox").bind("focus", core.configure_box);
  $("#element_type").bind("change", core.change_type);
};

HN.Tools.Elements.Core.make_configured_element = function() {
  var elem;
  var type;
  var binding;
  var source;
  var bindingtype;
  var size;
  var html;
  var new_id;
  elem = HN.Util.id(Register.selected);
  type = HN.Util.id("element_type").value;
  binding = HN.Util.id("element_binding").value;
  bindingtype = HN.Util.id("element_binding_type").value;
  elem.setAttribute("data-gui-element-type", type);
  elem.setAttribute("data-render-type", bindingtype);
  elem.setAttribute("data-binding", binding);
  if (type !== "block" && type !== "image" & type !== "inputtext") {
    source = HN.Util.id("element_sourcebinding").value;
    elem.setAttribute("data-source-binding", source);
  }
  size = HN.Util.id("element_size").value;
  HN.Tools.Util.set_size(Register.selected, size);
  elem.value = "";
  $("#" + Register.selected).removeClass("unconfigured");
  $("#" + Register.selected).addClass("hn");
  // Reregistering zaps all the ids so we need to mark that this one
  // is selected and get its id back later
  $("#" + Register.selected).addClass("reregister_selected");
  HNGui.reregister();
  new_id = $(".reregister_selected")[0].id;
  $("#" + new_id).removeClass("reregister_selected");
  Register.selected = new_id;
  Register.currentelement = new_id;
  $("#" + Register.selected).unbind();
  $("#" + Register.selected).bind("click", HN.Tools.Util.click_on_element("elementsconfigbox"));
  $("#" + Register.selected).bind("dblclick", HN.Tools.Util.pop_config_tab);
};

//
// After Create Fns
// Called after an instance is created...
//
HN.Tools.Elements.Core.aftercreate = function(id) {
  //console.log("HN.Tools.Elements.Core.aftercreate called with id of " + id);
};

//
// Internal Fns
//

HN.Tools.Elements.Core.is_valid = function() {
  var type;
  var binding;
  var source;
  var ok;
  type = HN.Util.id("element_type").value;
  binding = HN.Util.id("element_binding").value;
  source = HN.Util.id("element_sourcebinding").value;
  ok = HN.Util.id("elementok");
  if (type === "block"
      || type === "image"
      || type === "inputtext"
      || type === "inputtextarea"
      || type === "checkbox") {
    if (binding && binding !== "") {
      ok.style.display = "block";
    } else {
      ok.style.display = "none";
    }
  } else if (type === "radio" || type === "select") {
    if ((binding && binding !== "")
        && (source && source !== "")) {
      ok.style.display = "block";
    } else {
      ok.style.display = "none";
    }
  }
};

HN.Tools.Elements.Core.configure_bindings = function(type) {
  var row;
  row = HN.Util.id("source_td");
  if (type === "lable"
    || type === "image"
    || type === "inputtext") {
      row.style.display="none";
  } else if (type === "inputradio"
    || type === "select") {
    row.style.display="";
  }
};

HN.Tools.Elements.Core.configure_box = function() {
  var elem;
  var bindings;
  var source;
  var sourceval;
  var type;
  var select1;
  var size;
  var select2;
  var bindings_range;
  elem = HN.Util.id(Register.currentelement);
  // clear the bindings boxes
  bindings = HN.Util.id("element_binding");
  bindings.value = elem.getAttribute("data-binding");
  sourceval = elem.getAttribute("data-source-binding");
  source = HN.Util.id("element_sourcebinding");
  if (sourceval) {
    source.value = sourceval;
  } else {
    source.value = "";
  };
  // zap the selection
  Register.startselection = "";
  Register.endselection = "";
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