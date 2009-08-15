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
    + "<tr>"
    + "<td>Bind To:</td>"
    + "<td><input id=\"element_binding\" class=\"input\" type=\"textarea\" /></td>"
    + "</tr>"
    + "<tr id=\"source_td\">"
    + "<td>Source Binding:</td>"
    + "<td><input id=\"element_sourcebinding\" class=\"input\" type=\"textarea\" /></td>"
    + "</tr>"
    + "<tr>"
    + "<td>Type:</td>"
    + "<td><select id=\"element_type\">"
    + "<option value = \"image\">Image</option>"
    + "<option value = \"lable\">Lable</option>"
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
    + "</table>"
    + "</div>";

  c1 = {toolboxlable: "lable",
        classname:    "lable",
        htmlfn:       HN.Tools.Elements.Core.make_lable,
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

HN.Tools.Elements.Core.make_lable = function() {
  var elem;
  elem = document.createElement('p');
  elem.setAttribute("class", "element unconfigured");
  elem.setAttribute("data-element-type", "lable");
  elem.innerHTML = "Unconfigured Lable";
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
  $("#element_binding").bind("blur", core.hide_bindings_table);
  $("#element_sourcebinding").bind("focus",core.show_bindings_table);
  $("#element_sourcebinding").bind("blur",core.hide_bindings_table);
  // Functions that drive the dialog box
  $("#elementsconfigbox").bind("focus", core.configure);
  $("#element_type").bind("change", core.change_type);
  $("#bindings_full_path").bind("change", configure);
  $("#bindings_range").bind("change", core.change_bindings_range);
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

HN.Tools.Elements.Core.make_relative = function(page) {
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
};

HN.Tools.Elements.Core.show_bindings_table = function(ev) {
  Register.currentbindings = ev.currentTarget.id;
  $("#bindings").show();
  ev.stopPropagation();
};

HN.Tools.Elements.Core.hide_bindings_table = function(ev) {
  //$("#bindings").hide();
};

HN.Tools.Elements.Core.configure = function() {
  var elem;
  var type;
  var select1;
  var select2;
  var size;
  var bindings_range;
  elem = HN.Util.id(Register.currentelement);
  type = elem.getAttribute("data-element-type");
  // populate the size drop-down
  size = HN.Tools.Util.get_size(elem.className);
  // set the right value of the type drop-down
  select1 = HN.Util.id("element_type");
  HN.Tools.Util.set_selection(select1, type);
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
};

HN.Tools.Elements.Core.pad = function(string) {
  var length;
  var diff;
  var pad;
  var padding;
  length = string.length;
  if (length < 10) {
    diff = parseInt(10 - length);
    pad = "&nbsp;";
    padding = function repeat(pad, diff){
      var r = "";
      for (var a = 0; a < diff; a++)
        r += pad;
      return r;
    }(pad, diff);
    return string + padding;
    } else {
      return string;
  }
};

//
// Finally make the calls to register the core elements...
//
var core = new HN.Tools.Elements.Core();
HN.Tools.Util.new_element(core['core1']);

