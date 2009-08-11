/*
 * @class HN.Tools.Elements.Core is the basic layout class
 * This class is the prototype for more advanced layouts to be added to the
 * GUI Builder
 *
 * These layouts work by first initialising themselves and then,
 * finally, loading themselves into the toolbox register
 */

// Set up a constructor - bit of overkill for this simple example
HN.Tools.Elements = {};

HN.Tools.Elements.Core = function() {
  var elements = {};
  var d1;
  var c1;

  // First define the dialog boxes
  d1 = "<div id=\"elementsdialogbox\" class=\"mdialog standarddialog\">"
    + "<h2>Configure The Element</h2>"
    + "<table>"
    + "<tr>"
    + "<td>Bind To:</td>"
    + "<td><input id=\"element_binding\" class=\"input\" type=\"textarea\" /></td>"
    + "</tr>"
    + "<tr>"
    + "<td id=\"source_td\">Source Binding:</td>"
    + "<td><input id=\"element_sourcebinding\" class=\"input\" type=\"textarea\" /></td>"
    + "</tr>"
    + "<tr>"
    + "<td>Type:</td>"
    + "<td><select id=\"element_type\">"
    + "<option value = \"lable\">Lable</option>"
    + "<option value = \"image\">Image</option>"
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
    + "<option value = \"normal\">Normal</option>"
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
    + "<form id=\"elementform\" onClick=\"return HN.Tools.Elements.Core.close_dialog1();\">"
    + "<input type=\"button\" class=\"action\" value=\"OK\"  />"
    + "</form>"
    + "</div>";

  c1 = {toolboxlable: "lable",
        classname:    "lable",
        htmlfn:       HN.Tools.Elements.Core.make_lable,
        dialogbox:    d1,
        dialogfn:     HN.Tools.Elements.Core.configure1
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
  elem.setAttribute("class", "element unconfigured lable");
  elem.innerHTML = "Unconfigured Lable <em>(Double Click To Configure)</em>";
  return elem;
};

//
// Dialog Box functions
//
HN.Tools.Elements.Core.close_dialog1 = function()
{
  // close the binding_table dialog box (it might be open...)
  HN.Tools.Util.dialog_close(HN.Util.id("binding_table"));
  HN.Tools.Util.dialog_close("#elementsdialogbox");
};


HN.Tools.Elements.Core.configure1 = function()
{
  var fn1;
  var fn2;
  fn1 = function() {
    HN.Tools.Util.offset_open(HN.Util.id("binding_table"));
  };
  fn2 = function() {
    HN.Tools.Util.dialog_close(HN.Util.id("binding_table"));
  };
  $("#element_binding").bind("focus", fn1);
  $("#element_sourcebinding").bind("focus", fn1);
  $("#element_size").bind("focus", fn2);
  $("#element_type").bind("focus", fn2);
  $("#element_bindingtype").bind("focus", fn2);
};

var core = new HN.Tools.Elements.Core();
HN.Tools.Util.new_element(core['core1']);

