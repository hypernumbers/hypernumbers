/*
 * @class HN.Tools.Layouts.Basic is the basic layout class
 * This class is the prototype for more advanced layouts to be added to the
 * GUI Builder
 *
 * These layouts work by first initialising themselves and then,
 * finally, loading themselves into the toolbox register
 */

// Set up a constructor - bit of overkill for this simple example
HN.Tools.Layouts.Basic = function() {
  var layouts = {};
  var d1;
  var d2;
  var d3;
  var d4;
  var b1;
  var b2;
  var b3;
  var b4;

  // First define the dialog boxes
  d1 = "<div id=\"basicdialog1\" class=\"mdialog standarddialog\">"
    + "<h2>Adjust the width of the columns</h2>"
    + "<table>"
    + "<tr><td>Column 1</td>"
    + "<td><input id=\"basic1width1\" class = \"input\" type=\"text\" />%</td></tr>"
    + "<tr><td>Column 2</td>"
    + "<td><input id=\"basic1width2\" class = \"input\" type=\"text\" />%</td></tr>"
    + "</table><p>Widths are expressed as percentages</p>"
    + "<form id=\"basic1form\" onClick=\"return HN.Tools.Layouts.Basic.close_dialog1();\">"
    + "<input type=\"button\" class=\"action\" value=\"OK\"  />"
    + "</form>"
    + "</div>";

  d2 = "<div id=\"basicdialog2\" class=\"mdialog standarddialog\">"
    + "<h2>Adjust the width of the columns</h2>"
    + "<table>"
    + "<tr><td>Column 1</td>"
    + "<td><input id=\"basic2width1\" class = \"input\" type=\"text\" />%</td></tr>"
    + "<tr><td>Column 2</td>"
    + "<td><input id=\"basic2width2\" class = \"input\" type=\"text\" />%</td></tr>"
    + "<tr><td>Column 3</td>"
    + "<td><input id=\"basic2width3\" class = \"input\" type=\"text\" />%</td></tr>"
    + "</table><p>Widths are expressed as percentages</p>"
    + "<form id=\"basic2form\" onClick=\"return HN.Tools.Layouts.Basic.close_dialog2();\">"
    + "<input type=\"button\" class=\"action\" value=\"OK\"  />"
    + "</form>"
    + "</div>";

d3 = "<div id=\"basicdialog3\" class=\"mdialog standarddialog\">"
    + "<h2>Adjust the height of the rows</h2>"
    + "<table>"
    + "<tr><td>Row 1</td>"
    + "<td><input id=\"basic3height1\" class = \"input\" type=\"text\" />px</td></tr>"
    + "<tr><td>Row 2</td>"
    + "<td><input id=\"basic3height2\" class = \"input\" type=\"text\" />px</td></tr>"
    + "</table><p>Heights are expressed as pixcels (px)</p>"
    + "<form id=\"basic3form\" onClick=\"return HN.Tools.Layouts.Basic.close_dialog3();\">"
    + "<input type=\"button\" class=\"action\" value=\"OK\"  />"
    + "</form>"
    + "</div>";

d4 = "<div id=\"basicdialog4\" class=\"mdialog standarddialog\">"
    + "<h2>Adjust the height of the rows</h2>"
    + "<table>"
    + "<tr><td>Row 1</td>"
    + "<td><input id=\"basic4height1\" class = \"input\" type=\"text\" />px</td></tr>"
    + "<tr><td>Row 2</td>"
    + "<td><input id=\"basic4height2\" class = \"input\" type=\"text\" />px</td></tr>"
    + "<tr><td>Row 3</td>"
    + "<td><input id=\"basic4height3\" class = \"input\" type=\"text\" />px</td></tr>"
    + "</table><p>Heights are expressed as pixcels (px)</p>"
    + "<form id=\"basic4form\" onClick=\"return HN.Tools.Layouts.Basic.close_dialog4();\">"
    + "<input type=\"button\" class=\"action\" value=\"OK\"  />"
    + "</form>"
    + "</div>";

  b1 = {toolboxlable: "1 by 2",
        classname:    "template1x2",
        htmlfn:       function() {return HN.Tools.Layouts.Basic.make_table(1,2);},
        dialogbox:    d1,
        dialogfn:     HN.Tools.Layouts.Basic.configure1},

  b2 = {toolboxlable: "1 by 3",
        classname:    "template1x3",
        htmlfn:       function() {return HN.Tools.Layouts.Basic.make_table(1,3);},
        dialogbox:    d2,
        dialogfn:     HN.Tools.Layouts.Basic.configure2},

  b3 = {toolboxlable: "2 by 1",
        classname:    "template2x1",
        htmlfn:       function() {return HN.Tools.Layouts.Basic.make_table(2,1);},
        dialogbox:    d3,
        dialogfn:     HN.Tools.Layouts.Basic.configure3},

  b4 = {toolboxlable: "3 by 1",
        classname:    "template3x1",
        htmlfn:       function () {return HN.Tools.Layouts.Basic.make_table(3,1);},
        dialogbox:    d4,
        dialogfn:     HN.Tools.Layouts.Basic.configure4},

  // first up define the layouts
  layouts =
    {
      basic1: b1,
      basic2: b2,
      basic3: b3,
      basic4: b4
    };
  return layouts;
};

//
// Dialog Box functions
//
HN.Tools.Layouts.Basic.close_dialog1 = function()
{
  HN.Tools.Util.dialog_close("#basicdialog1");
};


HN.Tools.Layouts.Basic.configure1 = function()
{
  var input1;
  var input2;
  var fn1;
  var fn2;
  var fn3;
  input1 = HN.Util.id("basic1width1");
  input2 = HN.Util.id("basic1width2");
  fn1 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input2.value = 100 - input1.value;
    elem.firstChild.firstChild.width = input1.value + "%";
    elem.firstChild.lastChild.width = input2.value + "%";
  };
  fn2 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input1.value = 100 - input2.value;
    elem.firstChild.firstChild.width = input1.value + "%";
    elem.firstChild.lastChild.width = input2.value + "%";
  };
  fn3 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input1.value = elem.firstChild.firstChild.width.replace("%","");
    input2.value = elem.firstChild.lastChild.width.replace("%","");
  };
  $("#basic1width1").bind("blur", fn1);
  $("#basic1width2").bind("blur", fn2);
  $("#basicdialog1").bind("focus", fn3);
};

HN.Tools.Layouts.Basic.close_dialog2 = function()
{
  HN.Tools.Util.dialog_close("#basicdialog2");
};

HN.Tools.Layouts.Basic.configure2 = function()
{
  var input1;
  var input2;
  var input3;
  var fn1;
  var fn2;
  var fn3;
  var fn4;
  input1 = HN.Util.id("basic2width1");
  input2 = HN.Util.id("basic2width2");
  input3 = HN.Util.id("basic2width3");
  fn1 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input2.value = (100 - input3.value) - input1.value;
    elem.firstChild.firstChild.width = input1.value + "%";
    elem.firstChild.firstChild.nextSibling.width = input2.value + "%";
  };
  fn2 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input3.value = (100 - input1.value) - input2.value;
    elem.firstChild.firstChild.nextSibling.width = input2.value + "%";
    elem.firstChild.lastChild.width = input3.value + "%";
  };
  fn3 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input1.value = (100 - input2.value) - input3.value;
    elem.firstChild.firstChild.width = input1.value + "%";
    elem.firstChild.lastChild.width = input3.value + "%";
  };
  fn4 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input1.value = elem.firstChild.firstChild.width.replace("%","");
    input2.value = elem.firstChild.firstChild.nextSibling.width.replace("%","");
    input3.value = elem.firstChild.lastChild.width.replace("%","");
  };
  $("#basic2width1").bind("blur", fn1);
  $("#basic2width2").bind("blur", fn2);
  $("#basic2width3").bind("blur", fn3);
  $("#basicdialog2").bind("focus", fn4);
};

HN.Tools.Layouts.Basic.close_dialog3 = function()
{
  HN.Tools.Util.dialog_close("#basicdialog3");
};

HN.Tools.Layouts.Basic.configure3 = function()
{
  var input1;
  var input2;
  var fn1;
  var fn2;
  input1 = HN.Util.id("basic3height1");
  input2 = HN.Util.id("basic3height2");
  fn1 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    elem.firstChild.style.height = input1.value + "px";
    elem.lastChild.style.height = input2.value + "px";
  };
  fn2 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input1.value = elem.firstChild.clientHeight;
    input2.value = elem.lastChild.clientHeight;
  };
  $("#basic3height1").bind("blur", fn1);
  $("#basic3height2").bind("blur", fn1);
  $("#basicdialog3").bind("focus", fn2);
};

HN.Tools.Layouts.Basic.close_dialog4 = function()
{
  HN.Tools.Util.dialog_close("#basicdialog4");
};


HN.Tools.Layouts.Basic.configure4 = function()
{
  var input1;
  var input2;
  var input3;
  var fn1;
  var fn2;
  input1 = HN.Util.id("basic4height1");
  input2 = HN.Util.id("basic4height2");
  input3 = HN.Util.id("basic4height3");
  fn1 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    elem.firstChild.style.height = input1.value + "px";
    elem.firstChild.nextSibling.style.height = input2.value + "px";
    elem.lastChild.style.height = input3.value + "px";
  };
  fn2 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input1.value = elem.firstChild.clientHeight;
    input2.value = elem.firstChild.nextSibling.clientHeight;
    input3.value = elem.lastChild.clientHeight;
  };
  $("#basic4height1").bind("blur", fn1);
  $("#basic4height2").bind("blur", fn1);
  $("#basic4height3").bind("blur", fn1);
  $("#basicdialog4").bind("focus", fn2);
};

//
// Internal Functions
//
HN.Tools.Layouts.Basic.make_table = function(rows, columns) {
  var i;
  var j;
  var elem;
  var myrow;
  var width;
  width = 100/columns + "%";
  elem = document.createElement('table');
  elem.setAttribute("class", "make_draggable");
  elem.setAttribute("class", "layout");
  for (i = 0; i < rows; i += 1) {
    myrow = document.createElement('tr');
    for (j = 0; j < columns; j+= 1) {
      myrow.appendChild(document.createElement('td'));
      myrow.lastChild.setAttribute("class", "make_droppable");
      myrow.lastChild.setAttribute("class", "layout_target");
      if (i === 0) {
        myrow.lastChild.setAttribute("width", width);
      }
    }
    elem.appendChild(myrow);
  }
  return elem;
};

// Now insert into the toolbox register
// The 'toolbox' name is the name that will appear in the toolbox
// the 'classname' is the clasname that will be applied to the toolbox
// item
// ** IT IS THE LAYOUT CREATORS RESPONSIBILITY TO CREATE A STYLESHEET
//    FOR THEIR LAYOUT
// Usually this just involves adding an icon for the toolbox
var basic = new HN.Tools.Layouts.Basic();
HN.Tools.Util.new_layout(basic['basic1']);
HN.Tools.Util.new_layout(basic['basic2']);
HN.Tools.Util.new_layout(basic['basic3']);
HN.Tools.Util.new_layout(basic['basic4']);
