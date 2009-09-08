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
  var c1;
  var c2;
  var c3;
  var c4;
  var c5;
  var c6;
  var c7;
  var b1;
  var b2;
  var b3;
  var b4;
  var b5;
  var b6;
  var b7;

  // First define the config boxes
  c1 = "<div id=\"basicconfig1\" class=\"standardconfig\">"
    + "<p class=\"toolstitle\">Adjust The Width Of The Columns</p><hr />"
    + "<table>"
    + "<tbody>"
    + "<tr><td>Column 1</td>"
    + "<td><input id=\"basic1width1\" class = \"input ui-state-default\" type=\"text\" />%</td></tr>"
    + "<tr><td>Column 2</td>"
    + "<td><input id=\"basic1width2\" class = \"input ui-state-default\" type=\"text\" />%</td></tr>"
    + "</tbody>"
    + "</table><p>Widths are expressed as percentages</p>"
    + "</div>";

  c2 = "<div id=\"basicconfig2\" class=\"standardconfig\">"
    + "<p class=\"toolstitle\">Adjust The Width Of The Columns</p><hr />"
    + "<table>"
    + "<tbody>"
    + "<tr><td>Column 1</td>"
    + "<td><input id=\"basic2width1\" class = \"input ui-state-default\" type=\"text\" />%</td></tr>"
    + "<tr><td>Column 2</td>"
    + "<td><input id=\"basic2width2\" class = \"input ui-state-default\" type=\"text\" />%</td></tr>"
    + "<tr><td>Column 3</td>"
    + "<td><input id=\"basic2width3\" class = \"input ui-state-default\" type=\"text\" />%</td></tr>"
    + "</tbody>"
    + "</table><p>Widths are expressed as percentages</p>"
    + "</div>";

  c3 = "<div id=\"basicconfig3\" class=\"standardconfig\">"
    + "<p class=\"toolstitle\">Adjust The Height Of The Rows</p><hr />"
    + "<table>"
    + "<tbody>"
    + "<tr><td>Row 1</td>"
    + "<td><input id=\"basic3height1\" class = \"input ui-state-default\" type=\"text\" />px</td></tr>"
    + "<tr><td>Row 2</td>"
    + "<td><input id=\"basic3height2\" class = \"input ui-state-default\" type=\"text\" />px</td></tr>"
    + "</tbody>"
    + "</table><p>Heights are expressed as pixcels (px)</p>"
    + "</div>";

  c4 = "<div id=\"basicconfig4\" class=\"standardconfig\">"
    + "<p class=\"toolstitle\">Adjust The Height Of The Rows</p><hr />"
    + "<table>"
    + "<tbody>"
    + "<tr><td>Row 1</td>"
    + "<td><input id=\"basic4height1\" class = \"input ui-state-default\" type=\"text\" />px</td></tr>"
    + "<tr><td>Row 2</td>"
    + "<td><input id=\"basic4height2\" class = \"input ui-state-default\" type=\"text\" />px</td></tr>"
    + "<tr><td>Row 3</td>"
    + "<td><input id=\"basic4height3\" class = \"input ui-state-default\" type=\"text\" />px</td></tr>"
    + "</tbody>"
    + "</table><p>Heights are expressed as pixcels (px)</p>"
    + "</div>";

  c5 = "<div id=\"basicconfig5\" class=\"standardconfig\">"
    +  "<p class=\"toolstitle\">Set The Size Of The Blank Panel</p><hr />"
    + "<table>"
    + "<tbody>"
    + "<tr><td>Height:</td><td><input id=\"basic5height\" class=\"input ui-state-default\" type = \"text\">px</td></tr>"
    + "<tr><td>Width:</td><td><input id=\"basic5width\" class=\"input ui-state-default\" type = \"text\">px</td></tr>"
    + "</tbody>"
    + "</table>"
    + "</div>";

  c6 = "<div id=\"basicconfig6\" class=\"standardconfig\">"
    +  "<p class=\"toolstitle\">Set The Fragment Size</p><hr />"
    + "<table>"
    + "<tbody>"
    + "<tr><td>Height:</td><td><input id=\"basic6height\" class=\"input ui-state-default\" type = \"text\">px</td></tr>"
    + "<tr><td>Width:</td><td><input id=\"basic6width\" class=\"input ui-state-default\" type = \"text\">px</td></tr>"
    + "</tbody>"
    + "</table>"
    + "</div>";

  c7 = "<div id=\"basicconfig7\" class=\"standardconfig\">"
    +  "<p class=\"toolstitle\">Set The Line Thickness</p><hr />"
    + "<form name=\"configure_line\">"
    + "<table>"
    + "<tbody>"
    + "<tr><td>Very Thin</td><td><input id=\"basic7thickness1\" class=\"input ui-state-default\" type = \"radio\" value=\"verythin\" name=\"linethickness\"></td></tr>"
    + "<tr><td>Thin</td><td><input id=\"basic7thickness2\" class=\"input ui-state-default\" type = \"radio\" value=\"thin\" name=\"linethickness\"></td></tr>"
    + "<tr><td>Normal</td><td><input id=\"basic7thickness3\" class=\"input ui-state-default\" type = \"radio\" value=\"normal\" name=\"linethickness\"></td></tr>"
      + "<tr><td>Thick</td><td><input id=\"basic7thickness4\" class=\"input ui-state-default\" type = \"radio\" value=\"thick\" name=\"linethickness\"></td></tr>"
      + "<tr><td>Very Thick</td><td><input id=\"basic7thickness5\" class=\"input ui-state-default\" type = \"radio\" value=\"verythick\" name=\"linethickness\"></td></tr>"
    + "</tbody>"
    + "</table>"
    + "</form>"
    + "</div>";

  b1 = {toolboxlable:  "1 by 2",
        classname:     "template1x2",
        htmlfn:        function() {return HN.Tools.Layouts.Basic.make_table("template1x2",1,2);},
        configbox:     c1,
        configboxfn:   HN.Tools.Layouts.Basic.configure1,
        aftercreatefn: HN.Tools.Layouts.Basic.aftercreate(),
        helptext:      "This is a simple table layout to put things in."},

  b2 = {toolboxlable:  "1 by 3",
        classname:     "template1x3",
        htmlfn:        function() {return HN.Tools.Layouts.Basic.make_table("template1x3",1,3);},
        configbox:     c2,
        configboxfn:   HN.Tools.Layouts.Basic.configure2,
        aftercreatefn: HN.Tools.Layouts.Basic.aftercreate(),
        helptext:      "This is a simple table layout to put things in."},

  b3 = {toolboxlable:  "2 by 1",
        classname:     "template2x1",
        htmlfn:        function() {return HN.Tools.Layouts.Basic.make_table("template2x1",2,1);},
        configbox:     c3,
        configboxfn:   HN.Tools.Layouts.Basic.configure3,
        aftercreatefn: HN.Tools.Layouts.Basic.aftercreate(),
        helptext:      "This is a simple table layout to put things in."},

  b4 = {toolboxlable:  "3 by 1",
        classname:     "template3x1",
        htmlfn:        function () {return HN.Tools.Layouts.Basic.make_table("template3x1",3,1);},
        configbox:     c4,
        configboxfn:   HN.Tools.Layouts.Basic.configure4,
        aftercreatefn: HN.Tools.Layouts.Basic.aftercreate(),
        helptext:      "This is a simple table layout to put things in."},

  b5 = {toolboxlable:  "Blank",
        classname:     "blank",
        htmlfn:        function () {return HN.Tools.Layouts.Basic.make_blank();},
        configbox:     c5,
        configboxfn:   HN.Tools.Layouts.Basic.configure5,
        aftercreatefn: HN.Tools.Layouts.Basic.aftercreate(),
        helptext:     "This is a blank box you can use to space your layout"},

  b6 = {toolboxlable:  "Fragment",
        classname:     "fragment",
        htmlfn:        function () {return HN.Tools.Layouts.Basic.make_fragment();},
        configbox:     c6,
        configboxfn:   HN.Tools.Layouts.Basic.configure6,
        aftercreatefn: HN.Tools.Layouts.Basic.aftercreate(),
        helptext:     "When you want to build a lot of differnet pages with the same overlook, you build a page with one or more fragments in which is the overarching layout. Then you bind different fragments to it for different pages. This means that you can edit the overall look and feel in once place and have it changed everywhere."},

  b7 = {toolboxlable:  "Line",
        classname:     "line",
        htmlfn:        function () {return HN.Tools.Layouts.Basic.make_line();},
        configbox:     c7,
        configboxfn:   HN.Tools.Layouts.Basic.configure7,
        aftercreatefn: HN.Tools.Layouts.Basic.aftercreate(),
        helptext:     "This allows you to put a horizontal line on your page."},


  // first up define the layouts
  layouts =
    {
      basic1: b1,
      basic2: b2,
      basic3: b3,
      basic4: b4,
      basic5: b5,
      basic6: b6,
      basic7: b7
    };
  return layouts;
};

//
// Dialog Box functions
//

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
    elem.firstChild.firstChild.firstChild.width = input1.value + "%";
    elem.firstChild.firstChild.lastChild.width = input2.value + "%";
  };
  fn2 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input1.value = 100 - input2.value;
    elem.firstChild.firstChild.firstChild.width = input1.value + "%";
    elem.firstChild.firstChild.lastChild.width = input2.value + "%";
  };
  fn3 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input1.value = elem.firstChild.firstChild.firstChild.width.replace("%","");
    input2.value = elem.firstChild.firstChild.lastChild.width.replace("%","");
  };
  $("#basic1width1").bind("blur", fn1);
  $("#basic1width2").bind("blur", fn2);
  $("#basicconfig1").bind("focus", fn3);
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
    elem.firstChild.firstChild.firstChild.width = input1.value + "%";
    elem.firstChild.firstChild.firstChild.nextSibling.width = input2.value + "%";
  };
  fn2 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input3.value = (100 - input1.value) - input2.value;
    elem.firstChild.firstChild.firstChild.nextSibling.width = input2.value + "%";
    elem.firstChild.firstChild.lastChild.width = input3.value + "%";
  };
  fn3 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input1.value = (100 - input2.value) - input3.value;
    elem.firstChild.firstChild.firstChild.width = input1.value + "%";
    elem.firstChild.firstChild.lastChild.width = input3.value + "%";
  };
  fn4 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input1.value = elem.firstChild.firstChild.firstChild.width.replace("%","");
    input2.value = elem.firstChild.firstChild.firstChild.nextSibling.width.replace("%","");
    input3.value = elem.firstChild.firstChild.lastChild.width.replace("%","");
  };
  $("#basic2width1").bind("blur", fn1);
  $("#basic2width2").bind("blur", fn2);
  $("#basic2width3").bind("blur", fn3);
  $("#basicconfig2").bind("focus", fn4);
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
    elem.firstChild.firstChild.style.height = input1.value + "px";
    elem.firstChild.lastChild.style.height = input2.value + "px";
  };
  fn2 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input1.value = elem.firstChild.firstChild.clientHeight;
    input2.value = elem.firstChild.lastChild.clientHeight;
  };
  $("#basic3height1").bind("blur", fn1);
  $("#basic3height2").bind("blur", fn1);
  $("#basicconfig3").bind("focus", fn2);
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
    elem.firstChild.firstChild.style.height = input1.value + "px";
    elem.firstChild.firstChild.nextSibling.style.height = input2.value + "px";
    elem.firstChild.lastChild.style.height = input3.value + "px";
  };
  fn2 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input1.value = elem.firstChild.firstChild.clientHeight;
    input2.value = elem.firstChild.firstChild.nextSibling.clientHeight;
    input3.value = elem.firstChild.lastChild.clientHeight;
  };
  $("#basic4height1").bind("blur", fn1);
  $("#basic4height2").bind("blur", fn1);
  $("#basic4height3").bind("blur", fn1);
  $("#basicconfig4").bind("focus", fn2);
};

HN.Tools.Layouts.Basic.configure5 = function()
{
  var input1;
  var input2;
  var fn1;
  var fn2;
  var fn3;
  input1 = HN.Util.id("basic5height");
  input2 = HN.Util.id("basic5width");
  fn1 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    elem.style.height = input1.value + "px";
  };
  fn2 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    elem.style.width = input2.value + "px";
  };
  fn3 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input1.value = elem.clientHeight;
    input2.value = elem.clientWidth;
  };
  $("#basic5height").bind("blur", fn1);
  $("#basic5width").bind("blur", fn2);
  $("#basicconfig5").bind("focus", fn3);
};

HN.Tools.Layouts.Basic.configure6 = function()
{
  var input1;
  var input2;
  var fn1;
  var fn2;
  var fn3;
  input1 = HN.Util.id("basic6height");
  input2 = HN.Util.id("basic6width");
  fn1 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    elem.style.height = input1.value + "px";
  };
  fn2 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    elem.style.width = input2.value + "px";
  };
  fn3 = function(ev, ui) {
    var elem;
    elem = HN.Util.id(Register.currentlayout);
    input1.value = elem.clientHeight;
    input2.value = elem.clientWidth;
  };
  $("#basic6height").bind("blur", fn1);
  $("#basic6width").bind("blur", fn2);
  $("#basicconfig6").bind("focus", fn3);
};

HN.Tools.Layouts.Basic.configure7 = function()
{
  console.log("configure the line thickness...");
  var fn1;
  var fn2;
  fn1 = function(ev) {
    console.log("configure7 clicked...");
    console.log(ev.target.value);
    HN.Tools.Util.set_thickness(Register.selected, ev.target.value);
  };
  fn2 = function(ev, ui) {
  console.log("Existing thickness is "
              + HN.Tools.Util.get_thickness(Register.selected));
    var i;
    var thickness;
    thickness = HN.Tools.Util.get_thickness(Register.selected);
    console.log(document.configure_line.linethickness);
    for (i = 0; i < document.configure_line.linethickness.length; i++) {
      // console.log(document.configure_line.linethickness[i].value);
      if (document.configure_line.linethickness[i].value === thickness) {
        console.log(document.configure_line.linethickness[i].value);
        document.configure_line.linethickness[i].checked = true;
      }
    }
  };
  $("#basicconfig7").bind("click", fn1);
  $("#basicconfig7").bind("focus", fn2);
};

//
// After Create Fns
// Called after an instance is created...
//

HN.Tools.Layouts.Basic.aftercreate = function(id) {
  return function() { };
};

//
// Internal Functions
//
HN.Tools.Layouts.Basic.make_table = function(classname, rows, columns) {
  var i;
  var j;
  var elem;
  var body;
  var myrow;
  var width;
  width = 100/columns + "%";
  elem = document.createElement('table');
  elem.setAttribute("class", "layout " + classname);
  body = document.createElement("tbody");
  for (i = 0; i < rows; i += 1) {
    myrow = document.createElement('tr');
    for (j = 0; j < columns; j+= 1) {
      myrow.appendChild(document.createElement('td'));
      myrow.lastChild.setAttribute("class", "layout_target");
      if (i === 0) {
        myrow.lastChild.setAttribute("width", width);
      }
    }
    body.appendChild(myrow);
    elem.appendChild(body);
  }
  return elem;
};

HN.Tools.Layouts.Basic.make_fragment = function() {
  var html;
  var elem;
  html = "This fragment is to be used in building templates of web pages and will be replaced with a piece of a page later.";
  elem = document.createElement("div");
  elem.setAttribute("id", "fragment_1");
  elem.setAttribute("class", "fragmenttext layout");
  elem.innerHTML = html;
  return elem;
};

HN.Tools.Layouts.Basic.make_line = function() {
  var elem;
  elem = document.createElement("hr");
  elem.setAttribute("class", "line layout hn_normal");
  return elem;
};

HN.Tools.Layouts.Basic.make_blank = function() {
  var elem;
  elem = document.createElement("div");
  elem.setAttribute("class", "blankpanel layout");
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
HN.Tools.Util.new_layout(basic['basic5']);
//HN.Tools.Util.new_layout(basic['basic6']);
HN.Tools.Util.new_layout(basic['basic7']);
