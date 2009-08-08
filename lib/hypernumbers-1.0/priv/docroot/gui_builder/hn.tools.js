/**
 * @class HN.Tools
 * provides tool support for the Gui Builder
 */

HN.Tools = function() {
  var iphone;
  var normal;
  var netbook;
  var custom;
  var elemcanvas;
  var elemcanvheight;
  var elemcanvwidth;
  var elemcanvpresets;
  var fn;
  var widgets;
  elemcanvas = HN.Util.id("canvas2");
  elemcanvheight  = HN.Util.id("canvasheight");
  elemcanvwidth   = HN.Util.id("canvaswidth");
  elemcanvpresets = HN.Util.id("canvaspresets");

  // First set up the various page size settings
  iphone  = {height: "600", width: "200"};
  normal  = {height: "600", width: "1000"};
  netbook = {height: "450", width: "600"};
  custom  = {height: "600", width: "1000"};
  this.settings =  {"iphone":  iphone,
                    "normal":  normal,
                    "netbook": netbook,
                    "custom":  custom};
  this.selected = "normal";
  //
  // Now add a function to the defaults box
  //
  fn = function(e) {
    Tools.change_selected(e.target.value);
  };
  HN.Util.addEvent(elemcanvpresets, "change", fn);

  fn = function(e) {
    var elem1 = elemcanvheight;
    var elem2 = elemcanvwidth;
    var elem3 = elemcanvpresets;
    Tools.set_new_custom_size(elem1.value, elem2.value);
    elem3.selectedindex=3;
    elem3.options[3].selected=true;
    Tools.change_selected("custom");
  };
  HN.Util.addEvent(elemcanvheight, "change", fn);
  HN.Util.addEvent(elemcanvwidth,  "change", fn);

  elemcanvheight.value = elemcanvas.clientHeight;
  elemcanvwidth.value  = elemcanvas.clientWidth;
  elemcanvpresets.options[0].selected=true;
  //
  // Now initialise the Templates and Toolbox
  //
  this.templates = new HN.Tools.Templates();
  //this.toolbox = new HN.Tools.Toolbox();
};

//
// Methods of the class
//
HN.Tools.prototype.set_new_custom_size = function(height, width)
{
  this.settings['custom'].height = height;
  this.settings['custom'].width  = width;
  this.selected="custom";
};

HN.Tools.prototype.change_selected = function(newselection) {
  this.selected = newselection;
  this.set_canvas_size();
  this.resize_canvas();
};

HN.Tools.prototype.set_canvas_size = function() {
  var element;
  element = HN.Util.id("canvasheight");
  element.value = this.settings[this.selected].height;
  element = HN.Util.id("canvaswidth");
  element.value= this.settings[this.selected].width;
};

HN.Tools.prototype.resize_canvas = function() {
  var element;
  element = HN.Util.id("canvas2");
  element.style.minHeight = this.settings[this.selected].height + "px";
  element.style.width  = this.settings[this.selected].width + "px";
};

//
// Internal functions
//
HN.Tools.get_new_id = function()
{
  return 'id_' + new Date().getTime();
};

HN.Tools.make_new_elem = function(type) {
  switch (type) {
    case "template1x2":
      return HN.Tools.make_table(1, 2);
  case "template1x3":
      return HN.Tools.make_table(1, 3);
    case "template2x1":
      return HN.Tools.make_table(2, 1);
    case "template3x1":
      return HN.Tools.make_table(3, 1);
    case "lable":
      return HN.Tools.make_elem("lable", "Lable");
    case "smalllable":
      return HN.Tools.make_elem("smalllable", "Small Lable");
    case "largelable":
      return HN.Tools.make_elem("largelable", "Large Lable");
    case "largestlable":
      return HN.Tools.make_elem("largestlable", "Largest Lable");
    case "image":
      return HN.Tools.make_elem("image", "Image");
    case "textinput":
      return HN.Tools.make_elem("inputtext", "Input Text");
    case "radiobutton":
      return HN.Tools.make_elem("inputradio", "Radio Button");
    case "dropdownmenu":
      return HN.Tools.make_elem("select", "Drop Down");
    default:
      console.log("hit default...");
     };
};

HN.Tools.make_elem = function(type, text) {
  var new_elem;
  var text_elem;
  new_elem = document.createElement('div');
  new_elem.setAttribute("class", "hn htmltag dynamic unconfigured " + type);
  text_elem = document.createTextNode(text);
  new_elem.appendChild(text_elem);
  return new_elem;
};

HN.Tools.make_table = function(rows, columns) {
  var i;
  var j;
  var elem;
  var myrow;
  var width;
  width = 100/columns + "%";
  elem = document.createElement('table');
  elem.setAttribute("class", "container");
  for (i = 0; i < rows; i += 1) {
    myrow = document.createElement('tr');
    for (j = 0; j < columns; j+= 1) {
      myrow.appendChild(document.createElement('td'));
      myrow.lastChild.setAttribute("class", "container_td");
      if (i === 0) {
        myrow.lastChild.setAttribute("width", width);
      }
    }
    elem.appendChild(myrow);
  }
  return elem;
};