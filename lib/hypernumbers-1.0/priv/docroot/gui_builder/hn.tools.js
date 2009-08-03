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
  elemcanvas = HN.Util.id("canvas2");
  elemcanvheight = HN.Util.id("canvasheight");
  elemcanvwidth = HN.Util.id("canvaswidth");
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
  return this;
};

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
  element.style.height = this.settings[this.selected].height + "px";
  element.style.width  = this.settings[this.selected].width + "px";
};
