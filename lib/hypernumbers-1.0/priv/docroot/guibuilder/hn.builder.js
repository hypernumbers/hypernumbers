HN.Builder = function()
{
  // This is the html that is being built
  this.globid = 0;
  this.model = this.set_model(HN.Builder.DEFAULT);

  this.dom = {
    "preview":HN.Util.id("preview")
  };

  this.setup_events();
  this.select_tab("add");
  this.render_output();
};

//HN.Builder.DEFAULT = "<div style='padding:10px;'>\n"
//  +"\t<hn type='h1' value='Hello World' />"
//  +"\n\t<hn type='input' value='foo' />"
//  +"\n</div>";

HN.Builder.DEFAULT = "<div class='ventris'>"
  +"\n\t<div class='header'><h1>hypernumbers</h1></div>"
  +"\n\t<div class='container'>"
  +"\n\t\t<hn type='input' />"
  +"\n\t\t<div class='grid_8'>stuff<br />and more<br />stuff</div>"
  +"\n\t\t<div class='rightpanel'>right</div>"
  +"\n\t\t<div class='grid_12'>footer</div>\n\t</div>\n</div>";

// Takes a string of html, gets jquery to build a dom object from it,
// then adds id's to each element so they can be referenced after cloning
HN.Builder.prototype.set_model = function(str)
{
  var base = $(str)[0];
    globid = 0;

  var addIds = function(obj)
  {
    if( (obj.nodeName == "DIV" || obj.nodeName == "HN")
      && obj.getAttribute("data-hnid") == null ) {
      obj.setAttribute("data-hnid", ""+globid++);
    }
    for( var i = 0; i < obj.childNodes.length; i++ ) {
      addIds(obj.childNodes[i]);
    }
  };

  addIds(base);
  return base;
};

HN.Builder.prototype.select_tab = function(id) {
  $("#tabs a").removeClass("selected");
  $("#tabs a[name="+id+"]").addClass("selected");
  $(".configurepanel .tab").hide();
  $("#"+id).show();
};

HN.Builder.prototype.setup_events = function()
{
  var that = this;

  $(".togglesubpanel").bind("click", function() {

    var parent = $(this).parent("div");
    if( parent.hasClass("disabled") ) {
      parent.removeClass("disabled").children(".subpanel").show();
    } else {
      parent.addClass("disabled").children(".subpanel").hide();
    }
  });


  $("#tabs a").bind("click", function() {
    that.select_tab($(this).attr("name"));
  });

  HN.Util.addEvent(HN.Util.id("toggleview"), "change", function() {
    if( this.checked ) {
      that.view_code();
    } else {
      that.model = that.set_model($("#previewcode")[0].value);
      that.render_output();
    }
  });

  HN.Util.addEvent(HN.Util.id("add"), "mousedown", function(e) {
    if( e.target.nodeName == "INPUT") {
      that.start_drag(e.target);
    }
  });

  HN.Util.addEvent(this.dom.preview, "click", function() {
    that.select_tab("configure");
    HN.Builder.Widgets.configure(this);
  });
};

HN.Builder.prototype.start_drag = function(orig)
{
  var that = this,
       obj = null,
droptarget = null,
    placer = $("<div id='placer'> </div>");

  var check_target = function(obj) {

    if( droptarget && obj == droptarget[0] || obj == placer[0]) {
      return;
    }

    if( obj.className && obj.className.match("hn") ) {
      droptarget = $(obj).append(placer.remove());
    } else if( obj == that.dom.preview ) {
      droptarget = $(that.dom.preview).append(placer.remove());
    } else if ( obj.nodeName == "DIV"
                && obj.getAttribute("data-hnid") !== null ) {
      droptarget = $(obj).append(placer.remove());
    }
  };

  var move = function(e) {
    var css = {top:e.clientY+10+"px", "left":e.clientX+10+"px"};
    if( obj === null ) {
      obj = $(orig).clone().css({"position":"absolute"})
        .css(css).appendTo("body");
    } else {
      obj.css(css);
      check_target(e.target);
    }
  };

  var up = function(e) {

    placer.remove();
    if( obj ) {
      obj.remove();
    }
    HN.Util.removeEvent(document, "mousemove", move);
    HN.Util.removeEvent(document, "mouseup", up);

    that.addElement(orig, droptarget);
  };

  HN.Util.addEvent(document, "mousemove", move);
  HN.Util.addEvent(document, "mouseup", up);
};

HN.Builder.prototype.addElement = function(obj, target)
{
  var el = $("<hn type='input' data-hnid='"+(this.globid++)+"'' />")[0];

  if( target === null || target[0] == this.dom.preview ) {
    this.model.appendChild(el);
  } else {
    var id = target[0].getAttribute("data-hnid");
    var targ = $(this.model).find("[data-hnid="+id+"]")[0];
    targ.parentNode.insertBefore(el, targ.nextSibling);
  }
  this.render_output();
};

HN.Builder.prototype.view_code = function()
{
  this.dom.preview.innerHTML =
      "<textarea id='previewcode'>"
      +HN.Builder.render_html(this.model)+"</textarea>";
};

HN.Builder.prototype.render_output = function()
{
  var toRender = $(this.model).clone()[0];
  $(this.dom.preview).empty().append(this.render_model(toRender, "preview"));

};

HN.Builder.prototype.render_model = function(obj, rendertype)
{
  if( obj.nodeName == "HN" ) {
    return HN.Builder.objToHtml(obj, rendertype);
  } else {
    for(var i = 0; i < obj.childNodes.length; i++) {
      var nchild = this.render_model(obj.childNodes[i], rendertype);
      obj.replaceChild(nchild, obj.childNodes[i]);
    }
    return obj;
  }
};

HN.Builder.render_html = function(obj)
{
  if( obj.nodeName == "#text" ) {
    return obj.textContent;
  }

  var tag = obj.nodeName.toLowerCase();

  for( var children="",  i=0; i<obj.childNodes.length; i++ ) {
      children += this.render_html(obj.childNodes[i]);
    }

  for( var attr = "", x = 0; x<obj.attributes.length; x++ ) {
    var tmp = obj.attributes[x];
    if( tmp.nodeName != "data-hnid" ) {
      attr += " "+tmp.nodeName+"='"+tmp.nodeValue+"'";
    }
  }

  return "<"+tag+attr+">" + children + "</"+tag+">";
};

HN.Builder.objToHtml = function(obj, render)
{
  var type = obj.getAttribute("type");
  var attr = (render == "preview")
    ? " data-hnid='"+obj.getAttribute("data-hnid")+"'"
    : "";

  attr += " class='hn'";

  if( type == "h1" ) {
    return $("<h1"+attr+">"+obj.getAttribute("value")+"</h1>")[0];
  } else if( type == "input" ) {
    return $("<div"+attr+"><input type='text' /></div>")[0];
  }
  throw "crap";
};

HN.Builder.Widgets = {};
HN.Builder.Widgets.configure = function(element)
{
  console.log(element);
};


var sheet, layout;
var options = {

  dataLoaded: function() {
    new HN.Builder();
    sheet  = new HN.Sheet(data);
    layout = new HN.Layout(sheet);
  },

  dataReloaded: function(data) {
    console.log("reload");
  },

  update: function() {
    sheet.calc_size();
    layout.panes.refresh();
    layout.selection.show_selection();
  }
};

var data = new HN.Data(document.location.pathname, options);
