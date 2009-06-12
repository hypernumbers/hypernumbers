/**
 * @class HN.ToolBar
 * provides functionality for the styles toolbar
 */
HN.ToolBar = function(layout)
{
  var that = this;
  this.formula = "";
  this.functions = {};

  this.layout = layout;
  this.drop_menus();
  this.dialogs();
  this.show_hidden();
  this.loadFunctions();
  this.addPages();
  this.fileUpload();
  this.newPage();

  this.intro_pages = $("#introwrapper").children().length;
  this.intro_current = 1;
  this.show_intro_page(1);
  this.setup_intro();

  var add_events = function(id, style, val) {
    HN.Util.addEvent(HN.Util.id(id), "mousedown", function(e) {
      if(e.target.nodeName== "A") {
       that.setStyle(style, val(e.target));
      }
    });
  };

  var split = function(obj) {
    return obj.getAttribute("id").split("_")[1];
  };

  HN.Util.addEvent(HN.Util.id("formats"), "mousedown", function(e) {
    HN.Util.trail("looking at format menu");
    if(e.target.nodeName== "A") {
      that.setFormat(e.target.getAttribute("id"));
    }
  });

  HN.Util.addEvent(HN.Util.id("deletebtn"), "mousedown", function(e) {
    HN.Callbacks.deletePage();
  });

  HN.Util.addEvent(HN.Util.id("functionsbtn"), "mousedown", function(e) {
    HN.Util.trail("looking at functions dialog box from btn");
    HN.Util.id("functions").style.display = "block";
  });

  HN.Util.addEvent(HN.Util.id("functionshbtn"), "mousedown", function(e) {
    HN.Util.trail("looking at functions dialog box from help");
    HN.Util.id("functions").style.display = "block";
  });


  HN.Util.addEvent(HN.Util.id("markbtn"), "mousedown", function(e) {
    HN.Util.trail("looking at mark dialog box");
    that.dialog_open(HN.Util.id("mark"));
  });

  HN.Util.addEvent(HN.Util.id("introductionbtn"), "mousedown", function(e) {
    setTimeout(function() {
      that.run_intro();
    }, 50);
  });

  HN.Util.addEvent(HN.Util.id("submitmark"), "mousedown", function(e) {
    that.dialog_close(HN.Util.id("mark"));
    var mark = HN.Util.id("feedback");
    HN.Callbacks.setMark(mark);
    mark.value = "";
  });

  HN.Util.addEvent(HN.Util.id("borders"), "mousedown", function(e) {
    var where = split(e.target);
    var sel = that.layout.selection;
    var border = "solid";
    var border_style = "medium";
    var border_color = "#000000";
    HN.Callbacks.setBorders(sel.bounds, where, border, border_style, border_color);
  });

  add_events("ffamily","font-family", split);
  add_events("aligntext","text-align", split);
  add_events("bgcolors","background-color",
    function(obj) { return "#"+split(obj); });
  add_events("fontcolors","color",
    function(obj) { return "#"+split(obj); });
  add_events("bold","font-weight",
    function() { return "bold"; });
  add_events("italic","font-style",
    function() { return "italic"; });
  add_events("strike","text-decoration",
    function() { return "line-through"; });
  add_events("sizes","font-size",
    function(obj) { return split(obj)+"px"; });
};

HN.ToolBar.prototype.setup_intro = function()
{
  var t = this;

  $("#intronext").click( function() {
    var id = ( t.intro_current == t.intro_pages ) ? 1 : t.intro_current+1;
    t.show_intro_page(id);
  });

  $("#introback").click( function() {
    var id = ( t.intro_current == 1 ) ? t.intro_pages : t.intro_current-1;
    t.show_intro_page(id);
  });

};
HN.ToolBar.prototype.show_intro_page = function(x)
{
  this.intro_current = x;
  HN.Util.id("intropager").innerHTML = "Page "+this.intro_current+" of "
    +this.intro_pages+" Pages";
  $("#introwrapper").children().hide();
  $("#introwrapper").children().eq(x-1).show();
};


HN.ToolBar.prototype.run_intro = function()
{
  var intro = HN.Util.id("introduction");
  this.dialog_open(intro);
};

HN.ToolBar.prototype.newPage = function()
{
  var that = this;

  var is_valid_name = function(nm) {
    return nm !==  "" && nm.match(/^[0-9a-zA-Z_-]+$/);
  };

  var do_new = function(name) {
    var strip = name.split("/"), arr = [];
    for( var x = 0; x < strip.length; x++ ) {
      if ( is_valid_name(strip[x]) ) {
        arr.push(strip[x]);
      }
    }

    if( arr.length == 0 ) {
      $("#newpageerror").html("<strong>Error : </strong> The name can only contain letters,"
                              + "<br />numbers or the characters  \"_\" or \"-\"");
    } else {
      var url = "/u/"+that.layout.p.user+"/"+arr.join("/")+"/";
      window.open(url);
      HN.Util.id("newpagename").value = "";
      that.dialog_close(HN.Util.id("newpage"));
    }
  };

  HN.Util.addEvent(HN.Util.id("newpagebtn"), "mousedown", function(e) {
    setTimeout(function() {
      that.dialog_open(HN.Util.id("newpage"));
    },50);
  });

  HN.Util.addEvent(HN.Util.id("newpageform"), "submit", function(e) {
    do_new(HN.Util.id("newpagename").value);
    e.preventDefault();
    return false;
  });

};


HN.ToolBar.prototype.loadFunctions = function(e)
{
  var that = this;

  var fn = "/hypernumbers/fns_" + lang + ".json";
  $.getJSON(fn, function(data) {
    that.functions = data;
    that.generateFunCategories();
  });
};

HN.ToolBar.prototype.show_hidden = function()
{
  var hidden = HN.TOSHOW[HN.INSTALL];
  for( var x = 0; x < hidden.length; x++ ) {
    HN.Util.id(hidden[x]).style.display = "block";
  }
};

HN.ToolBar.prototype.generateFunCategories = function()
{
  var that = this, html = "", cat = {}, cathtml = "", funs = [],
    catdom = HN.Util.id("catlist"),
    fundom = HN.Util.id("funlist");

  cat["All Functions"] = [];
  for( var i=0, len=this.functions.length; i < len; i++ ) {
    var fun = this.functions[i], str = fun.category;
    funs[fun.name] = fun;
    if( typeof cat[fun.category] == "undefined") {
      cat[fun.category] = [];
    }
    cat["All Functions"].push(fun);
    cat[fun.category].push(fun);
  }

  for (var x in cat) {
    cathtml += "<option value=\""+x+"\">"+x+"</option>";
  }
  catdom.innerHTML = cathtml;

  HN.Util.addEvent(catdom, "change", function(e) {
    var index = catdom.selectedIndex;
    var category = catdom.childNodes[index].getAttribute("value");
    that.filterCategory(fundom, category, cat[category]);
  });

  var enter = function(e) {
    e.preventDefault();
    var sel = that.layout.selection;
    if( !sel.is_editing() ) {
      sel.startEditing("="+that.formula+"(");
      sel.formula.value = "="+that.formula+"(";
    } else {
      sel.formula.value += that.formula+"(";
      sel.input.value   += that.formula+"(";
    }
    sel.calculateInputWidth();
  };

  HN.Util.addEvent(HN.Util.id("enterformula"), "mousedown", enter);

  this.filterCategory(fundom, "All Functions", cat["All Functions"]);
  this.selectFun(cat["All Functions"][0]);
  HN.Util.addEvent(fundom, "mousedown", function(e) {
    e.preventDefault();
    if(e.target.nodeName == "A") {
      that.selectFun(funs[e.target.getAttribute("name")]);
    }
  });
};

HN.ToolBar.prototype.selectFun = function(fun)
{
  this.formula = fun.name;
  HN.Util.id("funname").innerHTML = fun.name;

  var funlist = HN.Util.id("funlist").childNodes[0];
  for( var i=0, len=funlist.childNodes.length; i < len; i++ ) {
    var a = funlist.childNodes[i].childNodes[0];
    a.style.backgroundColor = (a.getAttribute("name") == fun.name)
      ? "#FFF" : "";
  }
};

HN.ToolBar.prototype.filterCategory = function(fundom, name, funs)
{
  var html = "<ul>";
  for (var x in funs) {
    html += "<li><a name=\""+funs[x].name+"\">"+funs[x].name+"</a></li>";
  }
  fundom.innerHTML = html + "</ul>";
};

HN.ToolBar.prototype.addPages = function()
{
  function isEmpty(ob) {
    for( var i in ob ){
      if(ob.hasOwnProperty(i)) {
        return false;
      }
    }
    return true;
  }

  var add = function(obj, path, top) {
    var html = !top ? "<ul>" : "";
    for (var x in obj) {
      var p = path+x+"/";
      html += "<li><a href=\""+p+"\"> "+x+"</a>";
      html += (isEmpty(obj[x])) ? "" : add(obj[x], p, false);
      html += "</li>";
    }
    html += !top ? "</ul>" : "";
    return html;
  };

  $.getJSON("?pages", function(data) {

    if( !isEmpty(data) ) {
      var empty = HN.Util.id("empty");
      empty.innerHTML = add(data, "/", true);
    }

    $("#menu").filemenu();
  });

};


/**
 * Given format key, set the format for current selection
 */
HN.ToolBar.prototype.setFormat = function(value)
{
  var formats = {
    "fmt_0":"General",
    "fmt_1":"0",
    "fmt_2":"0.00",
    "fmt_3":"##0;[Red](#,##0)",
    "fmt_4":"##0;(#,##0)",
    "fmt_5":"##0.00;(#,##0.00)",
    "fmt_6":"\"$\"##0",
    "fmt_7":"\"$\"##0.00",
    "fmt_8":"\"$\"##0;[Red]\"$\"#,##0",
    "fmt_9":"0%",
    "fmt_10":"0.00%",
    "fmt_11":"d/m/yyyy",
    "fmt_12":"hh:mm:ss",
    "fmt_13":"d/m/yyyy hh:mm:ss"
  };

  var sel = this.layout.selection,
   format = formats[value];

  if( sel.is_selected() ) {
    HN.Callbacks.format(sel.bounds, format);
  }
};

/**
 * Set the style on current selection, bold / italic / strike styles
 * need to toggle based on current cell
 */
HN.ToolBar.prototype.setStyle = function(style, value)
{
  var sel = this.layout.selection,
    sheet = this.layout.s;

  var vals = {
    "font-weight":     {k : "font-weight:bold",             v:"normal"},
    "font-style":      {k : "font-style:italic",            v:"normal"},
    "text-decoration": {k : "text-decoration:line-through", v:"none"}
  };
  if(  sel.is_selected() ) {
    // Toggle bold / italic / strikethrough
    if( typeof vals[style] != "undefined" &&
        sheet.get_style_by_cell(sel.cell).match(vals[style].k) ) {
      value = vals[style].v;
    }
    HN.Callbacks.style(sel.bounds, style, value);
  }
};


// TODO: Clean up, code is nasty
HN.ToolBar.prototype.fileUpload = function()
{
  var that = this;

  HN.Util.addEvent(HN.Util.id("importbtn"), "mousedown", function(e) {
    setTimeout(function() {
      that.dialog_open(HN.Util.id("import"));
    }, 50);
  });

  var success = function(data, status) {
    HN.Util.id("importloading").style.display = "none";
    if( typeof data.error != "undefined") {
      $("#filewrapper").empty().html("<input type='file' name='Filedata' "
        + "id='Filedata' value='Upload Spreadsheet' />");
      setTimeout(function() {
        HN.Util.id("importerror").style.display = "none";
      }, 5000);
      HN.Util.id("importerror").style.display = "block";
      $("#Filedata").change(change);
    } else {
      document.location.href = data.location;
    }
  };

  var click = function() {
    $.ajaxFileUpload( {datatype: "json",
                       fileElementId:'Filedata',
                       success:success});

    HN.Util.id("importloading").style.display = "block";
    HN.Util.id("importerror").style.display = "none";
  };

  $("#doupload").click(click);
};

HN.ToolBar.prototype.dialog_close = function(dialog)
{
  var sel = this.layout.selection;

  sel.state = (sel.bounds.y1 == sel.bounds.y2
               && sel.bounds.x1 == sel.bounds.x2)
    ? HN.States.SELECTED_CELL : HN.States.SELECTED_RANGE;

  $(dialog).fadeOut("fast");
  $("#cover").fadeOut("fast");
};

HN.ToolBar.prototype.dialog_open = function(dialog)
{
  $(dialog).css("margin-left", -($(dialog).width()/2));
  $(dialog).css("margin-top", -($(dialog).height()/2));
  this.layout.selection.state = HN.States.NOT_EDITING;
  $(dialog).fadeIn("fast");
  $("#cover").fadeIn("fast");
};

/**
 * Setup the drop down menus (colors / alignment / font etc)
 */
HN.ToolBar.prototype.dialogs = function()
{
  var el = document.getElementsByClassName("dialog"),
     len = el.length,
    that = this,
     cur = null,
    dpos = {x:0, y:0},
   start = {x:0, y:0};


  $(".mclose").click( function() {
    that.dialog_close($(this).parent()[0]);
  });

  var up = function( e ) {
    HN.Util.removeEvent(document, "mouseup", up);
    HN.Util.removeEvent(document, "mousemove", move);
  };

  var move = function( e ) {
    var y = (dpos.y + (e.clientY - start.y));
    var x = (dpos.x + (e.clientX - start.x));
    cur.style.top  = (( y > 0 ) ? y : 0) + "px";
    cur.style.left = (( x > 0 ) ? x : 0) + "px";
  };

  var down = function( e ) {
    if( e.target.className == "close") {
      that.dialog_close(this);
    } else if( e.target.nodeName == "H2" || e.target.parentNode.nodeName == "H2" ) {
      e.preventDefault();
      cur   = this;
      dpos  = {x:parseInt(this.style.left), y:parseInt(this.style.top)};
      start = {x:e.clientX, y:e.clientY};

      HN.Util.addEvent(document, "mousemove", move);
      HN.Util.addEvent(document, "mouseup", up);
    }
  };

  for( var i = 0; i < len; i++ ) {
    el[i].style.top = "50px";
    el[i].style.left = "50px";
    el[i].innerHTML = "<div class='close'>&nbsp;</div>" + el[i].innerHTML;
    HN.Util.addEvent(el[i], "mousedown", down);
  }
};

/**
 * Setup the drop down menus (colors / alignment / font etc)
 */
HN.ToolBar.prototype.drop_menus = function()
{
  var el = document.getElementsByClassName("expand"),
     len = el.length;

  var click = function(e) {

    var parent = e.currentTarget;
    var menu = parent.childNodes[3];
    var name = parent.childNodes[3].id;
    HN.Util.trail("looking at " + name + " menu");

    HN.Util.removeEvent(parent, "mousedown", click);

    var hide = function(e) {
      if( e.target == menu ) {
        return;
      }
      HN.Util.addEvent(parent, "mousedown", click);
      HN.Util.removeEvent(document, "mousedown", hide);
      $(parent).removeClass("active");
      menu.style.display = "none";
    };

    $(parent).addClass("active");
    menu.style.display = "block";
    window.setTimeout(function() {
      HN.Util.addEvent(document, "mousedown", hide);
    },0);
  };

  for( var i = 0; i < len; i++ ) {
    HN.Util.addEvent(el[i], "mousedown", click);
  }
};

HN.ToolBar.prototype.user_navigation = function()
{
  var user = this.layout.p.user;

  if( user !== "anonymous" ) {

    HN.Util.id("loggedin").style.display = "block";
    HN.Util.id("home").innerHTML = user;
    HN.Util.id("home").setAttribute("href", "/u/"+user+"/");

    HN.Util.addEvent(HN.Util.id("logout"), "mousedown", function(e) {
      HN.Util.eraseCookie("auth");
      window.location.reload( true );
    });

    this.show_languages();
    HN.Util.addEvent(HN.Util.id("lang"), "mousedown", function(e) {
      var el = e.target.nodeName == "A" ? e.target : e.target.parentNode;
      HN.Callbacks.setLanguage(el.getAttribute("name"));
    });

  } else {
    HN.Util.id("anonymous").style.display = "block";
  }
};

HN.ToolBar.prototype.show_languages = function()
{
  var lang = HN.LANGUAGES[HN.INSTALL];
  $("#lang").children().each(function() {
    if( $.inArray($(this).attr("name"), lang) !== -1 ) {
      $(this).css("display", "block");
    }
  });
};