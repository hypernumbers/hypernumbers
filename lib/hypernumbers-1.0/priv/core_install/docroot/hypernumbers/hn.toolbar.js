/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false */

/**
 * @class HN.ToolBar
 * provides functionality for the styles toolbar
 */
HN.ToolBar = function (layout)
{
    var that = this, enable_paste, split, add_events,
    editor = HN.Util.id("editor"), toggleButton, buttons, i;

    this.formula = "";
    this.functions = {};

    this.layout = layout;
    this.drop_menus();

    add_events = function (id, style, val) {
        HN.Util.addEvent(HN.Util.id(id), "mousedown", function (e) {
            if (e.target.nodeName === "A") {
                that.setStyle(style, val(e.target));
            }
        });
    };
    
    enable_paste = function () {
        if (HN.Util.readCookie("copied") !== null) {
            $(".paste").removeClass("disabled");
        }
    };

    HN.Util.addEvent(window, "focus", function (e) {
        enable_paste();
    });
    enable_paste();

    split = function (obj) {
        return obj.getAttribute("id").split("_")[1];
    };

    HN.Util.addEvent(HN.Util.id("formats"), "mousedown", function (e) {
        if (e.target.nodeName === "A") {
            that.setFormat(e.target.getAttribute("id"));
        }
    });

    HN.Util.addEvent(HN.Util.id("functionsbtn"), "mousedown", function (e) {
        HN.Util.id("functions").style.display = "block";
    });

    HN.Util.addEvent(HN.Util.id("borders"), "mousedown", function (e) {

        if (e.target.nodeName === "A" && 
            e.target.getAttribute("id") !== "border-head") {

            var where    = split(e.target),
            sel          = that.layout.selection,
            border       = "1px",
            border_style = "solid",
            border_color = "#000000",
            bounds       = layout.tabUI().currentSelectedBounds();

            HN.Callbacks.setBorders(layout.currentSheet().path(),
                                    bounds, where, border,
                                    border_style, border_color);
        }
    });

    // add_events("ffamily", "font-family", split);
    // add_events("aligntext", "text-align", split);
    // add_events("bgcolors", "background-color", function (obj) { 
    //     return "#" + obj.getAttribute("data-color"); 
    // });
    // add_events("fontcolors", "color", function (obj) { 
    //     return "#" + obj.getAttribute("data-color"); 
    // });
    // add_events("wordwrap","white-space",
    //            function() { return "nowrap"; });
    // add_events("italic","font-style",
    //            function() { return "italic"; });
    // add_events("strike","text-decoration",
    //            function() { return "line-through"; });

    var setNewStyle = function (style, value) {

        var sheet = this.layout.currentSheet();
        var tab   = this.layout.tabUI();
        var cell  = tab.currentSelectedCell();
    
        var vals = {
            "font-weight":     {k : "font-weight:bold",             v:"normal"},
            "white-space":     {k : "white-space:nowrap",           v:"normal"},
            "font-style":      {k : "font-style:italic",            v:"normal"},
            "text-decoration": {k : "text-decoration:line-through", v:"none"}
        };
        
        var css = sheet.lookupCSS(cell.y, cell.x);
        // Toggle bold / italic / strikethrough
        if (typeof vals[style] !== "undefined" && 
            (css && css.match(vals[style].k))) {
            value = vals[style].v;
        }
        
        HN.Callbacks.style(sheet.path(), 
                           tab.currentSelectedBounds(), style, value);
    };

    toggleButton = function (id, style, val, def) { 

        HN.Util.addEvent(HN.Util.id(id), "mousedown", function (e) {
            e.preventDefault();
            if (e.target.nodeName === "A") {                
                if (layout.tabUI().is_editing()) { 
                    var cmd   = e.target.getAttribute("data-command");
                    var param = e.target.getAttribute("data-param") || "";
                    document.execCommand(cmd, false, param);
                } else {
                    
                    var toggle = e.target.getAttribute("data-toggle");
                    var style = e.target.getAttribute("data-style");
                    var value = e.target.getAttribute("data-value");
                    var sheet = layout.currentSheet();

                    if (toggle !== undefined) {
                        var cell = layout.tabUI().currentSelectedCell();
                        var css  = sheet.lookupCSS(cell.y, cell.x);
                        var def = style + ":" + value;
                        if (css && css.match(def)) {
                            value = toggle;
                        }                        
                    }

                    HN.Callbacks.style(sheet.path(), 
                                       layout.tabUI().currentSelectedBounds(), 
                                       style, value);                    
                }
            }
        });        
    };
    
    
    buttons = [
        {"id": "bold", "style": "font-weight", "def": "normal", 
         "value": function () { return "bold"; }},
        {"id": "italic", "style": "font-style", "def": "italic", 
         "value": function () { return "italic"; }},
        {"id": "strike", "style": "text-decoration", "def": "strikethrough", 
         "value": function () { return "line-through"; }},
        {"id": "fontsizelist", "style": "text-decoration", "def": "strikethrough", 
         "value": function () { return "line-through"; }}
    ];


    for (i=0; i < buttons.length; i++) {
        btn = buttons[i];
        toggleButton(btn.id, btn.style, btn.value, btn.def);
    }
    
    var opts = {
        "callback" : function(el) {
            return layout.tabUI().menuItemPicked(el);
        }
    };
    HN.FileMenu($("#hnmenu"), opts);
};

HN.ToolBar.prototype.loadFunctions = function(data)
{
    if( data ) {
        this.functions = data;
        this.generateFunCategories();
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
        var sel = that.layout.tabUI();
        if( !sel.is_editing() ) {
            sel.startEditing("="+that.formula+"(");
            sel.formula.value = "="+that.formula+"(";
        } else {
            sel.c.formula.value += that.formula+"(";
            sel.c.input.value   += that.formula+"(";
        }
        sel.c.calculateWidth();
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
        html += "<li><a name=\""+funs[x].name+"\">"
            +funs[x].name+"</a></li>";
    }
    fundom.innerHTML = html + "</ul>";
};

/**
 * Given format key, set the format for current selection
 */
HN.ToolBar.prototype.setFormat = function(value)
{
    var formats = {
        "fmt_0":"General",
        "fmt_1":"#,0",
        "fmt_2":"#,0.00",
        "fmt_3":"###,0;[Red](#,##0,0)",
        "fmt_4":"###,0;(#,##0,0)",
        "fmt_5":"###,0.00;(#,##0,0.00)",
        "fmt_6":"\"$\"###,0",
        "fmt_7":"\"$\"###,0.00",
        "fmt_8":"\"$\"###,0;[Red]\"$\"#,##0,0",
        "fmt_9":"0%",
        "fmt_10":"0.00%",
        "fmt_11":"d/m/yyyy",
        "fmt_12":"hh:mm:ss",
        "fmt_13":"d/m/yyyy hh:mm:ss",
        "fmt_14":"Markdown"
    };
    
    var sheet = this.layout.currentSheet();
    var tab   = this.layout.tabUI();

    format = formats[value];

    HN.Callbacks.format(this.layout.currentSheet().path(), 
                        tab.currentSelectedBounds(), format);
};

/**
 * Set the style on current selection, bold / italic / strike styles
 * need to toggle based on current cell
 */
HN.ToolBar.prototype.setStyle = function(style, value)
{
    var sheet = this.layout.currentSheet();
    var tab   = this.layout.tabUI();
    var cell  = tab.currentSelectedCell();
    
    var vals = {
        "font-weight":     {k : "font-weight:bold",             v:"normal"},
        "white-space":     {k : "white-space:nowrap",           v:"normal"},
        "font-style":      {k : "font-style:italic",            v:"normal"},
        "text-decoration": {k : "text-decoration:line-through", v:"none"}
    };
    var css = sheet.lookupCSS(cell.y, cell.x);
    // Toggle bold / italic / strikethrough
    if (typeof vals[style] !== "undefined" && 
        (css && css.match(vals[style].k))) {
        value = vals[style].v;
    }

    HN.Callbacks.style(sheet.path(), 
                       tab.currentSelectedBounds(), style, value);
};

/**
 * Setup the drop down menus (colors / alignment / font etc)
 */
HN.ToolBar.prototype.drop_menus = function () {
    
    var el  = document.getElementsByClassName("expand");
    var len = el.length;

    var click = function(e) {

        e.preventDefault();

        var parent = e.currentTarget;
        var menu = parent.childNodes[3];
        var name = parent.childNodes[3].id;

        HN.Util.removeEvent(parent, "mousedown", click);

        var hide = function (e) {
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
        window.setTimeout(function () {
            HN.Util.addEvent(document, "mousedown", hide);
        },0);
    };

    for( var i = 0; i < len; i++ ) {
        HN.Util.addEvent(el[i], "mousedown", click);
    }
};

HN.ToolBar.prototype.user_navigation = function(user) {

    if( user !== "anonymous" ) {

        HN.Util.id("loggedin").style.display = "block";
        HN.Util.id("home").innerHTML = user;

        HN.Util.addEvent(HN.Util.id("logout"), "mousedown", function(e) {
            HN.Util.eraseCookie("auth");
            window.location.reload( true );
        });

        HN.Util.addEvent(HN.Util.id("lang"), "mousedown", function(e) {
            var el = ( e.target.nodeName == "A" )
                ? e.target : e.target.parentNode;
            HN.Callbacks.setLanguage(el.getAttribute("name"));
        });

    } else {
        HN.Util.id("anonymous").style.display = "block";
    }
};