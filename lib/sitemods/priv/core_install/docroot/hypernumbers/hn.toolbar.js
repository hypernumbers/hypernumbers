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

    this.fileUpload();

    var enable_paste = function() {
        if ( HN.Util.readCookie("copied") !== null ) {
            $(".paste").removeClass("disabled");
        }
    };

    HN.Util.addEvent(window, "focus", function(e) {
        enable_paste();
    });
    enable_paste();

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
        if(e.target.nodeName== "A") {
            that.setFormat(e.target.getAttribute("id"));
        }
    });

    HN.Util.addEvent(HN.Util.id("functionsbtn"), "mousedown", function(e) {
        HN.Util.id("functions").style.display = "block";
    });

    HN.Util.addEvent(HN.Util.id("submitmark"), "mousedown", function(e) {
        HN.UIActions.close_dialog(that.layout, "mark");
        var mark = HN.Util.id("feedback");
        HN.Callbacks.setMark(mark);
        mark.value = "";
    });

    HN.Util.addEvent(HN.Util.id("rtebtn"), "mousedown", function(e) {
        var c    = that.layout.selection.cell;
        var cell = that.layout.s.cell(c.y, c.x);
        $("#wmd-preview").val("");
        $("#wmd-input").val(cell ? cell.formula : "");
        HN.UIActions.open_dialog(that.layout, "rte_editor");
    });

    HN.Util.addEvent(HN.Util.id("rte_submit"), "click", function(e) {
        var val = $("#wmd-input").val();
        HN.Callbacks.set_cell(that.layout.s.path(),
                               that.layout.selection.cell, val);
        HN.Callbacks.format(that.layout.s.path(),
                               that.layout.selection.bounds, "Markdown");
        HN.UIActions.close_dialog(that.layout, "rte_editor");
    });

    HN.Util.addEvent(HN.Util.id("borders"), "mousedown", function(e) {
        if( e.target.nodeName== "A"
            && e.target.getAttribute("id") !== "border-head" ) {
            var where = split(e.target);
            var sel = that.layout.selection;
            var border = "1px";
            var border_style = "solid";
            var border_color = "#000000";

            HN.Callbacks.setBorders(that.layout.s.path(),
                                      sel.bounds, where, border,
                                      border_style, border_color);
        }
    });

    add_events("ffamily","font-family", split);
    add_events("aligntext","text-align", split);
    add_events("bgcolors","background-color",
               function(obj) { return "#"+obj.getAttribute("data-color"); });
    add_events("fontcolors","color",
               function(obj) { return "#"+obj.getAttribute("data-color"); });
    add_events("bold","font-weight",
               function() { return "bold"; });
    add_events("wordwrap","white-space",
               function() { return "normal"; });
    add_events("italic","font-style",
               function() { return "italic"; });
    add_events("strike","text-decoration",
               function() { return "line-through"; });
    add_events("sizes","font-size",
               function(obj) { return split(obj)+"px"; });

    var opts = {
        "callback" : function(el) {
            return layout.selection.menuItemPicked(el);
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
        "fmt_1":"0,0",
        "fmt_2":"0,0.00",
        "fmt_3":"##0,0;[Red](#,##0,0)",
        "fmt_4":"##0,0;(#,##0,0)",
        "fmt_5":"##0,0.00;(#,##0,0.00)",
        "fmt_6":"\"$\"##0,0",
        "fmt_7":"\"$\"##0,0.00",
        "fmt_8":"\"$\"##0,0;[Red]\"$\"#,##0,0",
        "fmt_9":"0%",
        "fmt_10":"0.00%",
        "fmt_11":"d/m/yyyy",
        "fmt_12":"hh:mm:ss",
        "fmt_13":"d/m/yyyy hh:mm:ss",
        "fmt_14":"Markdown"
    };

    var sel = this.layout.selection,
    format = formats[value];

    HN.Callbacks.format(layout.s.path(), sel.bounds, format);
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
        "white-space":     {k : "white-space:normal",           v:"nowrap"},
        "font-style":      {k : "font-style:italic",            v:"normal"},
        "text-decoration": {k : "text-decoration:line-through", v:"none"}
    };

    // Toggle bold / italic / strikethrough
    if( typeof vals[style] != "undefined" &&
        sheet.lookupCSS(sel.cell.y, sel.cell.x).match(vals[style].k) ) {
        value = vals[style].v;
    }
    HN.Callbacks.style(sel.layout.s.path(), sel.bounds, style, value);
};


// TODO: Clean up, code is nasty
HN.ToolBar.prototype.fileUpload = function()
{
    var that = this;

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

HN.ToolBar.prototype.user_navigation = function(user)
{
    if( user !== "anonymous" ) {

        HN.Util.id("loggedin").style.display = "block";
        HN.Util.id("home").innerHTML = user;
        HN.Util.id("home").setAttribute("href", "/u/"+user+"/");

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