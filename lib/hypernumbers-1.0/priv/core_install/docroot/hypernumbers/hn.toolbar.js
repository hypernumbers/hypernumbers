/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false */

/**
 * @class HN.ToolBar
 * provides functionality for the styles toolbar
 */
HN.ToolBar = function (layout) {
    
    var api   = {},
    editor    = HN.Util.id("editor"), 
    formula   = "",
    functions = {};

    dropMenus();
    initEvents();
    addButtons();
    colorPickers();

    api.userNavigation = function (user) {

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

    api.loadFunctions = function (data) {
        if( data ) {
            functions = data;
            api.generateFunCategories();
        }
    };
    
    api.generateFunCategories = function () {
        var html = "", cat = {}, cathtml = "", funs = [],
        catdom = HN.Util.id("catlist"),
        fundom = HN.Util.id("funlist");
        
        cat["All Functions"] = [];
        
        for( var i=0, len=functions.length; i < len; i++ ) {
            var fun = functions[i], str = fun.category;
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
            api.filterCategory(fundom, category, cat[category]);
        });
        
        var enter = function(e) {
            e.preventDefault();
            var sel = layout.tabUI();
            if( !sel.is_editing() ) {
                sel.startEditing("="+formula+"(");
            } else {
                //sel.editor.formula.value += formula+"(";
                //sel.editor.input.value   += formula+"(";
            }
            sel.editor.calculateWidth();
        };

        HN.Util.addEvent(HN.Util.id("enterformula"), "mousedown", enter);
        
        api.filterCategory(fundom, "All Functions", cat["All Functions"]);
        api.selectFun(cat["All Functions"][0]);
        HN.Util.addEvent(fundom, "mousedown", function(e) {
            e.preventDefault();
            if(e.target.nodeName == "A") {
                api.selectFun(funs[e.target.getAttribute("name")]);
            }
        });
    };

    api.selectFun = function(fun) {
        formula = fun.name;
        HN.Util.id("funname").innerHTML = fun.name;
        
        var funlist = HN.Util.id("funlist").childNodes[0];
        for( var i=0, len=funlist.childNodes.length; i < len; i++ ) {
            var a = funlist.childNodes[i].childNodes[0];
            a.style.backgroundColor = (a.getAttribute("name") == fun.name)
                ? "#FFF" : "";
        }
    };

    api.filterCategory = function (fundom, name, funs) {
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
    api.setFormat = function (value) {
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
        
        var sheet = layout.currentSheet();
        var tab   = layout.tabUI();

        format = formats[value];

        HN.Callbacks.format(layout.currentSheet().path(), 
                            tab.currentSelectedBounds(), format);
    };

    function enablePaste () {
        if (HN.Util.readCookie("copied") !== null) {
            $(".paste").removeClass("disabled");
        }
    };

    function toggleButton (id) { 

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
                    var value = e.target.getAttribute("data-value") || 
                        e.target.getAttribute("data-param");
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

    /**
     * Setup the drop down menus (colors / alignment / font etc)
     */
    function dropMenus () {
    
        var el  = document.getElementsByClassName("expand");
        var len = el.length;
        
        var click = function(e) {
            
            e.preventDefault();
            
            var parent = e.currentTarget;
            var menu   = parent.childNodes[3];
            var name   = parent.childNodes[3].id;
            
            HN.Util.removeEvent(parent, "mousedown", click);
            
            var hide = function (e) {
                if( e.target === menu ) {
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

    function addButtons () { 

        var i, 
        buttonIds = ["bold", "italic", "strike", "fontsizelist", 
                     "ffamily", "aligntext"];

        for (i=0; i < buttonIds.length; i++) {
            toggleButton(buttonIds[i]);
        }
    };

    function cssDefined (css, value) { 
        
        var i, tmp, styles = css.split(";");

        for(i=0; i<styles.length;i++) {
            if (styles[i].split(":")[0] == value) { 
                return true;
            }
        }
        return false;
    };

    function colorPickers () { 

        var opts, colors;

        opts = {
            "pickerOnfocus"   : false,
            "pickerPosition"  : "top",
            "pickerZIndex"    : 99999999999999,
            "pickerFace"      : 5,
            "pickerFaceColor" :"white",
            "pickerBorder"    : 0,
            "hash"            : true
        };
        
        colors = {
            "forecolor" : new jscolor.color(HN.Util.id("forecolor"), opts), 
            "bgcolor"   : new jscolor.color(HN.Util.id("bgcolor"), opts)
        };

        $(".colormenu form").bind("submit", function(e) { 
            
            e.preventDefault();
            
            var wrapper = $(this).parent().parent().parent(),
            input       = $(this).find("input"),
            jscolor     = colors[input.attr("id")],
            colour      = input[0].value,
            toggle      = $(this).attr("data-toggle"),
            style       = $(this).attr("data-style");
            
            wrapper.removeClass("active");
            wrapper.find(".hiddenmenu").hide();
            jscolor.hidePicker();
            
            if (layout.tabUI().is_editing()) { 
                // var cmd   = e.target.getAttribute("data-command");
                // var param = e.target.getAttribute("data-param") || "";
                // document.execCommand(cmd, false, param);            
            } else {                 
                HN.Callbacks.style(layout.currentSheet().path(),
                                   layout.tabUI().currentSelectedBounds(), 
                                   style, colour);            
            }
        });
        
        $(".colormenu").bind("mousedown", function (e) {
            
            var menu = $(this).find(".hiddenmenu"),
            input    = $(this).find("input"),
            jscolor  = colors[input.attr("id")];

            if (HN.Util.is_inside(e.target, menu[0])) { 
                e.preventDefault();
                return;
            } else {
                if (menu.is(":visible")) { 
                    $(this).removeClass("active");
                    menu.hide();
                    jscolor.hidePicker();
                } else { 
                    e.preventDefault();
                    $(this).addClass("active");
                    menu.show();
                    jscolor.showPicker();
                    setTimeout(function () { input[0].focus(); });
                }
            }
        });
    };

    function initEvents () {

        // Enable paste menu if something has been copied in other window
        HN.Util.addEvent(window, "focus", function (e) { enablePaste(); });
        enablePaste();
        
        // Formats menu
        $("#formats").bind("mousedown", function (e) {
            if (e.target.nodeName === "A") {
                api.setFormat(e.target.getAttribute("id"));
            }
        });
        
        // Functions Dialog
        $("#functionsbtn").bind("mousedown", function (e) { 
            $("#functions").show();
        });        
    };

    function calculateContrast(rgb) {
        return 0.213 * rgb[0] + 0.715 * rgb[1] + 0.072 * rgb[2] < 0.5 
            ? '#FFFFFF' : '#000000';
    };

    return api;
}