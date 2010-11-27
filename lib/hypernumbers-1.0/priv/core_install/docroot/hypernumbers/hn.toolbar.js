/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 10000 */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false, Finder: false */

/**
 * @class HN.ToolBar
 * provides functionality for the styles toolbar
 */
HN.ToolBar = function (layout) {
    
    var api       = {},
        editor    = HN.Util.id("editor"), 
        formula   = "",
        newPath;
    
    api.loadSite = function () {
        api.loadFunctions();
        api.loadPages();
        api.loadViews();
    };

    api.pokeGroup = function (group) {
        hn.groups.push(group);
    };

    api.loadPages = function () {
        if (hn.pages.length === 0) {
            // not loaded yet
            return;
        }
        var opts = {
            "menuPicked"   : function () {
                hn.ctrlPanel.open();
            },
            "activate"     : function () { },
            "deactivate"   : function () { },
            "itemSelected" : function () {
                hn.ctrlPanel.pageSelected(this.asList());
            },
            "itemChosen"   : function () {
                newPath = this.asString();
                window.location.hash = hn.hashUrl.setParam("path", newPath);
                hn.ctrlPanel.close();
            }
        };
        
        hn.finder = new Finder($("#finder").eq(0), hn.pages, opts);
        hn.finder.select(HN.Util.pathToList(newPath));
        
        $("#pdvisit").bind("click", function () {
            hn.finder.chooseCurrent();
        });    
    };
    
    api.userNavigation = function (user) {
        
        if (user !== "anonymous") {
            
            HN.Util.id("loggedin").style.display = "block";
            HN.Util.id("home").innerHTML = user;
            
            HN.Util.addEvent(HN.Util.id("logout"), "mousedown", function (e) {
                HN.Util.logout();
            });
            
            HN.Util.addEvent(HN.Util.id("lang"), "mousedown", function (e) {
                var el = (e.target.nodeName === "A") ?
                    e.target : e.target.parentNode;
                HN.Callbacks.setLanguage(el.getAttribute("name"));
            });
            
        } else {
            HN.Util.id("anonymous").style.display = "block";
        }
    };
    
    // this function should be refactored
    // the problem is that we use hn.data.key to reach into the bowels of the 
    // hn.data object and then push stuff onto groups when we should use the api to
    // do selective operations on stuff
    // caused by me being not so hot at javascript :(

    api.loadViews = function () {
        var path = layout.currentSheet().path(),
        perms = hn.data.readPermissions(path),
        table = HN.Util.makePermsTable(perms, path),
        user  = HN.Util.parseEmail(HN.Util.readCookie("auth")),
        clickPublic, advperms, advusers, p, g, grp, id;

        // Fire the bits in
        $("#permstablelayout").html(table);
        // Now the groups menu
        $("#addgroupmenu").html(HN.Util.makeGroupMenu());

        // Set up the permissions
        if (perms.champion) {
            $("#newviewas input[value=" + perms.champion + "]")
                .attr("checked", "checked");
        } else {
            $("#newviewas input[type=radio]").removeAttr("checked");
        }
        // first clear all attributes
        $(".groupinput").removeAttr("checked");
        for (p in perms.views) {
            if (perms.views.hasOwnName(p)) {
                perms.views[p].everyone ? 
                    $("#newpublic" + p).attr("checked", "checked") :
                    $("#newpublic" + p).removeAttr("checked");
                for (g in perms.views[p].groups) {
                    if (perms.views[p].groups.hasOwnName(g)) {
                        grp = perms.views[p].groups[g];
                        if (grp !== "admin") {
                            id = p + "_" + grp;
                            $("#" + id).attr("checked", "checked");
                        }
                    }
                }
            }
        }
        
        // Now set up the functions and stuff
        // First up the default options
        $("#newviewas input[name=newdefaultpage]").bind("change", function () {
            
            hn.data.pokeChampion(path, $(this).val());
            HN.Callbacks.setChampion(path, $(this).val(), null);
            
        });
        // Now define the handling functions for the 'everyone' stuff
        clickPublic = function () {
            var view = $(this).attr("data-value"),
                everyone = $(this).is(":checked");
            hn.data.pokePublic(path, view, everyone);
            HN.Callbacks.setView(path, view, everyone, perms.views[view].groups, null);
        };
        for (p in perms.views) {
            if (perms.views.hasOwnName(p)) {
                $("#newpublic" + p).bind("change", clickPublic);
            }
        }
        // and for the groups/views stuff
        $(".groupinput").bind("change", function () {
            var a = $(this).attr("id").split("_"),
            checked = $(this).is(":checked"),
            view = a[0],
            group = a[1],
            newperms = {};
            if (checked) {
                hn.data.pokeGroup(path, view, group);
            } else {
                hn.data.unpokeGroup(path, view, group);
            }
            // need to reread the permission
            newperms = hn.data.readPermissions(path);
            HN.Callbacks.setView(path, view, newperms.views[view].everyone, 
                                 newperms.views[view].groups, null);
        });
        // We need to set up the showbasicgroups function
        // because we trigger in on setting the view up
        $("#showbasicgroups").bind("click", function () {
            $("#advperms").hide();
            $("#advusers").hide();
            $("#invitecolleague").val("workmate@example.com");
            $("#newcolleaguefeedback").html("&nbsp;");
            layout.grabFocus();
            $("#invitecolleague").focus();
            $("#permstable").show();
        });
        // Set up the basic button
        // If the user is "anonymous" we need to disable this lot 
        // because they need to see it, but they
        // can't do hee-haw
        
        if (user === "anonymous") {
            $("#newviewas #permstable input").attr("disabled", true);
            $("#addgroup").hide();
            $("#invite").hide();
        }
        // Now (if necessary) make the advanced options panel
        //
        if (hn.is_admin) {
            advperms = HN.Util.makeAdvPerms(perms);
            advusers = HN.Util.makeAdvUsers(perms);
            path     = layout.currentSheet().path();
            perms    = hn.data.readPermissions(path);
            
            // Set up the options panels
            $("#advpermstable").html(advperms);
            $("#advuserstable").html(advusers);
                        
            // Now set up the options bindings
            $("#showadvperms").bind("click", function () {
                $("#permstable").hide();
                $("#advusers").hide();
                $("#newgroup").val("");
                $("#newgroupfeedback").html("&nbsp;");
                // clean up the defaults
                $("#advperms input").removeAttr("checked");
                if (perms.champion) {
                    $("#advperms input[value=" + perms.champion + "]")
                        .attr("checked", "checked");
                } 
                layout.grabFocus();
                $("#newgroup").focus();
                $("#advperms").show();
            });
            $("#showadvusers").bind("click", function () {
                $("#permstable").hide();
                $("#advperms").hide();
                $("#emailforgroup").val("workmate@example.com");
                $("#newusermsg").val("Dear workmate, please take a look at this page...");
                $("#newuserfeedback").html("&nbsp;");
                $("#administrator").removeAttr("checked");
                $("#groups option").removeAttr("selected");
                layout.grabFocus();
                $("#emailforgroup").focus();
                $("#advusers").show();
            });
        } else {
            $("#showbasicgroups").hide();                
            $("#showadvperms").hide();                
            $("#showadvusers").hide();                
            $(".miniseparator").hide();
            $("#permstable").show();
        }
    };
    
    api.loadFunctions = function () {
        if (hn.functions.length === 0) {
            // not loaded yet
            return;
        }
        var html    = "",
            cat     = {},
            cathtml = "",
            funs    = {},
            catdom  = HN.Util.id("catlist"),
            fundom  = HN.Util.id("funlist"),
            i, len, fun, str, x, category, functionName, enter, sel;
        
        cat["All Functions"] = [];
        
        for (i = 0, len = hn.functions.length; i < len; i += 1) {
            
            fun = hn.functions[i];
            functionName = fun[1];
            category = fun[0];

            if  (category !== "category") { 
                funs[functionName] = fun;
                
                if (typeof cat[category] === "undefined") {
                    cat[category] = [];
                }
                cat["All Functions"].push(fun);
                cat[category].push(fun);
            }
        }
        
        for (x in cat) {
            if (cat.hasOwnName(x)) {
                cathtml += "<option value=\"" + x + "\">" + x + "</option>";
            }
        }
        catdom.innerHTML = cathtml;
        
        HN.Util.addEvent(catdom, "change", function (e) {
            var index    = catdom.selectedIndex,
                category = catdom.childNodes[index].getAttribute("value");
            api.filterCategory(fundom, category, cat[category]);
        });
        
        enter = function (e) {
            e.preventDefault();
            sel = layout.tabUI();
            //if (!sel.is_editing()) {
            //} else {
                //sel.editor.formula.value += formula+"(";
                //sel.editor.input.value   += formula+"(";
            //}
            sel.editor.calculateWidth();
        };
        
        HN.Util.addEvent(HN.Util.id("enterformula"), "mousedown", enter);
        
        api.filterCategory(fundom, "All Functions", cat["All Functions"]);
        api.selectFun(cat["All Functions"][0]);
        HN.Util.addEvent(fundom, "mousedown", function (e) {
            e.preventDefault();
            if (e.target.nodeName === "A") {
                api.selectFun(funs[e.target.getAttribute("name")]);
            }
        });
    };
    
    api.selectFun = function (fun) {
        formula = fun[1];
        HN.Util.id("funname").innerHTML = fun[1];
        HN.Util.id("fundesc").innerHTML = fun[2];
        
        var funlist = HN.Util.id("funlist").childNodes[0],
            len = funlist.childNodes.length, a, i;
        for (i = 0; i < len; i += 1) {
            a = funlist.childNodes[i].childNodes[0];
            a.style.backgroundColor = (a.getAttribute("name") === fun[1]) ?
                "#FFF" : "";
        }
    };

    api.filterCategory = function (fundom, name, funs) {
        var html = "<ul>", x;
        for (x in funs) {
            if (funs.hasOwnName(x)) {
                html += "<li><a name=\"" + funs[x][1] + "\">" +
                    funs[x][1] + "</a></li>";
            }
        }
        fundom.innerHTML = html + "</ul>";
    };

    /**
     * Given format key, set the format for current selection
     */
    api.setFormat = function (value) {
        var formats = {
            "fmt_0"   : "General",
            "fmt_1"   : "#,0",
            "fmt_2"   : "#,0.00",
            "fmt_3"   : "###,0;[Red](#,##0,0)",
            "fmt_4"   : "###,0;(#,##0,0)",
            "fmt_5"   : "###,0.00;(#,##0,0.00)",
            "fmt_6"   : "\"$\"###,0",
            "fmt_7"   : "\"$\"###,0.00",
            "fmt_8"   : "\"$\"###,0;[Red]\"$\"#,##0,0",
            "fmt_9"   : "0%",
            "fmt_10"  : "0.00%",
            "fmt_11"  : "d/m/yyyy",
            "fmt_11a" : "dd-mmm-yy",
            "fmt_11b" : "d-mmmm-yyyy",
            "fmt_12"  : "hh:mm:ss",
            "fmt_13"  : "d/m/yyyy hh:mm:ss"
        },
            sheet  = layout.currentSheet(),
            tab    = layout.tabUI(),        
            format = formats[value];

        HN.Callbacks.format(layout.currentSheet().path(), 
                            tab.currentSelectedBounds(), format);
    };

    function enableSiteMenu() {

        $("#sitemenulist a").bind("mousedown", function (e) {
            e.preventDefault();
            hn.ctrlPanel.selectTab($(e.currentTarget).attr("data-tab"));
            hn.ctrlPanel.open();
        });
    }

    function enablePaste() {
        if (HN.Util.localStorage() && localStorage.copied) {
            $(".paste").removeClass("disabled");
        }
    }

    function toggleButton(id) { 

        HN.Util.addEvent(HN.Util.id(id), "mousedown", function (e) {
            e.preventDefault();
            var cmd, param, toggle, style, value, sheet, cell, css, def;
            if (e.target.nodeName === "A") {                
                if (layout.tabUI().is_editing()) { 
                    cmd   = e.target.getAttribute("data-command");
                    param = e.target.getAttribute("data-param") || "";
                    document.execCommand(cmd, false, param);
                } else {                    
                    toggle = e.target.getAttribute("data-toggle");
                    style = e.target.getAttribute("data-style");
                    value = e.target.getAttribute("data-value") || 
                        e.target.getAttribute("data-param");
                    sheet = layout.currentSheet();

                    if (toggle !== undefined) {
                        cell = layout.tabUI().currentSelectedCell();
                        css  = sheet.lookupCSS(cell.y, cell.x);
                        def  = style + ":" + value;
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
    }

    function dialogButton(button, dialog) { 

        $("#" + dialog + " .cancel").bind("mousedown", function () { 
            HN.UI.close_dialog(layout, dialog);
            layout.tabUI().editor.restoreSelection();
        });

        $("#" + button).bind("mousedown", function () { 

            layout.tabUI().editor.saveSelection();
            HN.UI.open_dialog(layout, dialog);

            if (dialog === "insertlinkdialog") {
                var text = layout.tabUI().is_editing() ?
                    layout.tabUI().editor.selectionText() :
                    layout.tabUI().currentCellValue();
                $("#insertlinktext").val(text);
            }
            setTimeout(function () { 
                $("#" + dialog).find("input")[0].focus();
            }, 0);
        });
    }

    /**
     * Setup the drop down menus (colors / alignment / font etc)
     */
    function initViews() {
        
        $("#addcolleaguebutton").bind("click", function () {
            var path  = layout.currentSheet().path(),
                perms  = hn.data.readPermissions(path),
                email  = $("#invitecolleague").val(),
                bits   = [],
                groups = [],
                msg    = "Please take a look at this page";
            if (email === "") {
                return;
            } else if (email === "workmate@example.com") {
                $("#newcolleaguefeedback").html("please enter a real email");
                return;
            }
            bits = email.split("@");
            groups[0] = bits[0];
            $("#newcolleaguefeedback").html("user " + bits[0] + " invited");
            $("#invitecolleague").val("");
            HN.Callbacks.inviteUser(path, email, groups, perms.champion, msg);
        });
        // setting up the menu click
        $("#addgroup").bind("click", function (data) {
            var path  = layout.currentSheet().path(),
                perms = hn.data.readPermissions(path),
                pos = $(data.currentTarget).offset(),
                hide = function (e) {
                    if (HN.Util.is_inside(e.target, $("#addgroupmenu")[0])) {
                        var Groups   = [],
                            group    = $(e.target).text(),
                            everyone = false,
                            newperms = {},
                            view = perms.champion;
                        hn.data.pokePublic(path, view, everyone);
                        hn.data.pokeGroup(path, view, group);
                        // reread the permissons
                        newperms = hn.data.readPermissions(path);
                        HN.Callbacks.setView(path, view, everyone,
                                             newperms.views[view].groups, null);
                    }
                    HN.Util.removeEvent(document, "mousedown", hide);
                    $("#addgroupmenu").hide();
                };
            $("#addgroupmenu").css({"display" : "block",
                                    "top"     : pos.top + 12,
                                    "left"    : pos.left - 5});
            HN.Util.addEvent(document, "mousedown", hide);
        });
        // now bind the buttons
        $("#addnewgroupbutton").bind("click", function () {
            var path  = layout.currentSheet().path(),
                perms = hn.data.readPermissions(path),
                divarray = [],
                views    = [],
                name, i;
            name = $("#newgroup").val();
            if (name === "") {
                return;
            }
            divarray = $("#advperms :checked");
            for (i = 0; i < divarray.length; i = i + 1) {
                views.push($(divarray[i]).val());
            }
            $("#newgroupfeedback").html("group " + name + " added");
            $("#newgroup").val("");
            HN.Callbacks.createGroups(path, views, name);
        });
        $("#addnewuserbutton").bind("click", function  () {
            var path  = layout.currentSheet().path(),
                perms = hn.data.readPermissions(path),
                email   = "",
                groups  = [],
                gparray = [],
                msg     = "",
                admin   = false,
                i;
            email = $("#emailforgroup").val();
            if (email === "") {
                return;
            } else if (email === "workmate@example.com") {
                $("#newuserfeedback").html("please enter a real email");
                return;
            }                
            gparray = $("#groups :selected");
            for (i = 0; i < gparray.length; i = i + 1) {
                groups.push($(gparray[i]).val());
            }
            if ($("#administrator").is(":checked")) {
                groups.push("admin");
            }
            $("#newuserfeedback").html("user " + email + " added");
            msg = $("#newusermsg").val();
            HN.Callbacks.createUser(path, email, groups, msg);
        });
    }
    
    function dropMenus() {
        var i, el = $(".expand"),
            len   = el.length,
            click = function (e) {
                e.preventDefault();
                var parent = e.currentTarget,
                    menu   = $("#" + parent.id + " .expandinner")[0],
                    name = $("#" + parent.id + " .expandinner").attr("id"),
                    hide;

                HN.Util.removeEvent(parent, "mousedown", click);
                
                hide = function (e) {
 
                    if (e.target === menu || name === "newviewas" &&
                        HN.Util.is_inside(e.target, menu)) {
                        return;
                    }
                    if (name !== "sitemenulist") {
                        layout.resumeSelection();
                    }
                    HN.Util.addEvent(parent, "mousedown", click);
                    HN.Util.removeEvent(document, "mousedown", hide);
                    $(parent).removeClass("active");
                    menu.style.display = "none";
                };
                
                // need to initialise the newviewas menu (get focus etc)
                if (name === "newviewas") {
                    $("#showbasicgroups").trigger("click", []);
                }
                
                $(parent).addClass("active");
                menu.style.display = "block";
                window.setTimeout(function () {
                    HN.Util.addEvent(document, "mousedown", hide);
                }, 0);
            };
        
        for (i = 0; i < len; i += 1) {
            HN.Util.addEvent(el[i], "mousedown", click);
        }
    }

    function addButtons() { 

        var i, buttonIds = ["bold", "italic", "strike", "fontsizelist", 
                            "ffamily", "aligntext"];

        for (i = 0; i < buttonIds.length; i += 1) {
            toggleButton(buttonIds[i]);
        }
    }

    function cssValue(css, value) { 

        var i, tmp, styles = css && css.split(";") || [];

        for (i = 0; i < styles.length; i += 1) {
            tmp = styles[i].split(":");
            if (tmp[0] === value) { 
                return tmp[1];
            }
        }
        return false;
    }

    function savePickedColour(color, background, css) {

        var colVal = cssValue(css, "color"),
            bgVal = cssValue(css, "background-color"),
            obj    = {},
            recent = HN.Util.localStorage() && 
            localStorage.recentColours &&
            JSON.parse(localStorage.recentColours) ||
            HN.DEFAULT_COLOUR;

        obj.color = color.substr(1);
        obj["background-color"] = background.substr(1); // need to cos of the name
        
        recent.combos.unshift(obj);
        recent.combos.pop();

        if (HN.Util.localStorage()) {
            localStorage.recentColours = JSON.stringify(recent);
        }
    }
    
    function colorPickers() { 

        var opts, colors, hide, actualHide, click, el, i;

        opts = {
            "pickerOnfocus"   : true,
            "pickerPosition"  : "bottom",
            "pickerZIndex"    : 999999999,
            "pickerFace"      : 5,
            "pickerFaceColor" : "white",
            "pickerBorder"    : 1,
            "hash"            : true
        };
        
        colors = {
            "forecolor" : new jscolor.color(HN.Util.id("forecolor"), "foreground", opts), 
            "bgcolor"   : new jscolor.color(HN.Util.id("bgcolor"), "background", opts)
        };

        $(".colormenu form").bind("submit", function (e) { 
            
            e.preventDefault();
            
            var wrapper = $(this).parent().parent(),
                input   = $(this).find("input"),
                jscolor = colors[input.attr("id")],
                col     = input[0].value,
                style   = $(this).attr("data-style"),
                sheet   = layout.currentSheet(),
                cell    = layout.tabUI().currentSelectedCell(),
                css     = sheet.lookupCSS(cell.y, cell.x),
                color   = "",
                backgroundcolor = "",
                rgb = [];
            actualHide(wrapper[0]);
            if (col.length === 7) {
                rgb[0] = parseInt(col.substr(1, 2), 16) / 255;
                rgb[1] = parseInt(col.substr(3, 2), 16) / 255;
                rgb[2] = parseInt(col.substr(5, 2), 16) / 255;
            } else if (col.length === 4) {
                rgb[0] = parseInt(col.charAt(1) + col.charAt(1), 16) / 255;
				        rgb[1] = parseInt(col.charAt(2) + col.charAt(2), 16) / 255;
				        rgb[2] = parseInt(col.charAt(3) + col.charAt(3), 16) / 255;
            }
            switch (style) {
            case "color":
                color = input[0].value;
                backgroundcolor = HN.Util.invertColor(rgb[0], rgb[1], rgb[2]);
                savePickedColour(color, backgroundcolor, css);
                break;
            case "background-color" :
                backgroundcolor = input[0].value;
                color = HN.Util.invertColor(rgb[0], rgb[1], rgb[2]);
                savePickedColour(color, backgroundcolor, css);
                break;
            }
            
            if (!layout.tabUI().is_editing()) {
                
                // var cmd   = e.target.getAttribute("data-command");
                // var param = e.target.getAttribute("data-param") || "";
                // document.execCommand(cmd, false, param);            
                // } else {                 
                HN.Callbacks.colours(sheet.path(),
                                     layout.tabUI().currentSelectedBounds(), 
                                     color, backgroundcolor);            
            }
        });

        actualHide = function (menu) { 
            colors.forecolor.hidePicker();
            colors.bgcolor.hidePicker();
            $(menu).removeClass("active");
            $(menu).find(".hiddenmenu").hide();
            HN.Util.addEvent(menu, "mousedown", click);
            $(document).unbind("mousedown.colorpicker");
            layout.resumeSelection();
            HN.UI.createComboList("#combocolorsbg", HN.THEMES);
        };
        
        click = function (e) {

            var current = e.currentTarget,
                menu    = $(current).find(".hiddenmenu");
            
            e.preventDefault();            
            HN.Util.removeEvent(current, "mousedown", click);
            
            hide = function (e) { 
                
                if (HN.Util.is_inside(e.target, menu[0]) ||
                    HN.Util.is_inside(e.target, $("#jscolor")[0])) { 
                    
                    if (e.target.nodeName === "A") { 
                        
                        var path   = layout.currentSheet().path(),
                            bounds = layout.tabUI().currentSelectedBounds(),
                            obj    = e.target;
                        
                        HN.Callbacks.colours(path, bounds, 
                                             "#" + obj.getAttribute("data-textcolor"),
                                             "#" + obj.getAttribute("data-bgcolor"));
                    } else { 
                        return;
                    }
                }
                
                actualHide(current);
            };

            layout.grabFocus();
            $(current).addClass("active");
            menu.show();
            
            window.setTimeout(function () {
                $(document).bind("mousedown.colorpicker", hide);
            }, 0);

        };

        el = $(".colormenu");
        for (i = 0; i < el.length; i += 1) {
            HN.Util.addEvent(el[i], "mousedown", click);
        }
    }

    function initEvents() {

        enableSiteMenu();

        // Enable paste menu if something has been copied in other window
        HN.Util.addEvent(window, "focus", function (e) {
            enablePaste();
        });
        enablePaste();
        
        $("#imageform").bind("submit", function (e) {

            e.preventDefault();

            var imgsrc = $("#insertimageinput").val();

            if (!layout.tabUI().is_editing()) { 
                layout.tabUI().startEditing("");
            } else { 
                layout.tabUI().editor.restoreSelection();
                layout.tabUI().editor.deleteSelection();                
            }
            
            layout.tabUI().editor.execCommand("insertimage", false, imgsrc);
            
            HN.UI.close_dialog(layout, "insertimagedialog", function () {
                $("#insertimageinput").val("http://");      
            });
        });        

        $("#linkform").bind("submit", function (e) {
            
            e.preventDefault();
            
            var linkhref = $("#insertlinkinput").val(),
                linktext = $("#insertlinktext").val(),
                html     = "<a href='" + linkhref + "'>" + linktext + "</a>";

            if (!layout.tabUI().is_editing()) { 
                layout.tabUI().startEditing("");
            } else { 
                layout.tabUI().editor.restoreSelection();
                layout.tabUI().editor.deleteSelection();
            }
            layout.tabUI().editor.execCommand("inserthtml", false, html);
            
            HN.UI.close_dialog(layout, "insertlinkdialog", function () { 
                $("#insertlinkinput").val("http://");      
                $("#insertlinktext").val("");
            });
        });        
        
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
        $("#funsclose").bind("mousedown", function (e) { 
            $("#functions").hide();
        });
        
        $("#forminput, #formtextarea, #formselect, #formradio")
            .bind("mousedown", function (e) {
                // Dale was being too fancy trying to make the input insertion smart
                // this is a bug - TODO let formula have trailing blanks...
                //var regex = /^=(input|textarea|radio|select)\(([\w\W]+)\)$/;
                //    current = layout.tabUI().is_editing()
                //      ? layout.tabUI().editor.selectionText()
                //      : layout.tabUI().currentCellFormula(),
                //    match = current.match(regex);
                
                //if (match) {
                //    params = match[2];
                //} else {
                //    params = $(this).attr("data-default")
                //        .replace(/'/g, "\"");
                //}
                var params = $(this).attr("data-default").replace(/'/g, "\""),
                    formula = "=" + $(this).attr("data-input") +
                    "`(" + params + ")";
                
                HN.Callbacks.set_cell(layout.currentSheet().path(), 
                                      layout.tabUI().currentSelectedCell(),
                                      formula);
            });

        $("#formbutton").bind("mousedown", function (e) {

            var button = "=button(\"Submit Form\", " +
                "\"Thanks for submitting my form\")";
            
            HN.Callbacks.set_cell(layout.currentSheet().path(), 
                                  layout.tabUI().currentSelectedCell(),
                                  button);
        });

        $("#mergecells").bind("mousedown", function (e) {
            var bounds = layout.tabUI().order_bounds(
                layout.tabUI().currentSelectedBounds());
            HN.Callbacks.mergeCells(layout.currentSheet().path(), 
                                    bounds);
        });

        $("#inlineinput").bind("mousedown", function (e) {
            var path, cell, range, input,
                status = "";
            path  = layout.currentSheet().path();
            cell  = layout.tabUI().currentSelectedCell();
            range = layout.tabUI().currentSelectedBounds();
            input = layout.currentSheet().cell(cell.y, cell.x).input;
            status = "";
            if (input === "inline") {
                status = "none";
            } else {
                status = "inline";
            }
            HN.Callbacks.set_input(path, range, status);
        });
    }
    
    dropMenus();
    initEvents();
    addButtons();
    colorPickers();
    dialogButton("insertimage", "insertimagedialog");
    dialogButton("insertlink", "insertlinkdialog");
    initViews();

    return api;
};
