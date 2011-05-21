/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true , maxerr: 10000 */
/*global HN: false, alert: false,  $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */
var hn = {},
    lang;

HN.GridRender = function () {

    var oldcell = "",
    api = {},
    notify, cleanUp, pokeIntoInput, inputBoxBlur;

    notify = function () {
        alert("The hypernumbers spreadsheet does not support your browser yet." +
              "\n\nThe webpage and wikipages both work with all browsers."+
              "\n\nPlease try Firefox, Safari or Chrome.");
    };
    
    cleanUp = function (cell, sheet) {
        var oldval   = "",
            oldcontents = "",
            oldref = {};
        if (oldcell !== "") {
            oldref = HN.Util.parseRef(oldcell);
            oldcontents = sheet.cell(oldref.obj.y, oldref.obj.x);
            oldval = oldcontents.value;
            pokeIntoInput(oldcell, oldval);
        }
        oldcell = cell;
    };
    
    pokeIntoInput = function (ref, val) {
        $(".inline[data-ref=" + ref + "]").text(val);
    };
    
    inputBoxBlur = function (e) {
        var path  = document.location.pathname,
            ref   = $(e.currentTarget).attr("data-ref"),
            cell  = HN.Util.parse_ref(ref),
            val   = $(e.currentTarget).text(),
            formula = hn.data.readCell(path, cell.y, cell.x).formula;
        pokeIntoInput(ref, val);
        // only fire if there has been a change
        if (val !== formula) {
            HN.Callbacks.inline_input(path, cell, val);
        }
    };

    function showDialog(msg) {
        
        var close = $("<form id='closedialog'>" +
                      "<input value='ok' class='button' type='submit' />" +
                      "</form>")
            .bind("submit", function (e) {
                e.preventDefault();
                $("#cover,#dialog").remove();
            });
        
        $("body")
            .append("<div id='cover'> </div>")
            .append("<div id='dialog'>" + msg + "</div>");
        
        $("#dialog").append(close);
    }
    
    function getInputValue($el) {

        if ($el[0].nodeName === "DIV") {
            return $el.find(":checked").val() || "";
        } else {
            return $el.val();
        }
    }

    api.bindInline = function (sheet) {
        // Only bind the inline stuff if you are on wiki page
        var params = HN.Util.getParams();
        if (params.view && (params.view !== "wikipage")) {
            // switch off the inline styling
            $(".inline").css("border", "none");
            return;
        }
        // handle enter in the input box
        $(".inline").click(function (e) {
            e.stopPropagation();
            var top = $(e.target).parent().position().top,
                left = $(e.target).parent().position().left,
                height = $(e.target).height(),
                width = $(e.target).width(),            
                cell = e.target.getAttribute("data-ref"),
                html = "<div id='inputbox' " +
                "data-ref='" + cell + "' " +
                "contenteditable='true' " +
                "style='" +
                "height:" + height + "px;" +
                "width:" + width + "px;" + 
                "overflow:hidden;" +
                "margin:1px;'>",
                wrapperstyle = "top:" + top +
                "px;left:" + left +
                "px;position:absolute;z-index:999;'",
                path     = document.location.pathname,
                ref      = HN.Util.parseRef(cell),
                contents = sheet.cell(ref.obj.y, ref.obj.x),
                formula  = "";
            if (contents.formula) {
                formula = contents.formula;
            }
            $("#clinput").attr("style", wrapperstyle);
            $("#clinput").html(html + formula + "</div>");
            $("#inputbox").blur(inputBoxBlur);
            $("#inputbox").focus();
            // now clean up the old current cell
            cleanUp(cell, sheet);
        });
        $("body").click(function (e) {
            $("#clinput").css("display", "none");
            $("#inputbox").css("display", "none");
            // there is no new 'old cell' so pass in an empty string
            cleanUp("", sheet);
        });
    };

    api.bindControls = function () {
        $("input[type=submit].hn-webcontrol").bind("mousedown", function (e) {
            var button  = $(this),
                payload = button.attr("data-payload"),
                url = button.attr("data-origin"),
                action = button.attr("data-action"),
                data = JSON.stringify({postcreatepages : payload});
            $.ajax({
                "type"     : "POST",
                "url"      : url,
                "dataType" : "json",
                "data"     : data,
                "success" : function (e) {if (e.status === "ok") {
                                           window.location = e.redirect;
                                         } else if (e.status === "err") {
                                             showDialog(e.response);
                                         };},
                "error"   : function (e) {showDialog("Server error: " +
                                                     e.status + ". "+
                                                     "This has been logged " +
                                                     "and will be " +
                                                     "investigated.");
                                         }
            });
        });
    };
                                                   
    api.bindForms = function (sheet) {

        var signupfn = function (e, email, sitetype, error) {

        // TODO : dirty dirty hack (for front page signups)
            
            e.preventDefault();
            
            $.ajax({
                "type"     : "POST",
                "url"      : "/_hooks/",
                "dataType" : "json",
                "data"     : JSON.stringify({"signup": {"email": email,
                                                       "sitetype": sitetype}}),
                "success"  : function (data) { 
                    if (data.result === "success") { 
                        document.location.href = data.url;
                    } else if (data.result === "error") { 
                        error(data.reason);
                    } 
                }
            });            
        };

        $("#signupform").bind("submit", function (e) { 
            if ($.browser.msie || $.browser.opera) {
                notify();
                return;
            }
            var email = $("#signupemail").val(),
                sitetype = $("#signupemail").attr("data-type-sitetype"),
                error = function (reason) { 
                    $("#signupform input").removeAttr("disabled");
                    $("#signupfeedback").empty().addClass("error")
                        .append("<strong>Error:</strong> " + reason);
                };
            $("#signupform input").attr("disabled", "disabled");
            $("#signupfeedback").empty().removeClass("error")
                .append("<img src='/img/loading2.gif' />" + 
                        " Building your site... ");
            signupfn(e, email, sitetype, error);
        });

        $("#signupdemoform").bind("submit", function (e) { 
            var email = "guest" + Math.floor(Math.random() * 999999) +
                "@hypernumbers.com",
                sitetype = "demo",
                error = function (reason) {
                    $("#signupdemofeedback").empty().addClass("error")
                        .append("<strong>Error:</strong> " + reason);
                };
            if ($.browser.msie || $.browser.opera) {
                notify();
                return;
            }
            $("#signupdemofeedback").empty().removeClass("error")
                .append("<img src='/img/loading2.gif' />" + 
                        " Building your demo site.");
            signupfn(e, email, sitetype, error);
        });
        
        $("input[type=submit].hninput").bind("mousedown", function (e) {

            var button = $(this),
                name        = button.attr("data-form-name"),
                inputs      = $("[data-name=" + name + "]"),
                values      = [],
                div         = document.createElement("div"),
                mungedvalue = "",
                rawvalue    = "";
            
            inputs.each(function (i) {
                rawvalue = $(this).attr("data-label");
                // munge it through a div to turn it into html from text
                $(div).html(rawvalue);
                mungedvalue = $(div).html();
                values.push({ 
                    "label"   : mungedvalue,
                    "formula" : getInputValue($(this))
                });
            });
            $.ajax({
                "type"     : "POST",
                "url"      : button.attr("data-origin"),
                "dataType" : "json",
                "data"     : JSON.stringify({
                    "postform": {
                        "results" : button.attr("data-results"),
                        "values"  : values
                    }
                }),
                "success"  : function (urm) {
                    showDialog(button.attr("data-response"));
                    api.render(sheet);
                },
                "error" : function (xhr, ajaxOptions, err) {
                    showDialog("There has been an error on this submission. " +
                               "It has been logged and will be investigated");
                    api.render(sheet);
                }
            });
        });

        // Now force the radio buttons to show as checked
        // this is a wierd bug thing in Chrome/Firefox
        $(".hninput input[type=radio][Checked=true]").attr("checked", "true");
    };
    
    api.render = function (sheet) { 
        var data   = sheet.pageData(),
            inner  = $("#inner"),
            bottom = null,
            y, x, cell, position, width, height, value, css, html,
            tmpheight, totalHeight = 0, totalWidth = 0, tmpWidth, 
            inp, ref, div, inlcss, outerpadding;

        inner.empty();

        // Need to precalculate offsets, will do some other way later
        for (y in data.data.cell) {
            if (data.data.cell.hasOwnProperty(y)) {
                tmpheight = 0;
                tmpWidth  = 0;
                
                for (x in data.data.cell[y]) {
                    if (data.data.cell[y].hasOwnProperty(x)) {
                        
                        cell     = sheet.cell(y, x);
                        position = sheet.cell_offset(y, x);
                        width    = sheet.cellWidth(cell, x);
                        height   = sheet.cellHeight(cell, y);
                        value    = cell.value || "";
                        css      = sheet.lookupCSSIndex(cell.style) || "";
                        if (position.left + width > tmpWidth) {
                            tmpWidth = position.left + width;
                        }
                        
                        if (bottom === null || (position.top + height) > bottom) { 
                            bottom = (position.top + height);
                        }                    
                    }
                }
                if (tmpWidth > totalWidth) {
                    totalWidth = tmpWidth;
                }
            }
            
            totalHeight += tmpheight;
        }
        
        for (y in data.data.cell) {
            if (data.data.cell.hasOwnProperty(y)) {
                for (x in data.data.cell[y]) {
                    if (data.data.cell[y].hasOwnProperty(x)) {
                        cell = sheet.cell(y, x);
                        
                        if (!cell.invisible) { 
                            
                            position = sheet.cell_offset(y, x);
                            width    = sheet.cellWidth(cell, x);
                            height   = sheet.cellHeight(cell, y);
                            value    = cell.value || "";
                            css      = sheet.lookupCSSIndex(cell.style) || "";
                            inp      = cell.input || "none";
                            ref = HN.Util.to_b26(parseInt(x, 10)) + y,
                            css += "-moz-border-radius: 4px 4px 4px 4px;"+
                                "-webkit-border-radius: 4px 4px 4px 4px;";
                            if (value || css || inp === "inline") {
                                if (inp === "inline") {
                                    inlcss = "style='width:" +
                                        (parseInt(width) - 8) +
                                        "px;height:" +
                                        (parseInt(height) - 4) + "px'";
                                    outerpadding = 1;
                                    div = "<div class='hn_padded'>" +
                                        "<div class='inline' " +
                                        inlcss + " data-ref='" +
                                        ref + "'>" + value + "</div></div>";
                                } else if (inp === "none") {
                                    outerpadding = 3;
                                    div = "<div data-ref='" + ref + "'>" +
                                        value + "</div>";
                                } else {
                                    // on spreadsheet pages the uniq id is
                                    // a pane ID - here we just use "wikipage"
                                    div = HN.Util.makeSelect(inp.select, value,
                                                             ref, "wikipage");
                                };
                                html = $(div)
                                    .attr("style", css)
                                    .css({
                                        "position" : "absolute",
                                        "overflow" : "hidden",
                                        "top"      : position.top + "px",
                                        "left"     : position.left + "px",
                                        "width"    : width - 4 + "px",
                                        "height"   : height - 2 + "px",
                                        "padding"  : "1px " + outerpadding + "px"
                                    });
                                
                                inner.append(html);
                            }
                        }
                    }
                }
            }
        }

        inner.css({
            "height"   : (bottom + 50) + "px"
        });

        $("#outer").css({
            "margin" : "0px auto",
            "width"  : totalWidth + "px"
        });

        api.bindInline(sheet);
        api.bindForms(sheet);
        api.bindControls();
        // Now bind the inline select boxes (do it only once!)
        // on spreadsheet pages the selects have a uniq pane id
        // on wiki pages the uniq id is wikipage
        HN.Util.setupInlineSelect("wikipage", "wikipage");
    };

    return api;
};

HN.RenderPage = (function () {

    var data,
        sitedata,
        sheets = [],
        gridRender = new HN.GridRender(),
        path       = document.location.pathname,
        options = {
            "stopUpdate"   : true,
            "dataLoaded"   : function () {
                sheets[path] = new HN.Sheet(path, data, false);
                gridRender.bindInline(sheets[path]);
                gridRender.bindForms(sheets[path]);
                gridRender.bindControls();
                document.body.focus();
                HN.Util.initLogin("web_or_wikipage");
                // Now bind the inline select boxes (do it only once!)
                // on spreadsheet pages the selects have a uniq pane id
                // on wiki pages the uniq id is wikipage
                HN.Util.setupInlineSelect("wikipage", "wikipage");
            },
            "dataReloaded" : function (data) {  
                sheets[path].reload_data(data);
                gridRender.render(sheets[path]);
            },
            "update"       : function () { 
                sheets[path].processLoaded();            
                gridRender.render(sheets[path]);
                HN.Includes.reload();
            }
        },
        successFun = function (data) {
            hn.functions = data.functions;
            hn.pages = data.pages;
            hn.groups = data.groups;
            hn.is_admin = data.is_admin;
            lang = data.lang;
            HN.Includes.reload();
        };
    sitedata = new HN.SiteData();
    sitedata.loadSiteData(successFun);
    data = new HN.Data(options);
    data.addPage(path);    
    // data has to be 'not encapsulated' to allow the callbacks to poke into it
    // fugly, fugly, fugly
    // basically the main sheet routines use a global HN.data to co-ordinate
    // and so we have to as well :(
    HN.data = data;
    hn = HN;
}());

