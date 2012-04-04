/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true , maxerr: 10000 */
/*global HN: false, alert: false,  $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */
var hn = {},
    lang;

HN.GridRender = function () {

    var oldcell = "",
    editor,
    currentCellInRichEdit = "",
    currRichEditPos = {},
    richEditorCreated = false,
    inEditor = false,
    api = {},
    notify, cleanUp, pokeIntoInput, inputBoxBlur, 
    stripHTMLfn, openRichEditor, closeRichEditor;

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
    
    openRichEditor = function () {
        var config = {
            width:  currRichEditPos.width, 
            height: currRichEditPos.height,
            controls: "pastetext " +
                "| style font " +
                "| alignleft center alignright " +
                "| bold italic strikethrough " +
                "| color " +
                "| bullets numbering | outdent indent " + 
                "| undo redo " +
                "| rule image link unlink | " +
                "| removeformat",
            fonts: "Sans Serif,Serif,Monospace,Arial," +
               "Georgia,Garamond,Trebuchet MS,Tahoma",
            styles: [["Bigger", "<h1>"], ["Big", "<h2>"], ["Larger", "<h3>"],
                        ["Large", "<h4>"],  ["Normal","<h5>"],  ["Small","<h6>"]],
            bodyStyle: "margin:0px; font:12px/1.7 Verdana,Tahoma,sans-serif; " + 
                "cursor:text"
        }, 
        offset       = {},
        urlref       = HN.Util.parseRef(document.location.pathname.toLowerCase()),
        path         = urlref.path,
        cell         = HN.Util.parse_ref(currentCellInRichEdit),
        data         = HN.data.readCell(path, cell.y, cell.x),
        richContents = data.formula;
        
        inEditor = true;
        
        if (richEditorCreated === false) {
            richEditorCreated = true;
            editor = $("#hn_cleditor").cleditor(config)[0];
            $(".cleditorMain, .cleditorPopup").mousedown(function (e) {
                                                             e.stopPropagation();
                                         });
            $(window).mousedown(function (e) {
                                    if (inEditor) {
                                        closeRichEditor();
                                        inEditor = false;
                                    }
                                });
        } else if (richEditorCreated === true) {
            $(editor.$main).css("width",  currRichEditPos.width);
            $(editor.$main).css("height", currRichEditPos.height);
        }
        // The cleditor control takes its position off the underlying text area
        // we use a fixed hidden text area and resize it to the target
        // then position it ove the target and finally
        // we create the rich text editor
        $("#hn_cleditor").css(currRichEditPos.offset);
        // and then move that over the target
        $(".cleditorMain").css(currRichEditPos.offset);
        $("#hn_cleditor").val(stripHTMLfn(richContents));
        $(".cleditorMain").css("display", "block");
        editor.updateFrame();
        editor.refresh();
        editor.focus();
    };

    stripHTMLfn = function(contents) {
        var pattern1 = new RegExp("^=html\.[0-9]+x[0-9]+\\(\""),
        pattern2 = new RegExp("\".$"); // matching ") at the end
        return contents.replace(pattern2, '').replace(pattern1, '');
    };

    closeRichEditor = function () {
        var val, currentCell,
        urlref = HN.Util.parseRef(document.location.pathname.toLowerCase()),
        path   = urlref.path,
        cell   = HN.Util.parse_ref(currentCellInRichEdit),
        data  = HN.data.readCell(path, cell.y, cell.x),
        width, height;
        editor.updateTextArea();
        width = data.merge.right + 1;
        height = data.merge.down + 1;
        val = "=html." + width + "x" + height + 
            "(\"" + $("#hn_cleditor").val() + "\")";
        $(".inlinerich[data-ref=" + currentCellInRichEdit + "]").val(val);
        HN.Callbacks.inline_input(path, cell, val);
        $(".cleditorMain").css("display", "none");
        currentCellInRichEdit = "";
        currRichEditPos = {};
    };

    inputBoxBlur = function (e) {
        var urlref = HN.Util.parseRef(document.location.pathname.toLowerCase()),
        path       = urlref.path,
        ref        = $(e.currentTarget).attr("data-ref"),
        cell       = HN.Util.parse_ref(ref),
        val        = $(e.currentTarget).text(),
        formula    = hn.data.readCell(path, cell.y, cell.x).formula;
        pokeIntoInput(ref, val);
        // only fire if there has been a change
        if (val !== formula) {
            HN.Callbacks.inline_input(path, cell, val);
        }
    };    
    function getInputValue($el) {

        if ($el[0].nodeName === "DIV") {
            return $el.find(":checked").val() || "";
        } else {
            return $el.val();
        }
    }

    api.styleImageLinks = function() {
        $("a img").parent().css("border-bottom", "none");
    };

    api.bindInline = function (sheet) {
        // Only bind the inline stuff if you are on wiki page
        var inlinefn, inlinerichfn,
        view = $("body").attr("data-view");
        if (view !== "wikipage") {
            // switch off the inline styling
            $(".inline").css("border", "none");
            return;
        }
        // handle enter in the input box
        inlinefn = function (e) {
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
                "overflow:hidden;'>",
            wrapperstyle = "top:" + top +
                "px;left:" + left +
                "px;position:absolute;z-index:999;'",
            urlref = HN.Util.parseRef(document.location.pathname.toLowerCase()),
            path     = urlref.path,
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
        };
 
        $(".inline").click(inlinefn);

        inlinerichfn = function (e) {
            var offset = $(e.currentTarget).offset(),
            val = $(e.currentTarget).val();
            $("#hn_cleditor").val(val);
            currentCellInRichEdit = $(e.currentTarget).attr("data-Ref");
            currRichEditPos["offset"] = offset;
            currRichEditPos["width"] = $(e.currentTarget).width();
            currRichEditPos["height"] = $(e.currentTarget).height();
            openRichEditor();
        };

        $(".inlinerich").click(inlinerichfn);

        $("body").click(function (e) {
            $("#clinput").css("display", "none");
            $("#inputbox").css("display", "none");
            // there is no new 'old cell' so pass in an empty string
            cleanUp("", sheet);
        });
    };

    api.bindControls = function () {
        var successfn, errorfn, mfn;
        successfn = function (e) {if (e.status === "ok") {
            window.location = e.redirect;
        } else if (e.status === "err") {
            HN.Util.showDialog(e.response);
        };};
        errorfn = function (e) {var msg = "Server error: " +
                                e.status + ". "+
                                "This has been logged " +
                                "and will be " +
                                "investigated.";
                                HN.Util.showDialog(msg);};
        mfn = function (e) {
            var button  = $(this),
            payload = button.attr("data-payload"),
            url = button.attr("data-origin"),
                action = button.attr("data-action"),
                data = JSON.stringify({postwebcontrols : payload});
            $.ajax({
                "type"     : "POST",
                "url"      : url,
                "dataType" : "json",
                "data"     : data,
                "success"  : successfn,
                "error"    : errorfn
            });
        };
        $("input[type=submit].hn-webcontrol").bind("mousedown", mfn);
    };

    api.bindForms = function (sheet, ref) {
        var signupfn = function (e, email, sitetype, error) {

        // TODO : dirty dirty hack (for front page signups)
            
            e.preventDefault();
            var json = {"signup": {"email"    : email,
                                   "sitetype" : sitetype}},
                data = JSON.stringify(json);
            $.ajax({
                "type"     : "POST",
                "url"      : "/_hooks/",
                "dataType" : "json",
                "data"     : data,
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
        
        $(".hn_prompt").bind("focus", function (e) {
                               if ($(this).hasClass("hn_prompt")) {
                                   $(this).val("");
                               }
                               $(this).removeClass("hn_prompt");
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
                    HN.Util.showDialog(button.attr("data-response"));
                    api.render(sheet, ref);
                },
                "error" : function (xhr, ajaxOptions, err) {
                    HN.Util.showDialog("There has been an error "+
                                       "on this submission. " +
                                       "It has been logged and will " +
                                       "be investigated");
                    api.render(sheet, ref);
                }
            });
        });

        // Now force the radio buttons to show as checked
        // this is a wierd bug thing in Chrome/Firefox
        $(".hninput input[type=radio][Checked=true]").attr("checked", "true");
    };
    
    api.render = function (sheet, pageref) {
        var data   = sheet.pageData(),
        inner  = $("#inner"),
        bottom = null,
        view = $("body").attr("data-view"),
        y, x, cell, position, offsetpos, width, height, value, css, html,
        tmpheight, totalHeight = 0, totalWidth = 0, tmpWidth, 
        inp, ref, div, inlcss, outerpadding;
        if (pageref.type === "range") {
            offsetpos = sheet.cell_offset(pageref.obj.y1, pageref.obj.x1);
        } else {
            offsetpos = {"top" : 0, "left" : 0};
        };
        inner.empty();

        // Need to precalculate offsets, will do some other way later
        for (y in data.data.cell) {
            if (data.data.cell.hasOwnProperty(y)) {
                tmpheight = 0;
                tmpWidth  = 0;
                
                for (x in data.data.cell[y]) {
                    if (data.data.cell[y].hasOwnProperty(x)) {
                        if (pageref.type === "path" || 
                            (pageref.type === "range" &&
                             HN.Util.intersect(x, y, pageref.obj))) {
                            cell     = sheet.cell(y, x);
                            position = sheet.cell_offset(y, x);
                            position.left = position.left - offsetpos.left;
                            position.top = position.top - offsetpos.top;
                                            
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
                            // only show the cell if it is a page
                            // or if it is within the current range
                            if (pageref.type === "path" || 
                                (pageref.type === "range" &&
                                 HN.Util.intersect(x, y, pageref.obj))) {
                                position = sheet.cell_offset(y, x);
                                position.left = position.left - offsetpos.left;
                                position.top = position.top - offsetpos.top;
                              
                                width    = sheet.cellWidth(cell, x);
                                height   = sheet.cellHeight(cell, y);
                                value    = cell.value || "";
                                css      = sheet.lookupCSSIndex(cell.style) || "";
                                inp      = cell.input || "none";
                                ref = HN.Util.to_b26(parseInt(x, 10)) + y,
                                css += "-moz-border-radius: 2px 2px 2px 2px;"+
                                    "-webkit-border-radius: 2px 2px 2px 2px;";
                                if (value || css || inp === "inline") {
                                    if (inp === "inline" && view === "wikipage") {
                                        inlcss = "style='width:" +
                                            (parseInt(width) - 8) +
                                            "px;height:" +
                                            (parseInt(height) - 4) + "px'";
                                        outerpadding = 0;
                                        div = "<div>" +
                                            "<div class='inline' " +
                                            inlcss + " data-ref='" +
                                            ref + "'>" + value + "</div></div>";
                                    } else if (inp === "inlinerich" && 
                                               view === "wikipage") {
                                        inlcss = "style='width:" +
                                            (parseInt(width) - 8) +
                                            "px;height:" +
                                            (parseInt(height) - 4) + "px'";
                                        outerpadding = 0;
                                        div = "<div>" +
                                            "<div class='inlinerich' " +
                                            inlcss + " data-ref='" +
                                            ref + "'>" + value + "</div></div>";
                                    } else if (inp !== "none" 
                                               && view === "wikipage") {
                                        // on spreadsheet pages the uniq id is
                                        // a pane ID - here we just use "wikipage"
                                        div = HN.Util.makeWikiSelect(inp, value,
                                                                     ref, "wikipage");
                                    } else  {
                                        outerpadding = 3;
                                        div = "<div data-ref='" + ref + "'>" +
                                            value + "</div>";
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
        }
        inner.css({
                      "height"   : (bottom + 50) + "px"
                  });

        $("#outer").css({
            "margin" : "0px auto",
            "width"  : totalWidth + "px"
        });

        api.bindInline(sheet);
        api.bindForms(sheet, pageref);
        api.bindControls();
        api.styleImageLinks();
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
        ref = HN.Util.parseRef(document.location.pathname.toLowerCase()),
        path = ref.path,
        options = {
            "stopUpdate"   : true,
            "dataLoaded"   : function () {
                sheets[path] = new HN.Sheet(path, data, false);
                gridRender.bindInline(sheets[path]);
                gridRender.bindForms(sheets[path], ref);
                gridRender.bindControls();
                document.body.focus();
                HN.Util.initLogin("web_or_wikipage");
                // Now bind the inline select boxes (do it only once!)
                // on spreadsheet pages the selects have a uniq pane id
                // on wiki pages the uniq id is wikipage
                HN.Util.setupInlineSelect("wikipage", "wikipage");
                HN.Includes.reload();
            },
            "dataReloaded" : function (data) {  
                sheets[path].reload_data(data);
                gridRender.render(sheets[path], ref);
                HN.Includes.reload();
            },
            "update"       : function () { 
                sheets[path].processLoaded();            
                gridRender.render(sheets[path], ref);
                HN.Includes.reload();
            }
        },
        successFun = function (data) {
            hn.groups = data.groups;
            hn.is_admin = data.is_admin;
            lang = data.lang;
            HN.Includes.reload();
            gridRender.styleImageLinks();
        };
    data = new HN.Data(options);
    data.addPage(path);    
    // data has to be 'not encapsulated' to allow the callbacks to poke into it
    // fugly, fugly, fugly
    // basically the main sheet routines use a global HN.data to co-ordinate
    // and so we have to as well :(

    HN.data = data;
    hn = HN;
    sitedata = new HN.SiteData();
    sitedata.loadSiteData(successFun);
                   
}());

