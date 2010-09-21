/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */
hn = {};

HN.GridRender = function () {
    
    var api = {};

    var inputBoxBlur = function(e) {
        var path  = document.location.pathname,
        ref   = $(e.currentTarget).attr("data-ref"),
        cell  = HN.Util.parse_ref(ref),
        val   = $(e.currentTarget).text(),
        formula = hn.data.readCell(path, cell.y, cell.x).formula;
        $(".inline[data-ref="+ref+"]").text(val);
        // only fire if there has been a change
        if (val !== formula) {
            HN.Callbacks.inline_input(path, cell, val);
        } ;
    };

    function showDialog(msg) {
        
        var close = $("<form id='closedialog'>" +
                      "<input value='ok' class='button' type='submit' />" +
                      "</form>")
            .bind("submit", function(e) {
                e.preventDefault();
                $("#cover,#dialog").remove();
            }) ;
        
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

    api.bindInline = function(sheet) {
        // handle enter in the input box
        $(".inline").click(function(e) {
                               var top = $(e.target).position().top,
                               left = $(e.target).position().left,
                               height = $(e.target).height(),
                               width = $(e.target).width(),
                               cell = e.target.getAttribute("data-ref"),
                               html = "<div id='inputbox' "
                                   + "data-ref='"+cell+"' "
                                   +"contenteditable='true' "
                                   +"style='"
                                   +"height:"+height
                                   +"px;width:"+width
                                   +"px;overflow:auto'>",
                               wrapperstyle = "top:"+top
                                   +"px;left:"+left
                                   +"px;position:absolute;z-index:999;'",
                               path     = document.location.pathname,
                               ref      = HN.Util.parseRef(cell),
                               contents = sheet.cell(ref.obj.y, ref.obj.x),
                               formula  = "";
                               if (contents.formula) {
                                   formula = contents.formula;
                               };
                               $("#clinput").attr("style",wrapperstyle);
                               $("#clinput").html(html+formula+"</div>");
                               $("#inputbox").blur(inputBoxBlur);
                               $("#inputbox").focus();
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
            var email = HN.Util.readCookie("auth").split("|")[1].substring(0,10)+"@demo.hypernumbers.com",
                sitetype = "demo",
                error = function(reason) {
                    $("#signupdemofeedback").empty().addClass("error")
                        .append("<strong>Error:</strong> " + reason);
                };
            $("#signupdemofeedback").empty().removeClass("error")
                .append("<img src='/img/loading2.gif' />" + 
                        " Building your demo site. Your username is "+email);
            signupfn(e, email, sitetype, error);
        });
        
        $("input[type=submit].hninput").bind("mousedown", function (e) {

            var button = $(this),
                name        = button.attr("data-form-name"),
                inputs      = $("[data-name="+name+"]"),
                values      = [],
                div         = document.createElement("div"),
                mungedvalue = ""
                rawvalue    = "";
            
            inputs.each(function(i) {
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
                "success"  : function(urm) {
                    showDialog(button.attr("data-response"));
                    api.render(sheet);
                },
                 "error" : function(xhr, ajaxOptions, err) {
                     showDialog("There has been an error on this submission. "
                                + "It has been logged and will be investigated");
                     api.render(sheet);
                         }
            });
        });         
    };
    
    api.render = function (sheet) { 
        var data   = sheet.pageData(),
            inner  = $("#inner"),
            bottom = null,
            y, x, cell, position, width, height, value, css, html,
            tmpheight, totalHeight = 0, totalWidth = 0, tmpWidth, 
            inp, ref, div;
        
        inner.empty();

        // Need to precalculate offsets, will do some other way later
        for (y in data.data.cell) {

            tmpheight = 0;
            tmpWidth  = 0;
            
            for (x in data.data.cell[y]) {  
                
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
                
            if (tmpWidth > totalWidth) {
                totalWidth = tmpWidth;
            }
            
            totalHeight += tmpheight;
        }
        
        for (y in data.data.cell) {
            for (x in data.data.cell[y]) {
                
                cell = sheet.cell(y, x);

                if (!cell.invisible) { 
                    
                    position = sheet.cell_offset(y, x);
                    width    = sheet.cellWidth(cell, x);
                    height   = sheet.cellHeight(cell, y);
                    value    = cell.value || "";
                    css      = sheet.lookupCSSIndex(cell.style) || "";
                    inp      = cell.input || "none";
                    ref = HN.Util.to_b26(parseInt(x, 10)) + y;
                    if (value || css || inp === "inline") {
                        if (inp === "inline") {
                            div = "<div class='inline' data-ref='"+ref+"'>";
                            } else {
                                div = "<div data-ref='"+ref+"'>";
                            };
                        html = $(div+value + "</div>")
                            .attr("style", css)
                            .css({
                                "position" : "absolute",
                                "overflow" : "hidden",
                                "top"      : position.top + "px",
                                "left"     : position.left + "px",
                                "width"    : width + "px",
                                "height"   : height + "px"
                            });
                        
                        inner.append(html);
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
                document.body.focus();
                HN.Util.initLogin();                
            },
            "dataReloaded" : function (data) {  
                sheets[path].reload_data(data);
                gridRender.render(sheets[path]);
            },
            "update"       : function () { 
                sheets[path].processLoaded();            
                gridRender.render(sheets[path]);
            }
        },
        successFun = function(data) {
                hn.functions = data.functions;
                hn.pages = data.pages;
                hn.groups = data.groups;
                hn.is_admin = data.is_admin;
                lang = data.lang;
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

