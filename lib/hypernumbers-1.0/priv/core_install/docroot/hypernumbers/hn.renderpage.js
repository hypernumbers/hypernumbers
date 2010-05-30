/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */

HN.GridRender = function () {
    
    var api = {};

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
    
    api.bindForms = function (sheet) {

        // TODO : dirty dirty hack (for front page signups)
        $("#signupform").bind("submit", function (e) { 
            
            e.preventDefault();
            
            var email = $("#signupemail").val(),
                error = function (reason) { 
                    $("#signupform input").removeAttr("disabled");
                    $("#signupfeedback").empty().addClass("error")
                        .append("<strong>Error:</strong> " + reason);
                };

            $("#signupform input").attr("disabled", "disabled");
            
            $("#signupfeedback").empty().removeClass("error")
                .append("<img src='/img/loading2.gif' />" + 
                        " Building your site ... ");
            
            $.ajax({
                "type"     : "POST",
                "url"      : "/_hooks/",
                "dataType" : "json",
                "data"     : JSON.stringify({"signup": {"email": email}}),
                "success"  : function (data) { 
                    if (data.result === "success") { 
                        document.location.href = data.url;
                    } else if (data.result === "error") { 
                        error(data.reason);
                    } 
                }
            });            
        });

        
        $("input[type=submit].hninput").bind("mousedown", function (e) {
            
            var button = $(this),
                name   = button.attr("data-form-name"),
                inputs = $("[data-name="+name+"]"),
                values = [];
            
            inputs.each(function(i) {
                values.push({ 
                    "label"   : $(this).attr("data-label"),
                    "formula" : $(this).val()
                });
            });
            
            $.ajax({
                "type"     : "POST",
                "url"      : document.location.pathname,
                "dataType" : "json",
                "data"     : JSON.stringify({"set":{"list": values}}),
                "success"  : function(urm) {
                    showDialog(button.attr("data-response"));
                    inputs.val("");
                }, 
                "error"    : function() {
                }
            });
        });         
    };
    
    api.render = function (sheet) { 
        
        var data   = sheet.pageData(),
            inner  = $("#inner"),
            bottom = null,
            y, x, cell, position, width, height, value, css, html,
            tmpheight, totalHeight = 0, totalWidth = 0, tmpWidth;
        
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
                
                if (value || css) {

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
                    
                    if (value || css) {
                        html = $("<div>" + value + "</div>")
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

        api.bindForms(sheet);
    };

    return api;
};

HN.RenderPage = (function () {

    var data,
        sheets = [],
        gridRender = new HN.GridRender(),
        path       = document.location.pathname,
        options = {
            "stopUpdate"   : true,
            "dataLoaded"   : function () { 
                sheets[path] = new HN.Sheet(path, data);
                gridRender.bindForms(sheets[path]);
                document.body.focus();
                
            },
            "dataReloaded" : function (data) {  
                sheets[path].reload_data(data);
                gridRender.render(sheets[path]);
            },
            "update"       : function () { 
                sheets[path].processLoaded();            
                gridRender.render(sheets[path]);
            }
        };
    
    data = new HN.Data(options);
    data.addPage(path);

}());

setTimeout(function () {
    $("#editspreadsheet").fadeIn("slow");
}, 1000);
