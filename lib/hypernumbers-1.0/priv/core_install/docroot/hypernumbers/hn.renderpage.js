/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */

HN.GridRender = function () {
    
    var api = {};
    
    api.render = function (sheet) { 
        
        var data  = sheet.pageData(),
            inner = $("#inner"),
            bottom = null,
            left  = null, 
            right = null,
            y, x, cell, position, width, height, value, css, tmpright, html,
            tmpheight, totalHeight = 0;
        
        inner.empty();

        // Need to precalculate offsets, will do some other way later
        for (y in data.data.cell) {

            tmpheight = 0;
            
            for (x in data.data.cell[y]) {  

                cell     = sheet.cell(y, x);
                position = sheet.cell_offset(y, x);
                width    = sheet.cellWidth(cell, x);
                height   = sheet.cellHeight(cell, y);
                tmpright = position.left + width;
                
                if (bottom === null || (position.top + height) > bottom) { 
                    bottom = (position.top + height);
                }
                
                if (left === null || position.left < left) { 
                    left = position.left;
                }

                if (right === null || tmpright > right) { 
                    right = tmpright;
                }
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
                                "left"     : (position.left - left) + "px",
                                "width"    : width + "px",
                                "height"   : height + "px"
                            });
                        
                        inner.append(html);
                    }
                }
            }
        }

        inner.css({
            "position" : "relative",
            "height"   : (bottom + 50) + "px"
        });
        
        $("#outer").css({
            // "height"   : "20px",
            "margin"   : "0px auto",
            "width"    : (right - left) + "px"
        });

        // TODO : dirty dirty hack
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
                .append("<img src='/img/loading3.gif' />" + 
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

        // FORMS!
        $("input[type=submit]").bind("mousedown", function (e) {
            
            var button = $(this),
                name   = button.attr("data-form-name"),
                values = [];

            $("[data-name="+name+"]").each(function(i) {

                var col = HN.Util.to_b26(i+1),
                    ref = document.location.pathname + "replies/" +
                       col + ":" + col;
                
                values.push({ 
                    "ref"     : ref,
                    "formula" : $(this).val()
                });
            });
                
            $.ajax({
                "type"     : "POST",
                "url"      : document.location.pathname,
                "dataType" : "json",
                "data"     : JSON.stringify({"set":{"list": values}}),
                "success"  : function(urm) {
                    button.replaceWith("Thanks! your data was submitted.");
                }, 
                "error"    : function() {
                }
            });
        });
            

    };

    return api;
};

HN.RenderPage = (function () {

    var data, sheets = [],
    gridRender = new HN.GridRender(),
    path       = document.location.pathname,
                     options = {
        "stopUpdate"   : true,
        "dataLoaded"   : function () { 
            sheets[path] = new HN.Sheet(path, data);            
            gridRender.render(sheets[path]);
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
