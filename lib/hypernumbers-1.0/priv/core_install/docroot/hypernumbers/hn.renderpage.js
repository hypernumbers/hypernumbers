HN.GridRender = function() {
    
    var public = {};

    public.render = function(sheet) { 

        var data  = sheet.pageData();
        var inner = $("#inner");
        var left  = null, right = null;
        
        inner.empty();

        // Need to precalculate offsets, will do some other way later
        for( var y in data.data.cell ) {
            for( var x in data.data.cell[y] ) {                

                var cell     = sheet.cell(y, x);
                var position = sheet.cell_offset(y, x);
                var width    = cell.mergeWidth || sheet.col_width(x);
                var tmpright = position.left + width;

                if( left == null || position.left < left ) { 
                    left = position.left;
                }

                if( right == null || tmpright > right ) { 
                    right = tmpright;
                }
            }
        }
        
        for( var y in data.data.cell ) {
            for( var x in data.data.cell[y] ) {
                
                var cell = sheet.cell(y, x);

                if( !cell.invisible ) { 
                    var position = sheet.cell_offset(y, x);
                    var width    = sheet.cellWidth(cell, x); 
                    var height   = sheet.cellHeight(cell, y);
                    var value    = cell.value || "";
                    var css      = sheet.lookupCSSIndex(cell.style) || "";
                    
                    if( value || css ) {
                        var html = $("<div>"+value+"</div>")
                            .attr("style", css)
                            .css({
                                "position" : "absolute",
                                "top"      : position.top+"px",
                                "left"     : (position.left-left)+"px",
                                "width"    : width+"px",
                                "height"   : height+"px"
                            });
                        
                        inner.append(html);
                    }
                }
            }
        }

        inner.css({
            "position" : "relative"
        });
        
        $("#outer").css({
            "height"   : "20px",
            "margin"   : "0px auto",
            "width"    : (right - left) + "px"
        });

        // TODO : dirty dirty hack
        $("#signupform").bind("submit", function(e) { 
            
            e.preventDefault();
            
            var email = $("#signupemail").val();
            $("#signupform input").attr("disabled", "disabled");
            
            $("#signupfeedback").empty().removeClass("error")
                .append("<img src='/img/loading2.gif' />"+
                        " Warming up the tubes");

            var error = function(reason) { 
                $("#signupform input").removeAttr("disabled");
                $("#signupfeedback").empty().addClass("error")
                    .append("<strong>Error:</strong> "+reason);
            };

            $.ajax({
                "type"     : "POST",
                "url"      : "/_hooks/",
                "dataType" : "json",
                "data"     : JSON.stringify({"signup": {"email":email}}),
                "success"  : function(data) { 
                    if( data.result == "success" ) { 
                        document.location.href = data.url;
                    } else if( data.result == "error" ) { 
                        error(data.reason);
                    } 
                }
            });            
        });

    };

    return public;
};

HN.RenderPage = function()
{
    var data, sheets = [];
    
    var gridRender = new HN.GridRender();
    var path       = document.location.pathname;
    
    var options = {
        "stopUpdate"   : true,
        "dataLoaded"   : function() { 
            sheets[path] = new HN.Sheet(path, data);            
            gridRender.render(sheets[path]);
        },
        "dataReloaded" : function(data) {  },
        "update"       : function() { 
            gridRender.render(sheets[path]);
        }
    };
    
    data = new HN.Data(options);
    data.addPage(path);    
}();