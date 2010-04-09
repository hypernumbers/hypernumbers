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
                    var width    = cell.mergeWidth || sheet.col_width(x);
                    var height   = cell.mergeHeight || sheet.row_height(x);
                    var value    = cell.value || "";
                    var css      = sheet.lookupCSSIndex(cell.style) || "";

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

        inner.css({
            "position" : "relative"
        });
        
        $("#outer").css({
            "height"   : "20px",
            "margin"   : "0px auto",
            "width"    : (right - left) + "px"
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