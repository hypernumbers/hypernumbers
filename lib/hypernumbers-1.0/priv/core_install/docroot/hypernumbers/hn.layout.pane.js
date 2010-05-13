/**
 * HN.Layout.Pane
 *
 */
HN.Layout.Pane = function(sheet, row, col)
{
    var api    = {};
    api.height = 0;
    api.width  = 0;

    api.div = document.createElement("div");
    api.div.style.position = "absolute";
    api.div.style.overflow = "visible";
    
    api.row = row;
    api.col = col;

    var DEF_STYLE = "border:1px solid #ddd;"
        + "position:absolute;background:#FFFFFF;"
        + "overflow:hidden;";
    
    api.setColumn = function(column) {        
        api.col = column;
        api.updatePane();
    };

    api.setRow = function(row) {
        api.row = row;
        api.updatePane();
    };

    api.updatePane = function() { 

        var fun = function(dom, style, value) { 
            dom.setAttribute("style", style);
            dom.innerHTML = HN.Util.previewMedia(value);
        };

        api.iterate(fun);
    };
    
    api.iterate = function(callback) {

        var children = api.div.childNodes,
            rows     = children.length,
            totalh   = 0,
            totalw   = 0, y;
        
        for (y = 0; y < rows; y++) {
            
            var trow   = y+api.row,
                el     = children[y],
                height = sheet.row_height(trow),
                nchild = el.childNodes,
                cols   = nchild.length;
            
            totalw = 0;
            
            for( var x = 0; x < cols; x++ ) {

                var dom    = nchild[x],
                    tcol   = x+api.col,            
                    width  = sheet.col_width(tcol),
                    cell   = sheet.cell(trow, tcol),
                    zindex = trow*100 + tcol;
            
                if ((!cell || tcol < 0 || trow < 0) && !cell.invisible) {

                    var style = DEF_STYLE + xyz + "z-index:"+(zindex)+";"
                        +"top:"+(totalh-1)+"px;left:"+(totalw-1)+"px;"
                        +"height:"+(height-1)+"px;width:"+(width-1)+"px;";
                    callback(dom, style, "");

                } else if (!cell.invisible) { 
                        
                    var nheight = cell.mergeHeight || height,
                        nwidth  = cell.mergeWidth  || width,
                        xyz     = "top:"+(totalh-1)+"px;left:"+(totalw-1)+"px;"
                        +"height:"+(nheight-1)+"px;width:"+(nwidth-1)+"px;",
                        s       = sheet.lookupCSSIndex(cell.style) || "",
                        val     = cell.value || "",
                        style   = DEF_STYLE + s + xyz +
                        "z-index:"+(zindex+3)+";";
                    
                    callback(dom, style, val);
                }
                totalw += width;
            }
            totalh += height;
        }
        
        api.height = totalh;
        api.width = totalw;
    };
    
    function createPane() { 

        var cells = [], x,y;

        for (y = 0;  y < HN.Layout.Pane.DEF_ROWS; y+=1) {
            cells[cells.length] = '<div>';            
            for( x = 0; x < HN.Layout.Pane.DEF_COLS; x++ ) {
                cells[cells.length] = '<div class="cell"></div>';
            }
            cells[cells.length] = '</div>';
        }

        api.div.innerHTML = cells.join("");
        api.updatePane();
    };

    createPane();    
    return api;
};

HN.Layout.Pane.DEF_ROWS = 8;
HN.Layout.Pane.DEF_COLS = 5;