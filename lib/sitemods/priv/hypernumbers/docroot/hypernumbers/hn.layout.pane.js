/**
 * HN.Layout.Pane
 *
 */
HN.Layout.Pane = function(sheet, row, col)
{
    var public    = {};
    public.height = 0;
    public.width  = 0;

    public.test = 99;

    public.div = document.createElement("div");
    public.div.style.position = "absolute";
    public.div.style.overflow = "visible";
    
    public.row = row;
    public.col = col;

    var DEF_STYLE = "border:1px solid #ddd;white-space:nowrap;"
        + "position:absolute;background:#FFFFFF;";

    createPane();
    
    public.setColumn = function(column) {
        public.col = column;
        public.updatePane();
    };

    public.setRow = function(row) {
        public.row = row;
        public.updatePane();
    };
    
    public.updatePane = function() {

        var children = public.div.childNodes,
        rows         = children.length,
        totalh       = 0,
        totalw       = 0;
        
        for( var y = 0; y < rows; y++ ) {
            
            var el = children[y],
            height = sheet.row_height(y+public.row),
            nchild = el.childNodes,
            cols   = nchild.length,
            count  = y+public.row;
            
            totalw = 0;
            
            for( var x = 0; x < cols; x++ ) {
                
                var tmp = nchild[x],
                width   = sheet.col_width(x+public.col),
                cell    = sheet.cell(count, x+public.col),
                s       = cell.style && 
                    sheet.data.data.styles[cell.style] || "",
                val     = (cell && cell.value) || "",
                zindex  = val == "" && s == "" 
                    ? y*100 : (y*100) + (public.col+x);
                
                var xyz = "top:"+(totalh-1)+"px;left:"+(totalw-1)+"px;"
                    +"height:"+(height-1)+"px;width:"+(width-1)+"px;"
                    +"z-index:"+zindex;
                
                tmp.setAttribute("style", DEF_STYLE + s + xyz);
                tmp.innerHTML = val;
                
                totalw += width;
            }
            totalh += height;
        }
        
        //    this.div.parentNode.replaceChild(node, this.div);
        public.height = totalh;
        public.width = totalw;
    };
    
    function createPane() { 

        var twidth = 0,
        theight    = 0,
        cells      = [],
        top        = 0;
        
        for( var y=public.row, y1=0; 
             y < public.row + HN.Layout.Pane.DEF_ROWS; y++ ) {
            y1++;
            var height = sheet.row_height(y),
            left       = 0;
            twidth     = 0;
            theight    += height;

            cells[cells.length] = '<div>';
            
            for( var x = public.col; 
                 x < public.col+HN.Layout.Pane.DEF_COLS; x++ ) {
                var width = sheet.col_width(x),
                cell      = sheet.cell(y,x),
                val       = (cell && cell.value) || "",
                styl      = (cell && cell.style 
                             && sheet.data.data.styles[cell.style]) || "",
                zindex    = (val == "" && styl == "") ? y1*100 : (y1*100)+x;
                
                cells[cells.length] = '<div style="';
                cells[cells.length] = 'z-index:'+zindex+';';
                cells[cells.length] = 'left:'+(left-1)+'px;';
                cells[cells.length] = 'top:'+(top-1)+'px;';
                cells[cells.length] = 'height:'+(height-1)+'px;';
                cells[cells.length] = 'width:'+(width-1)+'px; ';
                cells[cells.length] = DEF_STYLE + styl;
                cells[cells.length] = '" rel="cell-'+y+'-'+x+'">';
                cells[cells.length] = val+'</div>';
                
                twidth += width;
                left += width;
            }
            top += height;
            cells[cells.length] = '</div>';
        }
        public.height = theight;
        public.width  = twidth;
        
        public.div.innerHTML = cells.join("");
    };

    return public;
};

HN.Layout.Pane.DEF_ROWS = 8;
HN.Layout.Pane.DEF_COLS = 5;