/**
 * HN.Layout.Pane
 *
 */
HN.Layout.Pane = function(sheet, row, col)
{
    var public    = {};
    public.height = 0;
    public.width  = 0;

    public.div = document.createElement("div");
    public.div.style.position = "absolute";
    public.div.style.overflow = "visible";
    
    public.row = row;
    public.col = col;

    var DEF_STYLE = "border:1px solid #ddd;white-space:nowrap;"
        + "position:absolute;background:#FFFFFF;font-size:12px;";
    
    public.setColumn = function(column) {        
        public.col = column;
        public.updatePane();
    };

    public.setRow = function(row) {
        public.row = row;
        public.updatePane();
    };

    public.updatePane = function() { 
        
        var fun = function(dom, style, value) { 
            dom.setAttribute("style", style);
            dom.innerHTML = value;            
        };

        public.iterate(fun);

    };
    
    public.iterate = function(callback) {

        var children = public.div.childNodes;
        var rows     = children.length;
        var totalh   = 0;
        var totalw   = 0;
        
        for( var y = 0; y < rows; y++ ) {
            
            var trow   = y+public.row;
            var el     = children[y];
            var height = sheet.row_height(trow);
            var nchild = el.childNodes;
            var cols   = nchild.length;
            
            totalw = 0;
            
            for( var x = 0; x < cols; x++ ) {

                var dom    = nchild[x];
                var tcol   = x+public.col;                
                var width  = sheet.col_width(tcol);
                var cell   = sheet.cell(trow, tcol);
                var zindex = trow*100 + tcol;
            
                if( (!cell || tcol < 0 || trow < 0) && !cell.invisible ) {

                    var style = DEF_STYLE + xyz + "z-index:"+(zindex)+";"
                        +"top:"+(totalh-1)+"px;left:"+(totalw-1)+"px;"
                        +"height:"+(height-1)+"px;width:"+(width-1)+"px;";
                    callback(dom, style, "");

                } else if( !cell.invisible ) { 
                        
                    var nheight = cell.mergeHeight || height;
                    var nwidth  = cell.mergeWidth  || width;
                    
                    var xyz = "top:"+(totalh-1)+"px;left:"+(totalw-1)+"px;"
                        +"height:"+(nheight-1)+"px;width:"+(nwidth-1)+"px;";
                    
                    var s     = sheet.lookupCSSIndex(cell.style) || "";
                    var val   = cell.value || "";
                    var style = DEF_STYLE + s + xyz + "z-index:"+(zindex+3)+";";
                    callback(dom, style, val);
                }
                totalw += width;
            }
            totalh += height;
        }
        
        //    this.div.parentNode.replaceChild(node, this.div);
        public.height = totalh;
        public.width = totalw;
    };
    
    function createPane() { 

        var cells = [];
        var x,y;

        for( y=0;  y<HN.Layout.Pane.DEF_ROWS; y++ ) {
            cells[cells.length] = '<div>';            
            for( x = 0; x < HN.Layout.Pane.DEF_COLS; x++ ) {
                cells[cells.length] = '<div></div>';
            }
            cells[cells.length] = '</div>';
        }

        public.div.innerHTML = cells.join("");
        public.updatePane();
    };

    createPane();    
    return public;
};

HN.Layout.Pane.DEF_ROWS = 8;
HN.Layout.Pane.DEF_COLS = 5;