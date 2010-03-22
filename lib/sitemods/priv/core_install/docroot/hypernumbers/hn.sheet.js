HN.Sheet = function(path, data)
{
    var public = {};

    var dataObj = data;
    
    public.offset = {"x":1,  "y":1};
    public.max    = {"x":26, "y":50};

    var data = data.getPageData(path);
    
    var totalSheetHeight = 0;
    var totalSheetWidth  = 0;

    // expose hn.data.js api (per path)
    public.cell = function(y, x) {
        return dataObj.readCell(path, y, x);
    };

    public.lookupCSS = function(y, x) {
        return dataObj.lookupCSS(path, y, x);
    };

    public.lookupCSS = function(y, x) {
        return dataObj.lookupCSS(path, y, x);
    };

    public.lookupCSSIndex = function(i) {
        return dataObj.lookupCSSIndex(path, i);
    };

    public.key = function(key) {
        return dataObj.key(path, key);
    };
    

    // 
    public.path = function() {
        return path;
    };

    public.height = function() { 
        return totalSheetHeight;
    };
    
    public.width = function() {
        return totalSheetWidth;
    };
    
    public.extend = function(axis, amount) {
        public.max[axis.str] += amount;
        public.calc_size();
    };

    public.reload_data = function(data) {
        data.data = data;
        public.find_max();
        public.calc_size();        
    };

    public.find_max = function() {
        for( var i in data.data.cell ) {
            if( parseInt(i) > public.max.y ) {
                public.max.y = parseInt(i);
            }
            for ( var x in data.data.cell[i] ) {
                if( parseInt(x) > public.max.x ) {
                    public.max.x = parseInt(x);
                }
            }
        }
    };

    public.calc_height = function(row)
    {
        if( data.data.row[row] && data.data.row[row][row] &&
            data.data.row[row][row].height ) {
            return data.data.row[row][row].height;
        }
        
        var height = HN.Sheet.CELL_HEIGHT;

        for (var i in data.data.cell[row]) {
            
            var hidden = HN.Util.id("hidden_input");
            var width  = public.col_width(i);
            var cell   = public.cell(row, i);
            var val    = (cell && cell.value) || "";
            var style  = (cell && cell.style 
                          && data.data.styles[cell.style]) || "";
            
            hidden.innerHTML = val;
            hidden.setAttribute("style", style+"width:"+width+"px;");
            
            if( hidden.clientHeight+2 > height
                && style.match(/white-space:normal/) !== null) {
                height = hidden.clientHeight+2;
            }
        }
        
        if( height != HN.Sheet.CELL_HEIGHT) {
            data.set("row", row, "aheight", height);
        } else {
            data.remove("row", row, "aheight");
        }
        
        return height;
    };

    public.calc_size = function()
    {
        var height = 0, count = 0, rows = [], dirty = [];

        for (var i in data.data.row) {
            if (String(i >>> 0) == i && i >>> 0 != 0xffffffff) {
                rows[i] = true;
            }
        }
        
        for( var s in data.data.styles ) {
            if( data.data.styles[s].match(/white-space:normal/) ) {
                dirty.push(s);
            }
        };
        
        for( var ix in data.data.cell ) {
            for ( var iy in data.data.cell[ix] ) {
                if( jQuery.inArray(data.data.cell[ix][iy].style+"", dirty) 
                    != -1) {
                    rows[ix] = true;
                }
            }
        }
        
        for (var y in rows) {
            count++;
            height += public.calc_height(y);
        }
        totalSheetHeight = height + ((public.max.y - count) * 20);
        
        var width = 0, c = 0;
        for (var x in data.data.column) {
            if (String(x >>> 0) == x && x >>> 0 != 0xffffffff) {
                c++;
                width += data.data.column[x][x].width;
            }
        }
        totalSheetWidth = width + ((public.max.x - c) * 80);
    };

    public.row_height = function(i)  {
        if( !data.data.row[i] || !data.data.row[i][i] ) {
            return HN.Sheet.CELL_HEIGHT;
        }
        
        if( data.data.row[i][i].height ) {
            return data.data.row[i][i].height;
        }

        if( data.data.row[i][i].aheight ) {
            var tmp = data.data.row[i][i].aheight;
            return (tmp > HN.Sheet.CELL_HEIGHT) ? tmp : HN.Sheet.CELL_HEIGHT;
        }
        
        return HN.Sheet.CELL_HEIGHT;
    };

    public.col_width = function(i) {
        return data.data.column[i] && data.data.column[i][i]
            && data.data.column[i][i].width || HN.Sheet.CELL_WIDTH;
    };

    public.cell_offset = function(y,x)
    {
        for (var iy = 1, top = 0; iy < y; iy++) {
            top += public.row_height(iy);
        }
        
        for (var ix = 1, left = 0; ix < x; ix++) {
            left += public.col_width(ix);
        }
        
        return {"top": top, "left": left};
    };
    
    public.find_max();
    public.calc_size();

    return public;
};

HN.Sheet.CELL_WIDTH  = 80;
HN.Sheet.CELL_HEIGHT = 20;
