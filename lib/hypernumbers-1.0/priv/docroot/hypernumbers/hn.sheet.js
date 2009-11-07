HN.Sheet = function(data)
{
    this.offset = {x:1,  y:1};
    this.max    = {x:26, y:50};
    this.dt     = data;
    this.data   = data.getPage(document.location.pathname);
    this.height = 0;
    this.width  = 0;
    
    this.find_max();
    this.calc_size();
};

HN.Sheet.CELL_WIDTH  = 80;
HN.Sheet.CELL_HEIGHT = 20;


HN.Sheet.prototype.reload_data = function(data)
{
    this.data = data;
};

HN.Sheet.prototype.find_max = function()
{
    for( var i in this.data.cell ) {
        if( parseInt(i) > this.max.y ) {
            this.max.y = parseInt(i);
        }
        for ( var x in this.data.cell[i] ) {
            if( parseInt(x) > this.max.x ) {
                this.max.x = parseInt(x);
            }
        }
    }
};

HN.Sheet.prototype.extend = function(axis, amount)
{
    this.max[axis.str] += amount;
    this.calc_size();
};

HN.Sheet.prototype.calc_height = function(row)
{
    if( this.data.row[row] && this.data.row[row][row] &&
        this.data.row[row][row].height ) {
        return this.data.row[row][row].height;
    }
    
    var height = HN.Sheet.CELL_HEIGHT;
    
    for (var i in this.data.cell[row]) {
        
        var hidden    = HN.Util.id("hidden_input");
        var width = this.col_width(i);
        var cell  = this.cell(row, i);
        var val   = (cell && cell.value) || "";
        var style = (cell && cell.style && this.data.styles[cell.style]) || "";
        
        hidden.innerHTML = val;
        hidden.setAttribute("style", style+"width:"+width+"px;");
        
        if( hidden.clientHeight+2 > height
            && style.match(/white-space:normal/) !== null) {
            height = hidden.clientHeight+2;
        }
    }
    
    if( height != HN.Sheet.CELL_HEIGHT) {
        this.dt.set("row", row, "aheight", height);
    } else {
        this.dt.remove("row", row, "aheight");
    }
    
    return height;
};

HN.Sheet.prototype.calc_size = function()
{
    var height = 0, count = 0;
    
    var rows = [], dirty = [];
    
    for (var i in this.data.row) {
        if (String(i >>> 0) == i && i >>> 0 != 0xffffffff) {
            rows[i] = true;
        }
    }
    
    for( var s in this.data.styles ) {
        if( this.data.styles[s].match(/white-space:normal/) ) {
            dirty.push(s);
        }
    };
    
    for( var ix in this.data.cell ) {
        for ( var iy in this.data.cell[ix] ) {
            if( jQuery.inArray(this.data.cell[ix][iy].style+"", dirty) != -1) {
                rows[ix] = true;
            }
        }
    }
    
    for (var y in rows) {
        count++;
        height += this.calc_height(y);
    }
    this.height = height + ((this.max.y - count) * 20);
    
  var width = 0, c = 0;
    for (var x in this.data.column) {
        if (String(x >>> 0) == x && x >>> 0 != 0xffffffff) {
            c++;
            width += this.data.column[x][x].width;
        }
    }
    this.width = width + ((this.max.x - c) * 80);
};

HN.Sheet.prototype.get_style_by_cell = function(cell)
{
    var c = this.cell(cell.y, cell.x);
    
    if(c) {
        return this.data.styles[c.style] || "";
    } else {
        return "";
    }
};

HN.Sheet.prototype.row_height = function(i)
{
    if( !this.data.row[i] || !this.data.row[i][i] ) {
        return HN.Sheet.CELL_HEIGHT;
    }
    
    if( this.data.row[i][i].height ) {
        return this.data.row[i][i].height;
    }

    if( this.data.row[i][i].aheight ) {
        var tmp = this.data.row[i][i].aheight;
        return (tmp > HN.Sheet.CELL_HEIGHT) ? tmp : HN.Sheet.CELL_HEIGHT;
    }
    
    return HN.Sheet.CELL_HEIGHT;
};

HN.Sheet.prototype.col_width = function(i)
{
  return this.data.column[i] && this.data.column[i][i]
    && this.data.column[i][i].width || HN.Sheet.CELL_WIDTH;
};

HN.Sheet.prototype.cell = function(y, x)
{
    if( x < 0 ) {
        for (var nx = this.max.y; ny > 0; ny--) {
            if( this.data.cell[ny] && this.data.cell[ny][x] 
                && this.data.cell[ny][x].value ) {
                if( x == -1 ) {
                    return this.data.cell[ny][x];
                } else {
                    x += 1;
                }
            }
        }
        return false;        
    }

    if( y < 0 ) {
        // erm, pretty damn ineficient
        for (var ny = this.max.y; ny > 0; ny--) {
            if( this.data.cell[ny] && this.data.cell[ny][x] 
                && this.data.cell[ny][x].value ) {
                if( y == -1 ) {
                    return this.data.cell[ny][x];
                } else {
                    y += 1;
                }
            }
        }
        return false;
    }
    
    return this.data.cell[y] && this.data.cell[y][x] || false;
};

HN.Sheet.prototype.cell_offset = function(y,x)
{
    for (var iy = 1, top = 0; iy < y; iy++) {
        top += this.row_height(iy);
    }
    
    for (var ix = 1, left = 0; ix < x; ix++) {
        left += this.col_width(ix);
    }
    
    return {"top": top, "left": left};
};

