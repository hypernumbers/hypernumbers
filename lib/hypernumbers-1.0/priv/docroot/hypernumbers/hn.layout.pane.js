/**
 * HN.Layout.Pane
 *
 */
HN.Layout.Pane = function(sheet, row, col)
{
    this.row = row;
    this.col = col;

    this.s = sheet;
    this.div = document.createElement("div");
    this.div.style.position = "absolute";
    this.div.style.overflow = "visible";

    var style = "border:1px solid #ddd;white-space:nowrap;"
        + "position:absolute;background:#FFFFFF;";

    var twidth = 0,
    theight    = 0,
    cells      = [],
    top        = 0;

    for( var y = row; y < row + HN.Layout.Pane.DEF_ROWS; y++ ) {
        var height = this.s.row_height(y),
        left       = 0;
        twidth     = 0;
        theight    += height;

        cells[cells.length] = '<div>';

        for( var x = col; x < col+HN.Layout.Pane.DEF_COLS; x++ ) {
            var width = this.s.col_width(x),
            cell      = this.s.cell(y,x),
            val       = (cell && cell.value) || "",
            styl      = (cell && cell.style 
                         && this.s.data.data.styles[cell.style]) || "",
            zindex    = (val == "" && styl == "") ? y*100 : (y*100)+x;

            cells[cells.length] = '<div style="';
            cells[cells.length] = 'z-index:'+zindex+';';
            cells[cells.length] = 'left:'+(left-1)+'px;';
            cells[cells.length] = 'top:'+(top-1)+'px;';
            cells[cells.length] = 'height:'+(height-1)+'px;';
            cells[cells.length] = 'width:'+(width-1)+'px; ';
            cells[cells.length] = style + styl;
            cells[cells.length] = '" rel="cell-'+y+'-'+x+'">';
            cells[cells.length] = val+'</div>';

            twidth += width;
            left += width;
        }
        top += height;
        cells[cells.length] = '</div>';
    }

    this.height = theight;
    this.width  = twidth;

    this.div.innerHTML = cells.join("");
};

HN.Layout.Pane.DEF_ROWS = 8;
HN.Layout.Pane.DEF_COLS = 5;

HN.Layout.Pane.prototype.update_view = function()
{
    var node = this.div;//.cloneNode(true);

    var children = node.childNodes,
    rows = children.length,
    totalh = 0,
    totalw = 0;

    var style = "border:1px solid #ddd;white-space:nowrap;"
        + "position:absolute;background:#FFFFFF;";

    for( var y = 0; y < rows; y++ ) {
        var el  = children[y],
        height = this.s.row_height(y+this.row),
        nchild = el.childNodes,
        cols = nchild.length,
        count = y+this.row;

        totalw = 0;

        for( var x = 0; x < cols; x++ ) {
            var tmp = nchild[x],
            styl    = tmp.style,
            width   = this.s.col_width(x+this.col),
            cell    = this.s.cell(count, x+this.col),
            s       = cell.style && this.s.data.data.styles[cell.style] || "",
            val     = (cell && cell.value) || "",
            zindex  = val == "" && s == "" 
                ? count*100 : (count*100) + (this.col+x);

            tmp.setAttribute("style", style + s);
            styl.top      = (totalh-1)+"px";
            styl.left     = (totalw-1)+"px";
            styl.height   = (height-1)+"px";
            styl.width    = (width-1)+"px";
            styl.zIndex   = zindex;
            tmp.innerHTML = val;

            totalw += width;
        }

        totalh += height;
    }

    //this.div.parentNode.replaceChild(node, this.div);
    this.height = totalh;
    this.width = totalw;
};