/**
 * @class HN.Layout.Panes
 *
 */
HN.Layout.Panes = function(layout, selection, sheet) {

    var api = {};

    api.bounds = {"x1":0, "y1":0, "x2":0, "y2":0};

    var grid   = calcNoOfPanes( layout.gridSizePix() );
    
    var rows   = grid.rows;
    var cols   = grid.cols;
    
    var axis   = layout.axis_dom;
    var panes  = HN.Util.id("panes");
    var inner  = HN.Util.id("scroller");
    var p      = [];
    var index  = {"x":[], "y":[]};
        
    drawIndex(Y, true, false, 0, 0, 1, (rows * HN.Layout.Pane.DEF_ROWS) + 1);
    drawIndex(X, true, false, 0, 0, 1, (cols * HN.Layout.Pane.DEF_COLS) + 1);

    buildCellDivs({});

    for( var i = 0; i < p[0].length; i++ ) {
        api.bounds.x2 += p[0][i].width;
    }

    api.firstPane = function() {
        return p[0][0];
    };

    api.dataUpdated = function() {
        api.refresh();
    };

    api.gridResized = function() {
        var tmp = calcNoOfPanes( layout.gridSizePix() );
        rows = tmp.rows;
        cols = tmp.cols;
        api.refresh();
    };
    
    api.refresh = function(args) {

        api.bounds.x2 = api.bounds.x1;
        api.bounds.y2 = api.bounds.y1;

        if( typeof opts == "undefined" ) {
            var tmp    = p[0][0];
            args = { 
                "row"  : tmp.row, 
                "col"  : tmp.col,
                "top"  : parseInt(tmp.div.style.top, 10),
                "left" : parseInt(tmp.div.style.left, 10) 
            };
        }

        p = [];
        removeTiles();
        buildCellDivs(args);

        var bottom = p[p.length-1][0].row + HN.Layout.Pane.DEF_ROWS;
        var right  = p[0][p[0].length-1].col + HN.Layout.Pane.DEF_COLS;

        axis.y.innerHTML = "";
        axis.x.innerHTML = "";
        drawIndex(Y, true, false, args.top, 0, args.row, bottom);
        drawIndex(X, true, false, 0, args.left, args.col, right);
        
        for( var i = 0; i < p[0].length; i++ ) {
            api.bounds.x2 += p[0][i].width;
        }
    };
    
    api.moveTo = function(topI, leftI) {    

        var pos = sheet.cell_offset(topI, leftI);

        var args = { 
            "row"  : topI, 
            "col"  : leftI, 
            "top"  : pos.top, 
            "left" : pos.left
        };

        api.bounds = {"x1":0, "y1":0, "x2":0, "y2":0};
        
        layout.setGridPos.top(-pos.top);
        layout.setGridPos.left(-pos.left);

        removeTiles();
        buildCellDivs(args);

        axis.y.innerHTML = "";
        axis.x.innerHTML = "";
        
        var cellsy = rows *  HN.Layout.Pane.DEF_ROWS;
        var cellsx = cols *  HN.Layout.Pane.DEF_COLS;

        drawIndex(Y, true, false, 0, pos.top, topI, topI+cellsy);
        drawIndex(X, true, false, pos.left, 0, leftI, leftI+cellsx);
        
        for( var i = 0; i < p[0].length; i++ ) {
            api.bounds.x2 += p[0][i].width;
        }

        layout.calcScrollbarPos(Y);
        layout.calcScrollbarPos(X);
    };

    function removeTiles() {
        panes.innerHTML="";
    };

    api.get_cell = function(y, x) {
        
        var obj = get_pane(y, x);
        
        if( !obj ) {
            return false;
        }
        
        var iny = (obj.y) % HN.Layout.Pane.DEF_ROWS;
        var inx = (obj.x) % HN.Layout.Pane.DEF_COLS;

        return obj.pane.div.childNodes[iny].childNodes[inx];
    };

    function get_pane(y, x) {

        var bottomleft = p[0][0];
        var topright   = p[p.length-1][p[0].length-1];

        // (0 indexes)
        y -= bottomleft.row;
        x -= bottomleft.col;

        if( x < 0 || x > (HN.Layout.Pane.DEF_COLS * (p[0].length)) || 
            y < 0 || y > (HN.Layout.Pane.DEF_ROWS * (p.length))) {
            return false;
        }

        var paney = Math.floor( y / HN.Layout.Pane.DEF_ROWS );
        var panex = Math.floor( x / HN.Layout.Pane.DEF_COLS );

        return { 
            "pane" : p[paney][panex], 
            "x"    : x, 
            "y"    : y
        };
    };

    api.shift_y = function(up) {

        if( up ) {
            var y      = p.length-1;
            var rownum = p[y][0].row+8;
            var row    = p.shift();
            p[y]       = [];
        } else {
            var y      = 0;
            var tmp    = p[y][0].row-8;
            var rownum = tmp;
            var row    = p[p.length-1];
            p.length -= 1;
            p.unshift([]);
        }

        var oldheight = row[0].height;
        var top       = null;
        var newheight = null;
        var len       = row.length;

        for( var x = 0; x < len; x++ ) {

            p[y][x]  = row[x];

            var pane = p[y][x];
            var orig = pane.div;
            pane     = row[x];

            pane.setRow(rownum);
            
            if( x == 0 ) {
                newheight = pane.height;
                
                top = up
                    ? parseInt(p[y-1][0].div.style.top, 10) + p[y-1][0].height
                    : parseInt(p[1][0].div.style.top, 10) - newheight;
            }

            pane.div.style.top = top+"px" ;
        }

        if( up ) {
            api.bounds.y1 += oldheight;
            api.bounds.y2 += newheight;
        } else {
            api.bounds.y1 -= newheight;
            api.bounds.y2 -= oldheight;
        }

        drawIndex(Y, up, true, top, 0, rownum,
                   rownum + HN.Layout.Pane.DEF_ROWS);
    };

    api.shift_x = function(left) {

        if( left ) {
            var   x    = p[0].length-1;
            var colnum = p[0][x].col+5;
            var col    = [];
            var len    = p.length;
            for( var i = 0; i < len; i++ ) {
                var tmp = p[i].shift();
                col.push(tmp);
                p[i][x] = [];
            }
        } else {
            var x      = 0;
            var tmp    = p[0][0].col-5;
            var plen   = p[0].length-1;
            var colnum = tmp;
            var col    = [];
            var len    = p.length;
           
            for( var i = 0; i < len; i++ ) {
                col.push(p[i][plen]);
                p[i].length -= 1;
                p[i].unshift([]);
            }
        }

        var oldwidth = col[0].width;
        var lleft    = null;
        var newwidth = null;
        var len      = col.length;

        for( var y = 0; y < len; y++ ) {

            p[y][x] = col[y];

            var pane = p[y][x];
            var orig = pane.div;

            pane = col[y];
            pane.setColumn(colnum);

            if( y == 0 ) {
                newwidth = pane.width;

                lleft = left
                    ? parseInt(p[0][x-1].div.style.left, 10) + p[0][x-1].width
                    : parseInt(p[0][1].div.style.left, 10) - newwidth;
            }

            pane.div.style.left = lleft+"px" ;
        }

        if( left ) {
            api.bounds.x1 += oldwidth;
            api.bounds.x2 += newwidth;
        } else {
            api.bounds.x1 -= newwidth;
            api.bounds.x2 -= oldwidth;
        }

        drawIndex(X, left, true, 0, lleft, colnum,
                   colnum + HN.Layout.Pane.DEF_COLS);
    };
    
    function drawIndex(xy, append, del, top, left, start, end) {
        
        var index = [];
        var axiss = axis[xy.str];
        var style = (xy == Y)
            ? "padding:3px 2px 0px 0px;border-bottom:1px solid #aaa;"
            +"border-right:1px solid #aaa;text-align:right;vertical-align:top;"
            : "padding-top:4px;border-bottom:1px solid #aaa;border-right:"
            +"1px solid #aaa;text-align:center;";

        for( var i = start; i < end; i++ ) {
            if( del ) {
                if( append ) {
                    axiss.removeChild(axiss.childNodes[0]);
                } else {
                    axiss.removeChild(axiss.childNodes[
                        axiss.childNodes.length-1]);
                }
            }

            var val = (xy == Y)
                ? [sheet.row_height(i), 24]
                : [19, sheet.col_width(i)];

            index.push('<div style="position:absolute;');
            index.push('font: 9px sans-serif;background:#DDD;');
            index.push('top:'+top+'px;left:'+left+'px;');
            index.push('height:'+(val[0]-4)+'px;width:'+(val[1]-1)+'px;');
            index.push(style+'"><div class="handle"></div>'
                       +xy.to_index(i)+'</div>');

            if( xy.str == "y" ) {
                top  += val[0];
            } else {
                left += val[1];
            }
        }

        if( append ) {
            axiss.innerHTML += index.join("");
        } else {
            axiss.innerHTML = index.join("") + axiss.innerHTML;
        }
    };
    
    function buildCellDivs(o) {

        o.row  = o.row  || 1;
        o.col  = o.col  || 1;
        o.top  = o.top  || 0;
        o.left = o.left || 0;

        var top = o.top;

        for( var y=0; y<rows; y++ ) {

            var row  = y * 8;
            var left = o.left;
            
            p[y] = [];

            for( var x = 0; x < cols; x++ ) {

                p[y][x] = new HN.Layout.Pane(sheet, row+o.row, (x*5)+o.col);

                p[y][x].div.style.top  = top + "px";
                p[y][x].div.style.left = left + "px";

                panes.appendChild(p[y][x].div);

                left += p[y][x].width;
            }

            top += p[y][0].height;
            api.bounds.y2 += p[y][0].height;
        }
    };

    function calcNoOfPanes(size) {
        return {
            "rows" : Math.ceil(size.y / HN.Sheet.CELL_HEIGHT / 8)+1,
            "cols" : Math.ceil(size.x / HN.Sheet.CELL_WIDTH  / 5)+1
        };
    };

    return api;
};