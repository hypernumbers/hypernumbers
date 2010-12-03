/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 10000 */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false */

HN.Layout.Panes = function (layout, selection, sheet) {

    var bounds = {"x1": 0, "y1": 0, "x2": 0, "y2": 0},
        grid   = null,    
        rows   = null,
        cols   = null,
        axis   = layout.axis_dom,
        panes  = HN.Util.id("panes"),
        inner  = HN.Util.id("scroller"),
        p      = [];

    function drawIndex(xy, append, del, top, left, start, end) {
        
        var i, val,
            index = [],
            axiss = axis[xy.str],
            style = (xy === Y) ? "padding:3px 2px 0px 0px;border-bottom:1px solid #aaa;" + "border-right:1px solid #aaa;text-align:right;vertical-align:top;"
            : "padding-top:4px;border-bottom:1px solid #aaa;border-right:" + "1px solid #aaa;text-align:center;";

        for (i = start; i < end; i += 1) {
            if (del) {
                if (append) {
                    axiss.removeChild(axiss.childNodes[0]);
                } else {
                    axiss.removeChild(axiss.childNodes[
                        axiss.childNodes.length - 1]);
                }
            }

            val = (xy === Y) ? [sheet.row_height(i), 24]
                : [19, sheet.col_width(i)];

            index.push('<div style="position:absolute;');
            index.push('font: 9px sans-serif;background:#DDD;');
            index.push('top:' + top + 'px;left:' + left + 'px;');
            index.push('height:' + (val[0] - 4) + 'px;width:' +
                       (val[1] - 1) + 'px;');
            index.push(style + '"><div class="handle"></div>' +
                       xy.to_index(i) + '</div>');

            if (xy.str === "y") {
                top  += val[0];
            } else {
                left += val[1];
            }
        }

        if (append) {
            axiss.innerHTML += index.join("");
        } else {
            axiss.innerHTML = index.join("") + axiss.innerHTML;
        }
    }

    function calcNoOfPanes(size) {
        return {
            "rows" : Math.ceil(size.y / HN.Sheet.CELL_HEIGHT / 8) + 2,
            "cols" : Math.ceil(size.x / HN.Sheet.CELL_WIDTH  / 5) + 2
        };
    }

    function buildCellDivs(o) {

        o.row  = o.row  || 1;
        o.col  = o.col  || 1;
        o.top  = o.top  || 0;
        o.left = o.left || 0;

        var y, row, left, x, top = o.top;

        for (y = 0; y < rows; y += 1) {

            row  = y * 8;
            left = o.left;
            
            p[y] = [];

            for (x = 0; x < cols; x += 1) {

                p[y][x] = new HN.Layout.Pane(sheet, row + o.row,
                                             (x * 5) + o.col);

                p[y][x].div.style.top  = top + "px";
                p[y][x].div.style.left = left + "px";

                panes.appendChild(p[y][x].div);

                left += p[y][x].width;
            }

            top += p[y][0].height;
            bounds.y2 += p[y][0].height;
        }
    }

    function init() {
        
        drawIndex(Y, true, false, 0, 0, 1, (rows * HN.Layout.Pane.DEF_ROWS) + 1);
        drawIndex(X, true, false, 0, 0, 1, (cols * HN.Layout.Pane.DEF_COLS) + 1);
        
        grid = calcNoOfPanes(layout.gridSizePix());
        rows = grid.rows;
        cols = grid.cols;
        
        buildCellDivs({});
        
        for (var i = 0, len = p[0].length; i < len; i += 1) {
            bounds.x2 += p[0][i].width;
        }
    }
    
    function firstPane() {
        return p[0][0];
    }

    function removeTiles() {
        panes.innerHTML = "";
    }

    function refresh() {

        var bottom, right, i,
            tmp = p[0][0],
            args = {
                "row" : tmp.row,
                "col" : tmp.col,
                "top" : parseInt(tmp.div.style.top, 10),
                "left" : parseInt(tmp.div.style.left, 10)
            };
        
        bounds.x2 = bounds.x1;
        bounds.y2 = bounds.y1;
        
        p = [];
        removeTiles();

        buildCellDivs(args);

        bottom = p[p.length - 1][0].row + HN.Layout.Pane.DEF_ROWS;
        right  = p[0][p[0].length - 1].col + HN.Layout.Pane.DEF_COLS;

        axis.y.innerHTML = "";
        axis.x.innerHTML = "";
        drawIndex(Y, true, false, args.top, 0, args.row, bottom);
        drawIndex(X, true, false, 0, args.left, args.col, right);
        
        for (i = 0; i < p[0].length; i += 1) {
            bounds.x2 += p[0][i].width;
        }
    }

    function dataUpdated() {
        refresh();
    }
    
    function gridResized() {
        var tmp = calcNoOfPanes(layout.gridSizePix());
        rows = tmp.rows;
        cols = tmp.cols;
        refresh();
    }
    
    function moveTo(topI, leftI) {    

        var pos  = sheet.cell_offset(topI, leftI),
            cellsy, cellsx, i, 
            args = { 
                "row"  : topI, 
                "col"  : leftI, 
                "top"  : pos.top, 
                "left" : pos.left
            };

        bounds.x1 = 0;
        bounds.y1 = 0;
        bounds.x2 = 0;
        bounds.y2 = 0;
        
        layout.setGridPos.top(-pos.top);
        layout.setGridPos.left(-pos.left);

        removeTiles();
        buildCellDivs(args);

        axis.y.innerHTML = "";
        axis.x.innerHTML = "";
        
        cellsy = rows * HN.Layout.Pane.DEF_ROWS;
        cellsx = cols * HN.Layout.Pane.DEF_COLS;

        drawIndex(Y, true, false, 0, pos.top, topI, topI + cellsy);
        drawIndex(X, true, false, pos.left, 0, leftI, leftI + cellsx);
        
        for (i = 0; i < p[0].length; i += 1) {
            bounds.x2 += p[0][i].width;
        }

        layout.calcScrollbarPos(Y);
        layout.calcScrollbarPos(X);
    }
    
    function get_pane(y, x) {

        var bottomleft = p[0][0],
            topright   = p[p.length - 1][p[0].length - 1],
            paney, panex;

        // (0 indexes)
        y -= bottomleft.row;
        x -= bottomleft.col;

        if (x < 0 || x > (HN.Layout.Pane.DEF_COLS * (p[0].length)) || 
            y < 0 || y > (HN.Layout.Pane.DEF_ROWS * (p.length))) {
            return false;
        }

        paney = Math.floor(y / HN.Layout.Pane.DEF_ROWS);
        panex = Math.floor(x / HN.Layout.Pane.DEF_COLS);

        return { 
            "pane" : p[paney][panex], 
            "x"    : x, 
            "y"    : y
        };
    }
    
    function get_cell(y, x) {
        
        var obj = get_pane(y, x), iny, inx;
        
        if (!obj) {
            return false;
        }
        
        iny = (obj.y) % HN.Layout.Pane.DEF_ROWS;
        inx = (obj.x) % HN.Layout.Pane.DEF_COLS;

        return obj.pane.div.childNodes[iny].childNodes[inx];
    }

    function shift_y(up) {

        var x, y, rownum, row, oldheight, top, newheight, len, pane, orig;
        
        if (up) {
            y      = p.length - 1;
            rownum = p[y][0].row + 8;
            row    = p.shift();
            p[y]   = [];
        } else {
            y      = 0;
            rownum = p[y][0].row - 8;
            row    = p[p.length - 1];
            p.length -= 1;
            p.unshift([]);
        }

        oldheight = row[0].height;
        top       = null;
        newheight = null;

        for (x = 0, len = row.length; x < len; x += 1) {

            p[y][x] = row[x];

            pane = p[y][x];
            orig = pane.div;
            pane = row[x];

            pane.setRow(rownum);
            
            if (x === 0) {
                newheight = pane.height;
                
                top = up ?
                    parseInt(p[y - 1][0].div.style.top, 10) + p[y - 1][0].height
                    : parseInt(p[1][0].div.style.top, 10) - newheight;
            }

            pane.div.style.top = top + "px";
        }
        
        if (up) {
            bounds.y1 += oldheight;
            bounds.y2 += newheight;
        } else {
            bounds.y1 -= newheight;
            bounds.y2 -= oldheight;
        }

        drawIndex(Y, up, true, top, 0, rownum,
                  rownum + HN.Layout.Pane.DEF_ROWS);
    }
    
    function shift_x(left) {

        var x, y, colnum, col, len, i, tmp, plen, oldwidth, newwidth, lleft,
            pane, orig;
        
        if (left) {
            x      = p[0].length - 1;
            colnum = p[0][x].col + 5;
            col    = [];
            len    = p.length;
            for (i = 0; i < len; i += 1) {
                tmp = p[i].shift();
                col.push(tmp);
                p[i][x] = [];
            }
        } else {
            x      = 0;
            tmp    = p[0][0].col - 5;
            plen   = p[0].length - 1;
            colnum = tmp;
            col    = [];
            len    = p.length;
           
            for (i = 0; i < len; i += 1) {
                col.push(p[i][plen]);
                p[i].length -= 1;
                p[i].unshift([]);
            }
        }

        oldwidth = col[0].width;
        lleft    = null;
        newwidth = null;
        len      = col.length;

        for (y = 0; y < len; y += 1) {

            p[y][x] = col[y];
            
            pane = p[y][x];
            orig = pane.div;

            pane = col[y];
            pane.setColumn(colnum);

            if (y === 0) {
                newwidth = pane.width;

                lleft = left ? parseInt(p[0][x - 1].div.style.left, 10) + p[0][x - 1].width
                    : parseInt(p[0][1].div.style.left, 10) - newwidth;
            }

            pane.div.style.left = lleft + "px";
        }

        if (left) {
            bounds.x1 += oldwidth;
            bounds.x2 += newwidth;
        } else {
            bounds.x1 -= newwidth;
            bounds.x2 -= oldwidth;
        }

        drawIndex(X, left, true, 0, lleft, colnum,
                   colnum + HN.Layout.Pane.DEF_COLS);
    }    
    
    init();
    
    return {
        "bounds"      : bounds,
        "firstPane"   : firstPane,
        "dataUpdated" : dataUpdated, 
        "gridResized" : gridResized, 
        "moveTo"      : moveTo, 
        "get_cell"    : get_cell, 
        "shift_y"     : shift_y,
        "shift_x"     : shift_x
    };
};