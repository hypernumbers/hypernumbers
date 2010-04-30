/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false */

HN.Sheet = function (path, data)
{
    var api          = {},
    dataObj          = data,
    max              = {"x": 0,  "y": 0},
    totalSheetHeight = 0,
    totalSheetWidth  = 0,
    
    // Inner
    processMergedCell, processMergedCells, findMax;

    data = data.getPageData(path);

    api.offset = {"x": 1,  "y": 1};    
    api.max    = {"x": 26, "y": 50};
        
    api.pageData = function () { 
        return data;
    };
    
    // expose hn.data.js api (per path)
    api.cell = function (y, x) {
        return dataObj.readCell(path, y, x);
    };
    
    api.lookupCSS = function (y, x) {
        return dataObj.lookupCSS(path, y, x);
    };

    api.lookupCSSIndex = function (i) {
        return dataObj.lookupCSSIndex(path, i);
    };

    api.key = function (key) {
        return dataObj.key(path, key);
    };
    
    // 
    api.path = function () {
        return path;
    };

    api.height = function () { 
        return totalSheetHeight;
    };
    
    api.width = function () {
        return totalSheetWidth;
    };

    api.cellWidth = function (cell, x) { 
        return cell.mergeWidth  || api.col_width(x);
    };
    api.cellHeight = function (cell, y) { 
        return cell.mergeHeight || api.row_height(y);         
    };
    
    api.extend = function (axis, amount) {
        api.max[axis.str] += amount;
        api.processLoaded();
    };

    api.reload_data = function (data) {
        data.data = data;
        findMax();
        api.processLoaded();        
    };
    
    //
    api.calc_height = function (row) {

        if (data.data.row[row] && data.data.row[row][row] &&
            data.data.row[row][row].height) {
            return data.data.row[row][row].height;
        }
        
        var height = HN.Sheet.CELL_HEIGHT, i, hidden, cell, width,
        val, style;
        
        for (i in data.data.cell[row]) {
            
            hidden = HN.Util.id("hidden_input");
            cell   = api.cell(row, i);
            width  = api.cellWidth(cell, i);
            val    = (cell && cell.value) || "";
            style  = (cell && cell.style && 
                      data.data.styles[cell.style]) || "";
            
            hidden.innerHTML = val;
            hidden.setAttribute("style", style + 
                                "width:" + width + "px;overflow:visible;");
            
            if (hidden.clientHeight + 2 > height && 
                style.match(/white-space:normal/) !== null) {
                height = hidden.clientHeight + 2;
            }
        }
        
        if (height !== HN.Sheet.CELL_HEIGHT) {
            data.set("row", row, "aheight", height);
        } else {
            data.remove("row", row, "aheight");
        }
        
        return height;
    };

    api.processLoaded = function () {
        
        processMergedCells();
        
        var height = 0,
        count  = 0,
        rows   = [],
        dirty  = [], i, s, ix, iy, cell, y, x, width = 0, c = 0;

        for (i in data.data.row ) {
            if (String(i >>> 0) == i && i >>> 0 != 0xffffffff) {
                rows[i] = true;
            }
        }
        
        for (s in data.data.styles) {
            if (data.data.styles[s].match(/white-space:normal/)) {
                dirty.push(s);
            }
        }
        
        for (ix in data.data.cell) {
            for (iy in data.data.cell[ix]) {
                cell = data.data.cell[ix][iy];
                if ($.inArray(cell.style + "", dirty) !== -1) {
                    rows[ix] = true;
                }
            }
        }
        
        for( y in rows) {
            count += 1;
            height += api.calc_height(y);
        }
        totalSheetHeight = height + ((api.max.y - count) * 20);
        
        for (x in data.data.column) {
            if (String(x >>> 0) == x && x >>> 0 != 0xffffffff) {
                c += 1;
                width += data.data.column[x][x].width;
            }
        }
        totalSheetWidth = width + ((api.max.x - c) * 80);
        
        // process these twice to handle wordwrapped cells (they need
        // merged cells width, to calculate height)
        processMergedCells();
    };

    api.row_height = function (i)  {
        if (!data.data.row[i] || !data.data.row[i][i]) {
            return HN.Sheet.CELL_HEIGHT;
        }
        
        if (data.data.row[i][i].height) {
            return data.data.row[i][i].height;
        }

        if (data.data.row[i][i].aheight) {
            var tmp = data.data.row[i][i].aheight;
            return (tmp > HN.Sheet.CELL_HEIGHT) ? 
                tmp : HN.Sheet.CELL_HEIGHT;
        }
        
        return HN.Sheet.CELL_HEIGHT;
    };

    api.col_width = function (i) {
        return data.data.column[i] && data.data.column[i][i] && 
            data.data.column[i][i].width || HN.Sheet.CELL_WIDTH;
    };

    api.cell_offset = function (y, x) {

        var iy, ix, top = 0, left = 0;
        for (iy = 1; iy < y; iy += 1) {
            top += api.row_height(iy);
        }
        
        for (ix = 1; ix < x; ix += 1) {
            left += api.col_width(ix);
        }
        
        return {"top": top, "left": left};
    };

    findMax = function () {
        var i, x;
        for (i in data.data.cell) {
            if (parseInt(i, 10) > max.y) {
                max.y = parseInt(i, 10);
            }
            for (x in data.data.cell[i]) {
                if (parseInt(x, 10) > max.x) {
                    max.x = parseInt(x, 10);
                }
            }
        }
        api.max.x = (max.x < 26) ? 26 : max.x;
        api.max.y = (max.y < 50) ? 50 : max.y;
    };
    
    processMergedCells = function () { 
        
        var dirty = [], y, x, i;
        
        for (y in data.data.cell) {
            for (x in data.data.cell[y]) {

                // For when cells are unmerged, need to clean out
                // stored vals
                delete data.data.cell[y][x].invisible;
                delete data.data.cell[y][x].mergeTo;

                if (data.data.cell[y][x].merge) {
                    dirty.push({
                        "x"    : parseInt(x, 10), 
                        "y"    : parseInt(y, 10),
                        "cell" : data.data.cell[y][x]
                    });
                } 
            }
        }

        // Need to order topleft to bottom right 
        // for overlapping merges
        dirty.sort(function(a, b) {
            return a.y - b.y || a.x - b.x;
        });

        for (i in dirty) { 
            processMergedCell(dirty[i].y, dirty[i].x, dirty[i].cell);
        }
    };
    
    processMergedCell = function (y, x, cell) { 

        if (cell.invisible) { 
            return;
        }

        var width = 0, height = 0, iy, ix;
        
        for (iy = y; iy <= y + cell.merge.down; iy += 1) {
                        
            width  = 0;
            height += api.row_height(iy);
            
            for (ix = x; ix <= x + cell.merge.right; ix += 1) {

                width += api.col_width(ix);

                data.ensure({
                    "reftype" : "cell", 
                    "ref"     : HN.Util.coord_to_ref({
                        "x": ix, 
                        "y": iy
                    })
                });

                if (!(ix === x && iy === y)) {
                    data.data.cell[iy][ix].invisible = true;
                    data.data.cell[iy][ix].mergeTo = {"x": x, "y": y};
                }
            }            
        }

        data.ensure({
            "reftype" : "cell", 
            "ref"     : HN.Util.coord_to_ref({"x": x, "y": y})
        });

        data.data.cell[y][x].mergeHeight = height;
        data.data.cell[y][x].mergeWidth  = width;
    };
    
    findMax();
    api.processLoaded();

    return api;
};

HN.Sheet.CELL_WIDTH  = 80;
HN.Sheet.CELL_HEIGHT = 22;
