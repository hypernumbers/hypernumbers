
/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false */

HN.Sheet = function (path, data, canResizeHeight) {

    var api              = {},
        dataObj          = data,
        max              = {"x": 0,  "y": 0},
        totalSheetHeight = 0,
        totalSheetWidth  = 0,
        resizeHeight     = canResizeHeight,
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

    api.canWrite = function() {
	views = dataObj.readViews(path);
	for(var i=0; i<views.length; i++) {
	  if (views[i] == 'spreadsheet') 
	      return true;
	}
	return false;
    }

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

    api.reload_data = function (newdata) {
        data.data = newdata;
        findMax();
        api.processLoaded();        
    };

    api.cellChanged = function(y, x) {
        var cell = api.cell(y, x);
        delete cell.cachedHeight;
    };
    
    //

    api.calc_height = function (rowIndex) {

        var hidden      = HN.Util.id("hidden_input"),
            oldHeight   = api.row_height(rowIndex),
            nHeight     = HN.Sheet.CELL_HEIGHT,
            row         = api.pageData().data.row[rowIndex],
            fixedHeight = row && row[rowIndex].fixedHeight,
            cell, width, val, style, i, css, cellHeight;

        if (fixedHeight) {
            return oldHeight;
        }

        for (i in data.data.cell[rowIndex]) {
            cell = api.cell(rowIndex, i);
            
            if (!cell.cachedHeight) {

                width  = api.cellWidth(cell, i);
                val    = cell.value;
                style  = api.lookupCSS(rowIndex, i);
                css    = style + "width:" + width + "px;overflow:visible;";
                hidden.setAttribute("style", css);
                hidden.innerHTML = val;
                cell.cachedHeight = hidden.offsetHeight + 2;
            } 

            if (cell.cachedHeight > nHeight &&
                (!cell.merge || cell.merge.down === 0)) {
                nHeight = cell.cachedHeight;
            }
        }
        
        if (Math.abs(oldHeight - nHeight) > 4 &&
            typeof HN.Callbacks !== "undefined" && resizeHeight) {
            HN.Callbacks.setHeight(path, rowIndex, nHeight);
        }
        // You need to delete the element after you have worked out its height
        hidden.innerHTML="";
        return nHeight;
    };
    
    api.processLoaded = function () {

        processMergedCells();
        
        var height = 0,
            count  = 0,
            rows   = [],
            dirty  = [],
            i, s, ix, iy, cell, y, x, width = 0, c = 0;

        for (y in data.data.cell) {            
            if (y) {
                count += 1;
                height += api.calc_height(y);
            }
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

        var dirty = [], y, x, i, tmp = data.data;
        
        for (y in tmp.cell) {
            for (x in tmp.cell[y]) {

                // For when cells are unmerged, need to clean out
                // stored vals
                delete tmp.cell[y][x].invisible;
                delete tmp.cell[y][x].mergeTo;
                delete tmp.cell[y][x].mergeHeight;
                delete tmp.cell[y][x].mergeWidth;

                if (tmp.cell[y][x].merge) {
                    dirty.push({
                        "x"    : parseInt(x, 10), 
                        "y"    : parseInt(y, 10),
                        "cell" : tmp.cell[y][x]
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
                    "ref"     : HN.Util.coord_to_ref({"x": ix, "y": iy})
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
