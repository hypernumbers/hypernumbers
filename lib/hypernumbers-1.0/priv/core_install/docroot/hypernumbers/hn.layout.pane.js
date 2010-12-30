/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 10000 */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false */

/**
 * HN.Layout.Pane
 *
 */
HN.Layout.Pane = function (sheet, row, col)
{
    var api    = {},
        DEF_STYLE = "border:1px solid #ddd;" +
        "position:absolute;background:#FFFFFF;" +
        "overflow:hidden;";

    api.height = 0;
    api.width  = 0;

    api.div = document.createElement("div");
    api.div.style.position = "absolute";
    api.div.style.overflow = "visible";
    
    api.row = row;
    api.col = col;
    
    api.setColumn = function (column) {        
        api.col = column;
        api.updatePane();
    };

    api.setRow = function (row) {
        api.row = row;
        api.updatePane();
    };

    api.updatePane = function () { 

        var fun = function (dom, style, preview, klass) { 
            dom.setAttribute("style", style);
            if (klass !== "") { 
                var inner = document.createElement('span');
                inner.className = klass;
                dom.innerHTML = preview;
                dom.appendChild(inner);
            } else {
                dom.innerHTML = preview;
            }
        };
        api.iterate(fun);
    };
    
    api.iterate = function (callback) {

        var children = api.div.childNodes,
            rows     = children.length,
            totalh   = 0,
            totalw   = 0, y,
            inlinehtml = "",
            trow, el, height, nchild, cols, x, dom, tcol, width, cell, zindex,
            style, klass, input, nheight, nwidth, xyz, s, preview;

        for (y = 0; y < rows; y = y + 1) {
            
            trow   = y + api.row;
            el     = children[y];
            height = sheet.row_height(trow);
            nchild = el.childNodes;
            cols   = nchild.length;
            
            totalw = 0;
            
            for (x = 0; x < cols; x = x + 1) {

                dom    = nchild[x];
                tcol   = x + api.col; 
                width  = sheet.col_width(tcol);
                cell   = sheet.cell(trow, tcol);
                zindex = trow * 100 + tcol;
                
                if ((!cell || tcol < 0 || trow < 0) && !cell.invisible) {
                    style = DEF_STYLE + xyz + "z-index:" + zindex + ";" +
                        "top:" + (totalh - 1) + "px;left:" + (totalw - 1) + "px;" +
                        "height:" + (height - 1) + "px;width:" + (width - 1) + "px;";
                    callback(dom, style, "", "");

                } else if (!cell.invisible) { 
                    klass = "";
                    input = cell.input;
                    if (input === "inline") {
                        klass = "inlineform";
                    }
                    nheight = cell.mergeHeight || height;
                    nwidth  = cell.mergeWidth  || width;
                    xyz     = "top:" + (totalh - 1) + "px;left:" + (totalw - 1) + "px;" +
                        "height:" + (nheight - 1) + "px;width:" + (nwidth - 1) + "px;";
                    s       = sheet.lookupCSSIndex(cell.style) || "";
                    preview = HN.Util.getPreview(cell);
                    style   = DEF_STYLE + s + xyz +
                        "z-index:" + (zindex + 3) + ";";
                    callback(dom, style, preview, klass); 
                } else {
                    callback(dom, "", "", "");
                }
                totalw += width;
            }
            totalh += height;
        }
        
        api.height = totalh;
        api.width = totalw;
    };
    
    function createPane() { 
        var cells = [], x, y,
            maxy = HN.Layout.Pane.DEF_ROWS,
            maxx = HN.Layout.Pane.DEF_COLS;

        for (y = 0;  y < maxy; y = y + 1) {
            cells[cells.length] = '<div>';            
            for (x = 0; x < maxx; x = x + 1) {
                cells[cells.length] = '<div></div>';
            }
            cells[cells.length] = '</div>';
        }

        api.div.innerHTML = cells.join("");

        api.updatePane();
    }

    createPane();    
    return api;
};

HN.Layout.Pane.DEF_ROWS = 8;
HN.Layout.Pane.DEF_COLS = 5;