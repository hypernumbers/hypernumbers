var HN = {};
HN.namespace = function(name) {
    var parts = name.split('.'), cur = HN;
    for (var p in parts) {
        if (! cur[parts[p]]) 
            cur[parts[p]] = {};
        cur  = cur[parts[p]];
    }
}

var Y = { 
    str       : "y", 
    coord     : "top",  
    dimension : "height",
    client    : "clientHeight",
    to_index  : function(x) { return x; }
};
var X = {
    str       : "x", 
    coord     : "left", 
    dimension : "width",
    client    : "clientWidth",
    to_index  : function(x) { return HN.Util.to_b26(x); }
};

HN.UP    = 1;
HN.DOWN  = 2;
HN.LEFT  = 3;
HN.RIGHT = 4;

/**
 * Key Constants
 */
HN.Keys = {};
HN.Keys.ENTER  = 13;
HN.Keys.DELETE = 46;
HN.Keys.UP     = 38;
HN.Keys.DOWN   = 40;
HN.Keys.LEFT   = 37;
HN.Keys.RIGHT  = 39;
HN.Keys.SHIFT  = 16;
HN.Keys.CTRL   = 17;
HN.Keys.TAB    = 9;
HN.Keys.ALT    = 18;
HN.Keys.BACKSPACE = 8;

/**
 * Constants relating to the current editing state of spreadsheet
 */
HN.States = {};
HN.States.DRAGGING = 1;
HN.States.SELECTED_RANGE = 2;
HN.States.SELECTED_CELL = 3;
HN.States.EDIT_LIGHT_CELL = 4;
HN.States.EDIT_LIGHT_RANGE = 5;
HN.States.EDIT_FULL_CELL = 6;
HN.States.NOT_EDITING = 7;
HN.States.COPY_URL = 8;

HN.BGCOLORS = ["FFFFFF", "CCCCCC", "999999", "666666", "333333", "000000", 
               "CC0000", "FF7E00", "FFCC00", "5D8AA8", "669900", "A020F0",
               "FFB7C5", "F7E98E", "FFFDD0", "CCCCFF", "77DD77", "DDBEC3"];

HN.FONTCOLORS = HN.BGCOLORS;