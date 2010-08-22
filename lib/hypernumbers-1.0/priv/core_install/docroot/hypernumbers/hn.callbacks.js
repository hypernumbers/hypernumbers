/**
 * @namespace HN.Callbacks
 * Defines the callbacks for server events such as changing values
 * styles etc.
 */
HN.Callbacks = {};

/**
 * Set the value of a cell, clears if the value is empty
 */
HN.Callbacks.set_input = function (path, range, value) {
    HN.Util.postRange(path, range, {"set": {"input": value}});
};

HN.Callbacks.set_cell = function (path, cell, value) {

    hn.data.getPageData(path).pokeCellValue(cell, value);
    
    if (value === "") {
        HN.Util.postCell(path, cell, {"clear": "contents"});
    } else {
        HN.Util.postCell(path, cell, {"set": {"formula": value}});
    };
};

/**
 * Clear a range of contents or format (or both)
 */
HN.Callbacks.clear = function(path, range, clear) {
    HN.Util.postRange(path, range, {clear: clear});
};


/**
 * Set the style on a range
 */
HN.Callbacks.style = function(path, range, style, val) {
    var x = {};
    x[style] = val;
    HN.Util.postRange(path, range, {set: x});
};

/**
 * Set the format on a range
 */
HN.Callbacks.format = function(path, range, val) {
    HN.Util.postRange(path, range, {"set": {"format": val}});
};

/**
 * Set the width of a column
 */
HN.Callbacks.setWidth = function(path, column, width) {
    HN.Util.postColumn(path, column, column, {set: {width: width}});
};

HN.Callbacks.setHeight = function(path, row, height) {
    hn.data.getPageData(path).pokeRowHeight(row, height);
    HN.Util.postRow(path, row, row, {"set": {"height": height}});
};

HN.Callbacks.drag = function(path, selected, dragged) {
    var msg = {"drag": {"range": HN.Util.range_to_str(dragged)}};
    HN.Util.postCell(path, {"x":selected.x1, "y":selected.y1}, msg);
};

HN.Callbacks.pasteValues = function(path, range, values) {
    HN.Util.postRange(path, range, {"set": {"formula": values}});
};

HN.Callbacks.pasteRange = function(path, range, srcurl, srcrange) {
    var src = srcurl + HN.Util.range_to_str(srcrange);
    HN.Util.postRange(path, range, {"copy": {"src": src}});
};

HN.Callbacks.deleteRowCol = function(path, range, axis) {
    if( axis == Y ) {
        HN.Util.postRow(path, range.y1, range.y2, {"delete":"all"});
    } else {
        HN.Util.postColumn(path, range.x1, range.x2, {"delete":"all"});
    }
};

HN.Callbacks.deleteRange = function(path, range, displacement) {
    HN.Util.postRange(path, range, {"delete": displacement});
};        

HN.Callbacks.insertRange = function(path, range, displacement) {
    HN.Util.postRange(path, range, {"insert": displacement});
};        

HN.Callbacks.setLanguage = function(language) {
    var cb = function() {
        window.location.reload(true);
    };
    HN.Util.postPath("/_user/", {"set": {"language": language}}, cb);
};

HN.Callbacks.setMark = function(mark) {
    HN.Util.postPath("?mark", {"set": {"mark": mark.value}}, null);
};

HN.Callbacks.setBorders = function(path, range, where, border,
                                   border_style, border_color) {
    var json = {"borders": {
        "where": where,
        "border": border,
        "border_style": border_style,
        "border_color": border_color
    }};
    HN.Util.postRange(path, range, json);
};

HN.Callbacks.deletePage = function(path, language) {
    var cb = function() {
        window.location.reload(true);
    };
    HN.Util.postPath(hn.currentPath(), {"delete": "all"}, cb);
};

HN.Callbacks.insertRowCol = function(path, range, axis, type) {
    if( axis == Y ) {
        HN.Util.postRow(path, range.y1, range.y2, {"insert":type});
    } else {
        HN.Util.postColumn(path, range.x1, range.x2, {"insert":type});
    }
};

HN.Callbacks.mergeCells = function(path, range) {    
    var cell = {"x":range.x1, "y":range.y1},
        right    = range.x2 - range.x1,
        down     = range.y2 - range.y1,
        post     = {
            "set": { 
                "merge": { 
                    "right" : right, 
                    "down"  : down
                } 
            } 
        };

    if (right === 0 && down === 0) {
        HN.Util.postCell(path, cell, {"clear": "merge"});
    } else {
        HN.Util.postCell(path, cell, post);
    }
};

HN.Callbacks.setPassword = function(password, cb) {
    var data = {"admin": {"set_password": {"password": password}}};
    HN.Util.postPath("/_admin/", data, cb);
};

HN.Callbacks.setChampion = function(path, view, cb) {
    var p = HN.Util.listToPath(path);
    HN.Util.postPath("/_admin/", 
                     {"admin": {"set_champion": {"path":p, "view":view}}}, 
                     cb);
};

HN.Callbacks.setView = function(path, view, perms, cb) {
    
    var p = HN.Util.listToPath(path);
    var args = {"set_view": {
        "path"     : p, 
        "view"     : view, 
        "everyone" : perms.everyone,
        "groups"   : perms.groups
    }};
    HN.Util.postPath("/_admin/", {"admin": args}, cb);
};

HN.Callbacks.pasteStyle = function (path, range, srcurl, srcrange) {
    var json = {"copystyle": {
        "src": srcurl + HN.Util.range_to_str(srcrange)
    }};
    HN.Util.postRange(path, range, json);
};

HN.Callbacks.pasteValue = function (path, range, srcurl, srcrange) {
    var json = {"copyvalue": {
        "src": srcurl + HN.Util.range_to_str(srcrange)
    }};
    HN.Util.postRange(path, range, json);
};
