/**
 * @namespace HN.Callbacks
 * Defines the callbacks for server events such as changing values
 * styles etc.
 */
HN.Callbacks = {};

/**
 * Set the value of a cell, clears if the value is empty
 */
HN.Callbacks.set_cell = function(path, cell, value)
{
    if (value == "") {
        HN.Util.postCell(path, cell, {clear: "contents"});
    } else {
        //data.poke_cell_value(cell, value);
        HN.Util.postCell(path, cell, {set: {formula: value}});
    };
};

/**
 * Clear a range of contents or format (or both)
 */
HN.Callbacks.clear = function(path, range, clear)
{
    HN.Util.postRange(path, range, {clear: clear});
};

/**
 * Set the style on a range
 */
HN.Callbacks.style = function(path, range, style, val)
{
    var x = {};
    x[style] = val;
    HN.Util.postRange(path, range, {set: x});
};

/**
 * Set the format on a range
 */
HN.Callbacks.format = function(path, range, val)
{
    HN.Util.postRange(path, range, {set: {format: val}});
};

/**
 * Set the width of a column
 */
HN.Callbacks.setWidth = function(path, column, width)
{
    HN.Util.postColumn(path, column, column, {set: {width: width}});
};

HN.Callbacks.setHeight = function(path, row, height)
{
    HN.Util.postRow(path, row, row, {set: {height: height}});
};

HN.Callbacks.drag = function(path, selected, dragged)
{
    if( selected.x1 == selected.x2 && selected.y1 == selected.y2 ) {
        var msg = {drag: {range: HN.Util.range_to_str(dragged)}};
        HN.Util.postCell(path, {x:selected.x1, y:selected.y1}, msg);
    }
};

HN.Callbacks.pasteValues = function(path, range, values)
{
    HN.Util.postRange(path, range, {set: {formula: values}});
};

HN.Callbacks.pasteRange = function(path, range, srcurl, srcrange)
{
    var src = srcurl + HN.Util.range_to_str(srcrange);
    HN.Util.postRange(path, range, {copy: {src: src}});
};

HN.Callbacks.deleteRowCol = function(path, range, axis)
{
    if( axis == Y ) {
        HN.Util.postRow(path, range.y1, range.y2, {"delete":"all"});
    } else {
        HN.Util.postColumn(path, range.x1, range.x2, {"delete":"all"});
    }
};

HN.Callbacks.setLanguage = function(language)
{
    var cb = function() {
        window.location.reload(true);
    };
    HN.Util.postPath("/_user/", {set: {language: language}}, cb);
};

HN.Callbacks.setMark = function(mark)
{
    HN.Util.postPath("./?mark", {set: {mark: mark.value}}, null);
};

HN.Callbacks.setBorders = function(range, where, border,
                                   border_style, border_color)
{
    HN.Util.postRange(range, {borders: {where: where, border: border,
                                        border_style: border_style,
                                        border_color: border_color}});
};

HN.Callbacks.deletePage = function(path, language)
{
    var cb = function() {
        window.location.reload(true);
    };
    HN.Util.postPath(document.location.pathname, {"delete": "all"}, cb);
};

HN.Callbacks.insertRowCol = function(path, range, axis, type)
{
    if( axis == Y ) {
        HN.Util.postRow(path, range.y1, range.y2, {insert:type});
    } else {
        HN.Util.postColumn(path, range.x1, range.x2, {insert:type});
    }
};
