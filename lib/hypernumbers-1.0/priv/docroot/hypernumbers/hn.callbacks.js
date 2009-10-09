/**
 * @namespace HN.Callbacks
 * Defines the callbacks for server events such as changing values
 * styles etc.
 */
HN.Callbacks = {};

/**
 * Set the value of a cell, clears if the value is empty
 */
HN.Callbacks.set_cell = function(cell, value)
{
    if (value == "") {
        HN.Util.postCell(cell, {clear: "contents"});
    } else {
        //data.poke_cell_value(cell, value);
        HN.Util.postCell(cell, {set: {formula: value}});
    };
};

/**
 * Clear a range of contents or format (or both)
 */
HN.Callbacks.clear = function(range, clear)
{
    HN.Util.postRange(range, {clear: clear});
};

/**
 * Set the style on a range
 */
HN.Callbacks.style = function(range, style, val)
{
    var x = {};
    x[style] = val;
    HN.Util.postRange(range, {set: x});
};

/**
 * Set the format on a range
 */
HN.Callbacks.format = function(range, val)
{
    HN.Util.postRange(range, {set: {format: val}});
};

/**
 * Set the width of a column
 */
HN.Callbacks.setWidth = function(column, width)
{
    HN.Util.postColumn(column, column, {set: {width: width}});
};

HN.Callbacks.setHeight = function(row, height)
{
    HN.Util.postRow(row, row, {set: {height: height}});
};

HN.Callbacks.drag = function(selected, dragged)
{
    if( selected.x1 == selected.x2 && selected.y1 == selected.y2 ) {
        var msg = {drag: {range: HN.Util.range_to_str(dragged)}};
        HN.Util.postCell({x:selected.x1, y:selected.y1}, msg);
    }
};

HN.Callbacks.pasteValues = function(range, values)
{
  HN.Util.postRange(range, {set: {formula: values}});
};

HN.Callbacks.pasteRange = function(range, srcurl, srcrange)
{
  var src = srcurl + HN.Util.range_to_str(srcrange);
  HN.Util.postRange(range, {copy: {src: src}});
};

HN.Callbacks.deleteRowCol = function(range, axis)
{
  if( axis == Y ) {
    HN.Util.postRow(range.y1, range.y2, {"delete":"all"});
  } else {
    HN.Util.postColumn(range.x1, range.x2, {"delete":"all"});
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

HN.Callbacks.deletePage = function(language)
{
  var cb = function() {
    window.location.reload(true);
  };
  HN.Util.postPath(document.location.pathname, {"delete": "all"}, cb);
};

HN.Callbacks.insertRowCol = function(range, axis, type)
{
  if( axis == Y ) {
    HN.Util.postRow(range.y1, range.y2, {insert:type});
  } else {
    HN.Util.postColumn(range.x1, range.x2, {insert:type});
  }
};
