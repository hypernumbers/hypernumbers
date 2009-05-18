

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
  var json = (value == "")
    ? {clear: "contents"}
    : {set: {formula: value}};

  HN.Util.postCell(cell, json);
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
  //var drag = HN.Util.coord_to_ref({"x":dragged.x1, "y":dragged.y1})+":"
  //  + HN.Util.coord_to_ref({"x":dragged.x2, "y":dragged.y2});
  //HN.Util.postRange(selected, {drag: {range: drag}});
};

HN.Callbacks.pasteValues = function(range, values)
{
  HN.Util.postRange(range, {set: {formula: values}});
};

HN.Callbacks.pasteRange = function(range, source)
{
  HN.Util.postRange(range, {copy: {range: HN.Util.range_to_str(source)}});
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

/**
 * @namespace HN.Keys
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
 * @namespace HN.States
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

/**
 * @class HN.Layout
 * Handles the main layout controls, the grid / axis / formula bar and toolbar
 */
HN.Layout = function(parent,name,sheet)
{
  this.p = parent;
  this.s = sheet;

  this.inner = HN.Util.id("scroller");

  // size of viewable area
  this.size      = {x:this.inner.clientWidth, y:this.inner.clientHeight};
  // maximum (top/right) position of scrollbar
  this.scrollmax = {x:0, y:0};
  // cache dom elements
  this.axis_dom  = {x:HN.Util.id("columns"),    y:HN.Util.id("rows")};
  this.scrollbar = {x:HN.Util.id("bottom-bar"), y:HN.Util.id("right-bar")};

  // Silly bug, need to be set expicitly before can read
  this.inner.style.top        = "0px";
  this.inner.style.left       = "0px";
  this.scrollbar.y.style.top  = "0px";
  this.scrollbar.x.style.left = "0px";

  this.calcScrollbarSize(Y);
  this.calcScrollbarSize(X);

  this.toolbar   = new HN.ToolBar(this);
  this.panes     = new HN.Layout.Panes(this, this.calcRowsCols());
  this.selection = new HN.Layout.Selection(this);

  var that = this;

  var button = function(id, mag, axis) {
    HN.Util.addEvent(HN.Util.id(id), "mousedown", function(e) {
      that.scrollButtonPressed(e, mag, axis);
    });
  };

  var bar = function(el, axis) {
    HN.Util.addEvent(el, "mousedown", function(e) {
      that.dragBoxPressed(e, axis);
    });
  };

  HN.Util.addEvent(window, "resize", function() {
    that.windowResize();
  });

  button("down",  true,  Y);
  button("up",    false, Y);
  button("right", true,  X);
  button("left",  false, X);

  bar(this.scrollbar.y, Y);
  bar(this.scrollbar.x, X);

  $("#footer").click(function(e) {
    if(e.target.nodeName == "SPAN") {
      that.selection.state = HN.States.NOT_EDITING;
      HN.Util.select(e.target);
      return true;
    } else {
      return false;
    }
  });
};

HN.Util.select = function( element )
{
  if ( document.selection ) {
    var range = document.body.createTextRange();
    range.moveToElementText(element);
    range.select();
  }
  else if ( window.getSelection )
  {
    var range = document.createRange();
    range.selectNodeContents(element);
    var selection = window.getSelection();
    selection.removeAllRanges();
    selection.addRange(range);
  }
};

/**
 * Calculate number of (5x8) panes needed to fill screen
 */
HN.Layout.prototype.calcRowsCols = function()
{
  //console.log(Math.ceil(this.size.x / HN.Sheet.CELL_WIDTH  / 5)+1);

  return {rows:Math.ceil(this.size.y / HN.Sheet.CELL_HEIGHT / 8)+1,
          cols:Math.ceil(this.size.x / HN.Sheet.CELL_WIDTH  / 5)+1};
};

/**
 * Resize the window, recalculate scrollbar size / position
 */
HN.Layout.prototype.windowResize = function()
{
  // Stupid bug where size isnt recalculated when firebux / search
  // pops up
  this.inner.style.top = (parseInt(this.inner.style.top)+1)+"px";
  this.size.y = this.inner.clientHeight;
  this.size.x = this.inner.clientWidth;
  this.inner.style.top = (parseInt(this.inner.style.top)-1)+"px";

  this.calcScrollbarSize(Y);
  this.calcScrollbarSize(X);

  this.calcScrollBarPos(X);
  this.calcScrollBarPos(Y);

  var tmp = this.calcRowsCols();
  this.panes.rows = tmp.rows;
  this.panes.cols = tmp.cols;
  this.panes.refresh();
};

HN.Layout.prototype.calcScrollBarPos = function(xy)
{
  var tmp = this.s[xy.dimension] - this.size[xy.str],
     tmp2 = Math.abs(parseInt(this.inner.style[xy.coord], 10) / tmp),
      pos = tmp2 * this.scrollmax[xy.str];

  if( pos > this.scrollmax[xy.str] ) {
    pos = this.scrollmax[xy.str];
  }

  this.scrollbar[xy.str].style[xy.coord] = Math.round(pos) + "px";
};

/**
 * Calculate Scrollbar size
 */
HN.Layout.prototype.calcScrollbarSize = function(xy)
{
  var tmp = Math.round( this.size[xy.str] / (this.s.max[xy.str] / 10));
  this.scrollbar[xy.str].style[xy.dimension] = tmp + "px";
  this.scrollmax[xy.str] = this.size[xy.str] - tmp - 32;
};

/**
 * Drag box has moved, scroll grid, pane if needed
 */
HN.Layout.prototype.dragBoxMoved = function(position, xy)
{
  // Adjust scrollbar position to no exceed bounds
  if(position < 0) {
    position = 0;
  } else if( position > this.scrollmax[xy.str]) {
    position = this.scrollmax[xy.str];
  }

  // If you scroll to 100%, position is full size - size on screen
  var    tmp = this.s[xy.dimension] - this.size[xy.str],
  // Approximate pixel position of grid
    grid_pos = tmp * (position / this.scrollmax[xy.str]),
      actual = this.get_new_offset(grid_pos, xy),
  // Difference between current and new position
        diff = -(parseInt(this.inner.style[xy.coord], 10)
                 + actual.position);

  this.scrollbar[xy.str].style[xy.coord] = position + "px";

  // If its not actually moved, dont do anthing
  if( diff == 0 ) {
    return;
  }

  // Move viewpane, scrollbar, store offset
  this.axis_dom[xy.str].style[xy.coord]  = -actual.position + "px";
  this.inner.style[xy.coord] = -actual.position+"px";
  this.s.offset[xy.str]      = actual.index;

  if( xy.str == "y" ) {
    HN.Util.id("rsel").style.top = -actual.position+"px";
  } else {
    HN.Util.id("csel").style.left = -actual.position+"px";
  }


  // Wrap panes if needed
  var that = this;
  window.setTimeout(function() {
    that.check_pane_bounds(diff, xy, diff < 0);
  },0);
};

/**
 * Drag box pressed
 */
HN.Layout.prototype.dragBoxPressed = function(event, axis)
{
  var that = this,
       bar = this.scrollbar[axis.str],
     mprop = (axis.str == "y") ? "clientY" : "clientX",
  // Position of initial click relative to scrollbar
    offset = event[mprop] - parseInt(bar.style[axis.coord], 10);

  var scroll = function(e) {
    that.dragBoxMoved((e[mprop] - offset), axis);
  };

  var onmouseup = function() {
    bar.className = "";
    HN.Util.removeEvent(document, "mouseup", onmouseup);
    HN.Util.removeEvent(document, "mousemove", scroll);
  };

  HN.Util.addEvent(document, "mouseup", onmouseup);
  HN.Util.addEvent(document, "mousemove", scroll);

  bar.className = "active";
  event.preventDefault();
  return false;
};

/**
 * Scroll Grid by 1 cell, forward = true mean down or right
 * axis is the direction to scroll
 */
HN.Layout.prototype.scrollBy1 = function(forward, xy)
{
  if( !forward && this.s.offset[xy.str] == 1 ) {
    return;
  }

  var       f = ( xy.str == "y" ) ? "row_height" : "col_width",
   scrollable = this.s[xy.dimension] - this.size[xy.str],
       offset = (forward)
                  ? -this.s[f](this.s.offset[xy.str])
                  :  this.s[f](this.s.offset[xy.str]-1),
     pane_pos = parseInt(this.inner.style[xy.coord], 10) + offset,
   scroll_pos = -(Math.round((pane_pos / scrollable)
                  * this.scrollmax[xy.str]));

  // Adjust scrollbar position
  if( scroll_pos >= this.scrollmax[xy.str] ) {
    // If scrollbar has hit of area, extend sheet size
    this.s.extend(xy, 2);
    this.calcScrollbarSize(xy);

    scroll_pos = this.scrollmax[xy.str];
    sheet_size = this.s[xy.dimension] - this.size[xy.str];
  }
  else if( scroll_pos < 0 ) {
    scroll_pos = 0;
  }

  // Move View Pane and store new offset
  this.inner.style[xy.coord] = pane_pos + "px";
  this.axis_dom[xy.str].style[xy.coord] = pane_pos + "px";
  this.s.offset[xy.str] += (forward) ? 1 : -1;
  this.scrollbar[xy.str].style[xy.coord] = scroll_pos + "px";

  if( xy.str == "y" ) {
    HN.Util.id("rsel").style.top = pane_pos+"px";
  } else {
    HN.Util.id("csel").style.left = pane_pos+"px";
  }

  // Wrap panes if needed
  this.check_pane_bounds(offset, xy, forward);
};

/**
 * Scroll Button pressed
 */
HN.Layout.prototype.scrollButtonPressed = function(event, forward, xy)
{
  var that = this, sheet = this.s, timer = null;

  var mouseup = function() {
    event.target.className = "";
    HN.Util.removeEvent(document, "mouseup", mouseup);
    clearInterval(timer);
  };

  event.target.className = "active";
  HN.Util.addEvent(document, "mouseup", mouseup);

  // Continue scrolling while mouse is pressed
  timer = setInterval(function() {
    if( forward || sheet.offset[xy.str] > 1 ) {
      that.scrollBy1(forward, xy);
    }
  }, 70);
};

/**
 * Notify the grid user has scrolled, move bounds, check that grid
 * can be displayed within current bounds and pane tiles when
 * needed
 */
HN.Layout.prototype.check_pane_bounds = function(offset, xy, upleft)
{
  var axis = xy.str, start = axis+"1", end = axis+"2",
         p = this.panes;

  p.bounds[start] += offset;
  p.bounds[end]   += offset;

  // Todo could do with cleverer logic on how to shift cells when
  // scrolling large increments, wasting work here
  if( Math.abs(offset) > p.p[0][0][xy.dimension] ) {
    var n = Math.floor(Math.abs(offset) / p.p[0][0][xy.dimension]);
    for( var x = 0; x < n; x++ ) {
      p["shift_"+axis](upleft);
    }
  }

  if( upleft && this.size[xy.str] > p.bounds[end] ) {
    p["shift_"+axis](upleft);
  } else if( !upleft && p.bounds[start] > -40 ) {
    p["shift_"+axis](upleft);
  }
};

/**
 * Given a coordinate, return the index at that position and a snapping
 * position for that index
 */
HN.Layout.prototype.get_new_offset = function(position, xy)
{
  var s = this.s, i = 1, total = 0,
      f = ( xy.str == "y" ) ? "row_height" : "col_width";

  while( true ) {
    total += s[f](i);

    if( total > position ) {
      return {index:i, position:total-s[f](i)};
    }

    ++i;
  };
};

/**
 * @class HN.Layout.Panes
 *
 */
HN.Layout.Panes = function(layout, size)
{
  this.layout = layout;
  this.axis   = layout.axis_dom;
  this.panes  = HN.Util.id("panes");
  this.inner  = HN.Util.id("scroller");
  this.p      = [];
  this.rows   = size.rows;
  this.cols   = size.cols;
  this.s      = layout.s;
  this.bounds = {x1:0, y1:0, x2:0, y2:0};
  this.index  = {x:[], y:[]};

  this.draw_index(Y, true, false, 0, 0, 1,
                  (this.rows * HN.Layout.Pane.DEF_ROWS) + 1);
  this.draw_index(X, true, false, 0, 0, 1,
                  (this.cols * HN.Layout.Pane.DEF_COLS) + 1);

  this.build_cells({});

  for( var i = 0; i < this.p[0].length; i++ ) {
    this.bounds.x2 += this.p[0][i].width;
  }
};

HN.Layout.Panes.prototype.refresh = function()
{
  this.bounds.x2 = this.bounds.x1;
  this.bounds.y2 = this.bounds.y1;

  var p = this.p[0][0];
  var args = { row:p.row, col:p.col,
               top: parseInt(p.div.style.top, 10),
               left:parseInt(p.div.style.left, 10)};

  this.removeTiles();
  this.build_cells(args);

  var bottom = this.p[this.p.length-1][0].row + HN.Layout.Pane.DEF_ROWS;
  var right  = this.p[0][this.p[0].length-1].col + HN.Layout.Pane.DEF_COLS;

  this.axis.y.innerHTML = "";
  this.axis.x.innerHTML = "";
  this.draw_index(Y, true, false, args.top, 0, args.row, bottom);
  this.draw_index(X, true, false, 0, args.left, args.col, right);

  for( var i = 0; i < this.p[0].length; i++ ) {
    this.bounds.x2 += this.p[0][i].width;
  }
  this.layout.selection.show_cell();
};

HN.Layout.Panes.prototype.redraw = function()
{
  var p = this.p[0][0];
  p.row = (p.row < 1) ? 1 : p.row;
  p.col = (p.col < 1) ? 1 : p.col;

  this.s.offset.y = p.row;
  this.s.offset.x = p.col;
  var pos = this.s.cell_offset(p.row, p.col);
  var args = { row:p.row, col:p.col, top:pos.top, left:pos.left};

  this.bounds =  {x1:0, y1:0, x2:0, y2:0};

  this.inner.style.top   = -pos.top + "px";
  this.inner.style.left  = -pos.left + "px";
  this.axis.y.style.top  = -pos.top + "px";
  this.axis.x.style.left = -pos.left + "px";

  this.removeTiles();
  this.build_cells(args);

  var bottom = this.p[this.p.length-1][0].row + HN.Layout.Pane.DEF_ROWS;
  var right  = this.p[0][this.p[0].length-1].col + HN.Layout.Pane.DEF_COLS;

  this.axis.y.innerHTML = "";
  this.axis.x.innerHTML = "";
  this.draw_index(Y, true, false, pos.top, 0, args.row, bottom);
  this.draw_index(X, true, false, 0, pos.left, args.col, right);

  for( var i = 0; i < this.p[0].length; i++ ) {
    this.bounds.x2 += this.p[0][i].width;
  }

  this.layout.calcScrollBarPos(Y);
  this.layout.calcScrollBarPos(X);
};

HN.Layout.Panes.prototype.removeTiles = function()
{
  this.panes.innerHTML="";
};

HN.Layout.Panes.prototype.get_cell = function(y, x)
{
  var obj = this.get_pane(y, x);

  if( !obj ) {
    return false;
  }

  var iny = (obj.y) % HN.Layout.Pane.DEF_ROWS,
      inx = (obj.x) % HN.Layout.Pane.DEF_COLS;

  return obj.pane.div.childNodes[iny].childNodes[inx];
};

HN.Layout.Panes.prototype.get_pane = function(y, x)
{
  var       p = this.p,
   bottomleft = p[0][0],
     topright = p[p.length-1][p[0].length-1];

  // (0 indexes)
  y -= bottomleft.row;
  x -= bottomleft.col;

  if(   y < 0 || x < 0
     || x > (HN.Layout.Pane.DEF_COLS * (p[0].length))
     || y > (HN.Layout.Pane.DEF_ROWS * (p.length))) {
    return false;
  }

  var paney = Math.floor( y / HN.Layout.Pane.DEF_ROWS ),
      panex = Math.floor( x / HN.Layout.Pane.DEF_COLS );

  return {"pane":p[paney][panex], "x":x, "y":y};
};


HN.Layout.Panes.prototype.shift_y = function(up)
{
  var p = this.p;

  if( up ) {
    var   y = p.length-1,
     rownum = p[y][0].row+8,
        row = p.shift();

    p[y] = [];
  }
  else
  {
    var   y = 0,
        tmp = p[y][0].row-8,
     rownum = tmp,
        row = p[p.length-1];

    p.length -= 1;
    p.unshift([]);
  }

  var oldheight = row[0].height,
            top = null,
      newheight = null,
            len = row.length;

  for( var x = 0; x < len; x++ )
  {
    p[y][x] = row[x];

    var pane = p[y][x],
        orig = pane.div;

    pane = row[x];
    pane.row = rownum;
    pane.update_view();

    if( x == 0 ) {
      newheight = pane.height;

      top = up
        ? parseInt(p[y-1][0].div.style.top, 10) + p[y-1][0].height
        : parseInt(p[1][0].div.style.top, 10) - newheight;
    }

    pane.div.style.top = top+"px" ;
  }

  if( up ) {
    this.bounds.y1 += oldheight;
    this.bounds.y2 += newheight;
  } else {
    this.bounds.y1 -= newheight;
    this.bounds.y2 -= oldheight;
  }

  this.draw_index(Y, up, true, top, 0, rownum,
                  rownum + HN.Layout.Pane.DEF_ROWS);
};

HN.Layout.Panes.prototype.shift_x = function(left)
{
  var p = this.p;

  if( left ) {
    var   x = p[0].length-1,
     colnum = p[0][x].col+5,
        col = [];

    for( var i = 0; i < this.p.length; i++ ) {
      var tmp = p[i].shift();
      col.push(tmp);
      p[i][x] = [];
    }
  }
  else
  {
    var   x = 0,
        tmp = p[0][0].col-5,
       plen = p[0].length-1,
     colnum = tmp,
        col = [];

     for( var i = 0; i < this.p.length; i++ ) {
       col.push(p[i][plen]);
       p[i].length -= 1;
       p[i].unshift([]);
     }
  }

  var oldwidth = col[0].width,
         lleft = null,
      newwidth = null,
           len = col.length;

  for( var y = 0; y < len; y++ ) {
    p[y][x] = col[y];

    var pane = p[y][x];
    var orig = pane.div;

    pane = col[y];
    //pane.div = orig.cloneNode(true);
    pane.col = colnum;
    pane.update_view();

    if( y == 0 ) {
      newwidth = pane.width;

      lleft = left
        ? parseInt(p[0][x-1].div.style.left, 10) + p[0][x-1].width
        : parseInt(p[0][1].div.style.left, 10) - newwidth;
    }

    pane.div.style.left = lleft+"px" ;
    //orig.parentNode.replaceChild(pane.div,orig);
  }

  if( left ) {
    this.bounds.x1 += oldwidth;
    this.bounds.x2 += newwidth;
  } else {
    this.bounds.x1 -= newwidth;
    this.bounds.x2 -= oldwidth;
  }

  this.draw_index(X, left, true, 0, lleft, colnum,
                  colnum + HN.Layout.Pane.DEF_COLS);
};

HN.Layout.Panes.prototype.draw_index = function(xy, append, del, top,
                                                left, start, end)
{
  var index = [],
       axis = this.axis[xy.str],
      sheet = this.s,
      style = (xy.str == "y")
        ? "padding:3px 2px 0px 0px;border-bottom:1px solid #aaa;"
          +"border-right:1px solid #aaa;text-align:right;vertical-align:top;"
        : "padding-top:4px;border-bottom:1px solid #aaa;border-right:"
          +"1px solid #aaa;text-align:center;";

  for( var i = start; i < end; i++ )
  {
    if( del ) {
      if( append ) {
        axis.removeChild(axis.childNodes[0]);
      } else {
        axis.removeChild(axis.childNodes[axis.childNodes.length-1]);
      }
    }

    var val = (xy.str == "y")
      ? [sheet.row_height(i),24]
      : [19,sheet.col_width(i)];

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
     axis.innerHTML += index.join("");
  } else {
     axis.innerHTML = index.join("") + axis.innerHTML;
  }
};

HN.Layout.Panes.prototype.build_cells = function(o)
{
  o.row  = (typeof o.row == 'undefined') ? 1 : o.row;
  o.col  = (typeof o.col == 'undefined') ? 1 : o.col;
  o.top  = (typeof o.top == 'undefined') ? 0 : o.top;
  o.left = (typeof o.left == 'undefined') ? 0 : o.left;

  var top = o.top;

  for( var y = 0; y < this.rows; y++ ) {
    var row = y * 8,
       left = o.left;

    this.p[y] = [];

    for( var x = 0; x < this.cols; x++ ) {
      this.p[y][x] = new HN.Layout.Pane(this.s, row+o.row, (x*5)+o.col);
      this.p[y][x].div.style.top  = top + "px";
      this.p[y][x].div.style.left = left + "px";
      this.panes.appendChild(this.p[y][x].div);

      left += this.p[y][x].width;
    }

    top += this.p[y][0].height;
    this.bounds.y2 += this.p[y][0].height;
  }
};

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

  var twidth  = 0,
      theight = 0,
        cells = [],
          top = 0;

  for( var y = row; y < row + HN.Layout.Pane.DEF_ROWS; y++ ) {
    var height = this.s.row_height(y),
          left = 0;

    twidth  = 0;
    theight += height;

    cells[cells.length] = '<div>';

    for( var x = col; x < col+HN.Layout.Pane.DEF_COLS; x++ ) {
      var width = this.s.col_width(x);
      var cell  = this.s.cell(y,x);
      var val   = (cell && cell.value) || "";
      var styl  = (cell && cell.style && this.s.data.styles[cell.style]) || "";
      var zindex = (val == "" && styl == "") ? 0 : x+y;

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
         styl = tmp.style,
        width = this.s.col_width(x+this.col),
         cell = this.s.cell(count, x+this.col);
            s = cell.style && this.s.data.styles[cell.style] || "",
          val = (cell && cell.value) || "",
       zindex = val == "" && s == "" ? 0 : count + (x+this.col);

      tmp.setAttribute("style", style + s);
      styl.top      = (totalh-1)+"px";
      styl.left     = (totalw-1)+"px";
      styl.height   = (height-1)+"px";
      styl.width    = (width-1)+"px";
      styl.zIndex   = zindex;
      tmp.innerHTML  = val;

      totalw += width;
    }

    totalh += height;
  }

  //this.div.parentNode.replaceChild(node, this.div);
  this.height = totalh;
  this.width = totalw;
};

/**
 * @class HN.ToolBar
 * provides functionality for the styles toolbar
 */
HN.ToolBar = function(layout)
{
  var that = this;
  this.formula = "";
  this.functions = {};

  this.layout = layout;
  this.drop_menus();
  this.dialogs();
  this.loadFunctions();
  this.addPages();
  this.fileUpload();

  var add_events = function(id, style, val) {
    HN.Util.addEvent(HN.Util.id(id), "mousedown", function(e) {
      if(e.target.nodeName== "A") {
        that.setStyle(style, val(e.target));
      }
    });
  };

  var split = function(obj) {
    return obj.getAttribute("id").split("_")[1];
  };

  HN.Util.addEvent(HN.Util.id("formats"), "mousedown", function(e) {
    if(e.target.nodeName== "A") {
      that.setFormat(e.target.getAttribute("id"));
    }
  });

  HN.Util.addEvent(HN.Util.id("deletebtn"), "mousedown", function(e) {
                     HN.Callbacks.deletePage();
  });

  HN.Util.addEvent(HN.Util.id("functionsbtn"), "mousedown", function(e) { 
                     HN.Util.id("functions").style.display = "block"; 
                   }); 
  
  add_events("ffamily","font-family", split);
  add_events("aligntext","text-align", split);
  add_events("bgcolors","background-color",
    function(obj) { return "#"+split(obj); });
  add_events("fontcolors","color",
    function(obj) { return "#"+split(obj); });
  add_events("bold","font-weight",
    function() { return "bold"; });
  add_events("italic","font-style",
    function() { return "italic"; });
  add_events("strike","text-decoration",
    function() { return "line-through"; });
  add_events("sizes","font-size",
    function(obj) { return split(obj)+"px"; });
};

HN.ToolBar.prototype.loadFunctions = function(e)
{
  var that = this;

  var fn = "/hypernumbers/fns_" + lang + ".json";
  $.getJSON(fn, function(data) {
    that.functions = data;
    that.generateFunCategories();
  });
};

HN.ToolBar.prototype.generateFunCategories = function()
{
  var that = this, html = "", cat = {}, cathtml = "", funs = [],
    catdom = HN.Util.id("catlist"),
    fundom = HN.Util.id("funlist");

  cat["All Functions"] = [];
  for( var i=0, len=this.functions.length; i < len; i++ ) {
    var fun = this.functions[i], str = fun.category;
    funs[fun.name] = fun;
    if( typeof cat[fun.category] == "undefined") {
      cat[fun.category] = [];
    }
    cat["All Functions"].push(fun);
    cat[fun.category].push(fun);
  }

  for (var x in cat) {
    cathtml += "<option value=\""+x+"\">"+x+"</option>";
  }
  catdom.innerHTML = cathtml;

  HN.Util.addEvent(catdom, "change", function(e) {
    var index = catdom.selectedIndex;
    var category = catdom.childNodes[index].getAttribute("value");
    that.filterCategory(fundom, category, cat[category]);
  });

  var enter = function(e) {
    e.preventDefault();
    var sel = that.layout.selection;
    if( !sel.is_editing() ) {
      sel.startEditing("="+that.formula+"(");
      sel.formula.value = "="+that.formula+"(";
    } else {
      sel.formula.value += that.formula+"(";
      sel.input.value   += that.formula+"(";
    }
    sel.calculateInputWidth();
  };

  HN.Util.addEvent(HN.Util.id("enterformula"), "mousedown", enter);


  this.filterCategory(fundom, "All Functions", cat["All Functions"]);
  this.selectFun(cat["All Functions"][0]);
  HN.Util.addEvent(fundom, "mousedown", function(e) {
    e.preventDefault();
    if(e.target.nodeName == "A") {
      that.selectFun(funs[e.target.getAttribute("name")]);
    }
  });
};

HN.ToolBar.prototype.selectFun = function(fun)
{
  this.formula = fun.name;
  HN.Util.id("funname").innerHTML = fun.name;

  var funlist = HN.Util.id("funlist").childNodes[0];
  for( var i=0, len=funlist.childNodes.length; i < len; i++ ) {
    var a = funlist.childNodes[i].childNodes[0];
    a.style.backgroundColor = (a.getAttribute("name") == fun.name)
      ? "#FFF" : "";
  }
};

HN.ToolBar.prototype.filterCategory = function(fundom, name, funs)
{
  var html = "<ul>";
  for (var x in funs) {
    html += "<li><a name=\""+funs[x].name+"\">"+funs[x].name+"</a></li>";
  }
  fundom.innerHTML = html + "</ul>";
};

HN.ToolBar.prototype.addPages = function()
{
  function isEmpty(ob) {
    for( var i in ob ){
      if(ob.hasOwnProperty(i)) {
        return false;
      }
    }
    return true;
  }

  var add = function(obj, path, top) {
    var html = !top ? "<ul>" : "";
    for (var x in obj) {
      var p = path+x+"/";
      html += "<li><a href=\""+p+"\"> "+x+"</a>";
      html += (isEmpty(obj[x])) ? "" : add(obj[x], p, false);
      html += "</li>";
    }
    html += !top ? "</ul>" : "";
    return html;
  };

  $.getJSON("?pages", function(data) {

    if( !isEmpty(data) ) {
      console.log(data);
      var empty = HN.Util.id("empty");
      empty.innerHTML = add(data, "/", true);
    }

    $("#menu").filemenu();
  });

};


/**
 * Given format key, set the format for current selection
 */
HN.ToolBar.prototype.setFormat = function(value)
{
  var formats = {
    "fmt_0":"General",
    "fmt_1":"0",
    "fmt_2":"0.00",
    "fmt_3":"##0;[Red](#,##0)",
    "fmt_4":"##0;(#,##0)",
    "fmt_5":"##0.00;(#,##0.00)",
    "fmt_6":"\"$\"##0",
    "fmt_7":"\"$\"##0.00",
    "fmt_8":"\"$\"##0;[Red]\"$\"#,##0",
    "fmt_9":"0%",
    "fmt_10":"0.00%",
    "fmt_11":"d/m/yyyy",
    "fmt_12":"hh:mm:ss",
    "fmt_13":"d/m/yyyy hh:mm:ss"
  };

  var sel = this.layout.selection,
   format = formats[value];

  if( sel.is_selected() ) {
    HN.Callbacks.format(sel.bounds, format);
  }
};

/**
 * Set the style on current selection, bold / italic / strike styles
 * need to toggle based on current cell
 */
HN.ToolBar.prototype.setStyle = function(style, value)
{
  var sel = this.layout.selection,
    sheet = this.layout.s;

  var vals = {
    "font-weight":     {k : "font-weight:bold",             v:"normal"},
    "font-style":      {k : "font-style:italic",            v:"normal"},
    "text-decoration": {k : "text-decoration:line-through", v:"none"}
  };

  if(  sel.is_selected() ) {
    // Toggle bold / italic / strikethrough
    if( typeof vals[style] != "undefined" &&
        sheet.get_style_by_cell(sel.cell).match(vals[style].k) ) {
      value = vals[style].v;
    }
    HN.Callbacks.style(sel.bounds, style, value);
  }
};


// TODO: Clean up, code is nasty
HN.ToolBar.prototype.fileUpload = function()
{
  HN.Util.addEvent(HN.Util.id("importbtn"), "mousedown", function(e) {
    HN.Util.id("import").style.display = "block";
  });

  var success = function(data, status) {
    HN.Util.id("importloading").style.display = "none";
    if( typeof data.error != "undefined") {
      $("#filewrapper").empty().html("<input type='file' name='Filedata' "
        + "id='Filedata' value='Upload Spreadsheet' />");
      setTimeout(function() {
        HN.Util.id("importerror").style.display = "none";
      }, 5000);
      HN.Util.id("importerror").style.display = "block";
      $("#Filedata").change(change);
    } else {
      document.location.href = data.location;
    }
  };

  var change = function() {
    $.ajaxFileUpload({datatype: "json", fileElementId:'Filedata', success:success});
    HN.Util.id("importloading").style.display = "block";
    HN.Util.id("importerror").style.display = "none";
  };

  $("#Filedata").change(change);
};

/**
 * Setup the drop down menus (colors / alignment / font etc)
 */
HN.ToolBar.prototype.dialogs = function()
{
  var el = document.getElementsByClassName("dialog"),
     len = el.length,
     cur = null,
    dpos = {x:0, y:0},
   start = {x:0, y:0};

  var up = function( e ) {
    HN.Util.removeEvent(document, "mouseup", up);
    HN.Util.removeEvent(document, "mousemove", move);
  };

  var move = function( e ) {
    var y = (dpos.y + (e.clientY - start.y));
    var x = (dpos.x + (e.clientX - start.x));
    cur.style.top  = (( y > 0 ) ? y : 0) + "px";
    cur.style.left = (( x > 0 ) ? x : 0) + "px";
  };

  var down = function( e ) {
    e.preventDefault();
    if( e.target.className == "close") {
      this.style.display = "none";

    } else if( e.target.nodeName == "H2" || e.target.parentNode.nodeName == "H2" ) {

      cur   = this;
      dpos  = {x:parseInt(this.style.left), y:parseInt(this.style.top)};
      start = {x:e.clientX, y:e.clientY};

      HN.Util.addEvent(document, "mousemove", move);
      HN.Util.addEvent(document, "mouseup", up);
    }
  };

  for( var i = 0; i < len; i++ ) {
    el[i].style.top = "50px";
    el[i].style.left = "50px";
    el[i].innerHTML = "<div class='close'>&nbsp;</div>" + el[i].innerHTML;
    HN.Util.addEvent(el[i], "mousedown", down);
  }
};

/**
 * Setup the drop down menus (colors / alignment / font etc)
 */
HN.ToolBar.prototype.drop_menus = function()
{
  var el = document.getElementsByClassName("expand"),
     len = el.length;

  var click = function(e) {

    var parent = e.currentTarget;
    var menu = parent.childNodes[3];

    HN.Util.removeEvent(parent, "mousedown", click);

    var hide = function(e) {
      if( e.target == menu ) {
        return;
      }
      HN.Util.addEvent(parent, "mousedown", click);
      HN.Util.removeEvent(document, "mousedown", hide);
      $(parent).removeClass("active");
      menu.style.display = "none";
    };

    $(parent).addClass("active");
    menu.style.display = "block";
    window.setTimeout(function() {
      HN.Util.addEvent(document, "mousedown", hide);
    },0);
  };

  for( var i = 0; i < len; i++ ) {
    HN.Util.addEvent(el[i], "mousedown", click);
  }
};

