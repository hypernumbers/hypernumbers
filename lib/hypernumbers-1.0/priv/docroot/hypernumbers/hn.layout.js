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
  var drag = HN.Util.coord_to_ref({"x":dragged.x1, "y":dragged.y1})+":"
    + HN.Util.coord_to_ref({"x":dragged.x2, "y":dragged.y2});
  HN.Util.postRange(selected, {drag: {range: drag}});
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
};

/**
 * Calculate number of (5x8) panes needed to fill screen
 */
HN.Layout.prototype.calcRowsCols = function()
{
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
  var that = this;
  window.setTimeout(function() {
    that.check_pane_bounds(offset, xy, forward);
  },0);
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
  this.inner  = layout.inner;
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
  var p = this.p[0][0];

  for( var y = 0; y < this.rows; y++ ) {
    for( var x = 0; x < this.cols; x++ ) {
      if( this.p[y] && this.p[y][x] ) {
        this.inner.removeChild(this.p[y][x].div);
      }
    }
  }
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
      this.inner.appendChild(this.p[y][x].div);

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
          val = cell.value || "",
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

HN.Layout.Selection = function(layout)
{
  this.state   = HN.States.SELECTED_CELL;
  this.layout  = layout;
  this.input   = HN.Util.id("input");
  this.formula = HN.Util.id("formula");
  this.hidden_input = HN.Util.id("hidden_input");
  this.wrapper = HN.Util.id("selection");
  this.dragger = HN.Util.id("dragger");
  this.dragged = HN.Util.id("dragged");
  this.input_width = 0;
  this.input_height = 0;

  this.has_scrolled = false;
  this.last_click = null;
  this.cell_dom   = null;
  this.cell       = null;
  this.in_formula_bar = false;
  this.copied_str  = "";
  this.copied      = {"x1":1, "y1": 1, "x2": 1, "y2": 1};
  this.bounds      = {"x1":1, "y1": 1, "x2": 1, "y2": 1};
  this.bounds_ord  = {"x1":0, "y1": 0, "x2": 0, "y2": 0};
  this.dragpos     = {"x1":0, "y1": 0, "x2": 0, "y2": 0};

  this.offset  = {
    "y":HN.Util.y_pos(this.layout.inner),
    "x":HN.Util.x_pos(this.layout.inner)
  };

  var that = this;

  HN.Util.addEvent(that.dragger, "mousedown", function(e) {
    that.dragdown(e);
  });

  HN.Util.addEvent(layout.inner, "mousedown", function(e) {
    that.mousedown(e);
  });

  HN.Util.addEvent(HN.Util.id("corner"), "mousedown", function(e) {
    that.bounds = {x1:1, y1:1, x2:that.layout.s.max.x, y2:that.layout.s.max.y};
    that.show_selection({});
  });


  HN.Util.addEvent(layout.inner, "contextmenu", function(e) {
    that.gridContextMenu(e);
  });

  var add_axis_events = function(id, axis) {
    HN.Util.addEvent(id, "mousedown", function(e) {
      that.mouseDownAxis(e, axis);
    });
    HN.Util.addEvent(id, "contextmenu", function(e) {
      that.axisContextMenu(e, axis);
    });
  };

  add_axis_events(HN.Util.id("columnsout"), X);
  add_axis_events(HN.Util.id("rowsout"), Y);

  HN.Util.addEvent(document, "keydown", function(e) {
    that.keydown(e);
  });

  HN.Util.addEvent(that.formula, "focus", function(e) {
    that.in_formula_bar = true;
  });

  this.set_cell(1, 1);
  this.show_selection();
};

HN.Layout.Selection.prototype.gridContextMenu = function(e)
{
  e.preventDefault();
  return false;
};

HN.Layout.Selection.prototype.axisContextMenu = function(e, axis)
{
  var that = this;

  var el = (axis == Y)
    ? HN.Util.id("axiscontextmenurow")
    : HN.Util.id("axiscontextmenucol");
  el.style.display = "block";
  el.style.top = e.clientY+"px";
  el.style.left = e.clientX+"px";

  var hide = function(e) {
    var action = e.target.getAttribute("name");
    if( action == "delete" ) {
      HN.Callbacks.deleteRowCol(that.bounds, axis);
    } else if ( action == "insertafter" ) {
      HN.Callbacks.insertRowCol(that.bounds, axis, "after");
    } else if ( action == "insertbefore" ) {
      HN.Callbacks.insertRowCol(that.bounds, axis, "before");
    }
    HN.Util.removeEvent(document, "mousedown", hide);
    el.style.display = "none";
  };

  HN.Util.addEvent(document, "mousedown", hide);

  e.preventDefault();
  return false;
};

HN.Layout.Selection.prototype.deselect = function(hide)
{
  if( this.in_formula_bar ) {
    this.in_formula_bar = false;
  }

  this.formula.blur();
  this.input.blur();

  var c = this.layout.s.cell(this.cell.y, this.cell.x);
  if( this.is_light_edit() ) {
    this.cell_dom.innerHTML = this.input.value;
    c.value = this.input.value;
    HN.Callbacks.set_cell(this.cell, this.input.value);
  } else if( this.is_full_edit() ) {
    var formula = (c && c.formula) || "";

    if( this.input.value != formula ) {
      this.cell_dom.innerHTML = this.input.value;
      c.value = this.input.value;
      HN.Callbacks.set_cell(this.cell, this.input.value);
    } else {
      this.cell_dom.innerHTML = (c && c.value) || "";
    }
  } else {
    this.cell_dom.innerHTML = this.input.value;
  }
};

/**
 * Takes the value from current input and expands the input to fit
 * @return {null}
 */
HN.Layout.Selection.prototype.calculateInputWidth = function()
{
  var input = this.input,
     hidden = this.hidden_input,
      sheet = this.layout.s;

  hidden.innerHTML = input.value;

  var width = hidden.clientWidth+5,
   tmpwidth = 0;

  while( width > tmpwidth ) {
    tmpwidth += sheet.col_width(this.cell.x + this.input_width);
    this.input.style.width = (tmpwidth-2)+ "px";
    this.input_width++;
  };
};

HN.Layout.Selection.prototype.mouseDownAxis = function(e, xy)
{
  e.preventDefault();

  if( e.target.className == "handle" && e.button != 2 ) {
    var  that = this,
         attr = (xy == Y) ? "clientY" : "clientX",
        start = e[attr],
       handle = e.target,
      new_val = 0,
         cell = handle.parentNode,
         orig = parseInt(cell.style[xy.dimension], 10),
        index = (xy == Y) ? parseInt(cell.textContent, 10)
                  : HN.Util.from_b26(cell.textContent),
         axis = (xy == Y) ? HN.Util.id("rsel") : HN.Util.id("csel");

    axis.style.display = "none";
    handle.style.backgroundColor = "#FF9966";
    document.body.style.cursor = (xy == Y) ? "ns-resize" : "ew-resize";

    var mouseup = function( e ) {
      handle.style.backgroundColor = "";
      document.body.style.cursor = "";
      axis.style.display = "block";
      HN.Util.removeEvent(document, "mouseup", mouseup);
      HN.Util.removeEvent(document, "mousemove", mousemove);

      if( xy == Y ) {
        HN.Callbacks.setHeight(index, new_val+4);
      } else {
        HN.Callbacks.setWidth(index, new_val+4);
      }
    };

    var mousemove = function( e ) {
      new_val = orig + (e[attr] - start);
      if( new_val > 0 ) {
        that.shiftAxis(cell, xy, index, (e[attr] - start));
        cell.style[xy.dimension] =  new_val + "px";
      }
    };

    HN.Util.addEvent(document, "mouseup", mouseup);
    HN.Util.addEvent(document, "mousemove", mousemove);

  } else {

    var that = this,
        attr = (xy == Y) ? "clientY" : "clientX",
      offset = (xy == Y) ? 148 : 26,
         max = (xy == Y) ? this.layout.s.max.x : this.layout.s.max.y,
           b = this.order_bounds(this.bounds),
       start = that.layout.get_new_offset(e[attr] - offset, xy);

    if( e.button == 2 &&
      ((xy == Y && start.index >= b.y1 && start.index <= b.y2 && b.x1 == 1) ||
      (xy == X && start.index >= b.x1 && start.index <= b.x2 && b.y1 == 1))) {
      return;
    }

    if( xy == Y ) {
      that.bounds = {y1:start.index, x1:1, y2:start.index, x2:max};
      that.cell = {x:1, y:start.index};
    } else {
      that.bounds = {x1:start.index, y1:1, x2:start.index, y2:max};
      that.cell = {y:1, x:start.index};
    }
    that.rebuild_input({});
    that.show_selection();

    var mouseup = function( e ) {
      HN.Util.removeEvent(document, "mouseup", mouseup);
      HN.Util.removeEvent(document, "mousemove", mousemove);
    };

    var mousemove = function( e ) {
      var end = that.layout.get_new_offset(e[attr] - offset, xy);

      if( xy == Y ) {
        that.bounds = {y1:Math.min(start.index, end.index), x1:1,
                       y2:Math.max(start.index, end.index), x2:max};

      } else {
        that.bounds = {x1:Math.min(start.index, end.index), y1:1,
                       x2:Math.max(start.index, end.index), y2:max};
      }
      that.show_selection();
      e.preventDefault();
      return false;
    };

    HN.Util.addEvent(document, "mouseup", mouseup);
    HN.Util.addEvent(document, "mousemove", mousemove);
  }
};

HN.Layout.Selection.prototype.shiftAxis = function(element, axis, index, diff)
{
  while( element.nextSibling && ++index) {
    element = element.nextSibling;
    var tmp = (axis == Y)
      ? this.layout.s.cell_offset(index, 1)
      : this.layout.s.cell_offset(1, index);
    element.style[axis.coord] = (tmp[axis.coord] + diff) + "px";
  }
};

/**
 * Takes the value from current input and expands the input to fit
 * @return {null}
 */
HN.Layout.Selection.prototype.rebuild_input = function(o)
{
  o.useformula = (typeof o.useformula == 'undefined') ? false : o.useformula;
  var input = this.input, cell = this.cell,
     layout = this.layout,
      sheet = layout.s;

  var cell_attr = sheet.cell(cell.y, cell.x);
  var cell_pos  = sheet.cell_offset(cell.y, cell.x);

  var formula = (cell_attr.formula) || "";
  var style   = "background:#FFF;"+sheet.get_style_by_cell(cell);
  var value   = (o.useformula) ? formula : (cell_attr.value || "");

  input.setAttribute("style", style);
  input.style.top  = (cell_pos.top-1)  + "px";
  input.style.left = (cell_pos.left+0) + "px";

  this.input_width  = 0;
  this.input_height = 0;

  this.formula.value = formula;
  this.cell_dom.innerHTML = "";

  input.value = value;
  input.style.width = "0px";
  input.style.height = (sheet.row_height(this.cell.y)-2) + "px";

  this.calculateInputWidth();
};

HN.Layout.Selection.prototype.is_viewable = function(y, x)
{
  var ret   = {x:true, y:true};
  var coord = this.layout.s.cell_offset(y, x);
  var top   = Math.abs(parseInt(this.layout.inner.style.top, 10));
  var left  = Math.abs(parseInt(this.layout.inner.style.left, 10));

  if( coord.top < top || (coord.top+20) > top + this.layout.size.y ) {
    ret.y = false;
  }

  if( coord.left < left || (coord.left+20) > left + this.layout.size.x ) {
    ret.x = false;
  }

  return ret;
};

HN.Layout.Selection.prototype.scrollIntoView = function(y, x, dir)
{
  var tmp = this.is_viewable(y, x), redraw = false;;

  if( !tmp.x && dir === true ) {
    this.layout.panes.p[0][0].col = x;
    redraw = true;
  } else if ( !tmp.x ) {
    this.layout.scrollBy1(dir == HN.RIGHT, X);
  }

  if( !tmp.y && dir === true ) {
    this.layout.panes.p[0][0].row = y;
    redraw = true;
  } else if ( !tmp.y ) {
    this.layout.scrollBy1(dir == HN.DOWN, Y);
  }

  if( redraw ) {
    this.layout.panes.redraw();
  }
  this.has_scrolled = false;
};

HN.Layout.Selection.prototype.set_cell = function(y, x, dir)
{
  this.cell = {y:y, x:x};
  if( dir ) {
    var arg = (!this.has_scrolled) ? dir : this.has_scrolled;
    this.scrollIntoView(y, x, arg);
  }
  this.cell_dom = this.layout.panes.get_cell(y, x);
};

HN.Layout.Selection.prototype.keydown = function(event)
{
  this.do_keydown(event);

  var that = this,
   timeout = null,
     timer;

  var keyup = function() {
    window.clearTimeout(timeout);
    HN.Util.removeEvent(document, "keyup", keyup);
    clearInterval(timer);
  };

  var set_interval = function() {
    timer = setInterval(function() {
      that.do_keydown(event);
    }, 30);
  };

  HN.Util.addEvent(document, "keyup", keyup);
  timeout = window.setTimeout(set_interval, 300);
};

HN.Layout.Selection.prototype.pasteText = function(val)
{
  if( val == this.copied_str ) {
    this.bounds.x2 = this.bounds.x1 + (this.copied.x2 - this.copied.x1);
    this.bounds.y2 = this.bounds.y1 + (this.copied.y2 - this.copied.y1);
    this.state = HN.States.EDIT_LIGHT_RANGE;
    this.show_selection();
    HN.Callbacks.pasteRange(this.bounds,this.copied);
    return;
  }

  if(val.match("\t") === null && val.match("\n") === null) {
    this.startEditing(val);
  } else {
    var lines = val.split("\n");
    for( var i = 0; i < lines.length; i++) {
      lines[i] = lines[i].split("\t");
    }
    if( lines[lines.length-1] == "") {
      lines.length--;
    }

    this.bounds.x2 = this.bounds.x1 + (lines[0].length-1);
    this.bounds.y2 = this.bounds.y1 + (lines.length-1);
    this.state = HN.States.EDIT_LIGHT_RANGE;
    this.show_selection();
    HN.Callbacks.pasteValues(this.bounds,lines);
  }
};

HN.Layout.Selection.prototype.startEditing = function(val)
{
  this.input.style.width = "0px";
  this.input_width = 0;
  this.input.value = val;
  this.input.focus();
  this.calculateInputWidth();

  HN.Util.id("copied").style.display = "none";

  this.state = (this.state == HN.States.SELECTED_RANGE)
    ? HN.States.EDIT_LIGHT_RANGE
    : HN.States.EDIT_LIGHT_CELL;
};

HN.Layout.Selection.prototype.do_keydown = function(e)
{
  if( this.in_formula_bar ) {
    if(e.keyCode == HN.Keys.ENTER || e.keyCode == HN.Keys.TAB) {
      this.select(e.keyCode, e);
    } else {
      if( this.is_selected() ) {
        this.startEditing();
        this.formula.focus();
      }
      var that = this;
      setTimeout(function() {
        that.input.value = that.formula.value;
      },0);
    }
  } else if( (e.keyCode == HN.Keys.ENTER || e.keyCode == HN.Keys.TAB)
     && (this.is_editing() || this.is_selected()) )
  {
    if( this.is_cell() ) {
      this.select(e.keyCode, e);
    } else {
      this.select_next(e.keyCode, e);
    }
  }

  else if( (this.is_light_edit() || this.is_selected())
    && (e.keyCode == HN.Keys.UP || e.keyCode == HN.Keys.DOWN
    || e.keyCode == HN.Keys.LEFT || e.keyCode == HN.Keys.RIGHT) )
  {
    this.select(e.keyCode, e);
  }

  else if(this.is_selected() && (e.keyCode == HN.Keys.DELETE
                                || e.keyCode == HN.Keys.BACKSPACE))
  {
    this.bounds = this.order_bounds();
    HN.Callbacks.clear(this.bounds, "contents");
  }

  else if(this.is_selected() && !(  e.keyCode == HN.Keys.SHIFT
                                 || e.keyCode == HN.Keys.TAB
                                 || e.keyCode == HN.Keys.CTRL))
  {
    this.scrollIntoView(this.cell.y, this.cell.x, true);

    // Paste
    if( e.ctrlKey && e.keyCode == 86 ) {
      var that = this, inp = HN.Util.id("paste");
      inp.value = "";
      inp.focus();

      setTimeout( function() {
        that.pasteText(inp.value);
        inp.blur();
      },0);

    // Copy
    } else if( e.ctrlKey && e.keyCode == 67 ) {

      var str = "";
      for(var y = this.bounds.y1; y <= this.bounds.y2; y++) {
        for(var x = this.bounds.x1; x <= this.bounds.x2; x++) {
          str += this.layout.s.cell(y, x).value || "";
          if(x < this.bounds.x2) {
            str += "\t";
          }
        }
        if(y < this.bounds.y2) {
          str += "\n";
        }
      }
      var inp = HN.Util.id("paste");
      this.copied_str = str;
      this.copied = this.bounds;
      this.show_copied();
      inp.value = str;
      inp.select();
      inp.focus();
      setTimeout( function() {
        inp.blur();
      },0);

    } else {
      this.startEditing("");
    }
  }

  else if( this.is_editing() ) {
    var that = this;
    setTimeout(function() {
      that.formula.value = that.input.value;
      that.calculateInputWidth();
    },0);
  }
};

HN.Layout.Selection.prototype.select_next = function(key, event)
{
  this.deselect();
  this.bounds_ord = this.order_bounds();

  var cell = this.cell,
         b = this.bounds_ord;

  if( key == HN.Keys.ENTER ) {
    if( cell.y < b.y2 ) {
      this.set_cell(cell.y+1, cell.x, HN.DOWN);
    } else if( cell.y == b.y2 && cell.x == b.x2 ) {
      this.has_scrolled = true;
      this.set_cell(b.y1, b.x1);
    } else {
      this.has_scrolled = true;
      this.set_cell(b.y1, cell.x+1, HN.RIGHT);
    }
  }
  else if( key == HN.Keys.TAB )
  {
    event.preventDefault();

    if( cell.x < b.x2 ) {
      this.set_cell(cell.y, cell.x+1, HN.RIGHT);
    } else if( cell.y == b.y2 && cell.x == b.x2 ) {
      this.has_scrolled = true;
      this.set_cell(b.y1, b.y2);
    } else {
      this.has_scrolled = true;
      this.set_cell(cell.y+1, b.x1, HN.DOWN);
    }
  }
  this.state = HN.States.SELECTED_RANGE;
  this.rebuild_input({});
};

HN.Layout.Selection.prototype.select = function(key, event)
{
  if( event.shiftKey || key == HN.Keys.TAB ) {
    event.preventDefault();
  }

  var b = this.bounds,
   cell = this.cell;

  if( !event.shiftKey ) {
    this.deselect();

    if( key == HN.Keys.ENTER || key == HN.Keys.DOWN ) {
      this.set_cell(cell.y+1, cell.x, HN.DOWN);
    } else if( key == HN.Keys.UP && cell.y > 1 ) {
      this.set_cell(cell.y-1, cell.x, HN.UP);
    } else if( key == HN.Keys.LEFT && cell.x > 1 ) {
      this.set_cell(cell.y, cell.x-1, HN.LEFT);
    } else if( key == HN.Keys.TAB || key == HN.Keys.RIGHT ) {
      this.set_cell(cell.y, cell.x+1, HN.RIGHT);
    }

    b.x1 = b.x2 = this.cell.x;
    b.y2 = b.y1 = this.cell.y;

    this.rebuild_input({});
    this.state = HN.States.SELECTED_CELL;
  } else {
    this.deselect(true);
    this.bounds = this.order_bounds();
    b = this.bounds;


    if( key == HN.Keys.DOWN && cell.y == b.y2 && b.y1 != b.y2 ) {
      b.y1++;
      this.scrollIntoView(b.y1, b.x1, HN.DOWN);
    } else if( key == HN.Keys.DOWN ) {
      b.y2++;
      this.scrollIntoView(b.y2, b.x2, HN.DOWN);
    } if( key == HN.Keys.UP && cell.y == b.y1 && b.y2 > 1 ) {
      b.y2--;
      this.scrollIntoView(b.y2, b.x2, HN.UP);
    } else if( key == HN.Keys.UP && b.y1 > 1 ) {
      b.y1--;
      this.scrollIntoView(b.y1, b.x1, HN.UP);
    } if( key == HN.Keys.RIGHT && cell.x == b.x2 && b.x1 != b.x2 ) {
      b.x1++;
      this.scrollIntoView(b.y1, b.x1, HN.RIGHT);
    } else if( key == HN.Keys.RIGHT ) {
      b.x2++;
      this.scrollIntoView(b.y2, b.x2, HN.RIGHT);
    } if( key == HN.Keys.LEFT && this.cell.x == b.x1 && b.x2 > 1 ) {
      b.x2--;
      this.scrollIntoView(b.y2, b.x2, HN.LEFT);
    } else if( key == HN.Keys.LEFT && b.x1 > 1 ) {
      b.x1--;
      this.scrollIntoView(b.y1, b.x1, HN.LEFT);
    }

    this.has_scrolled = true;
    this.rebuild_input({});
    this.state = HN.States.SELECTED_RANGE;
  }
  this.show_selection();
};

HN.Layout.Selection.prototype.is_in_grid = function(auto, e)
{
  var axis = auto.args.axis, dir = auto.args.forward,
         x = e.clientX, y = e.clientY;

  return (axis == Y && dir  && !(y > this.offset.y + this.layout.size.y))
      || (axis == X && dir  && !(x > this.offset.x + this.layout.size.x))
      || (axis == Y && !dir && !(y < this.offset.y))
      || (axis == X && !dir && !(x < this.offset.x));
};

HN.Layout.Selection.prototype.auto_scroll_coords = function(e)
{
  if( e.clientY > this.offset.y + this.layout.size.y) {
    return {forward:true, axis:Y};
  } else if( e.clientY < this.offset.y ) {
    return {forward:false, axis:Y};
  } else if( e.clientX > this.offset.x + this.layout.size.x) {
    return {forward:true, axis:X};
  } else if( e.clientX < this.offset.x ) {
    return {forward:false, axis:X};
  }
  return false;
};

HN.Layout.Selection.prototype.dragdown = function(e)
{
  var that = this,
     sheet = this.layout.s,
     style = this.layout.inner.style,
      auto = {timer:null, axis:null},
      offy = this.offset.y + parseInt(style.top, 10),
      offx = this.offset.x + parseInt(style.left, 10);

  this.state = HN.States.DRAGGING;
  this.has_scrolled = true;

  var auto_scroll = function() {
    that.layout.scrollBy1(auto.args.forward, auto.args.axis);
    offy = that.offset.y + parseInt(style.top, 10),
    offx = that.offset.x + parseInt(style.left, 10);
    var y = that.layout.get_new_offset(auto.mouse.y - offy, Y);
    var x = that.layout.get_new_offset(auto.mouse.x - offx, X);
    that.show_drag_selection({x:x.index, y:y.index});
  };

  var stop_auto_scroll = function() {
    window.clearInterval(auto.timer);
    auto = {timer:null, axis:null, forward:null, mouse:null};
  };

  var mousemove = function( e ) {
    if( !auto.timer ) {
      var args = that.auto_scroll_coords(e);
      if( args ) {
        auto = { args:  args,
                 mouse: {y:e.clientY, x:e.clientX},
                 timer: setInterval(auto_scroll, 30) };
      } else {
        var y = that.layout.get_new_offset(e.clientY - offy, Y);
        var x = that.layout.get_new_offset(e.clientX - offx, X);
        that.show_drag_selection({x:x.index, y:y.index});
      }
    }
    else if( that.is_in_grid(auto, e) ) {
      stop_auto_scroll();
    }
  };

  var mouseup = function(e) {

    if( auto.timer ) {
      window.clearInterval(auto.timer);
    }

    HN.Util.removeEvent(document, "mouseup", mouseup);
    HN.Util.removeEvent(document, "mousemove", mousemove);

    HN.Callbacks.drag(that.order_bounds(that.bounds), that.dragpos);

    that.bounds.y1 = Math.min(that.bounds.y1, that.dragpos.y1);
    that.bounds.x1 = Math.min(that.bounds.x1, that.dragpos.x1);
    that.bounds.y2 = Math.max(that.bounds.y2, that.dragpos.y2);
    that.bounds.x2 = Math.max(that.bounds.x2, that.dragpos.x2);

    that.state = (that.bounds.y1 == that.bounds.y2
               && that.bounds.x1 == that.bounds.x2)
          ? HN.States.SELECTED_CELL
          : HN.States.SELECTED_RANGE;

    that.show_selection();
    that.dragged.style.display = "none";
  };

  HN.Util.addEvent(document, "mouseup", mouseup);
  HN.Util.addEvent(document, "mousemove", mousemove);
};

HN.Layout.Selection.prototype.mousedown = function(e)
{
  if( e.target == this.wrapper && e.button == 2 ) {
    return;
  }

  e.preventDefault();

  var that = this,
     sheet = this.layout.s,
     style = this.layout.inner.style,
      auto = {},
      offy = this.offset.y + parseInt(this.layout.inner.style.top, 10),
      offx = this.offset.x + parseInt(this.layout.inner.style.left, 10);

  this.has_scrolled = true;

  if( this.last_click && new Date().getTime() - 250 < this.last_click ) {
    this.deselect();
    this.rebuild_input({"useformula":true});
    this.input.focus();
    this.state = HN.States.EDIT_FULL_CELL;
    return;
  } else {
    this.last_click = new Date().getTime();
  }

  if( this.in_formula_bar ) {
    this.in_formula_bar = false;
    if( e.target == this.input ) {
      return;
    }
  }

  if( e.shiftKey ) {
    var y = that.layout.get_new_offset(e.clientY - offy, Y);
    var x = that.layout.get_new_offset(e.clientX - offx, X);
    that.bounds.y2 = y.index;
    that.bounds.x2 = x.index;
    that.show_selection();
    return;
  }

  if(this.is_editing() || this.is_selected()) {
    this.deselect();
    this.state = (this.is_cell()) ? HN.States.SELECTED_CELL
      : HN.States.SELECTED_RANGE;
  }

  if( !this.is_selected() ) {
    return;
  }

  var auto_scroll = function() {
    that.layout.scrollBy1(auto.args.forward, auto.args.axis);
    offy = that.offset.y + parseInt(style.top, 10),
    offx = that.offset.x + parseInt(style.left, 10);
    var y = that.layout.get_new_offset(auto.mouse.y - offy, Y);
    var x = that.layout.get_new_offset(auto.mouse.x - offx, X);
    that.bounds.y2 = y.index;
    that.bounds.x2 = x.index;
    that.show_selection();
  };

  var stop_auto_scroll = function() {
    window.clearInterval(auto.timer);
    auto = {timer:null, axis:null, forward:null, mouse:null};
  };

  var y = this.layout.get_new_offset(e.clientY - offy, Y);
  var x = this.layout.get_new_offset(e.clientX - offx, X);

  this.bounds = {y1:y.index, x1:x.index, y2:y.index, x2:x.index};
  this.set_cell(y.index, x.index);
  this.rebuild_input({});

  var mousemove = function( e ) {
    if( !auto.timer ) {
      var args = that.auto_scroll_coords(e);
      if( args ) {
        auto = { args:  args,
                 mouse: {y:e.clientY, x:e.clientX},
                 timer: setInterval(auto_scroll, 30) };
      } else {

        var y2 = that.layout.get_new_offset(e.clientY - offy, Y);
        var x2 = that.layout.get_new_offset(e.clientX - offx, X);

        that.bounds.y2 = y2.index;
        that.bounds.x2 = x2.index;

        that.show_selection();
      }
    }
    else if( that.is_in_grid(auto, e) ) {
      stop_auto_scroll();
    }
  };

  var mouseup = function( e ) {

    if( auto.timer ) {
      window.clearInterval(auto.timer);
    }

    that.mouse   = {x1:0, y1: 0, x2: 0, y2: 0};
    HN.Util.removeEvent(document, "mouseup", mouseup);
    HN.Util.removeEvent(document, "mousemove", mousemove);
    that.state = (that.bounds.y1 == that.bounds.y2
                  && that.bounds.x1 == that.bounds.x2)
      ? HN.States.SELECTED_CELL : HN.States.SELECTED_RANGE;
  };

  HN.Util.addEvent(document, "mouseup", mouseup);
  HN.Util.addEvent(document, "mousemove", mousemove);

  this.show_selection();
};

HN.Layout.Selection.prototype.show_drag_selection = function(drag)
{
  var pos = function(d,b) {
    if( d.x >= b.x1 && d.x <= b.x2 && d.y > b.y2 )
      return [b.y2+1, b.x1, d.y, b.x2];
    if( d.x >= b.x1 && d.x <= b.x2 && d.y < b.y1 )
      return [d.y, b.x1, b.y1-1, b.x2];
    if( d.y >= b.y1 && d.y <= b.y2 && d.x > b.x2 )
      return [b.y1, b.x2+1, b.y2, d.x];
    if( d.y >= b.y1 && d.y <= b.y2 && d.x < b.x1 )
      return [b.y1, d.x, b.y2, b.x1-1];
    return null;
  };

  var npos = pos(drag, this.order_bounds(this.bounds));

  if( npos ) {
    this.dragpos.y1 = npos[0];
    this.dragpos.x1 = npos[1];
    this.dragpos.y2 = npos[2];
    this.dragpos.x2 = npos[3];
    this.draw_drag(npos[0], npos[1], npos[2], npos[3]);
  }
};

HN.Layout.Selection.prototype.draw_drag = function(top, left, bottom, right)
{
  var cell = this.layout.s.cell_offset(top, left);

  for(var x = left, width = 0; x < right+1; x++) {
    width += this.layout.s.col_width(x);
  }
  for(var y = top, height = 0; y < bottom+1; y++) {
    height += this.layout.s.row_height(y);
  }

  this.dragged.style.top  = (cell.top-1) + "px";
  this.dragged.style.left = (cell.left-1) + "px";
  this.dragged.style.width  = (width-2) + "px";
  this.dragged.style.height = (height-2) + "px";
  this.dragged.style.display = "block";
};

HN.Layout.Selection.prototype.order_bounds = function()
{
  var tmp = {};

  if( this.bounds.x1 <= this.bounds.x2 ) {
    tmp.x1 = this.bounds.x1;
    tmp.x2 = this.bounds.x2;
  } else {
    tmp.x1 = this.bounds.x2;
    tmp.x2 = this.bounds.x1;
  }
  if( this.bounds.y1 <= this.bounds.y2 ) {
    tmp.y1 = this.bounds.y1;
    tmp.y2 = this.bounds.y2;
  } else {
    tmp.y1 = this.bounds.y2;
    tmp.y2 = this.bounds.y1;
  }

  return tmp;
};

HN.Layout.Selection.prototype.show_selection = function()
{
  var sheet = this.layout.s,
    wrapper = this.wrapper,
     bounds = this.order_bounds(this.bounds),
     coords = sheet.cell_offset(bounds.y1, bounds.x1);

  for( var x = bounds.x1, width = 0; x < bounds.x2+1; x++ ) {
    width += sheet.col_width(x);
  }
  for( var y = bounds.y1, height = 0; y < bounds.y2+1; y++ ) {
    height += sheet.row_height(y);
  }

  HN.Util.id("rowselection").style.top = (coords.top-1) + "px";
  HN.Util.id("rowselection").style.height = (height) + "px";

  HN.Util.id("colselection").style.left = (coords.left-1) + "px";
  HN.Util.id("colselection").style.width = (width) + "px";

  wrapper.style.left   = (coords.left-1) + "px";
  wrapper.style.top    = (coords.top-1)  + "px";
  wrapper.style.width  = (width-2)   + "px";
  wrapper.style.height = (height-2)  + "px";
  wrapper.style.display = "block";
};


HN.Layout.Selection.prototype.show_copied = function()
{
  var sheet = this.layout.s,
  wrapper = HN.Util.id("copied"),
     bounds = this.order_bounds(this.copied),
     coords = sheet.cell_offset(bounds.y1, bounds.x1);

  for( var x = bounds.x1, width = 0; x < bounds.x2+1; x++ ) {
    width += sheet.col_width(x);
  }
  for( var y = bounds.y1, height = 0; y < bounds.y2+1; y++ ) {
    height += sheet.row_height(y);
  }

  wrapper.style.left   = (coords.left-1) + "px";
  wrapper.style.top    = (coords.top-1)  + "px";
  wrapper.style.width  = (width-2)   + "px";
  wrapper.style.height = (height-2)  + "px";
  wrapper.style.display = "block";
};

HN.Layout.Selection.prototype.is_editing = function()
{
  return this.is_full_edit() || this.is_light_edit();
};

HN.Layout.Selection.prototype.is_full_edit = function()
{
  return this.state == HN.States.EDIT_FULL_CELL;
};

HN.Layout.Selection.prototype.is_light_edit = function()
{
  return this.state == HN.States.EDIT_LIGHT_CELL
      || this.state == HN.States.EDIT_LIGHT_RANGE;
};

HN.Layout.Selection.prototype.is_cell = function()
{
  return this.state == HN.States.EDIT_FULL_CELL
      || this.state == HN.States.EDIT_LIGHT_CELL
      || this.state == HN.States.SELECTED_CELL;
};

HN.Layout.Selection.prototype.is_selected = function()
{
  return this.state == HN.States.SELECTED_RANGE
      || this.state == HN.States.SELECTED_CELL;
};

/**
 * @class HN.ToolBar
 * provides functionality for the styles toolbar
 */
HN.ToolBar = function(layout)
{
  var that = this;

  this.layout = layout;
  this.drop_menus();
  $("#menu").filemenu();

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

/**
 * Given format key, set the format for current selection
 */
HN.ToolBar.prototype.setFormat = function(value)
{
  var formats = {
    "fmt_0":"General",
    "fmt_1":"000",
    "fmt_2":"000.00",
    "fmt_3":"\"$\"##0_);[Red](\"$\"#,##0)",
    "fmt_4":"\"$\"##0_);[Red](\"$\"#,##0)",
    "fmt_5":"\"$\"##0_);[Red](\"$\"#,##0)",
    "fmt_6":"\"$\"##0_);[Red](\"$\"#,##0)",
    "fmt_7":"\"$\"##0_);[Red](\"$\"#,##0)",
    "fmt_8":"\"$\"##0_);[Red](\"$\"#,##0)",
    "fmt_9":"\"$\"##0_);[Red](\"$\"#,##0)",
    "fmt_10":"\"$\"##0_);[Red](\"$\"#,##0)",
    "fmt_11":"d/m/yyyy",
    "fmt_12":"\"$\"##0_);[Red](\"$\"#,##0)",
    "fmt_13":"\"$\"##0_);[Red](\"$\"#,##0)"
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

/**
 * Setup the drop down menus (colors / alignment / font etc)
 */
HN.ToolBar.prototype.drop_menus = function()
{
  var el = $(this.layout.p.root).find(".expand");

  $.each(el, function()
  {
    var menu = $(this).children("div"),
        that = $(this);

    $(this).click( function(e)
    {
      var tmp = that.hasClass("active"),
        click = function(e)
      {
        var is_parent = $(e.target).parents().andSelf().filter(
          function(){ return this===that[0]; } ).length;

        if( is_parent == 0 ) {
          $(document).unbind(".temp");
          that.removeClass("active");
          menu.hide();
        }
      };

      $(document).unbind(".temp");
      el.removeClass("active").children("div").hide();

      if( !tmp ) {
        $(document).bind("click.temp",click);
        menu.show();
        that.addClass("active");
      }
    });
  });
};

