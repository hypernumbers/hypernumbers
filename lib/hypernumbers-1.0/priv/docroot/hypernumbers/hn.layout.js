/**
 * @class HN.Layout
 * Handles the main layout controls, the grid / axis / formula bar and toolbar
 */
HN.Layout = function(sheet)
{
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
      that.selection.state = HN.States.COPY_URL;
      HN.Util.select(e.target);
      return true;
    } else {
      return false;
    }
  });
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