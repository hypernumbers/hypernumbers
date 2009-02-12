/**
 * @class HN.Layout
 *
 */
HN.ToolBar = function(parent)
{
  this.parent = parent;

  var el = $(this.parent.root).find(".expand");

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

        if(is_parent == 0) {
          $(document).unbind(".temp");
          that.removeClass("active");
          menu.hide();
        }
      };

      $(document).unbind(".temp");
      el.removeClass("active").children("div").hide();

      if(!tmp) {
        $(document).bind("click.temp",click);
        menu.show();
        that.addClass("active");
      }
    });
  });

  $("#menu").filemenu();
};

/**
 * @class HN.Layout
 *
 */
HN.Layout = function(parent,name,sheet)
{
  this.p = parent;
  this.s = sheet;

  this.inner = HN.Util.id("scroller");

  this.size = {"x":0, "y":0};
  this.scrollmax = {"x":0, "y":0};

  this.axis_dom = {
    "x":HN.Util.id("columns"),
    "y":HN.Util.id("rows")
  };

  this.scrollbar = {
    "x":HN.Util.id("bottom-bar"),
    "y":HN.Util.id("right-bar")
  };

  this.toolbar = new HN.ToolBar(parent);
  this.selection = new HN.Layout.Selection(this);

  this.inner.style.top  = "0px";
  this.inner.style.left = "0px";
  this.scrollbar.y.style.top  = "0px";
  this.scrollbar.x.style.left = "0px";

  var that = this;

  var add_scroll_button = function(id, mag, axis)
  {
    HN.Util.addEvent(HN.Util.id(id), "mousedown", function(e) {
      that.scroll_pressed(e, mag, axis);
    });
  };

  var add_scroll_bar = function(id, axis)
  {
    HN.Util.addEvent(HN.Util.id(id), "mousedown", function(e) {
      that.scrollbar_drag(e, axis);
    });
  };

  add_scroll_button("down",  true,  Y);
  add_scroll_button("up",    false, Y);
  add_scroll_button("right", true,  X);
  add_scroll_button("left",  false, X);

  add_scroll_bar("right-bar",  Y);
  add_scroll_bar("bottom-bar", X);

  HN.Util.addEvent(window, "resize", function() {
    that.window_resize();
  });

  this.window_resize();

  var rows = Math.ceil(this.size.y / 20 / 8)+1;
  var cols = Math.ceil(this.size.x / 80 / 5)+1;

  this.panes = new HN.Layout.Panes(this.axis_dom, this.inner, this.s,
                                   rows, cols);
};

HN.Layout.prototype.window_resize = function()
{
  // changing top to force table to resize,
  this.inner.style.top = (parseInt(this.inner.style.top)+1)+"px";
  this.size.y = this.inner.clientHeight;
  this.size.x = this.inner.clientWidth;
  this.inner.style.top = (parseInt(this.inner.style.top)-1)+"px";

  this.calc_scrollbar(Y);
  this.calc_scrollbar(X);
};

HN.Layout.prototype.calc_scrollbar = function(xy)
{
  var tmp = Math.round( this.size[xy.str] / (this.s.max[xy.str] / 10));

  this.scrollbar[xy.str].style[xy.dimension] = tmp + "px";
  this.scrollmax[xy.str] = this.size[xy.str] - tmp - 32;
};

HN.Layout.prototype.scrollbar_drag = function(event, xy)
{
  var      that = this,
          sheet = this.s,
            bar = this.scrollbar[xy.str],
     scroll_max = this.scrollmax[xy.str],
          mprop = (xy.str == "y") ? "clientY" : "clientX",
     sheet_size = sheet[xy.dimension] - this.size[xy.str],
         offset = event[mprop] - parseInt(bar.style[xy.coord]);

  var scroll = function(event)
  {
    var scroll_pos = event[mprop] - offset;

    // Adjust scrollbar position to no exceed bounds
    if(scroll_pos < 0) {
      scroll_pos = 0;
    } else if( scroll_pos > scroll_max) {
      scroll_pos = scroll_max;
    }

    var sheet_pos = sheet_size * (scroll_pos / scroll_max),
            index = that.get_new_offset(sheet, sheet_pos, xy),
             diff = -(parseInt(that.inner.style[xy.coord]) + index[1]);

    bar.style[xy.coord] = scroll_pos + "px";

    if(diff == 0) {
      return;
    }

    // Move viewpane, scrollbar, store offset
    that.axis_dom[xy.str].style[xy.coord] = -index[1] + "px";
    that.inner.style[xy.coord] = -index[1]+"px";
    that.s.offset[xy.str]      = index[0];

    // Wrap Panes if needed
    that.check_pane_bounds(that.panes, diff, xy, diff < 0);
  };

  var onmouseup = function() {
    document.body.style.cursor = '';
    bar.className = "";
    HN.Util.removeEvent(document, "mouseup", onmouseup);
    HN.Util.removeEvent(document, "mousemove", scroll);
  };

  bar.className = "active";
  document.body.style.cursor = 'default';

  HN.Util.addEvent(document, "mouseup", onmouseup);
  HN.Util.addEvent(document, "mousemove", scroll);

  event.preventDefault();
  return false;
};

HN.Layout.prototype.get_new_offset = function(sheet, y, xy)
{
  var f = function(index)
  {
    return (xy.str == "y")
      ? sheet.row_height(index)
      : sheet.col_width(index);
  };

  var i = 1, total = 0;

  while(true)
  {
    total += f(i);

    if(total > y)
      return [i, total-f(i)];

    ++i;
  };
};

HN.Layout.prototype.scroll_pressed = function(event, forward, xy)
{
  var    that = this,
        sheet = this.s,
        vpane = this.inner,
       button = event.target,
    pane_size = this.size[xy.str],
   sheet_size = sheet[xy.dimension] - pane_size,
        timer;

  var scroll = function()
  {
    var offset = (forward)
      ? -that.offset_from_i(xy, sheet.offset[xy.str])
      :  that.offset_from_i(xy, sheet.offset[xy.str]-1);

    var pane_pos = parseInt(vpane.style[xy.coord]) + offset,
      scroll_pos = -(Math.round((pane_pos / sheet_size)
        * that.scrollmax[xy.str]));

    // Adjust scrollbar position
    if(scroll_pos >= that.scrollmax[xy.str])
    {
      // If scrollbar has hit of area, extend area size
      sheet.extend(xy, 2);
      that.calc_scrollbar(xy);

      scroll_pos = that.scrollmax[xy.str];
      sheet_size = sheet[xy.dimension] - that.size[xy.str];
    }
    else if(scroll_pos < 0) {
      scroll_pos = 0;
    }

    // Move View Pane and store new offset
    vpane.style[xy.coord] = pane_pos + "px";
    that.axis_dom[xy.str].style[xy.coord] = pane_pos + "px";
    sheet.offset[xy.str] += (forward) ? 1 : -1;
    that.scrollbar[xy.str].style[xy.coord] = scroll_pos + "px";

    // Wrap panes if needed
    that.check_pane_bounds(that.panes, offset, xy, forward);
  };

  var mouseup = function() {
    button.className = "";
    HN.Util.removeEvent(document, "mouseup", mouseup);
    clearInterval(timer);
  };

  button.className = "active";
  HN.Util.addEvent(document, "mouseup", mouseup);

  // Continue scrolling while mouse is pressed
  timer = setInterval(function() {
    if(forward || sheet.offset[xy.str] > 1) { scroll();  }
  }, 70);
};

HN.Layout.prototype.move_pane_bounds = function(xy, panes, upleft)
{
    if(xy.str == "y") {
      panes.shift_y(upleft);
    }else {
      panes.shift_x(upleft);
    }
};

HN.Layout.prototype.check_pane_bounds = function(panes, offset, xy, upleft)
{
  var start = xy.str+"1", end = xy.str+"2";

  panes.bounds[start] += offset;
  panes.bounds[end]   += offset;

  if(upleft && this.size[xy.str] > panes.bounds[end]) {
    this.move_pane_bounds(xy, panes, true);
  } else if(!upleft && panes.bounds[start] > -40) {
    this.move_pane_bounds(xy, panes, false);
  }
};

HN.Layout.prototype.offset_from_i = function(axis, index)
{
  return (axis.str == "y")
    ? this.s.row_height(index)
    : this.s.col_width(index);
};


/**
 * @class HN.Layout.Panes
 *
 */
HN.Layout.Panes = function(axis, inner, sheet, rows, cols)
{
  this.axis   = axis;
  this.inner  = inner;
  this.p      = [];
  this.rows   = rows;
  this.cols   = cols;
  this.s      = sheet;
  this.bounds = {"x1":0,"y1":0,"x2":0,"y2":0};
  this.index  = {"x":[], "y":[]};

  this.draw_index(Y, true, false, 0, 0, 1,
                  (this.rows * HN.Layout.Pane.DEF_ROWS) + 1);
  this.draw_index(X, true, false, 0, 0, 1,
                  (this.cols * HN.Layout.Pane.DEF_COLS) + 1);

  this.build_cells();

  for(var i = 0; i < this.p[0].length; i++) {
    this.bounds.x2 += this.p[0][i].width;
  }
};

HN.Layout.Panes.prototype.update_cell = function(y, x, val)
{
  var       p = this.p,
   bottomleft = p[0][0],
     topright = p[p.length-1][p[0].length-1];

  // If the cell is not viewable, no need to update
  if(   y < parseInt(bottomleft.div.style.top)
     || x < parseInt(bottomleft.div.style.left)
     || x > (topright.left + HN.Layout.Pane.DEF_COLS)
     || y > (topright.top + HN.Layout.Pane.DEF_ROWS)) {
    return;
  }

  var paney = Math.floor( y / HN.Layout.Pane.DEF_ROWS ),
        iny = y % HN.Layout.Pane.DEF_ROWS - 1,
      panex = Math.floor( x / HN.Layout.Pane.DEF_COLS ),
        inx = x % HN.Layout.Pane.DEF_COLS - 1;

  p[paney][panex].div.childNodes[iny].childNodes[inx].innerHTML = val;
};

HN.Layout.Panes.prototype.shift_y = function(up)
{
  var p = this.p;

  if(up)
  {
    var   y = p.length-1,
     rownum = p[y][0].row+8,
        row = p.shift();

    p[y] = [];
  }
  else
  {
    var   y = 0,
        tmp = p[y][0].row-8,
     rownum = (tmp < 0) ? 0 : tmp,
        row = p[p.length-1];

    p.length -= 1;
    p.unshift([]);
  }

  var oldheight = row[0].height,
            top = null,
      newheight = null,
            len = row.length;

  for(var x = 0; x < len; x++)
  {
    p[y][x] = row[x];

    var pane = p[y][x],
        orig = pane.div;

    pane = row[x];
    pane.div = orig.cloneNode(true);
    pane.move_row(rownum);

    if(x == 0)
    {
      newheight = pane.height;

      top = up
        ? parseInt(p[y-1][0].div.style.top) + p[y-1][0].height
        : parseInt(p[1][0].div.style.top) - newheight;
    }

    pane.div.style.top = top+"px" ;
    orig.parentNode.replaceChild(pane.div,orig);
  }

  if(up) {
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

  if(left)
  {
    var   x = p[0].length-1,
     colnum = p[0][x].col+5,
        col = [];

    for(var i = 0; i < this.p.length; i++) {
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
     colnum = (tmp < 0) ? 0 : tmp,
        col = [];

     for(var i = 0; i < this.p.length; i++) {
       col.push(p[i][plen]);
       p[i].length -= 1;
       p[i].unshift([]);
     }
  }

  var oldwidth = col[0].width,
         lleft = null,
      newwidth = null,
           len = col.length;

  for(var y = 0; y < len; y++)
  {
    p[y][x] = col[y];

    var pane = p[y][x];
    var orig = pane.div;

    pane = col[y];
    pane.div = orig.cloneNode(true);
    pane.move_col(colnum);

    if(y == 0)
    {
      newwidth = pane.width;

      lleft = left
        ? parseInt(p[0][x-1].div.style.left) + p[0][x-1].width
        : parseInt(p[0][1].div.style.left) - newwidth;
    }

    pane.div.style.left = lleft+"px" ;
    orig.parentNode.replaceChild(pane.div,orig);
  }

  if(left) {
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

  for(var i = start; i < end; i++)
  {
    if(del) {
      if(append) {
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
    index.push(style+'">'+xy.to_index(i)+'</div>');

    if(xy.str == "y") {
      top  += val[0];
    } else {
      left += val[1];
    }
  }

  if(append) {
     axis.innerHTML += index.join("");
  } else {
     axis.innerHTML = index.join("") + axis.innerHTML;
  }
};

HN.Layout.Panes.prototype.build_cells = function()
{
  var top = 0;

  for(var y = 0; y < this.rows; y++)
  {
    var row = (y * 8) + 1,
       left = 0;

    this.p[y] = [];

    for(var x = 0; x < this.cols; x++)
    {
      this.p[y][x] = new HN.Layout.Pane(this.s, row, (x*5)+1);
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

  var twidth  = 0,
      theight = 0,
        cells = [],
          top = 0;

  for(var y = row; y < row + HN.Layout.Pane.DEF_ROWS; y++)
  {
    var height = this.s.row_height(y),
          left = 0;

    twidth = 0;
    theight += height;

    cells[cells.length] = '<div>';

    for(var x = col; x < col+HN.Layout.Pane.DEF_COLS; x++)
    {
      var width = this.s.col_width(x);
      twidth += width;

      var style = "border:1px solid #ddd;";

      cells[cells.length] = '<div style="';
      cells[cells.length] = 'position:absolute;';
      cells[cells.length] = 'z-index:'+(x+y)+';';
      cells[cells.length] = 'background:#FFFFFF;';
      cells[cells.length] = 'left:'+(left-1)+'px;';
      cells[cells.length] = 'top:'+(top-1)+'px;';
      cells[cells.length] = 'height:'+(height-1)+'px;';
      cells[cells.length] = 'width:'+(width-1)+'px; ';
      cells[cells.length] = style;
      cells[cells.length] = '" rel="cell-'+y+'-'+x+'">';
      cells[cells.length] = (this.s.cell(y,x).value)+'</div>';

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

HN.Layout.Pane.prototype.move_row = function(top)
{
  this.row = top;

  var children = this.div.childNodes,
          rows = children.length;

  for(var total = 0, y = 0; y < rows; y++)
  {
    var el  = children[y],
     height = this.s.row_height(y+this.row),
     nchild = el.childNodes,
       cols = nchild.length,
      count = y+top;

    for(var x = 0; x < cols; x++)
    {
      var tmp = nchild[x],
         styl = tmp.style;

      styl.top      = (total-1)+"px";
      styl.height   = (height-1)+"px";
      tmp.innerHTML = this.s.cell(count, x+this.col).value;
      tmp.setAttribute("rel", "cell-"+count+"-"+(x+this.col));
    }

    total += height;
  }
  this.height = total;

  return total;
};

HN.Layout.Pane.prototype.move_col = function(left)
{
  this.col = left;

  var children = this.div.childNodes,
          rows = children.length,
           top = 0;

  for(var y = 0; y < rows; y++)
  {
    var el  = children[y],
     nchild = el.childNodes,
       cols = nchild.length,
    row = y+this.row,
    total = 0;

    for(var x = 0; x < cols; x++)
    {
      var tmp = nchild[x],
         styl = tmp.style,
        width = this.s.col_width(x+this.col);

      styl.left     = (total-1)+"px";
      styl.width    = (width-1)+"px";
      tmp.innerHTML = this.s.cell(row, x+this.col).value;
      tmp.setAttribute("rel", "cell-"+row+"-"+(x+this.col));

      total += width;
    }
  }
  this.width = total;

  return total;
};

HN.Layout.Selection = function(layout)
{
  this.state   = this.INIT;

  this.layout  = layout;
  this.input   = HN.Util.id("input");
  this.wrapper = HN.Util.id("selection");
  this.current = HN.Util.id("current");
  this.dragged = HN.Util.id("dragged");

  this.coord   = {"x":0, "y":0};
  this.diff    = {"x":0, "y":0};
  this.mouse   = {"x1":0, "y1": 0, "x2": 0, "y2": 0};
  this.bounds  = {"x1":0, "y1": 0, "x2": 0, "y2": 0};

  this.offset  = {
    "y":HN.Util.y_pos(this.layout.inner),
    "x":HN.Util.x_pos(this.layout.inner)
  };

  var that = this;

  HN.Util.addEvent(layout.inner, "mousedown", function(e) {
    that.mousedown(e);
  });
};

HN.Layout.Selection.INIT = 0;
HN.Layout.Selection.SELECTED = 1;
HN.Layout.Selection.SELECTED_CELL = 2;
HN.Layout.Selection.DRAGGING = 3;

HN.Layout.Selection.prototype.mousedown = function(e)
{
  var that = this;

  if(this.STATE == this.INIT || this.STATE == this.SELECTED)
  {
    this.mouse.y1 = this.mouse.y2 = e.clientY - this.offset.y;
    this.mouse.x1 = this.mouse.x2 = e.clientX - this.offset.x;

    var y = this.layout.get_new_offset(this.layout.s, this.mouse.y1, Y);
    var x = this.layout.get_new_offset(this.layout.s, this.mouse.x1, X);

    this.coord.x = x[1];
    this.coord.y = y[1];

    this.diff.x  = this.mouse.x1 - this.coord.x;
    this.diff.y  = this.mouse.y1 - this.coord.y;

    //var cell = e.target.getAttribute("rel").split("-");
    //this.bounds.y1 = parseInt(cell[1]);
    //this.bounds.x1 = parseInt(cell[2]);
    //this.bounds.y2 = parseInt(cell[1]);
    //this.bounds.x2 = parseInt(cell[2]);

    this.STATE = this.START_DRAGGING;

    var mousemove = function(e) {
      //this.current.style.width  = e.target.style.width;
      //this.current.style.height = e.target.style.height;
      that.mouse.y2 = e.clientY - that.offset.y;
      that.mouse.x2 = e.clientX - that.offset.x;
      that.show_selection();

      /*var cell = e.target.getAttribute("rel");
      if(cell == null)
        return;

      cell = cell.split("-");

      if(cell[0] == "cell")
      {
        that.bounds.y2 = parseInt(cell[1]);
        that.bounds.x2 = parseInt(cell[2]);
      }*/
    };

    var mouseup = function(e) {
      that.mouse   = {"x1":0, "y1": 0, "x2": 0, "y2": 0};
      HN.Util.removeEvent(document, "mouseup", mouseup);
      HN.Util.removeEvent(document, "mousemove", mousemove);
      this.STATE = this.SELECTED;
    };

    HN.Util.addEvent(document, "mouseup", mouseup);
    HN.Util.addEvent(document, "mousemove", mousemove);

    //this.mouse.y1 = this.mouse.y2 = e.clientY;
    //this.mouse.x1 = this.mouse.x2 = e.clientX;

    //this.coord.x = parseInt(e.target.style.left) +
    //  parseInt(e.target.parentNode.parentNode.style.left);
    //this.coord.y = parseInt(e.target.style.top) +
    //  parseInt(e.target.parentNode.parentNode.style.top);

    //this.current.style.width  = e.target.style.width;
    //this.current.style.height = e.target.style.height;

    //this.dragged.style.width  = e.target.style.width;
    //this.dragged.style.height = e.target.style.height;
    this.show_selection();
  }

  e.preventDefault();
  return false;
};

HN.Layout.Selection.prototype.show_selection = function()
{
  var y = this.layout.get_new_offset(this.layout.s, this.mouse.y2, Y);
  var x = this.layout.get_new_offset(this.layout.s, this.mouse.x2, X);

/*  if(this.mouse.x1 <= this.mouse.x2) {
    this.wrapper.style.left  = this.coord.x + "px";
    this.wrapper.style.width = ((this.mouse.x2 - this.mouse.x1) +
      this.diff.x) + "px";
  }

  if(this.mouse.y1 <= this.mouse.y2) {
    this.wrapper.style.top    = this.coord.y + "px";
    this.wrapper.style.height = ((this.mouse.y2 - this.mouse.y1) +
      this.diff.y) + "px";
  }

  this.wrapper.style.display = "block";
*/

/*
  if(this.coord.x1 <= x[1]) {

    this.wrapper.style.left = (this.coord.x - 1) + "px";

    for(var x = this.bounds.x1, width = 0; x < this.bounds.x2 + 1; x++) {
      width += this.layout.s.col_width(x);
    }
    this.wrapper.style.width = width + "px";
  }

  if(this.bounds.y1 <= this.bounds.y2) {
    this.wrapper.style.top = (this.coord.y - 1) + "px";

    for(var y = this.bounds.y1, height = 0; y < this.bounds.y2 + 1; y++) {
      height += this.layout.s.row_height(y);
    }
    this.wrapper.style.height = height + "px";
  }
*/
};