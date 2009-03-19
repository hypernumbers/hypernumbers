HN.Callbacks = {};
HN.Callbacks.post = function(cell, value)
{
  var fullurl = document.location.pathname
    + HN.Util.coord_to_ref(cell)+"?attr";

  if(value == "") {
    $.post(fullurl, "{\"clear\": \"contents\"}",null,"json");
  } else {
    var val = HN.Util.addslashes(value);
    $.post(fullurl,"{\"set\" : {\"formula\":\""+val+"\"}}",null,"json");
  }
};

HN.Callbacks.delete_range = function(range)
{
  var fullurl = document.location.pathname
    + HN.Util.coord_to_ref({"x":range.x1, "y":range.y1})+":"
    + HN.Util.coord_to_ref({"x":range.x2, "y":range.y2})
    + "?attr";

  $.post(fullurl, "{\"clear\": \"contents\"}",null,"json");
};

HN.Callbacks.style = function(range, style, val)
{
  $.post( HN.Util.range_to_url(range),
         "{\"set\" : {\""+style+"\":\""+val+"\"}}",null,"json");
};

HN.Callbacks.format = function(range, val)
{
  $.post( HN.Util.range_to_url(range),
         "{\"set\" : {\"format\":\""+HN.Util.addslashes(val)+"\"}}",null,"json");
};

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

HN.States = {};
HN.States.DRAGGING = 1;
HN.States.SELECTED_RANGE = 2;
HN.States.SELECTED_CELL = 3;
HN.States.EDIT_LIGHT_CELL = 4;
HN.States.EDIT_LIGHT_RANGE = 5;
HN.States.EDIT_FULL_CELL = 6;

/**
 * @class HN.Layout
 *
 */
HN.ToolBar = function(parent)
{
  var that = this;

  this.parent = parent;
  this.drop_menus();

  var get_val = function(style, obj) {
    if(style == "background-color" || style == "color") {
      return "#"+obj.getAttribute("id").split("_")[1];
    } else if(style == "text-align" || style == "font-family") {
      return obj.getAttribute("id").split("_")[1];
    } else if(style == "font-size") {
      return obj.getAttribute("id").split("_")[1]+"px";
    } else if(style == "font-weight") {
      return "bold";
    } else if(style == "font-style") {
      return "italic";
    } else if(style == "text-decoration") {
      return "line-through";
    }

    return false;
  };

  var add_events = function(id, style) {
    HN.Util.addEvent(HN.Util.id(id), "mousedown", function(e) {
      if(e.target.nodeName== "A") {
        that.set_style(style, get_val(style, e.target));
      }
    });
  };

  add_events("bgcolors","background-color");
  add_events("fontcolors","color");
  add_events("bold","font-weight");
  add_events("italic","font-style");
  add_events("strike","text-decoration");
  add_events("aligntext","text-align");
  add_events("ffamily","font-family");
  add_events("sizes","font-size");

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

  HN.Util.addEvent(HN.Util.id("formats"), "mousedown", function(e) {
    if(e.target.nodeName== "A") {
      that.set_format(formats[e.target.getAttribute("id")]);
    }
  });

  $("#menu").filemenu();
};

HN.ToolBar.prototype.set_format = function(value)
{
  var sel = this.parent.layout.selection,
    state = sel.state;

  if(   state ==  HN.States.SELECTED_CELL
     || state == HN.States.SELECTED_RANGE)
  {
    HN.Callbacks.format(sel.bounds, value);
  }
};

HN.ToolBar.prototype.set_style = function(style, value)
{
  var sel = this.parent.layout.selection,
    state = sel.state;

  var vals = {
    "font-weight" : {"search" : "font-weight:bold", "value":"normal"},
    "font-style" : {"search" : "font-style:italic", "value":"normal"},
    "text-decoration" : {"search" : "text-decoration:line-through", "value":"none"}
  };

  if(   state ==  HN.States.SELECTED_CELL
     || state == HN.States.SELECTED_RANGE)
  {
    if(typeof vals[style] != "undefined") {
      var cell  = this.parent.layout.selection.cell;
      var sheet = this.parent.cur_sheet();
      var attr  = sheet.cell(cell.y, cell.x);
      if(attr && sheet.data.styles[attr.style].match(vals[style].search)) {
        value = vals[style].value;
      }
    }
    HN.Callbacks.style(sel.bounds, style, value);
  }
};

HN.ToolBar.prototype.drop_menus = function()
{
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

  this.selection = new HN.Layout.Selection(this);
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

  if(this.panes) {

    this.panes.rows = Math.ceil(this.size.y / 20 / 8)+1;
    this.panes.cols = Math.ceil(this.size.x / 80 / 5)+1;
    this.panes.refresh();

    // Bit of a hack, just pretend to scroll up / down
    // to reposition the scroll bar
    this.do_scroll_pressed(true, Y);
    this.do_scroll_pressed(false, Y);
    this.do_scroll_pressed(true, X);
    this.do_scroll_pressed(false, X);
  }
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
  var f = function(index) {
    return (xy.str == "y")
      ? sheet.row_height(index)
      : sheet.col_width(index);
  };

  var i = 1, total = 0;

  while(true) {
    total += f(i);

    if(total > y)
      return [i, total-f(i)];

    ++i;
  };
};

HN.Layout.prototype.do_scroll_pressed = function(forward, xy)
{
  var offset = (forward)
    ? -this.offset_from_i(xy, this.s.offset[xy.str])
    :  this.offset_from_i(xy, this.s.offset[xy.str]-1);

  var  sheet_size = this.s[xy.dimension] - this.size[xy.str],
         pane_pos = parseInt(this.inner.style[xy.coord]) + offset,
       scroll_pos = -(Math.round((pane_pos / sheet_size)
                                 * this.scrollmax[xy.str]));

  // Adjust scrollbar position
  if(scroll_pos >= this.scrollmax[xy.str])
  {
    // If scrollbar has hit of area, extend area size
    this.s.extend(xy, 2);
    this.calc_scrollbar(xy);

    scroll_pos = this.scrollmax[xy.str];
    sheet_size = this.s[xy.dimension] - this.size[xy.str];
  }
  else if(scroll_pos < 0) {
    scroll_pos = 0;
  }

  // Move View Pane and store new offset
  this.inner.style[xy.coord] = pane_pos + "px";
  this.axis_dom[xy.str].style[xy.coord] = pane_pos + "px";
  this.s.offset[xy.str] += (forward) ? 1 : -1;
  this.scrollbar[xy.str].style[xy.coord] = scroll_pos + "px";

  // Wrap panes if needed
  this.check_pane_bounds(this.panes, offset, xy, forward);
};


HN.Layout.prototype.scroll_pressed = function(event, forward, xy)
{
  var    that = this,
        sheet = this.s,
       button = event.target,
        timer;

  var mouseup = function() {
    button.className = "";
    HN.Util.removeEvent(document, "mouseup", mouseup);
    clearInterval(timer);
  };

  button.className = "active";
  HN.Util.addEvent(document, "mouseup", mouseup);

  // Continue scrolling while mouse is pressed
  timer = setInterval(function() {
    if(forward || sheet.offset[xy.str] > 1) {
      that.do_scroll_pressed(forward, xy);
    }
  }, 70);
};

HN.Layout.prototype.check_pane_bounds = function(panes, offset, xy, upleft)
{
  var axis = xy.str, start = axis+"1", end = axis+"2";

  panes.bounds[start] += offset;
  panes.bounds[end]   += offset;

  if(Math.abs(offset) > panes.p[0][0][xy.dimension]) {
    var n = Math.floor(Math.abs(offset) / panes.p[0][0][xy.dimension]);
    for(var x = 0; x < n; x++) {
      panes["shift_"+axis](upleft);
    }
  }

  if(upleft && this.size[xy.str] > panes.bounds[end]) {
    panes["shift_"+axis](upleft);
  } else if(!upleft && panes.bounds[start] > -40) {
    panes["shift_"+axis](upleft);
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

  this.build_cells({});

  for(var i = 0; i < this.p[0].length; i++) {
    this.bounds.x2 += this.p[0][i].width;
  }
};

HN.Layout.Panes.prototype.update_cell = function(y, x, val)
{
};

HN.Layout.Panes.prototype.refresh = function()
{
  this.bounds.x2 = this.bounds.x1;
  this.bounds.y2 = this.bounds.y1;

  var p = this.p[0][0];
  var row = p.row;
  var col = p.col;
  var top = parseInt(p.div.style.top);
  var left = parseInt(p.div.style.left);

  for(var y = 0; y < this.rows; y++) {
    for(var x = 0; x < this.cols; x++) {
      if(this.p[y] && this.p[y][x]) {
        this.inner.removeChild(this.p[y][x].div);
      }
    }
  }

  this.build_cells({"row":row, "col":col, "top":top, "left":left});

  var bottom = this.p[this.p.length-1][0].row + HN.Layout.Pane.DEF_ROWS;
  var right  = this.p[0][this.p[0].length-1].col + HN.Layout.Pane.DEF_COLS;

  this.axis.y.innerHTML = "";
  this.axis.x.innerHTML = "";
  this.draw_index(Y, true, false, top, 0, row, bottom);
  this.draw_index(X, true, false, 0, left, col, right);

  for(var i = 0; i < this.p[0].length; i++) {
    this.bounds.x2 += this.p[0][i].width;
  }
};

HN.Layout.Panes.prototype.update_col_width = function(x)
{
  this.refresh();
};

HN.Layout.Panes.prototype.get_col = function(x)
{
  var       p = this.p,
   bottomleft = p[0][0],
     topright = p[p.length-1][p[0].length-1];

  // (0 indexes)
  x -= bottomleft.col;

  if(x < 0 || x > (topright.col + HN.Layout.Pane.DEF_COLS)) {
    return false;
  }

  return [Math.floor( x / HN.Layout.Pane.DEF_COLS ),
          x % HN.Layout.Pane.DEF_COLS];
};

HN.Layout.Panes.prototype.get_cell = function(y, x)
{
  var obj = this.get_pane(y, x);

  if(!obj) {
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

  if(   y < 0
     || x < 0
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
     rownum = tmp,
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
    pane.row = rownum;
    pane.update_view();

    if(x == 0)
    {
      newheight = pane.height;

      top = up
        ? parseInt(p[y-1][0].div.style.top) + p[y-1][0].height
        : parseInt(p[1][0].div.style.top) - newheight;
    }

    pane.div.style.top = top+"px" ;
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
     colnum = tmp,
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
    //pane.div = orig.cloneNode(true);
    pane.col = colnum;
    pane.update_view();

    if(y == 0)
    {
      newwidth = pane.width;

      lleft = left
        ? parseInt(p[0][x-1].div.style.left) + p[0][x-1].width
        : parseInt(p[0][1].div.style.left) - newwidth;
    }

    pane.div.style.left = lleft+"px" ;
    //orig.parentNode.replaceChild(pane.div,orig);
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

HN.Layout.Panes.prototype.build_cells = function(o)
{
  o.row = (typeof o.row == 'undefined') ? 0 : o.row-1;
  o.col = (typeof o.col == 'undefined') ? 0 : o.col-1;

  o.top  = (typeof o.top == 'undefined') ? 0 : o.top;
  o.left = (typeof o.left == 'undefined') ? 0 : o.left;

  var top = o.top;

  for(var y = 0; y < this.rows; y++)
  {
    var row = (y * 8) + 1,
       left = o.left;

    this.p[y] = [];

    for(var x = 0; x < this.cols; x++)
    {
      this.p[y][x] = new HN.Layout.Pane(this.s, row+o.row, (x*5)+1+o.col);
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

  for(var y = row; y < row + HN.Layout.Pane.DEF_ROWS; y++)
  {
    var height = this.s.row_height(y),
          left = 0;

    twidth  = 0;
    theight += height;

    cells[cells.length] = '<div>';

    for(var x = col; x < col+HN.Layout.Pane.DEF_COLS; x++)
    {
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

  for(var y = 0; y < rows; y++)
  {
    var el  = children[y],
     height = this.s.row_height(y+this.row),
     nchild = el.childNodes,
       cols = nchild.length,
      count = y+this.row;

    totalw = 0;

    for(var x = 0; x < cols; x++)
    {
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

  this.last_click = null;
  this.cell_dom   = null;
  this.zindex     = null;

  this.in_formula_bar = false;
  this.cell        = {"x":1, "y":1};
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

  HN.Util.addEvent(document, "keydown", function(e) {
    that.keydown(e);
  });

  HN.Util.addEvent(that.formula, "focus", function(e) {
    that.in_formula_bar = true;
  });

  this.show_selection();
  this.rebuild_input({});
};

HN.Layout.Selection.prototype.deselect = function(hide)
{
  if(this.is_light_edit()) {
    this.cell_dom.innerHTML = this.input.value;
    HN.Callbacks.post(this.cell, this.input.value);
  } else if (this.is_full_edit()) {
    var c  = this.layout.s.cell(this.cell.y, this.cell.x);
    var formula = (c && c.formula) || "";

    if(this.input.value != formula) {
      this.cell_dom.innerHTML = this.input.value;
      HN.Callbacks.post(this.cell, this.input.value);
    } else {
      this.cell_dom.innerHTML = (c && c.value) || "";
    }
  } else {
    this.cell_dom.innerHTML = this.input.value;
  }

  if(!hide) {
    this.hide_input();
  }
};

HN.Layout.Selection.prototype.hide_input = function()
{
  this.in_formula_bar = false;
  this.input.blur();
  this.input.value = "";
  this.input.style.display = "none";
};

HN.Layout.Selection.prototype.calculate_input_width = function()
{
  this.hidden_input.innerHTML = this.input.value;
  var width = parseInt(this.input.style.width);

  if((this.hidden_input.offsetWidth)+5 > width) {
    var w = this.layout.s.col_width(this.cell.x + this.input_width);
    if(this.input_width == 0) {
      w -= 2;
    }
    this.input.style.width = ((width+w))+ "px";
    this.input_width++;
    this.calculate_input_width();
  }
};

HN.Layout.Selection.prototype.rebuild_input = function(o)
{
  o.useformula = (typeof o.useformula == 'undefined') ? false : o.useformula;
  this.input.blur();
  this.formula.blur();

  this.cell_dom = this.layout.panes.get_cell(this.cell.y, this.cell.x);
  var c  = this.layout.s.cell(this.cell.y, this.cell.x);
  var formula = (c && c.formula) || "";
  var style = (c && c.style && this.layout.s.data.styles[c.style]) || "";
  var value   = (o.useformula && formula) || (c && c.value) || "";
  var cell = this.layout.s.cell_offset(this.cell.y, this.cell.x);

  this.input.setAttribute("style", style);
  this.input.style.top  = (cell.top-1)  + "px";
  this.input.style.left = (cell.left+0) + "px";

  this.input_width = 0;
  this.input.value = value;
  this.formula.value = formula;
  this.cell_dom.innerHTML = "";
  this.input.style.width = "0px";
  this.input.style.borderColor = "blue";
  this.input.style.display = "block";

  this.calculate_input_width();
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
    }, 60);
  };

  HN.Util.addEvent(document, "keyup", keyup);
  timeout = window.setTimeout(set_interval, 300);
};

HN.Layout.Selection.prototype.do_keydown = function(e)
{
  if(this.in_formula_bar) {
    if(e.keyCode == HN.Keys.ENTER || e.keyCode == HN.Keys.TAB) {
      this.select(e.keyCode, e);
    } else {
      if(this.is_selected()) {
        this.state = (this.state == HN.States.SELECTED_RANGE)
          ? HN.States.EDIT_LIGHT_RANGE
          : HN.States.EDIT_LIGHT_CELL;
      }
      var that = this;
      setTimeout(function() {
                   that.input.value = that.formula.value;
                 },0);
    }
  } else if((e.keyCode == HN.Keys.ENTER || e.keyCode == HN.Keys.TAB)
     && (this.is_editing() || this.is_selected()))
  {
    if(this.is_cell()) {
      this.select(e.keyCode, e);
    } else {
      this.select_next(e.keyCode, e);
    }
  }

  else if((this.is_light_edit() || this.is_selected())
    && (e.keyCode == HN.Keys.UP || e.keyCode == HN.Keys.DOWN
    || e.keyCode == HN.Keys.LEFT || e.keyCode == HN.Keys.RIGHT))
  {
    this.select(e.keyCode, e);
  }

  else if(this.is_selected() && (e.keyCode == HN.Keys.DELETE
                                || e.keyCode == HN.Keys.BACKSPACE))
  {
    this.bounds = this.order_bounds();
    HN.Callbacks.delete_range(this.bounds);
  }

  else if(this.is_selected() && !(  e.keyCode == HN.Keys.SHIFT
                                 || e.keyCode == HN.Keys.TAB
                                 || e.keyCode == HN.Keys.CTRL))
  {
    //if(e.ctrlKey) {
      /*if(e.keyCode == 86) {
        this.input.style.width = "0px";
        this.input.style.display = "block";
        this.input.style.borderColor = "transparent";
        this.input.style.background = "transparent";
        this.input.focus();
        var inp = this.input;
        setTimeout(function() {
                     console.log([inp.value]);
                   },0);
      }*/
    //} else {
      this.input.style.width = "0px";
      this.input_width = 0;
      this.input.value = "";
      this.input.focus();
      this.calculate_input_width();

      this.state = (this.state == HN.States.SELECTED_RANGE)
        ? HN.States.EDIT_LIGHT_RANGE
        : HN.States.EDIT_LIGHT_CELL;
    //}
  }

  else if(this.is_editing()) {
    var that = this;
    setTimeout(function() {
      that.formula.value = that.input.value;
      that.calculate_input_width();
    },0);
  }
};

HN.Layout.Selection.prototype.select_next = function(key, event)
{
  this.deselect();
  this.bounds_ord = this.order_bounds();

  if(key == HN.Keys.ENTER) {
    if(this.cell.y < this.bounds_ord.y2) {
      this.cell.y++;
    } else if(this.cell.y == this.bounds_ord.y2
              && this.cell.x == this.bounds_ord.x2) {
      this.cell.x = this.bounds_ord.x1;
      this.cell.y = this.bounds_ord.y1;
    } else {
      this.cell.x++;
      this.cell.y = this.bounds_ord.y1;
    }
  } else if(key == HN.Keys.TAB) {
    event.preventDefault();

    if(this.cell.x < this.bounds_ord.x2) {
      this.cell.x++;
    } else if(this.cell.y == this.bounds_ord.y2
              && this.cell.x == this.bounds_ord.x2) {
      this.cell.x = this.bounds_ord.x1;
      this.cell.y = this.bounds_ord.y1;
    } else {
      this.cell.y++;
      this.cell.x = this.bounds_ord.x1;
    }
  }
  this.state = HN.States.SELECTED_RANGE;
  this.rebuild_input({});
};

HN.Layout.Selection.prototype.select = function(key, event)
{
  if(event.shiftKey || key == HN.Keys.TAB) {
    event.preventDefault();
  }

  var b = this.bounds;

  if(!event.shiftKey)
  {
    this.deselect();

    if(key == HN.Keys.ENTER || key == HN.Keys.DOWN) {
      b.y1 = b.y2 = ++this.cell.y;
      b.x2 = b.x1 = this.cell.x;
    } else if(key == HN.Keys.UP && this.cell.y > 1) {
      b.y1 = b.y2 = --this.cell.y;
      b.x2 = b.x1 = this.cell.x;
    } else if(key == HN.Keys.LEFT && this.cell.x > 1) {
      b.x1 = b.x2 = --this.cell.x;
      b.y2 = b.y1 = this.cell.y;
    } else if(key == HN.Keys.TAB || key == HN.Keys.RIGHT) {
      b.x1 = b.x2 = ++this.cell.x;
      b.y2 = b.y1 = this.cell.y;
    }

    this.cell = {"x":this.bounds.x1,"y":this.bounds.y1};
    this.rebuild_input({});
    this.state = HN.States.SELECTED_CELL;

  } else {
    this.deselect(true);
    this.bounds = this.order_bounds();
    b = this.bounds;

    if(key == HN.Keys.DOWN && this.cell.y == b.y2 && b.y1 != b.y2) {
      b.y1++;
    } else if(key == HN.Keys.DOWN) {
      b.y2++;
    } if(key == HN.Keys.UP && this.cell.y == b.y1 && b.y2 > 1) {
      b.y2--;
    } else if(key == HN.Keys.UP && b.y1 > 1) {
      b.y1--;

    } if(key == HN.Keys.RIGHT && this.cell.x == b.x2 && b.x1 != b.x2) {
      b.x1++;
    } else if(key == HN.Keys.RIGHT) {
      b.x2++;
    } if(key == HN.Keys.LEFT && this.cell.x == b.x1 && b.x2 > 1) {
      b.x2--;
    } else if(key == HN.Keys.LEFT && b.x1 > 1) {
      b.x1--;
    }
    this.rebuild_input({});
    this.state = HN.States.SELECTED_RANGE;
  }
  this.show_selection();
};

HN.Layout.Selection.prototype.dragdown = function(e)
{
  var that = this,
     sheet = this.layout.s,
      offy = this.offset.y + parseInt(this.layout.inner.style.top),
      offx = this.offset.x + parseInt(this.layout.inner.style.left);

  this.state = HN.States.DRAGGING;

  var mousemove = function(e) {
    var y = that.layout.get_new_offset(sheet, e.clientY - offy, Y);
    var x = that.layout.get_new_offset(sheet, e.clientX - offx, X);
    that.show_drag_selection({"x":x[0], "y":y[0]});
  };

  var mouseup = function(e) {
    HN.Util.removeEvent(document, "mouseup", mouseup);
    HN.Util.removeEvent(document, "mousemove", mousemove);

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
  var that = this,
     sheet = this.layout.s,
      offy = this.offset.y + parseInt(this.layout.inner.style.top),
      offx = this.offset.x + parseInt(this.layout.inner.style.left);

  if(this.last_click && new Date().getTime() - 250 < this.last_click) {
    this.deselect();
    this.rebuild_input({"useformula":true});
    this.input.focus();
    this.state = HN.States.EDIT_FULL_CELL;
    this.last_click = null;
    return;
  } else {
    this.last_click = new Date().getTime();
  }

  if(this.in_formula_bar) {
    this.in_formula_bar = false;

    if(e.target == this.input) {
      return;
    }
  }

  if(this.is_editing() || this.is_selected()) {
    this.deselect();
    this.state = (this.is_cell()) ? HN.States.SELECTED_CELL
      : HN.States.SELECTED_RANGE;
  }

  if(this.is_selected())
  {
    var y = this.layout.get_new_offset(sheet, e.clientY - offy, Y);
    var x = this.layout.get_new_offset(sheet, e.clientX - offx, X);

    this.bounds = {"y1":y[0], "x1":x[0], "y2":y[0], "x2":x[0]};
    this.cell   = {"x":x[0],"y":y[0]};
    this.rebuild_input({});

    var mousemove = function(e)
    {
      var y2 = that.layout.get_new_offset(sheet, e.clientY - offy, Y);
      var x2 = that.layout.get_new_offset(sheet, e.clientX - offx, X);

      that.bounds.y2 = y2[0];
      that.bounds.x2 = x2[0];

      that.show_selection();
    };

    var mouseup = function(e) {
      that.mouse   = {"x1":0, "y1": 0, "x2": 0, "y2": 0};
      HN.Util.removeEvent(document, "mouseup", mouseup);
      HN.Util.removeEvent(document, "mousemove", mousemove);
      that.state = (that.bounds.y1 == that.bounds.y2
                 && that.bounds.x1 == that.bounds.x2)
          ? HN.States.SELECTED_CELL
          : HN.States.SELECTED_RANGE;
    };

    HN.Util.addEvent(document, "mouseup", mouseup);
    HN.Util.addEvent(document, "mousemove", mousemove);

    this.show_selection();
  }
  e.preventDefault();
};

HN.Layout.Selection.prototype.show_drag_selection = function(drag)
{
  var pos = function(d,b)
  {
    if(d.x >= b.x1 && d.x <= b.x2 && d.y > b.y2)
      return [b.y2+1,b.x1,d.y,b.x2];
    if(d.x >= b.x1 && d.x <= b.x2 && d.y < b.y1)
      return [d.y,b.x1,b.y1-1,b.x2];
    if(d.y >= b.y1 && d.y <= b.y2 && d.x > b.x2)
      return [b.y1,b.x2+1,b.y2,d.x];
    if(d.y >= b.y1 && d.y <= b.y2 && d.x < b.x1)
      return [b.y1,d.x,b.y2,b.x1-1];

    return null;
  };

  var npos = pos(drag, this.order_bounds(this.bounds));

  if(npos != null) {
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

  if(this.bounds.x1 <= this.bounds.x2) {
    tmp.x1 = this.bounds.x1;
    tmp.x2 = this.bounds.x2;
  } else {
    tmp.x1 = this.bounds.x2;
    tmp.x2 = this.bounds.x1;
  }
  if(this.bounds.y1 <= this.bounds.y2) {
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

  for(var x = bounds.x1, width = 0; x < bounds.x2+1; x++) {
    width += sheet.col_width(x);
  }
  for(var y = bounds.y1, height = 0; y < bounds.y2+1; y++) {
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

