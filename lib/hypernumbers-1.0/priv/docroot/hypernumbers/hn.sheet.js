var HN = {};

var Y = {"str":"y", "coord":"top", "dimension":"height"};
var X = {"str":"x", "coord":"left", "dimension":"width"};
//
//  SpreadSheet Container
//
HN.SpreadSheet = function(root)
{
  this.root   = HN.Util.id(root);
  this.sheet  = 0;
  this.sheets = [];
  this.layout = null;

  this.add_sheet("Sheet1");
  this.layout = new HN.Layout(this,name,this.cur_sheet());
};

HN.SpreadSheet.prototype.cur_sheet = function()
{
  return this.sheets[this.sheet];
};

HN.SpreadSheet.prototype.add_sheet = function(name)
{
  this.sheets[this.sheets.length] = new HN.Sheet(name);
};

//
//  Sheet Container
//
HN.Sheet = function(name)
{
  this.name   = name;
  this.offset = {"x":1,"y":1};
  this.max    = {"x":26, "y":50};
  this.height = 0;
  this.width  = 0;

  this.cells  = [];
  this.rows   = [];
  this.cols   = [];

  this.load_data();
  this.calc_size();
};

HN.Sheet.CELL_WIDTH  = 80;
HN.Sheet.CELL_HEIGHT = 20;

HN.Sheet.prototype.load_data = function()
{
  for(y = 0; y < 100; y++)
  {
    this.cells[y] = [];
    for(x = 0; x < 26; x++)
    {
      this.cells[y][x] = {value:HN.Util.to_b26(x)+y, formula:"=1+1"};
    }
  }

  this.rows[2] = {"height":40};
  this.rows[6] = {"height":60};
  this.rows[20] = {"height":60};
  this.cols[3] = {"width":10};
  this.cols[7] = {"width":120};
};

HN.Sheet.prototype.extend = function(axis, amount)
{
  this.max[axis.str] += amount;
  this.calc_size();
};

HN.Sheet.prototype.calc_size = function()
{
  var height = 0, count = 0;
  for (var i in this.rows) {
    if (String(i >>> 0) == i && i >>> 0 != 0xffffffff) {
      count++;
      height += this.rows[i].height;
    }
  }
  this.height = height + ((this.max.y - count) * 20);

  var width = 0, c = 0;
  for (var x in this.cols) {
    if (String(x >>> 0) == x && x >>> 0 != 0xffffffff) {
      c++;
      width += this.cols[x].width;
    }
  }
  this.width = width + ((this.max.x - c) * 80);
};

HN.Sheet.prototype.row_height = function(i)
{
  return this.rows[i] && this.rows[i].height
    || HN.Sheet.CELL_HEIGHT;
};

HN.Sheet.prototype.col_width = function(i)
{
  return this.cols[i] && this.cols[i].width
    || HN.Sheet.CELL_WIDTH;
};

HN.Sheet.prototype.cell = function(y,x)
{
  return this.cells[y] && this.cells[y][x]
    || {value:""};
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

  this.indexfun   = {
    "x":function(x){ return HN.Util.to_b26(x);},
    "y":function(x){ return x;}
  };

  this.scrollbar = {
    "x":HN.Util.id("bottom-bar"),
    "y":HN.Util.id("right-bar")
  };

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

  this.panes = new HN.Layout.Panes(this.inner, this.s, rows, cols);
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

    // Move viewpane, scrollbar, store offset
    bar.style[xy.coord]        = scroll_pos + "px";
    that.inner.style[xy.coord] = -index[1]+"px";
    that.s.offset[xy.str]      = index[0];

    // Wrap Panes if needed
    that.check_pane_bounds(that.panes, diff, xy);
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
    sheet.offset[xy.str] += (forward) ? 1 : -1;
    that.scrollbar[xy.str].style[xy.coord] = scroll_pos + "px";

    // Wrap panes if needed
    that.check_pane_bounds(that.panes, offset, xy);
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

HN.Layout.prototype.check_pane_bounds = function(panes, offset, xy)
{
  var start = xy.str+"1", end = xy.str+"2";

  panes.bounds[start] += offset;
  panes.bounds[end]   += offset;

  if(this.size[xy.str] > panes.bounds[end]) {
    this.move_pane_bounds(xy, panes, true);
  } else if(panes.bounds[start] > -40) {
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
HN.Layout.Panes = function(inner, sheet, rows, cols)
{
  this.inner  = inner;
  this.p      = [];
  this.rows   = rows;
  this.cols   = cols;
  this.s      = sheet;
  this.bounds = {"x1":0,"y1":0,"x2":0,"y2":0};

  this.add_row();

  for(var i = 0; i < this.p[0].length; i++) {
    this.bounds.x2 += this.p[0][i].width;
  }
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
        ? p[y-1][0].top + p[y-1][0].height
        : p[1][0].top - newheight;
    }

    pane.top = top;
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
        ? p[0][x-1].left + p[0][x-1].width
        : p[0][1].left - newwidth;
    }
    pane.left = lleft;
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
};

HN.Layout.Panes.prototype.add_row = function()
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
      this.p[y][x].set_position(top, left);
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
  this.top  = null;
  this.left = null;

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

      var style = "border:1px solid #FFF;border-bottom:1px solid #ddd;"
        + "border-right:1px solid #ddd;";

      cells[cells.length] = '<div style="';
      cells[cells.length] = 'position:absolute;';
      cells[cells.length] = 'z-index:'+(x+y)+';';
      cells[cells.length] = 'background:#FFFFFF;';
      cells[cells.length] = 'left:'+left+'px;';
      cells[cells.length] = 'top:'+top+'px;';
      cells[cells.length] = 'height:'+(height-4)+'px;';
      cells[cells.length] = 'width:'+(width-4)+'px; ';
      cells[cells.length] = style;
      cells[cells.length] = '">'; //class="cell">';
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

      styl.top      = total+"px";
      styl.height   = (height-2)+"px";
      tmp.innerHTML = this.s.cell(count, x+this.col).value;
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

      styl.left      = total+"px";
      styl.width    = (width-2)+"px";
      tmp.innerHTML = this.s.cell(row, x+this.col).value;

      total += width;
    }
  }
  this.width = total;

  return total;
};


HN.Layout.Pane.prototype.set_position = function(top, left)
{
  this.top  = top;
  this.left = left;

  this.div.style.top  = top + "px";
  this.div.style.left = left + "px";
};

/**
 * @class HN.Util
 *
 */
HN.Util = {};

HN.Util.addEvent = function(obj, type, fn)
{
  if ( obj.attachEvent ) {
    obj['e'+type+fn] = fn;
    obj[type+fn] = function(){obj['e'+type+fn]( window.event );};
    obj.attachEvent( 'on'+type, obj[type+fn] );
  } else
    obj.addEventListener( type, fn, false );
};

HN.Util.removeEvent = function(obj, type, fn)
{
  if ( obj.detachEvent ) {
    obj.detachEvent( 'on'+type, obj[type+fn] );
    obj[type+fn] = null;
  } else
    obj.removeEventListener( type, fn, false );
};

HN.Util.id = function(id)
{
  return document.getElementById(id);
};

HN.Util.y_pos = function(obj)
{
  var top = 0;
  if (obj.offsetParent) {
    do {
	  top += obj.offsetTop;
    } while ((obj = obj.offsetParent));
  }
  return top;
};

HN.Util.x_pos = function(obj)
{
  var left = 0;
  if (obj.offsetParent) {
    do {
	  top += obj.offsetLeft;
    } while ((obj = obj.offsetParent));
  }
  return top;
};

HN.Util.to_b26 = function(cell)
{
  return (--cell < 26)
    ? String.fromCharCode(cell+65)
    : HN.Util.to_b26(Math.floor(cell / 26)) + HN.Util.to_b26(cell%26+1);
};

HN.Util.from_b26 = function(cell)
{
  return cell.charCodeAt(0)-96;
};