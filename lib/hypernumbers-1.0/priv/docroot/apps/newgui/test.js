var HN = {};

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
  this.sheets[this.sheets.length] = new HN.Sheet("Sheet1");
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

HN.Sheet.CELL_WIDTH  = 80;
HN.Sheet.CELL_HEIGHT = 20;

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
  this.inner.style.top  = "0px";
  this.inner.style.left = "0px";

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

  var that = this;

  HN.Util.addEvent(HN.Util.id("down"), "mousedown", function(e) {
    that.scroll_y_pressed(e, true);
  });

  HN.Util.addEvent(HN.Util.id("up"), "mousedown", function(e) {
    that.scroll_y_pressed(e, false);
  });

  HN.Util.addEvent(HN.Util.id("right"), "mousedown", function(e) {
    that.scroll_x_pressed(e, true);
  });

  HN.Util.addEvent(HN.Util.id("left"), "mousedown", function(e) {
    that.scroll_x_pressed(e, false);
  });

  HN.Util.addEvent(HN.Util.id("right-bar"), "mousedown", function(e) {
    that.scrollbar_right_pressed(e);
  });

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
  this.inner.style.top = (parseInt(this.inner.style.top)+1)+"px";
  this.size.y = this.inner.clientHeight;
  this.size.x = this.inner.clientWidth;
  this.inner.style.top = (parseInt(this.inner.style.top)-1)+"px";

  this.calc_scrollbar_y();
  this.calc_scrollbar_x();
};

HN.Layout.prototype.calc_scrollbar_y = function()
{
  var height = Math.round(this.size.y/(this.s.max.y/10));
  this.scrollbar.y.style.height = height+"px";
  this.scrollmax.y = this.size.y - height - 32;
};

HN.Layout.prototype.calc_scrollbar_x = function()
{
  var width = Math.round(this.size.x/(this.s.max.x/10));
  this.scrollbar.x.style.width = width+"px";
  this.scrollmax.x = this.size.x - width - 32;
};

HN.Layout.prototype.scrollbar_right_pressed = function(event)
{
  var that = this,
     panes = this.panes,
       bar = this.scrollbar.y,
       max = this.scrollmax.y,
    // Available scroll height
    sheight = this.size.y - 30 - parseInt(that.scrollbar.y.style.height),
    // Height of Scrollable Content
    scrollable = this.s.height - this.size.y,
    // normalise mouse position
    mouse_offset = event.clientY - (parseInt(bar.style.top) || 0);

  var scroll = function(event)
  {
    var pos = event.clientY - mouse_offset;

    if(pos < 0) {
      pos = 0;
    } else if( pos > max) {
      pos = max;
    }

    var    y = scrollable * (pos / sheight),
       index = that.get_row_at_y(y),
        diff = -(parseInt(that.inner.style.top) + index[1]);

    bar.style.top = pos+"px";

    that.inner.style.top = -index[1]+"px";
    that.s.offset.y = index[0];

    that.move_pane_bounds(panes, diff);
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

HN.Layout.prototype.get_row_at_y = function(y)
{
  var i = 1, total = 0;

  while(true)
  {
    total += this.s.row_height(i);

    if(total > y)
      return [i, total-this.s.row_height(i)];

    ++i;
  };
};

HN.Layout.prototype.scroll_y_pressed = function(event, down)
{
  var    that = this,
       button = event.target,
   scrollable = this.s.height - this.size.y,
      sheight = this.size.y - 30 - parseInt(this.scrollbar.y.style.height),
        timer;

  var scroll = function()
  {
    var top = parseInt(that.inner.style.top),
     offset = (down)
      ? -that.s.row_height(that.s.offset.y)
      : that.s.row_height(that.s.offset.y-1);

    that.inner.style.top = top + offset + "px";
    that.s.offset.y += (down) ? 1 : -1;

    var pos = -(Math.round((top / scrollable) * sheight));

    if(pos > that.scrollmax.y)
    {
      that.s.max.y += 2;

      that.s.calc_size();
      that.calc_scrollbar_y();

      pos = that.scrollmax.y;

      scrollable = that.s.height - that.size.y;
      sheight = that.size.y - 30 - parseInt(that.scrollbar.y.style.height);
    }
    else if(pos < 0) {
      pos = 0;
    }

    that.scrollbar.y.style.top = pos+"px";
    that.move_pane_bounds_y(that.panes, offset);
  };

  var onmouseup = function() {
    button.className = "";
    HN.Util.removeEvent(document, "mouseup", onmouseup);
    clearInterval(timer);
  };

  button.className = "active";
  HN.Util.addEvent(document, "mouseup", onmouseup);

  timer = setInterval( function() {
    if(down || that.s.offset.y > 1) {
      scroll();
    }
  }, 70);
};

HN.Layout.prototype.scroll_x_pressed = function(event, right)
{
  var    that = this,
       button = event.target,
   scrollable = this.s.width - this.size.x,
       swidth = this.size.x - 30 - parseInt(this.scrollbar.x.style.width),
        timer;

  var scroll = function()
  {
    var left = parseInt(that.inner.style.left),
      offset = (right)
      ? -that.s.col_width(that.s.offset.x)
      : that.s.col_width(that.s.offset.x-1);

    that.inner.style.left = left + offset + "px";
    that.s.offset.x += (right) ? 1 : -1;

    var pos = -(Math.round((left / scrollable) * swidth));

    if(pos > that.scrollmax.x)
    {
      that.s.max.x += 2;

      that.s.calc_size();
      that.calc_scrollbar_x();

      pos = that.scrollmax.x;

      scrollable = that.s.width - that.size.x;
      sheight = that.size.x - 30 - parseInt(that.scrollbar.x.style.width);
    }
    else if(pos < 0) {
      pos = 0;
    }

    that.scrollbar.x.style.left = pos+"px";
    that.move_pane_bounds_x(that.panes, offset);
  };

  var onmouseup = function() {
    button.className = "";
    HN.Util.removeEvent(document, "mouseup", onmouseup);
    clearInterval(timer);
  };

  button.className = "active";
  HN.Util.addEvent(document, "mouseup", onmouseup);

  timer = setInterval( function() {
    if(right || that.s.offset.x > 1) {
      scroll();
    }
  }, 70);
};

HN.Layout.prototype.move_pane_bounds_y = function(panes, offset)
{
  panes.bounds.y1 += offset;
  panes.bounds.y2 += offset;

  if(this.size.y > panes.bounds.y2) {
    panes.shift_y(true);
  } else if(panes.bounds.y1 > -40) {
    panes.shift_y(false);
  }
};

HN.Layout.prototype.move_pane_bounds_x = function(panes, offset)
{
  panes.bounds.x1 += offset;
  panes.bounds.x2 += offset;

  if(this.size.x > panes.bounds.x2) {
    panes.shift_x(true);
  } else if(panes.bounds.x1 > -40) {
    panes.shift_x(false);
  }
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
     colnum = p[0][x].row+5,
        col = [];

    for(var i = 0; i < this.p.length; i++) {
      col.push(p[i].shift());
    }
  }
  else
  {
    var   x = 0,
        tmp = p[0][0].col-5,
     colnum = (tmp < 0) ? 0 : tmp,
        col = [];

     for(var i = 0; i < this.p.length; i++) {
       col.push(p[i][0]);
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
    p[y][x] = col[x];

    var pane = p[y][x],
        orig = pane.div;

    pane = col[x];
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

  if(up) {
    this.bounds.y1 += oldheight;
    this.bounds.y2 += newheight;
  } else {
    this.bounds.y1 -= newheight;
    this.bounds.y2 -= oldheight;
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

      if(x == 0) {
        this.bounds.x2 += this.p[y][x].width;
      }
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