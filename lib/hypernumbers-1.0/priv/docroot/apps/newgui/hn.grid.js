/**
 *
 **/

function addEvent( obj, type, fn ) {
  if ( obj.attachEvent ) {
    obj['e'+type+fn] = fn;
    obj[type+fn] = function(){obj['e'+type+fn]( window.event );};
    obj.attachEvent( 'on'+type, obj[type+fn] );
  } else
    obj.addEventListener( type, fn, false );
}
function removeEvent( obj, type, fn ) {
  if ( obj.detachEvent ) {
    obj.detachEvent( 'on'+type, obj[type+fn] );
    obj[type+fn] = null;
  } else
    obj.removeEventListener( type, fn, false );
}

var id = function(id) {
  return document.getElementById(id);
};

var HN = {};

HN.Util = {};
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

// fake data loader
var cells = [];
for(y = 0; y < 500; y++)
{
  cells[y] = [];
  for(x = 0; x < 26; x++)
  {
    cells[y][x] = {value:HN.Util.to_b26(x)+y, formula:"=1+1"};
  }
}

var rows = [];
var cols = [];
rows[2] = {"height":40};
rows[6] = {"height":60};
rows[20] = {"height":60};
cols[3] = {"width":10};
cols[7] = {"width":120};

var DEF_WIDTH = 80;
var DEF_HEIGHT = 16;
var PANE_SIZE = 20;

//
//  SpreadSheet Container
//
HN.SpreadSheet = function(root)
{
  this.root   = id(root);
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
  var sheet = new HN.Sheet("Sheet1");
  this.sheets.push(sheet);
};

//
//  Sheet Container
//
HN.Sheet = function(name)
{
  this.name   = name;
  this.offset = {"x":1,"y":1};
  this.max    = {"x":26, "y":100};
};

HN.Sheet.prototype.row_height = function(i)
{
  return (typeof rows[i] != "undefined")
    ? rows[i].height : DEF_HEIGHT;
};

HN.Sheet.prototype.col_width = function(i)
{
  return (typeof cols[i] != "undefined")
    ? cols[i].width : DEF_WIDTH;
};

HN.Sheet.prototype.cell = function(y,x)
{
  return cells[y][x];
};

/**
 * @class HN.Layout
 *
 */
HN.Layout = function(parent,name,sheet)
{
  this.p = parent;
  this.s = sheet;
  this.div   = id("data");
  this.cells = id("cells");
  this.inner = id("scroller");

  this.size   = {"x":0, "y":0};
  this.num    = {"x":0, "y":0};

  this.axis_dom = {
    "x":id("columns"),
    "y":id("rows")
  };

  this.indexfun   = {
    "x":function(x){ return HN.Util.to_b26(x);},
    "y":function(x){ return x;}
  };

  this.scrollbar = {
    "x":id("bottom-bar"),
    "y":id("right-bar")
  };

  this.scrollbar_offset = {
    "y":HN.Util.y_pos(this.scrollbar.y),
    "x":HN.Util.x_pos(this.scrollbar.x)
  };

  var that = this;

  addEvent(id("down"), "mousedown", function(e) {
    that.scroll_down_pressed(e);
  });

  addEvent(id("up"), "mousedown", function(e) {
    that.scroll_up_pressed(e);
  });

  addEvent(id("right-bar"), "mousedown", function(e) {
    that.scrollbar_right_pressed(e);
  });

  addEvent(window, "resize", function() {
    that.window_resize();
  });

  this.window_resize();

  var rows = Math.ceil(this.size.y / 20 / 8)+1;
  var cols = Math.ceil(this.size.x / 80 / 5)+1;

  this.panes = new HN.Layout.Panes(this.inner, this.s, rows, cols);
};

HN.Layout.prototype.scrollbar_right_pressed = function(event)
{
  var that = this,
       max = this.size.y - that.scrollbar_offset.y + 10;

  var scroll = function(event)
  {
    var pos = event.clientY - that.scrollbar_offset.y - 10;

    if(pos < 0) {
      pos = 0;
    } else if( pos > max) {
      pos = max;
    }
    that.scrollbar.y.style.top = pos+"px";

    var  y = Math.round(((that.s.max.y * 20) / that.size.y) * pos),
     index = that.get_row_at_y(y),
      diff = (parseInt(that.inner.scrollTop) || 0) - index[1];

    that.inner.scrollTop = index[1];
    that.s.offset.y = index[0];

    that.panes.bounds.y1 += diff;
    that.panes.bounds.y2 += diff;

    //that.axis_dom.y.innerHTML = "";
    //that.add_index(true,"y", that.s.offset.y, that.s.offset.y+that.num.y);

    if(that.size.y > that.panes.bounds.y2) {
      that.panes.shift(true);
    } else if(that.panes.bounds.y1 > -40) {
      that.panes.shift(false);
    }
  };

  var onmouseup = function() {
    that.scrollbar.y.style.backgroundColor = "";
    removeEvent(document, "mouseup", onmouseup);
    removeEvent(document, "mousemove", scroll);
  };

  that.scrollbar.y.style.backgroundColor = "grey";

  addEvent(document, "mouseup", onmouseup);
  addEvent(document, "mousemove", scroll);

  event.preventDefault();
  return false;
};

HN.Layout.prototype.get_row_at_y = function(y)
{
  var i = 1, total = 0;

  while(true)
  {
    total += this.s.row_height(i)+4;

    if(total > y)
      return [i, total-(this.s.row_height(i)+4)];

    ++i;
  };
};

HN.Layout.prototype.scroll_down_pressed = function(event)
{
  var that = this,
      timer;

  var scroll = function()
  {
    var end = that.s.offset.y+that.num.y,
     offset = that.s.row_height(that.s.offset.y)+4;

    that.inner.scrollTop += offset;
    that.s.offset.y += 1;

    that.panes.bounds.y1 -= offset;
    that.panes.bounds.y2 -= offset;

    that.del_index("y",0,1);
    that.add_index(true,"y",end,end+1);

    if(that.size.y > that.panes.bounds.y2) {
      that.panes.shift(true);
    }
  };

  var onmouseup = function() {
    removeEvent(document, "mouseup", onmouseup);
    clearInterval(timer);
  };

  addEvent(document, "mouseup", onmouseup);
  timer = setInterval( scroll, 70);

};

HN.Layout.prototype.scroll_up_pressed = function(event)
{
  var that = this,
      timer;

  var scroll = function()
  {
    var offset = that.s.row_height(that.s.offset.y)+4;

    that.inner.scrollTop -= offset;
    that.s.offset.y -= 1;

    that.panes.bounds.y1 += offset;
    that.panes.bounds.y2 += offset;

    that.del_index("y",that.num.y-1,that.num.y);
    that.add_index("prepend","y",that.s.offset.y,that.s.offset.y-1);

    if(that.panes.bounds.y1 > -40) {
      that.panes.shift(false);
    }
  };

  var onmouseup = function() {
    removeEvent(document, "mouseup", onmouseup);
    clearInterval(timer);
  };

  addEvent(document, "mouseup", onmouseup);
  timer = setInterval( function() {
    if(that.s.offset.y > 1) {
      scroll();
    }
  }, 70);
};

HN.Layout.prototype.set_sheet = function(sheet)
{
  this.s = sheet;
};

HN.Layout.prototype.window_resize = function()
{
  this.size["y"] = document.body.clientHeight - 80;
  this.size["x"] = document.body.clientWidth  - 25;

  this.scrollbar.y.style.height =
    Math.round(this.size.y/(this.s.max.y/10))+"px";

  this.div.style.height = this.size["y"]+"px";
  this.div.style.width  = this.size["x"]+"px";
  this.axis_dom.y.style.height = this.size["y"];

  // Calculate number of on screen rows / cols
  // plus some padding for small cells
  this.num.y = Math.round(this.size.y / 20)+6;
  this.num.x = Math.round(this.size.x / 80)+3;

  // Redraw Axis
  this.axis_dom.x.innerHTML = "";
  this.axis_dom.y.innerHTML = "";
  this.add_index(true,"y", this.s.offset.y, this.s.offset.y+this.num.y);
  this.add_index(true,"x", this.s.offset.x, this.s.offset.x+this.num.x);
};

HN.Layout.prototype.add_index = function(append,axis,start,end)
{
  var el   = this.axis_dom[axis];
  var inc  = (append) ? 1 : -1;
  var html = [el.innerHTML];

  for(var y = start; y != end; y += inc)
  {
    var style = (axis == "y")
      ? "height:" + this.s.row_height(y) + "px"
      : "width:"  + this.s.col_width(y)  + "px";

    var div = '<div style="' + style + '">'
      + this.indexfun[axis](y) + '</div>';

    if(append) {
      html.push(div);
    } else {
     html.unshift(div);a
    }
  }

  el.innerHTML = html.join("");
};

HN.Layout.prototype.del_index = function(axis, start, end)
{
  for(var i = start; i < end; i++)
  {
    this.axis_dom[axis].removeChild(
        this.axis_dom[axis].childNodes[start]);
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

  for(var y = 0; y < rows; y++) {
    this.add_row(y);
  }
};

HN.Layout.Panes.prototype.shift = function(up)
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
     rownum = p[y][0].row-8,
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
    this.inner.removeChild(p[y][x].div);
    p[y][x].move_row(rownum);

    if(x == 0)
    {
      newheight = p[y][x].height;

      top = up
        ? p[y-1][0].top + p[y-1][0].height
        : p[1][0].top - newheight;
    }

    p[y][x].top = top;
    p[y][x].div.style.top = top+"px" ;
    this.inner.appendChild(p[y][x].div);
  }

  if(up) {
    this.bounds.y1 += oldheight;
    this.bounds.y2 += newheight;
  } else {
    this.bounds.y1 -= newheight;
    this.bounds.y2 -= oldheight;
  }
};

HN.Layout.Panes.prototype.add_row = function(y)
{
  var row = (y * 8) + 1;
  var top = (y == 0) ? 0
    : this.p[y-1][0].top + this.p[y-1][0].height;

  this.p[y] = [];

  for(var x = 0; x < this.cols; x++)
  {
    var left = (x == 0) ? 0
      : this.p[0][x-1].left + this.p[0][x-1].width;

    this.p[y][x] = new HN.Layout.Pane(this.s, row, (x*5)+1);
    this.p[y][x].set_position(top, left);
    this.inner.appendChild(this.p[y][x].div);
  }

  this.bounds.y2 += this.p[y][0].height;

  this.bounds.x1 = this.p[y][this.cols-1].left
    + this.p[y][this.cols-1].width;
};

/**
 * @class HN.Layout.Pane
 *
 */
HN.Layout.Pane = function(sheet, row, col)
{
  if(row < 0)
    row = 0;

  this.top  = null;
  this.left = null;

  this.row = row;
  this.col = col;

  this.s = sheet;
  this.div = document.createElement("div");

  this.div.style.position = "absolute";

  var twidth  = 0,
      theight = 0,
      cells   = [];

  for(var y = row; y < row + HN.Layout.Pane.DEF_ROWS; y++)
  {
    var height = this.s.row_height(y)+2;

    twidth = 0;
    theight += height+2;

    cells[cells.length] = '<div style="height:'+(height+2)+'px;">';

    for(var x = col; x < col+HN.Layout.Pane.DEF_COLS; x++)
    {
      var width = (this.s.col_width(x)-1);
      twidth += width+2;

      cells[cells.length] = '<div style="';
      cells[cells.length] = 'height:'+height+'px;';
      cells[cells.length] = 'width:'+width+'px;" ';
      cells[cells.length] = 'class="cell">';
      cells[cells.length] = (this.s.cell(y,x).value)+'</div>';
    }
    cells[cells.length] = '</div>';
  }

  this.height = theight;
  this.width  = twidth;

  this.div.style.width  = "20000px"; //twidth + "px";
  this.div.style.height = "20000px"; //theight + "px";
  this.div.innerHTML = cells.join("");
};

HN.Layout.Pane.DEF_ROWS = 8;
HN.Layout.Pane.DEF_COLS = 5;

HN.Layout.Pane.prototype.move_row = function(top)
{
  if(top < 0)
    top = 0;

  this.row = top;

  var children = this.div.childNodes,
          rows = children.length;

  for(var total = 0, y = 0; y < rows; y++)
  {
    var el  = children[y],
     height = this.s.row_height(y+this.row)+4,
     nchild = el.childNodes,
       cols = nchild.length;

    total += height;

    el.style.height = height+"px";

    for(var x = 0; x < cols; x++)
    {
      nchild[x].style.height = (height-2)+"px";
      nchild[x].innerHTML = this.s.cell(y+this.row, x+this.col).value;
    }
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