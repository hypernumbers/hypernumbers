/**
 * @author Dale Harvey <harveyd@gmail.com>
 *
 **/
var HN = {};

HN.to_b26 = function(cell)
{
  return (--cell < 26)
    ? String.fromCharCode(cell+65)
    : HN.to_b26(Math.floor(cell / 26)) + HN.to_b26(cell%26+1);
};

HN.from_b26 = function(cell)
{
  return cell.charCodeAt(0)-96;
};

// fake data loader
var cells = [];
for(x = 0; x < 32; x++)
{
  cells[x] = [];
  for(y = 0; y < 1000; y++)
  {
    cells[x][y] = {value:HN.to_b26(x)+y, formula:"=1+1"};
  }
}

var rows = [];
var cols = [];
rows[2] = {"height":40};
rows[6] = {"height":60};
cols[3] = {"width":10};
cols[7] = {"width":120};

var DEF_WIDTH = 80;
var DEF_HEIGHT = 16;
var PANE_SIZE = 20;

var DELETE = 0;
var OK = 1;

//
//  SpreadSheet Container
//
HN.SpreadSheet = function(root)
{
  this.root   = $(document.getElementById(root));
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
  return (   typeof cells[x]    != "undefined"
             && typeof cells[x][y] != "undefined")
    ? cells[x][y] : {value:""};
};


HN.Layout = function(parent,name,sheet)
{
  this.p = parent;
  this.s = sheet;
  this.div   = this.p.root.find("#data");
  this.cells = this.p.root.find("#cells");
  this.inner = this.p.root.find("#scroller");

  this.panes  = [];
  this.bounds = {"x1":0,"y1":0,"x2":0,"y2":0};
  this.size   = {"x":0, "y":0};
  this.num    = {"x":0, "y":0};

  this.axis_dom = {
    "x":this.p.root.find("#columns"),
    "y":this.p.root.find("#rows")
  };

  this.indexfun   = {
    "x":function(x){return HN.to_b26(x);},
    "y":function(x){return x;}
  };

  var that = this;

  $("#down").bind("mousedown",function(e) {
    that.scroll_down_pressed(e);
  });

  $("#up").bind("mousedown",function(e) {
    that.scroll_up_pressed(e);
  });

  /* this.div.bind("mousewheel",function(e) {
    self.scrollwheel(self,inner,e);
  }); */

  $(window).resize(function() {
    that.window_resize();
  });

  this.window_resize();
  this.draw_panes();
};

HN.Layout.prototype.draw_panes = function()
{
  var rows = Math.ceil(this.size.y / 20 / 8)+1;
  var cols = Math.ceil(this.size.x / 80 / 5)+1;

  for(var y = 0; y < rows; y++)
  {
    if(typeof this.panes[y] == "undefined") {
      this.panes[y] = [];
    }

    var top = (y == 0) ? 0
        : this.panes[y-1][0].top + this.panes[y-1][0].height;

    for(var x = 0; x < rows; x++)
    {
      var left = (x == 0) ? 0
        : this.panes[y][x-1].left + this.panes[y][x-1].width;

      var pane = new HN.Layout.Pane(this.s, top, left, (y*8)+1, (x*5)+1);
      this.panes[y][x] = pane;
      this.inner[0].appendChild(pane.div);
    }
  }

  this.bounds.x2 = pane.left + pane.width;
  this.bounds.y2 = pane.top  + pane.height;
};

HN.Layout.prototype.scroll_down_pressed = function(event)
{
  var that = this,
      timer;

  $(document).one("mouseup", function() {
    clearInterval(timer);
  });

  timer = setInterval( function() {
    scroll();
  },70);

  var scroll = function()
  {
    var end    = that.s.offset.y+that.num.y;
    var offset = that.s.row_height(that.s.offset.y)+4;

    that.inner[0].scrollTop += offset;
    that.s.offset.y += 1;

    that.bounds.y1 -= offset;
    that.bounds.y2 -= offset;

    that.del_index("y",0,1);
    that.add_index("append","y",end,end+1);

    var check_bounds = function()
    {
      if(that.size.y > that.bounds.y2)
      {
        var rm = that.panes.shift();
        for(var i = 0; i < rm.length; i++)
        {
          that.inner[0].removeChild(rm[i].div);
        }
        that.bounds.y1 += rm[0].height;

        var last_row_index = that.panes.length;
        var prev = that.panes[last_row_index-1][0];

        that.panes[last_row_index] = [];
        for(var x = 0; x < that.panes[0].length; x++)
        {
          var top  = prev.top + prev.height;
          var left = that.panes[0][x].left;

          var pane = new HN.Layout.Pane(that.s,
            top, left, prev.row+prev.DEF_ROWS, (x*5)+1);

          that.panes[last_row_index][x] = pane;
          that.inner[0].appendChild(pane.div);
        }
        that.bounds.y2 += pane.height;
      }
    };

    setTimeout(function(){ check_bounds(); },0);
  };
};

HN.Layout.prototype.scroll_up_pressed = function(event)
{
  var that = this,
      timer;

  $(document).one("mouseup", function() {
    clearInterval(timer);
  });

  timer = setInterval( function() {
    scroll();
  },70);

  var scroll = function()
  {
    if(that.s.offset.y > 1)
    {
      that.s.offset.y--;
      that.inner[0].scrollTop -= that.s.row_height(that.s.offset.y)+4;
      that.del_index("y",that.num.y-1,that.num.y);
      that.add_index("prepend","y",that.s.offset.y,that.s.offset.y-1);
    }
  };
};

HN.Layout.prototype.set_sheet = function(sheet)
{
  this.s = sheet;
};

HN.Layout.prototype.window_resize = function()
{
  this.size["y"] = $("body").height() - 80;
  this.size["x"] = $("body").width()  - 25;

  this.div.height(this.size["y"]).width(this.size["x"]);
  this.axis_dom.y.height(this.size["y"]);

  // Calculate number of on screen rows / cols
  // plus some padding for small cells
  this.num.y = Math.round(this.size.y / 20)+6;
  this.num.x = Math.round(this.size.x / 80)+3;

  // Redraw Axis
  this.axis_dom.x.innerHTML = "";
  this.axis_dom.y.innerHTML = "";
  this.add_index("append","y", this.s.offset.y, this.s.offset.y+this.num.y);
  this.add_index("append","x", this.s.offset.x, this.s.offset.x+this.num.x);
};

HN.Layout.prototype.draw_pane = function(top, left, row, col)
{
  this.inner[0].appendChild(pane);
};

HN.Layout.prototype.add_index = function(append,axis,start,end)
{
  var is_append = (append == "append");
  var inc       = (is_append) ? 1 : -1;
  var html      = [this.axis_dom[axis][0].innerHTML];

  for(var y = start; y != end; y+=inc)
  {
    var style = (axis == "y")
      ? "height:" + this.s.row_height(y) + "px"
      : "width:"  + this.s.col_width(y)  + "px";

    var div = '<div style="' + style + '">'
      + this.indexfun[axis](y) + '</div>';

    (is_append)
      ? html.push(div)
      : html.unshift(div);
  }

  this.axis_dom[axis][0].innerHTML = html.join("");
};

HN.Layout.prototype.del_index = function(axis, start, end)
{
  for(var i = start; i < end; i++)
  {
    this.axis_dom[axis][0].removeChild(
        this.axis_dom[axis][0].childNodes[start]);
  }
};

HN.Layout.Pane = function(sheet, top, left, row, col)
{
  this.DEF_ROWS = 8;
  this.DEF_COLS = 5;

  this.top  = top;
  this.left = left;
  this.row  = row;
  this.col  = col;

  this.s = sheet;
  this.div = document.createElement("div");

  this.div.style.position = "absolute";
  this.div.style.top  = top + "px";
  this.div.style.left = left + "px";

  var twidth = 0, theight = 0, cells = [];

  for(var y = row; y < row+this.DEF_ROWS; y++)
  {
    var height = this.s.row_height(y)+2;

    twidth = 0;
    theight += height+2;

    cells.push('<div style="height:'+(height+2)+'px;">');

    for(var x = col; x < col+this.DEF_COLS; x++)
    {
      var width = (this.s.col_width(x)-1);
      twidth += width+2;

      cells.push('<div style="');
      cells.push('height:'+height+'px;');
      cells.push('width:'+width+'px;" ');
      cells.push('class="cell">');
      cells.push((this.s.cell(y,x).value)+'</div>');
    }
    cells.push('</div>');
  }

  this.height = theight;
  this.width  = twidth;

  this.div.style.width  = "20000px"; //twidth + "px";
  this.div.style.height = "20000px"; //theight + "px";
  this.div.innerHTML = cells.join("");
};
