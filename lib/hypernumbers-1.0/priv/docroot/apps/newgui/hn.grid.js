/**
 * jQuery SpreadSheet
 * Version 0.1 - 18/03/2008
 * @author Dale Harvey <harveyd@gmail.com>
 *
 * http://code.google.com/p/jqueryspreadsheet/
 *
 **/

var to_b26 = function(cell)
{
  return (--cell < 26)
    ? String.fromCharCode(cell+65)
    : to_b26(Math.floor(cell / 26)) + to_b26(cell%26+1);
};

var from_b26 = function(cell)
{
  return cell.charCodeAt(0)-96;
};


// fake data loader
var cells = new Array();
for(x = 0; x < 32; x++)
{
  cells[x] = new Array();
  for(y = 0; y < 1000; y++)
  {
    cells[x][y] = {value:to_b26(x)+y, formula:"=1+1"};
  }
}

var rows = new Array();
var cols = new Array();
rows[2] = {"height":40};
rows[6] = {"height":60};
cols[3] = {"width":10};
cols[7] = {"width":120};

var DEF_WIDTH = 80;
var DEF_HEIGHT = 16;
var PANE_SIZE = 20;

var DELETE = 0;
var OK = 1;

var SpreadSheet = function(root)
{
  this.construct = function(root)
  {
    this.root = $(document.getElementById(root));

    this.sheet = 0;
    this.sheets = new Array();
    this.add_sheet("Sheet1");

    this.layout = new LayoutManager(this,name,this.cur_sheet());
  };

  this.cur_sheet = function()
  {
    return this.sheets[this.sheet];
  };

  this.add_sheet = function(name)
  {
    var sheet = new Sheet("Sheet1");
    this.sheets.push(sheet);
  };

  this.construct(root);
};

var Sheet = function(name)
{
  this.construct = function(name)
  {
    this.name   = name;
    this.offset = {"x":1,"y":1};
  };

  this.row_height = function(i)
  {
    return (typeof rows[i] != "undefined")
      ? rows[i].height : DEF_HEIGHT;
  };

  this.col_width = function(i)
  {
    return (typeof cols[i] != "undefined")
      ? cols[i].width : DEF_WIDTH;
  };

  this.cell = function(y,x)
  {
    return (   typeof cells[x]    != "undefined"
            && typeof cells[x][y] != "undefined")
      ? cells[x][y] : {value:""};
  };

  this.construct(name);
};

var LayoutManager = function(parent,name,sheet)
{
  this.construct = function(parent,name,sheet)
  {
    this.p = parent;
    this.s = sheet;
    this.div   = this.p.root.find("#data");
    this.cells = this.p.root.find("#cells");

    this.max  = {"x":26,"y":100};
    this.mpos = {"x":0, "y":0};
    this.size = {"x":0, "y":0};
    this.num  = {"x":0, "y":0};

    this.axis_dom = {
      "x":this.p.root.find("#columns"),
      "y":this.p.root.find("#rows")
    };

    this.indexfun   = {
      "x":function(x){return to_b26(x);},
      "y":function(x){return x;}
    };

    this.set_scroll_size();

    var self  = this;
    var inner = this.p.root.find("#scroller");

    this.div.bind("mousedown",function(e) {
      self.check_scroll_pressed(self,inner,e); });

    this.div.scroll(function(e) {
      self.scrollbar(self,e); });

    this.div.bind("mousewheel",function(e) {
      self.scrollwheel(self,inner,e); });

    $(window).resize(function() {
      self.window_resize(); });

    this.window_resize();
  };

  this.set_sheet = function(sheet)
  {
    this.s = sheet;
  };

  this.window_resize = function()
  {
    this.size["y"] = $("body").height() - 80;
    this.size["x"] = $("body").width()  - 25;

    this.div.height(this.size["y"]).width(this.size["x"]);
    this.axis_dom.y.height(this.size["y"]);

    this.draw_table();
  };

  this.set_scroll_size = function()
  {
    $("#scroller").width( this.max.x * DEF_WIDTH + this.max.x);
    $("#scroller").height(this.max.y * DEF_HEIGHT);
  };

  this.check_scroll_pressed = function(self,inn,e)
  {
    if(self.right_arrow(e,self.div) || self.bottom_arrow(e,self.div))
    {
      $(self.div).one("mouseup",function(e){ clearInterval(id); });
      var id = setInterval(function() { add(); },100);
    }

    var add = function()
    {
      if(self.right_arrow(e,self.div) && self.right_end(inn,self.div))
      {
        self.extend_scroll(self,"x",1);
      }
      else if(self.bottom_arrow(e,self.div) && self.bottom_end(inn,self.div))
      {
        self.extend_scroll(self,"y",2);
      }
    };
  };

  this.extend_scroll = function(self,axis,val)
  {
    var d = {"x":{"x":"scrollLeft","end":"scrollWidth"},
             "y":{"x":"scrollTop", "end":"scrollHeight"}};
    self.max[axis] += val;
    self.set_scroll_size();
    self.div[0][d[axis].x] = self.div[0][d[axis].end];
  };

  this.scrollwheel = function(self,inn,e)
  {
    if(this.bottom_end(inn,self.div))
      self.extend_scroll(self,"y",2);
  };

  this.scrollbar = function(self,e)
  {
    var check = function(val,axis)
    {
      // keep track of mouse position, so dont
      // recalc x+y positions every scroll
      if(val == self.mpos[axis])
        return false;

      self.mpos[axis] = val;

      var old = self.s.offset[axis];
      var def = (axis == "x") ? DEF_WIDTH : DEF_HEIGHT;
      var x   = Math.floor(val/def)+1;

      if(x !== old)
      {
        self.s.offset[axis] = x;
        self.shift_index(axis,old,self.s.offset[axis]);
      }
      return true;
    };

    check(e.target.scrollLeft,"x")
      || check(e.target.scrollTop,"y");
  };

  this.draw_table = function()
  {
    this.axis_dom.y[0].innerHTML = '';
    this.axis_dom.x[0].innerHTML = '';

    this.num.y = 0, this.num.x = 0;
    var y=0,x=0;
    do   { var tmp = this.s.row_height(this.num.y); }
    while((y += tmp) < this.size.y && ++this.num.y)
    ++this.num.y;

    do   { var tmp = this.s.col_width(this.num.x); }
    while((x += tmp)  < this.size.x && ++this.num.x)
    ++this.num.x;

    this.add_index("append","y",this.s.offset.y,this.s.offset.y+this.num.y);
    this.add_index("append","x",this.s.offset.x,this.s.offset.x+this.num.x);

    var top = -2,cells='';
    for(y = this.s.offset.y; y < this.s.offset.y+this.num.y; y++)
    {
      var left = -2;
      var height = this.s.row_height(y)+2;
      cells += '<div style="height:'+(height+2)+'px;overflow:auto;">';
      for(x = this.s.offset.x; x < this.s.offset.x + this.num.x;x++)
      {
        var width = this.s.col_width(x)-1;
        var text   = this.s.cell(y,x).value;
        cells += '<div style="height:'+height
          + 'px;width:'+width+'px;border:1px solid #FFF;'
          + 'border-bottom:1px solid #ddd;border-right:1px solid #ddd;'
          + 'overflow:hidden;font:10px Arial, sans-serif;float:left;'
          + '">'+text+'</div>';
        left += width;
      }
      cells += '</div>';
      top += height;
    }
    this.cells[0].innerHTML = "";
    this.cells[0].innerHTML = cells;
  };


  this.add_index = function(append,axis,start,end)
  {
    var inc = (append=="append")?1:-1;
    var total = 0;
    var html  = this.axis_dom[axis][0].innerHTML;
    for(var y = start; y != end; y+=inc)
    {
      var val = (axis == "y")
        ? this.s.row_height(y) : this.s.col_width(y);
      var index  = (axis == "y")
        ? '<div style="height:'+val+'px">'
          +this.indexfun[axis](y)+'</div>'
        : '<div style="width:'+val+'px">'
          +this.indexfun[axis](y)+'</div>';
      total += val + ((axis=="y")?4:1);

      html = (append=="append")
        ? html + index
        : index + html;
    }
    this.axis_dom[axis][0].innerHTML = html;
    return total;
  };

  this.del_index = function(axis,start,end)
  {
    var val = 0;
    for(var i = start; i < end; i++)
    {
      var cell = $(this.axis_dom[axis][0].childNodes[start]);
      val += (axis=="y") ? cell.height()+4 : cell.width()+1;
      cell.remove();
    }
    return val;
  };

  this.shift_index = function(axis,oldindex,newindex)
  {
    // Scrolled off the page
    if(Math.abs(newindex-oldindex) > this.num[axis])
    {
      this.axis_dom[axis].empty();
      this.add_index("append",axis,newindex,newindex+this.num[axis]);
      this.draw_table();
    }
    // Scroll Down
    else if (newindex > oldindex)
    {
      var off = this.del_index(axis,0,(newindex-oldindex)+0);
      this.add_index("append",axis,this.num[axis]+oldindex,
                    this.num[axis]+newindex);

      if(axis == "y")
      {
        this.del_rows(0,(newindex-oldindex)+0);
        this.add_rows("append",this.num[axis]+oldindex,
                      this.num[axis]+newindex);
      }
    }
    // Scroll Up
    else if(newindex < oldindex)
    {
      this.del_index(axis,this.num[axis]+(newindex-oldindex),this.num[axis]);
      var off = this.add_index("prepend",axis,oldindex-1,newindex-1);

      if(axis == "y")
      {
        this.del_rows(this.num[axis]+(newindex-oldindex),this.num[axis]);
        this.add_rows("prepend",oldindex-1,newindex-1);
      }
    }
  };

  this.del_rows = function(x,y)
  {
    for(var i = x; i < y; i++)
    {
      $(this.cells[0].childNodes[i]).remove();
    }
  };

  this.add_rows = function(append,x1,y1)
  {
    var cells = '';
    for(var i = x1; i < y1; i++)
    {
      console.log([x1,y1,i]);
      var height = this.s.row_height(y)+2;
      cells += '<div style="height:'+(height+2)+'px;overflow:auto;">';
      for(var x = this.s.offset.x; x < this.s.offset.x + this.num.x;x++)
      {
        var width = this.s.col_width(x)-1;
        var text   = this.s.cell(i,x).value;
        cells += '<div style="height:'+height
          + 'px;width:'+width+'px;border:1px solid #FFF;'
          + 'border-bottom:1px solid #ddd;border-right:1px solid #ddd;'
          + 'overflow:hidden;font:10px Arial, sans-serif;float:left;'
          + '">'+text+'</div>';
      }
      cells += '</div>';
    }
    if(append == "append")
      this.cells[0].innerHTML += cells;
    else
      this.cells[0].innerHTML = cells+this.cells[0].innerHTML;
  };


  /* Some nasty mouse position functions */
  this.right_arrow = function(e,x) {
    return e.pageY-53>x.height() && e.pageX+3>x.width(); };
  this.bottom_arrow = function(e,x) {
    return e.pageY-39>x.height() && e.pageX-9>x.width(); };
  this.right_end = function(x,y) {
    return Math.abs(x.offset().left)+y.offset().left+y.width()-15
      >= y[0].scrollWidth; };
  this.bottom_end = function(x,y) {
    return Math.abs(x.offset().top)+y.offset().top+y.height()-15
      >= y[0].scrollHeight; };

  this.construct(parent,name,sheet);
};

