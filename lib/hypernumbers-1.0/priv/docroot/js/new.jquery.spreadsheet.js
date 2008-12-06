/**
 * jQuery SpreadSheet
 * Version 0.1 - 18/03/2008
 * @author Dale Harvey <harveyd@gmail.com>
 *
 * http://code.google.com/p/jqueryspreadsheet/
 *
 **/
const DEF_CELL_HEIGHT = 20;
const DEF_CELL_WIDTH  = 80;

const BASE_HTML = '<div class="clearfix toolbar"></div>\
  <div class="clearfix formulabar">\
   <input type="text" id="name" />\
   <input type="button" id="functions" value="f(x)" />\
   <input type="text" id="formula"/>\
  </div>\
  <div class="sheetwrapper">\
   <div class="corner"></div>\
   <div class="columns"> </div>\
   <div class="rowsanddata">\
    <div class="rows"> </div>\
    <div class="data">\
     <div class="inner">\
       <div id="scroller"></div>\
     </div>\
    </div>\
   </div>\
  </div>';

var to_b26 = function(cell)
{
  if(--cell < 26)
    return String.fromCharCode(cell+65);
  else
    return to_b26(Math.floor(cell / 26)) + to_b26(cell%26+1);
};

var from_b26 = function(cell)
{
  return cell.charCodeAt(0)-96;
};


//var data = new Array();
//data{"Sheet1":[1][1]} = {width:200};
/*var sheet_data = new Array();

  for(x = 0; x < 26; x++)
  {
  for(y = 0; y < 10000; y++)
  {
  sheet_data[x][y] = {value:50, formula:"=1+1"};
  }
  }*/

var SpreadSheet = function(root)
{
  this.construct = function(root)
  {
    this.root = $(document.getElementById(root));
    this.root.get(0).className = "spreadsheet";
    this.root.get(0).innerHTML = BASE_HTML;
    this.sheets = new Array();

    this.init();
    this.window_resize();
  };

  this.init = function()
  {
    var t   = this;
    var div = this.root.find("div.data");

    t.add_sheet("Sheet1");

    $(window).resize(function()
      { t.window_resize(); });

    div.scroll(function(e)
      { t.sheets[0].scroll(t.sheets[0],div,e); });
  };

  this.window_resize = function()
  {
    this.height = $("body").height() - 80;
    this.width  = $("body").width() - 25;

    this.root.find("div.rows,div.data").height(this.height);
    this.root.find("div.data").width(this.width);

    this.sheets[0].create_col_index();
  };

  this.add_sheet = function(name)
  {
    var sheet = new Sheet(this,name);
    this.sheets.push(sheet);
  };
  this.construct(root);
};

var Sheet = function(parent,name)
{
  this.construct = function(parent,name)
  {
    this.parent = parent;
    this.max_rows = 2000;
    this.max_cols = 22;

    this.scroll_x = 0;
    this.scroll_y = 0;
    this.x_offset = 1;
    this.y_offset = 1;
    this.init();
  };

  this.init = function()
  {
    this.set_scroll_size();
    $(document).bind("mousedown",function(e)
    {
      console.log("down");
    });
  };

  this.set_scroll_size = function()
  {
    scroller = this.parent.root.find("div#scroller");
    scroller.width(this.max_cols * DEF_CELL_WIDTH + this.max_cols);
    scroller.height(this.max_rows * DEF_CELL_HEIGHT);
  };

  this.scroll = function(self,element,event)
  {
    var x,y;
    var inner = element.children('.inner');

    if((x = event.target.scrollLeft) != self.scroll_x)
    {
      self.scroll_x = x;

      // Hit the right end
      if((Math.abs(inner.offset().left)+element.offset().left
          + element.width() - 15) >= (element[0].scrollWidth/4)*3)
      {
        self.max_cols += 1;
        self.set_scroll_size();
        console.log("right end");
      }

      var new_offset =  Math.floor(x/DEF_CELL_WIDTH)+1;
      if(new_offset != self.x_offset)
      {
        self.x_offset = new_offset;
        self.update_col_index();
      }
    }
    else if((y = event.target.scrollTop) != self.scroll_y)
    {
      self.scroll_y = y;

      // Hit the bottom
      if ( Math.abs(inner.offset().top) + element.height()
           + element.offset().top >= inner.outerHeight() )
      {
        console.log("bottom end");
      }
    }

/*
    var elem = $('div.data');


      var total_width = $("#scroller").width();
      var viewable_width = $(".data").width();


    */
  };

  this.create_col_index = function()
  {
    var cols_par = this.parent.root.find("div.columns");
    cols_par.empty();

    var total_width=0,count=0;
    while(total_width < this.parent.width)
    {
      var col = count + this.x_offset;
      var width = DEF_CELL_WIDTH;
      var cell = $("<div>"+to_b26(col)+"</div>").width(width);

      cell.appendTo(cols_par);

      total_width += width;
      count++;
    }
  };

  this.update_col_index = function()
  {
    var t = this;
    var cols = t.parent.root.find("div.columns div");

    $.each(cols,function(i)
    {
      $(this).text(to_b26(t.x_offset+i));
    });
  };

  this.construct(parent,name);
};
