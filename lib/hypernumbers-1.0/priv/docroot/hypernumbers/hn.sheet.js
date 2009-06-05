//
//  Sheet Container
//
HN.Sheet = function(spreadsheet, url)
{
  this.spreadsheet = spreadsheet;
  this.url    = url;
  this.offset = {"x":1,"y":1};
  this.max    = {"x":26, "y":50};
  this.height = 0;
  this.width  = 0;

  this.data   = {"cell":{},"column":{}, "row":{},
                 "page":{}, "styles":{}};

  this.load_data();
  this.calc_size();
};

HN.Sheet.CELL_WIDTH  = 80;
HN.Sheet.CELL_HEIGHT = 20;

HN.Sheet.prototype.load_data = function()
{
  var that = this,
    layout = this.spreadsheet.layout;

  var fun = function(data)
  {
    that.data = data;
    that.spreadsheet.do_auth();
    that.find_max();
    that.calc_size();
    if( that.spreadsheet.layout ) {
      that.spreadsheet.layout.calcScrollbarSize(Y);
      that.spreadsheet.layout.calcScrollbarSize(X);
      that.spreadsheet.layout.panes.refresh();
      that.spreadsheet.layout.selection.show_selection();
    }
    that.updater(data.time);
  };

  $.ajax({
	type: "GET",
    url: this.url,
    dataType: "json",
    success: fun
  });
};

HN.Sheet.prototype.find_max = function()
{
  for( var i in this.data.cell ) {
    if( parseInt(i) > this.max.y ) {
      this.max.y = parseInt(i);
    }
    for ( var x in this.data.cell[i] ) {
      if( parseInt(x) > this.max.x ) {
        this.max.x = parseInt(x);
      }
    }
  }
};

HN.Sheet.prototype.extend = function(axis, amount)
{
  this.max[axis.str] += amount;
  this.calc_size();
};

HN.Sheet.prototype.calc_size = function()
{
  var height = 0, count = 0;
  for (var i in this.data.row) {
    if (String(i >>> 0) == i && i >>> 0 != 0xffffffff) {
      count++;
      height += this.data.row[i][i].height;
    }
  }
  this.height = height + ((this.max.y - count) * 20);

  var width = 0, c = 0;
  for (var x in this.data.column) {
    if (String(x >>> 0) == x && x >>> 0 != 0xffffffff) {
      c++;
      width += this.data.column[x][x].width;
    }
  }
  this.width = width + ((this.max.x - c) * 80);
};

HN.Sheet.prototype.get_style_by_cell = function(cell)
{
  var c = this.cell(cell.y, cell.x);

  if(c) {
    return this.data.styles[c.style] || "";
  } else {
    return "";
  }
};

HN.Sheet.prototype.row_height = function(i)
{
  return this.data.row[i] && this.data.row[i][i]
    && this.data.row[i][i].height || HN.Sheet.CELL_HEIGHT;
};

HN.Sheet.prototype.col_width = function(i)
{
  return this.data.column[i] && this.data.column[i][i]
    && this.data.column[i][i].width || HN.Sheet.CELL_WIDTH;
};

HN.Sheet.prototype.cell = function(y,x)
{
  return this.data.cell[y] && this.data.cell[y][x]
    || false;
};

HN.Sheet.prototype.cell_offset = function(y,x)
{
  for (var iy = 1, top = 0; iy < y; iy++) {
    top += this.row_height(iy);
  }

  for (var ix = 1, left = 0; ix < x; ix++) {
    left += this.col_width(ix);
  }

  return {"top": top, "left": left};
};

HN.Sheet.prototype.ensure = function(msg)
{
  if( msg.reftype == "cell" ) {
    var coord = HN.Util.parse_ref(msg.ref);

    if( typeof this.data.cell[coord.y] == "undefined" ) {
      this.data.cell[coord.y] = {};
    }
    if( typeof this.data.cell[coord.y][coord.x] == "undefined" ) {
      this.data.cell[coord.y][coord.x] = {};
    }

    if( coord.y > this.max.y ) {
      this.max.y = coord.y;
      this.calc_size();
      this.spreadsheet.layout.calcScrollbarSize(Y);
    }
    if( coord.x > this.max.x ) {
      this.max.x = coord.x;
      this.calc_size();
      this.spreadsheet.layout.calcScrollbarSize(X);
    }
  } else if( msg.reftype == "row" ) {

    var index = msg.ref;

    if( typeof this.data.row[index] == "undefined" ) {
      this.data.row[index] = {};
    }
    if( typeof this.data.row[index][index] == "undefined" ) {
      this.data.row[index][index] = {};
    }
  } else if( msg.reftype == "column" ) {
    var index = HN.Util.from_b26(msg.ref);

    if(typeof this.data.column[index] == "undefined") {
      this.data.column[index] = {};
    }
    if(typeof this.data.column[index][index] == "undefined") {
      this.data.column[index][index] = {};
    }
  }
};

HN.Sheet.prototype.handle_refresh = function(msg)
{
  var that = this,
    layout = this.spreadsheet.layout;

  var fun = function(data)
  {
    that.data = data;
    if( that.spreadsheet.layout ) {
      that.spreadsheet.layout.calcScrollbarSize(Y);
      that.spreadsheet.layout.calcScrollbarSize(X);
      that.spreadsheet.layout.panes.refresh();
      that.spreadsheet.layout.selection.show_selection();
    }
  };

  $.ajax({
	type: "GET",
    url: this.url,
    dataType: "json",
    success: fun
  });
};

HN.Sheet.prototype.handle_style = function(msg)
{
  this.data.styles[msg.index] = msg.css;
};

HN.Sheet.prototype.handle_error = function(msg)
{
  var ind = HN.Util.parse_ref(msg.ref),
    error = HN.Util.id("error"),
   select = this.spreadsheet.layout.selection;
     cell = this.spreadsheet.layout.s.cell(ind.y, ind.x);

  cell.value = "";
  select.set_cell(ind.y, ind.x);
  select.rebuild_input();
  select.bounds = {x1:ind.x, x2:ind.x, y1:ind.y, y2:ind.y};
  select.show_selection();
  select.input.value = msg.original;
  select.input.focus();
  select.state = HN.States.EDIT_FULL_CELL;

  error.style.top  = (parseInt(select.input.style.top)-16) + "px";
  error.style.left = select.input.style.left;
  error.innerHTML  = "Invalid Formula!";
  error.style.display = "block";

  window.setTimeout(function() {
    error.style.display = "none";
  }, 2000);
};

HN.Sheet.prototype.change_column = function(msg)
{
  var index = HN.Util.from_b26(msg.ref);
  this.data.column[index][index][msg.name] = msg.value;
};
HN.Sheet.prototype.change_row = function(msg)
{
  this.data.row[msg.ref][msg.ref][msg.name] = msg.value;
};
HN.Sheet.prototype.change_cell = function(msg)
{
  var ind = HN.Util.parse_ref(msg.ref);
  this.data.cell[ind.y][ind.x][msg.name] = msg.value;
};
HN.Sheet.prototype.handle_change = function(msg)
{
  this["change_"+msg.reftype](msg);
};

HN.Sheet.prototype.handle_delete = function(msg)
{
  this["delete_"+msg.reftype](msg);
};
HN.Sheet.prototype.delete_cell = function(msg)
{
  var ind = HN.Util.parse_ref(msg.ref);
  this.data.cell[ind.y][ind.x][msg.name] = null;
};

HN.Sheet.prototype.updater = function(time)
{
  var that = this;

  var loop = function(time) {
    $.getJSON("?updates="+time, function(data) {
      if( typeof data.timeout == "undefined" ) {
        var len = data.msgs.length;
        for( var i = 0; i < len; i++ ) {
          that.ensure(data.msgs[i]);
          that["handle_"+data.msgs[i].type](data.msgs[i]);
        }
        that.spreadsheet.layout.panes.refresh();
        that.spreadsheet.layout.selection.show_selection();
      }
      loop(data.time);
    });
  };
  loop(time);
};