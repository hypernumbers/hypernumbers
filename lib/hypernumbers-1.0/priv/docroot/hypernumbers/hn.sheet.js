var HN = {};

var Y = {str:"y", coord:"top", dimension:"height", to_index: function(x) { return x; }};
var X = {str:"x", coord:"left",dimension:"width",
         to_index: function(x) { return HN.Util.to_b26(x); }};
//
//  SpreadSheet Container
//
HN.SpreadSheet = function(root)
{
  this.root   = HN.Util.id(root);
  this.sheet  = 0;
  this.sheets = [];
  this.layout = null;

  this.add_sheet(document.location.pathname+"?attr");
  this.layout = new HN.Layout(this,name,this.cur_sheet());
};

HN.SpreadSheet.prototype.cur_sheet = function()
{
  return this.sheets[this.sheet];
};

HN.SpreadSheet.prototype.add_sheet = function(name)
{
  this.sheets[this.sheets.length] = new HN.Sheet(this, name);
};

//
//  Sheet Container
//
HN.Sheet = function(spreadsheet, url)
{
  this.spreadsheet = spreadsheet;
  this.url    = url;;
  this.offset = {"x":1,"y":1};
  this.max    = {"x":26, "y":50};
  this.height = 0;
  this.width  = 0;

  this.data   = {"cells":{}};

  this.cells  = {};
  this.rows   = {};
  this.cols   = {};

  this.load_data();
  this.calc_size();
};

HN.Sheet.CELL_WIDTH  = 80;
HN.Sheet.CELL_HEIGHT = 20;

HN.Sheet.prototype.set_cell = function(coord, name, value)
{
  if(typeof this.data.cells[coord[0]] == "undefined") {
    this.data.cells[coord[0]] = {};
  }
  if(typeof this.data.cells[coord[0]][coord[1]] == "undefined") {
    this.data.cells[coord[0]][coord[1]] = {};
  }

  if(name == "value" || name == "formula") {
    this.data.cells[coord[0]][coord[1]][name] = value.text();
  }

  if(name == "value") {
    var cell = this.spreadsheet.layout.panes.get_cell(coord[0], coord[1]);

    if(cell) {
      cell.innerHTML = value.text();
      cell.style.zIndex = coord[0] + coord[1];
    }
  }
};

HN.Sheet.prototype.handle_xml_ref = function(ref)
{
  var type = ref.attr("type");

  if(type == "cell") {

    var addr = HN.Util.parse_ref(ref.attr("ref"));
    var name = ((ref.children()[0]).nodeName).toLowerCase();

    this.set_cell(addr, name, ref);
  }
};

HN.Sheet.prototype.load_data = function()
{
  var that = this;

  var fun = function(data)
  {
	$(data).find("ref").each( function()
    {
      that.handle_xml_ref($(this));
    });
  };

  $.ajax({
	type: "GET",
    url: this.url,
    dataType: "xml",
    success: fun
  });
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
  return this.data.cells[y] && this.data.cells[y][x]
    || {value:""};
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
	  left += obj.offsetLeft;
    } while ((obj = obj.offsetParent));
  }
  return left;
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

HN.Util.parse_ref = function(ref)
{
    var x = HN.Util.from_b26((ref.match(/[a-z]+/i)[0]).toLowerCase());
    var y = parseInt(ref.match(/[0-9]+/)[0]);

  return [y, x];
};
