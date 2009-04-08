var HN = {};

var Y = {str:"y", coord:"top", dimension:"height",
         to_index: function(x) { return x; }};
var X = {str:"x", coord:"left",dimension:"width",
         to_index: function(x) { return HN.Util.to_b26(x); }};

HN.UP = 1;
HN.DOWN = 2;
HN.LEFT = 3;
HN.RIGHT = 4;

//
//  SpreadSheet Container
//
HN.SpreadSheet = function(root)
{
  this.root   = HN.Util.id(root);
  this.sheet  = 0;
  this.sheets = [];
  this.layout = null;

  this.do_auth();
  this.add_sheet(document.location.pathname+"?attr");
  this.layout = new HN.Layout(this,name,this.cur_sheet());
};

HN.SpreadSheet.prototype.do_auth = function()
{
  var cookie = HN.Util.readCookie("auth");
  if( cookie !== null ) {
    var el = HN.Util.id("logout");
    HN.Util.id("loggedin").style.display = "block";
    HN.Util.id("home").setAttribute("href", "/"+cookie.split(":")[1]+"/");
    HN.Util.addEvent(el, "mousedown", function(e) {
      HN.Util.eraseCookie("auth");
      window.location.reload( true );
    });
  } else {
    HN.Util.id("anonymous").style.display = "block";
  }
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

HN.Sheet.prototype.set_col = function(coord, name, value)
{
  if(typeof this.data.cols[coord] == "undefined") {
    this.data.column[coord][coord] = {};
  }

  this.data.cols[coord][name] = parseInt(value.text());
};

HN.Sheet.prototype.load_data = function()
{
  var that = this,
    layout = this.spreadsheet.layout;

  var fun = function(data)
  {
    that.data = data;
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

HN.Sheet.prototype.ensure = function(coord)
{
  if(typeof this.data.cell[coord.y] == "undefined") {
    this.data.cell[coord.y] = {};
  }
  if(typeof this.data.cell[coord.y][coord.x] == "undefined") {
    this.data.cell[coord.y][coord.x] = {};
  }

  if(coord.y > this.max.y) {
    this.max.y = coord.y;
    this.spreadsheet.layout.calc_scrollbar(Y);
    this.calc_size();
  }
  if(coord.x > this.max.x) {
    this.max.x = coord.x;
    this.spreadsheet.layout.calc_scrollbar(X);
    this.calc_size();
  }
};

HN.Sheet.prototype.handle_style = function(msg)
{
  this.data.styles[msg.index] = msg.css;
  this.spreadsheet.layout.panes.refresh();
};

HN.Sheet.prototype.handle_column = function(msg)
{
  var index = HN.Util.from_b26(msg.ref);

  if(typeof this.data.column[index] == "undefined") {
    this.data.column[index] = {};
  }
  if(typeof this.data.column[index][index] == "undefined") {
    this.data.column[index][index] = {};
  }

  this.data.column[index][index][msg.name] = msg.value;
  this.spreadsheet.layout.panes.refresh();
  this.spreadsheet.layout.selection.show_selection();
};

HN.Sheet.prototype.handle_row = function(msg)
{
  var index = msg.ref;

  if(typeof this.data.row[index] == "undefined") {
    this.data.row[index] = {};
  }
  if(typeof this.data.row[index][index] == "undefined") {
    this.data.row[index][index] = {};
  }

  this.data.row[index][index][msg.name] = msg.value;
  this.spreadsheet.layout.panes.refresh();
  this.spreadsheet.layout.selection.show_selection();
};

HN.Sheet.prototype.handle_error = function(msg)
{
  var ind = HN.Util.parse_ref(msg.ref),
    error = HN.Util.id("error"),
   select = this.spreadsheet.layout.selection;

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

HN.Sheet.prototype.handle_cell = function(msg)
{
  var ind = HN.Util.parse_ref(msg.ref);
  this.ensure(ind);

  if(msg.type == "change" && msg.reftype == "cell") {
    this.data.cell[ind.y][ind.x][msg.name] = msg.value;
  } else if (msg.type == "delete") {
    this.data.cell[ind.y][ind.x][msg.name] = null;
  }

  if(msg.reftype == "cell" && msg.name == "value") {

    var layout = this.spreadsheet.layout,
          cell = layout.panes.get_cell(ind.y, ind.x);

    if(cell) {

      var sel = this.spreadsheet.layout.selection;
      var val = (msg.type == "delete") ? "" : msg.value;

      if(sel.cell.y == ind.y && sel.cell.x == ind.x) {
        sel.input.value = val;
      }
      cell.innerHTML = val;
      cell.style.zIndex = (msg.type == "delete")
        ? 0 : ind.y + ind.x;
    }
  }

  if( msg.name == "style" ) {
    //if( !this.spreadsheet.layout.selection.is_editing() ) {}
    var obj = this.spreadsheet.layout.panes.get_pane(ind.y, ind.x);
    if( obj ) {
      obj.pane.update_view();
    }
  }
};

HN.Sheet.prototype.updater = function(time)
{
  var that = this;

  var loop = function(time) {
    $.getJSON("?updates="+time, function(data) {
      try {
        var len = data.msgs.length;
        for( var i = 0; i < len; i++ ) {
          if( data.msgs[i].type == "style" ) {
            that.handle_style(data.msgs[i]);
          } else if( data.msgs[i].type == "error" ) {
            that.handle_error(data.msgs[i]);
          } else if ( data.msgs[i].reftype == "cell" ) {
            that.handle_cell(data.msgs[i]);
          } else if ( data.msgs[i].reftype == "column" ) {
            that.handle_column(data.msgs[i]);
          } else if ( data.msgs[i].reftype == "row" ) {
            that.handle_row(data.msgs[i]);
          }
        }
      } catch(err) {
        console.log(err);
      }
      loop(data.time);
    });
  };
  loop(time);
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
  return cell.charCodeAt(0)-64;
};

HN.Util.parse_ref = function(ref)
{
  return {
    x:HN.Util.from_b26((ref.match(/[a-z]+/i)[0])),
    y:parseInt(ref.match(/[0-9]+/)[0])
  };
};

HN.Util.coord_to_ref = function(cell)
{
  return HN.Util.to_b26(cell.x)+cell.y;
};

HN.Util.clone = function(obj)
{
  var newObj = (obj instanceof Array) ? [] : {};
  for (i in obj) {
    if (i == 'clone') {
      continue;
    }
    if (obj[i] && typeof obj[i] == "object") {
      newObj[i] = obj[i].clone();
    } else {
      newObj[i] = obj[i];
    }
  }
  return newObj;
};

HN.Util.range_to_url = function(range)
{
  return document.location.pathname
    + HN.Util.coord_to_ref({"x":range.x1, "y":range.y1})+":"
    + HN.Util.coord_to_ref({"x":range.x2, "y":range.y2})
    + "?attr";
};

HN.Util.range_to_str = function(range)
{
  return HN.Util.coord_to_ref({"x":range.x1, "y":range.y1})+":"
    + HN.Util.coord_to_ref({"x":range.x2, "y":range.y2});
};


HN.Util.postCell = function(cell, json)
{
  var url = document.location.pathname
    + HN.Util.coord_to_ref(cell)+"?attr";

  $.post(url, JSON.stringify(json));
};

HN.Util.postColumn = function(start, end, json)
{
  var url = document.location.pathname + HN.Util.to_b26(start)
    + ":" + HN.Util.to_b26(end) +"?attr";
  $.post(url, JSON.stringify(json));
};

HN.Util.postRow = function(start, end, json)
{
  var url = document.location.pathname + start + ":" + end +"?attr";
  $.post(url, JSON.stringify(json));
};

HN.Util.postRange = function(range, json)
{
  $.post(HN.Util.range_to_url(range), JSON.stringify(json));
};

HN.Util.readCookie = function(name)
{
  var nameEQ = name + "=";
  var ca = document.cookie.split(';');
  for(var i=0;i < ca.length;i++) {
	var c = ca[i];
	while (c.charAt(0)==' ') c = c.substring(1,c.length);
	if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
  }
  return null;
};

HN.Util.createCookie = function(name,value,days)
{
	if (days) {
		var date = new Date();
		date.setTime(date.getTime()+(days*24*60*60*1000));
		var expires = "; expires="+date.toGMTString();
	}
	else var expires = "";
	document.cookie = name+"=\""+value+"\""+expires+"; path=/";
};

HN.Util.eraseCookie = function(name) {
	HN.Util.createCookie(name,"",-1);
};

