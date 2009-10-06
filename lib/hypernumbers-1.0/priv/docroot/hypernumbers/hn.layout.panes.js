/**
 * @class HN.Layout.Panes
 *
 */
HN.Layout.Panes = function(layout, size)
{
  this.layout = layout;
  this.axis   = layout.axis_dom;
  this.panes  = HN.Util.id("panes");
  this.inner  = HN.Util.id("scroller");
  this.p      = [];
  this.rows   = size.rows;
  this.cols   = size.cols;
  this.s      = layout.s;
  this.bounds = {x1:0, y1:0, x2:0, y2:0};
  this.index  = {x:[], y:[]};

  this.draw_index(Y, true, false, 0, 0, 1,
                  (this.rows * HN.Layout.Pane.DEF_ROWS) + 1);
  this.draw_index(X, true, false, 0, 0, 1,
                  (this.cols * HN.Layout.Pane.DEF_COLS) + 1);

  this.build_cells({});

  for( var i = 0; i < this.p[0].length; i++ ) {
    this.bounds.x2 += this.p[0][i].width;
  }
};

HN.Layout.Panes.prototype.refresh = function()
{
  this.bounds.x2 = this.bounds.x1;
  this.bounds.y2 = this.bounds.y1;

  var p = this.p[0][0];
  var args = { row:p.row, col:p.col,
               top: parseInt(p.div.style.top, 10),
               left:parseInt(p.div.style.left, 10)};

  this.removeTiles();
  this.build_cells(args);

  var bottom = this.p[this.p.length-1][0].row + HN.Layout.Pane.DEF_ROWS;
  var right  = this.p[0][this.p[0].length-1].col + HN.Layout.Pane.DEF_COLS;

  this.axis.y.innerHTML = "";
  this.axis.x.innerHTML = "";
  this.draw_index(Y, true, false, args.top, 0, args.row, bottom);
  this.draw_index(X, true, false, 0, args.left, args.col, right);

  for( var i = 0; i < this.p[0].length; i++ ) {
    this.bounds.x2 += this.p[0][i].width;
  }

  this.layout.selection.show_cell();
};

HN.Layout.Panes.prototype.redraw = function()
{
  var p = this.p[0][0];
  p.row = (p.row < 1) ? 1 : p.row;
  p.col = (p.col < 1) ? 1 : p.col;

  this.s.offset.y = p.row;
  this.s.offset.x = p.col;
  var pos = this.s.cell_offset(p.row, p.col);
  var args = { row:p.row, col:p.col, top:pos.top, left:pos.left};

  this.bounds =  {x1:0, y1:0, x2:0, y2:0};

  this.inner.style.top   = -pos.top + "px";
  this.inner.style.left  = -pos.left + "px";
  this.axis.y.style.top  = -pos.top + "px";
  this.axis.x.style.left = -pos.left + "px";

  this.removeTiles();
  this.build_cells(args);

  var bottom = this.p[this.p.length-1][0].row + HN.Layout.Pane.DEF_ROWS;
  var right  = this.p[0][this.p[0].length-1].col + HN.Layout.Pane.DEF_COLS;

  this.axis.y.innerHTML = "";
  this.axis.x.innerHTML = "";
  this.draw_index(Y, true, false, pos.top, 0, args.row, bottom);
  this.draw_index(X, true, false, 0, pos.left, args.col, right);

  for( var i = 0; i < this.p[0].length; i++ ) {
    this.bounds.x2 += this.p[0][i].width;
  }

  this.layout.calcScrollbarPos(Y);
  this.layout.calcScrollbarPos(X);
};

HN.Layout.Panes.prototype.removeTiles = function()
{
  this.panes.innerHTML="";
};

HN.Layout.Panes.prototype.get_cell = function(y, x)
{
  var obj = this.get_pane(y, x);

  if( !obj ) {
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

  if(   y < 0 || x < 0
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

  if( up ) {
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

  for( var x = 0; x < len; x++ )
  {
    p[y][x] = row[x];

    var pane = p[y][x],
        orig = pane.div;

    pane = row[x];
    pane.row = rownum;
    pane.update_view();

    if( x == 0 ) {
      newheight = pane.height;

      top = up
        ? parseInt(p[y-1][0].div.style.top, 10) + p[y-1][0].height
        : parseInt(p[1][0].div.style.top, 10) - newheight;
    }

    pane.div.style.top = top+"px" ;
  }

  if( up ) {
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

  if( left ) {
    var   x = p[0].length-1,
     colnum = p[0][x].col+5,
        col = [];

    for( var i = 0; i < this.p.length; i++ ) {
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

     for( var i = 0; i < this.p.length; i++ ) {
       col.push(p[i][plen]);
       p[i].length -= 1;
       p[i].unshift([]);
     }
  }

  var oldwidth = col[0].width,
         lleft = null,
      newwidth = null,
           len = col.length;

  for( var y = 0; y < len; y++ ) {
    p[y][x] = col[y];

    var pane = p[y][x];
    var orig = pane.div;

    pane = col[y];
    //pane.div = orig.cloneNode(true);
    pane.col = colnum;
    pane.update_view();

    if( y == 0 ) {
      newwidth = pane.width;

      lleft = left
        ? parseInt(p[0][x-1].div.style.left, 10) + p[0][x-1].width
        : parseInt(p[0][1].div.style.left, 10) - newwidth;
    }

    pane.div.style.left = lleft+"px" ;
    //orig.parentNode.replaceChild(pane.div,orig);
  }

  if( left ) {
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

  for( var i = start; i < end; i++ )
  {
    if( del ) {
      if( append ) {
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
    index.push(style+'"><div class="handle"></div>'
               +xy.to_index(i)+'</div>');

    if( xy.str == "y" ) {
      top  += val[0];
    } else {
      left += val[1];
    }
  }

  if( append ) {
     axis.innerHTML += index.join("");
  } else {
     axis.innerHTML = index.join("") + axis.innerHTML;
  }
};

HN.Layout.Panes.prototype.build_cells = function(o)
{
  o.row  = (typeof o.row == 'undefined') ? 1 : o.row;
  o.col  = (typeof o.col == 'undefined') ? 1 : o.col;
  o.top  = (typeof o.top == 'undefined') ? 0 : o.top;
  o.left = (typeof o.left == 'undefined') ? 0 : o.left;

  var top = o.top;

  for( var y = 0; y < this.rows; y++ ) {
    var row = y * 8,
       left = o.left;

    this.p[y] = [];

    for( var x = 0; x < this.cols; x++ ) {
      this.p[y][x] = new HN.Layout.Pane(this.s, row+o.row, (x*5)+o.col);
      this.p[y][x].div.style.top  = top + "px";
      this.p[y][x].div.style.left = left + "px";
      this.panes.appendChild(this.p[y][x].div);

      left += this.p[y][x].width;
    }

    top += this.p[y][0].height;
    this.bounds.y2 += this.p[y][0].height;
  }
};
