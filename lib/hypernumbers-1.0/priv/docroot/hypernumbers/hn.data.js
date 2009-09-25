HN.Data = function(url, options)
{
  // max is set to zero to begin with
  this.max = {x: 0, y: 0};
  this.options = options;
  this.url     = url;

  this.pages  = {};
  this.functions = {};
  this.data   = {cell:{}, column:{}, row:{}, page:{}, styles:{}};

  this.load_data();
  this.update = function() {};
};

HN.Data.prototype.update_max = function(msg) {
  var ref;
  if (msg.name === "formula") {
    ref = HN.Util.parse_ref(msg.ref);
    if ((ref.x >= this.max.x) || (ref.y >= this.max.y)) {
      this.set_max();
    };
  };
};

HN.Data.prototype.set_max = function() {
  this.max.x = 0;
  this.max.y = 0;
  for (var y in this.data.cell ) {
    for (var x in this.data.cell[y] ) {
      if (this.data.cell[y][x].formula) {
        if (parseInt(x) > this.max.x ) {
          this.max.x = parseInt(x);
        }
        if (parseInt(y) > this.max.y ) {
          this.max.y = parseInt(y);
        }
      }
    }
  }
};

HN.Data.prototype.load_data = function()
{
  var that = this;

  $.ajax({
    url: this.url+"?attr", dataType: "json",
    success:   function(data) {
      that.data = data;
      // first up set the maximum extents
      that.set_max();
      HN.Util.dispatch(that.options.dataLoaded, []);
      that.updater(data.time);
    }
  });
};

HN.Data.prototype.abort_update = function() {
  //console.log("in HN.Data.abort_update: " + this.url);
  //var that = this;
  //console.log(this.update);
  //console.log(this.update.abort);
  this.update.abort();
};

HN.Data.prototype.updater = function(time)
{
  var that = this;
  var ajax;
  ajax = $.getJSON(that.url + "?updates="+time, function(data) {
    if( typeof data.timeout == "undefined" ) {
      for( var i = 0; i < data.msgs.length; i++ ) {
        that.ensure(data.msgs[i]);
        that["handle_"+data.msgs[i].type](data.msgs[i]);
        HN.Util.dispatch(that.options.message, []);
      }
      HN.Util.dispatch(that.options.update, []);
    }
    that.update = {};
    that.updater(data.time);
  });
  that.update = ajax;
};

HN.Data.prototype.load_pages = function()
{
  var that = this;

  $.ajax({
    url: this.url+"?pages", dataType: "json",
    success:   function(data) {
      that.pages = data;
      HN.Util.dispatch(that.options.pagesLoaded, []);
    }
  });
};

HN.Data.prototype.load_functions = function()
{
  var that = this,
       url = "/hypernumbers/fns_" + that.data.lang + ".json";

  $.ajax({
    url: url, dataType: "json",
    success:   function(data) {
      that.functions = data;
      HN.Util.dispatch(that.options.functionsLoaded, []);
    }
  });
};


HN.Data.prototype.set = function(type, index, key, val)
{
  this.ensure({reftype:type, ref:index});

  if(type == "row") {
   this.data.row[index][index][key] = val;
  }
};

HN.Data.prototype.remove = function(type, index, key)
{
  if(type == "row") {
    if( this.data.row[index] && this.data.row[index][index] ) {
      delete this.data.row[index][index][key];
    }
  }
};


HN.Data.prototype.ensure = function(msg)
{
  if( msg.reftype == "cell" ) {
    var coord = HN.Util.parse_ref(msg.ref);

    if( typeof this.data.cell[coord.y] == "undefined" ) {
      this.data.cell[coord.y] = {};
    }
    if( typeof this.data.cell[coord.y][coord.x] == "undefined" ) {
      this.data.cell[coord.y][coord.x] = {};
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

HN.Data.prototype.handle_refresh = function(msg)
{
  var that = this;

  $.ajax({
    url: this.url+"?attr", dataType: "json",
    success:   function(data) {
      that.data = data;
      that.set_max();
      HN.Util.dispatch(that.options.dataReloaded, [data]);
    }
  });
};

HN.Data.prototype.handle_style = function(msg)
{
  this.data.styles[msg.index] = msg.css;
};

HN.Data.prototype.handle_error = function(msg)
{
  var args = [HN.Util.parse_ref(msg.ref), msg.original];
  HN.Util.dispatch(this.options.formulaError, args);
};

HN.Data.prototype.change_column = function(msg)
{
  var index = HN.Util.from_b26(msg.ref);
  this.data.column[index][index][msg.name] = msg.value;
};

HN.Data.prototype.change_row = function(msg)
{
  this.data.row[msg.ref][msg.ref][msg.name] = msg.value;
};

HN.Data.prototype.change_cell = function(msg)
{
  var ind = HN.Util.parse_ref(msg.ref);
  this.data.cell[ind.y][ind.x][msg.name] = msg.value;
};

HN.Data.prototype.handle_change = function(msg)
{
  this["change_"+msg.reftype](msg);
  this.update_max(msg);
};

HN.Data.prototype.handle_delete = function(msg)
{
  this["delete_"+msg.reftype](msg);
  this.update_max(msg);
};

HN.Data.prototype.delete_cell = function(msg)
{
  var ind = HN.Util.parse_ref(msg.ref);
  delete this.data.cell[ind.y][ind.x][msg.name];
};

HN.Data.prototype.delete_row = function(msg)
{
  delete this.data.row[msg.ref][msg.ref][msg.name];
};

// This function is to 'poke' data into the main model of the page
// It is used on updates to a cell to push the 'new' value into the model
// and stop any display flicker
HN.Data.prototype.poke_cell_value = function(cell, value)
{
  this.ensure({reftype:"cell", ref:HN.Util.coord_to_ref(cell)});
  this.data.cell[cell.y][cell.x]['value'] = value;
};