HN.Data = function(options)
{
    var public = {};
    var opaque = {};
    var pages  = {};
    var update = null;
    
    //
    public.read = function(path, y, x) 
    {    
        if( typeof pages[path] == "undefined") { 
            public.addPage(path);
            return false;
        }
      
        if( x < 0 ) {
            for (var nx = pages[path].max.y; ny > 0; ny--) {
                if( pages[path].data.cell[ny] && pages[path].data.cell[ny][x] 
                    && pages[path].data.cell[ny][x].value ) {
                    if( x == -1 ) {
                        return pages[path].data.cell[ny][x];
                    } else {
                        x += 1;
                    }
                }
            }
            return false;        
        }

        if( y < 0 ) {

            // erm, pretty damn ineficient
            for (var ny = pages[path].max.y; ny > 0; ny--) {
                if( pages[path].data.cell[ny] && pages[path].data.cell[ny][x] 
                    && pages[path].data.cell[ny][x].value ) {
                    
                    if( y == -1 ) {
                        return pages[path].data.cell[ny][x];
                    } else {
                        y += 1;
                    }
                }
            }
            return false;
        }
  
        return pages[path].data.cell[y] 
            && pages[path].data.cell[y][x] || false;
    };

    //
    public.getPage = function(path) 
    {
        return pages[path].data;
    };

    //
    public.getPageData = function(path) 
    {
        return pages[path];
    };

    //
    public.addPage = function(path) 
    {    
        pages[path] = true;
        
        var fun = function(time) {
            
            if( update != null ) {
                update.abort();
                update = null;
            }
                
            updater(time);
        }
        
        pages[path] = new HN.PageData(path, options, fun);
    };

    public.loadOpaque = function(path, name) 
    {
        $.ajax({
            url      : path+"?"+name, 
            dataType : "json",
            success  : function(data) {
                opaque[name] = data;
                HN.Util.dispatch(options.opaqueLoaded, [name, data]);
            }
        });
    };

    public.getLoadedUrls = function() 
    {
        var paths = [];
        for (x in pages) {
            paths.push(x);
        }
        return paths;
    };
        
    //
    function updater(time) 
    {
        if( pages.length == 0 ) {
            return;
        }   
        
        var fun = function(data) {

            if( typeof data.timeout == "undefined" ) {
                for( var i = 0; i < data.msgs.length; i++ ) {
                    var page = pages[data.msgs[i].path];
                    page.ensure(data.msgs[i]);
                    page["handle_"+data.msgs[i].type](data.msgs[i]);
                    HN.Util.dispatch(options.message, []);
                }
                HN.Util.dispatch(options.update, []);
            }
            update = null;
            updater(data.time);
        };
        
        var pathstr = public.getLoadedUrls().join(",");
        
        if( update === null ) {
            update = $.get("/", {"updates":time, "path":pathstr}, fun, "json");
        }
    };

    return public;
};

HN.PageData = function(url, options, loaded)
{
    // max is set to zero to begin with
    this.max     = {x: 0, y: 0};
    this.options = options;
    this.path    = url;
    
    this.data      = {cell:{}, column:{}, row:{}, page:{}, styles:{}};

    this.load_data(loaded);
};

HN.PageData.prototype.update_max = function(msg) {
    if (msg.name === "formula") {
        var ref = HN.Util.parse_ref(msg.ref);
        if ((ref.x >= this.max.x) || (ref.y >= this.max.y)) {
            this.set_max();
        };
    };
};

HN.PageData.prototype.set_max = function() 
{
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

HN.PageData.prototype.load_data = function(loaded)
{
    var that = this;

    $.ajax({
        url: this.path, dataType: "json",
        success:   function(data) {
            that.data = data;
            that.data.path = that.path;
            // first up set the maximum extents
            that.set_max();
            HN.Util.dispatch(that.options.dataLoaded, []);
            if( loaded != null ) {
                loaded(data.time);
            }
        }
    });
};

HN.PageData.prototype.set = function(type, index, key, val)
{
    this.ensure({reftype:type, ref:index});
    
    if(type == "row") {
        this.data.row[index][index][key] = val;
    }
};

HN.PageData.prototype.remove = function(type, index, key)
{
    if(type == "row") {
        if( this.data.row[index] && this.data.row[index][index] ) {
            delete this.data.row[index][index][key];
        }
    }
};

HN.PageData.prototype.ensure = function(msg)
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

HN.PageData.prototype.handle_refresh = function(msg)
{
    var that = this;
    
    $.ajax({
        url: this.path, dataType: "json",
        success:   function(data) {
            that.data = data;
            that.set_max();
            HN.Util.dispatch(that.options.dataReloaded, [data]);
        }
    });
};

HN.PageData.prototype.handle_style = function(msg)
{
    this.data.styles[msg.index] = msg.css;
};

HN.PageData.prototype.handle_error = function(msg)
{
    var args = [HN.Util.parse_ref(msg.ref), msg.original];
    HN.Util.dispatch(this.options.formulaError, args);
};

HN.PageData.prototype.change_column = function(msg)
{
    var index = HN.Util.from_b26(msg.ref);
    this.data.column[index][index][msg.name] = msg.value;
};

HN.PageData.prototype.change_row = function(msg)
{
    this.data.row[msg.ref][msg.ref][msg.name] = msg.value;
};

HN.PageData.prototype.change_cell = function(msg)
{
    var ind = HN.Util.parse_ref(msg.ref);
    this.data.cell[ind.y][ind.x][msg.name] = msg.value;
};

HN.PageData.prototype.handle_change = function(msg)
{
    this["change_"+msg.reftype](msg);
    this.update_max(msg);
};

HN.PageData.prototype.handle_delete = function(msg)
{
    this["delete_"+msg.reftype](msg);
    this.update_max(msg);
};

HN.PageData.prototype.delete_cell = function(msg)
{
    var ind = HN.Util.parse_ref(msg.ref);
    delete this.data.cell[ind.y][ind.x][msg.name];
};

HN.PageData.prototype.delete_row = function(msg)
{
    delete this.data.row[msg.ref][msg.ref][msg.name];
};

// This function is to 'poke' data into the main model of the page
// It is used on updates to a cell to push the 'new' value into the model
// and stop any display flicker
HN.PageData.prototype.poke_cell_value = function(cell, value)
{
    this.ensure({reftype:"cell", ref:HN.Util.coord_to_ref(cell)});
    this.data.cell[cell.y][cell.x]['value'] = value;
};