HN.Data = function(options)
{
    var public = {};
    var rootPath = document.location.pathname;
    var opaque = {};
    var pages  = {};
    var updateRequest = null;
    var tpl    = $("body").attr("data-view");
    
    //
    public.readCell = function(path, y, x) {

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

    public.lookupCSS = function(path, y, x) {
        var obj = public.readCell(path, y, x);
        return obj.style && 
            public.lookupCSSIndex(path, obj.style);
    };
    
    public.lookupCSSIndex = function(path, i) {
        return pages[path].data.styles[i];
    };
    
    public.key = function(path, key) {
        return pages[path].data[key];
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
            
            if( updateRequest != null ) {
                updateRequest.abort();
                updateRequest = null;
            }
                
            updater(time);
        };

        pages[path] = new HN.PageData(path, options, fun, tpl);
    };

    // Load opaque data into path free data section (functions, pages)
    public.loadOpaque = function(path, name) {
        $.ajax({
            "url"      : path+"?"+name,
            "dataType" : "json",
            "success"  : function(data) {
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
            updateRequest = null;
            updater(data.time);
        };
        
        if( updateRequest === null ) {        
            var f     = function(path) { return path != rootPath; };
            var paths = $.grep(public.getLoadedUrls(), f).join(",");
            var args  = {"updates":time, "paths":paths, "view":tpl}; 
            updateRequest = $.get(rootPath, args, fun, "json");
        }
    };
    
    return public;
};

// This should be removed and its functionality split into 
// HN.Date for generic data loading / access and HN.Sheet for 
// spreadsheet specific data (maxima etc)
HN.PageData = function(url, options, loaded, tpl)
{
    var public = {};
    // max is set to zero to begin with
    public.max     = {"x": 0, "y": 0};
    public.options = options;
    public.path    = url;
    public.tpl     = tpl;
    
    public.data    = {
        "cell":{}, "column":{}, "row":{}, 
        "page":{}, "styles":{}
    };

    public.update_max = function(msg) {
        if (msg.name === "formula") {
            var ref = HN.Util.parse_ref(msg.ref);
            if ((ref.x >= public.max.x) || (ref.y >= public.max.y)) {
                public.set_max();
            };
        };
    };

    public.set_max = function() 
    {
        public.max.x = 0;
        public.max.y = 0;
        for (var y in public.data.cell ) {
            for (var x in public.data.cell[y] ) {
                if (public.data.cell[y][x].formula) {
                    if (parseInt(x) > public.max.x ) {
                        public.max.x = parseInt(x);
                    }
                    if (parseInt(y) > public.max.y ) {
                        public.max.y = parseInt(y);
                    }
                }
            }
        }
    };
    
    public.load_data = function(loaded)  {
        var path = public.path;
        var args = ( public.path == document.location.pathname )
            ? {"view" : public.tpl}
        : {"via"  : document.location.pathname, "view": public.tpl};
        
        $.ajax({
            "data"     : args, 
            "url"      : public.path,
            "dataType" : "json",
            "success"  : function(data) {

                public.data = data;
                public.set_max();

                HN.Util.dispatch(public.options.dataLoaded, [path]);
                if( loaded != null ) {
                    loaded(data.time);
                }
            }
        });
    };

    public.set = function(type, index, key, val) {

        public.ensure({reftype:type, ref:index});
        
        if(type == "row") {
            public.data.row[index][index][key] = val;
        }
    };

    public.remove = function(type, index, key) {
        if(type == "row") {
            if( public.data.row[index] && public.data.row[index][index] ) {
                delete public.data.row[index][index][key];
            }
        }
    };
    
    public.ensure = function(msg) {
        if( msg.reftype == "cell" ) {
            var coord = HN.Util.parse_ref(msg.ref);
            
            if( typeof public.data.cell[coord.y] == "undefined" ) {
                public.data.cell[coord.y] = {};
            }
            if( typeof public.data.cell[coord.y][coord.x] == "undefined" ) {
                public.data.cell[coord.y][coord.x] = {};
            }
        } else if( msg.reftype == "row" ) {
            
            var index = msg.ref;
            
            if( typeof public.data.row[index] == "undefined" ) {
                public.data.row[index] = {};
            }
            if( typeof public.data.row[index][index] == "undefined" ) {
                public.data.row[index][index] = {};
            }
        } else if( msg.reftype == "column" ) {
            var index = HN.Util.from_b26(msg.ref);
            
            if(typeof public.data.column[index] == "undefined") {
                public.data.column[index] = {};
            }
            if(typeof public.data.column[index][index] == "undefined") {
                public.data.column[index][index] = {};
            }
        }
    };

    public.handle_refresh = function(msg)
    {
        $.ajax({
            "url"      : public.path, 
            "dataType" : "json",
            "success"  : function(data) {
                public.data = data;
                public.set_max();
                HN.Util.dispatch(public.options.dataReloaded, [data]);
            }
        });
    };
    
    public.handle_style = function(msg) {
        public.data.styles[msg.index] = msg.css;
    };

    public.handle_error = function(msg) {
        var args = [HN.Util.parse_ref(msg.ref), msg.original];
        HN.Util.dispatch(public.options.formulaError, args);
    };

    public.change_column = function(msg) {
        var index = HN.Util.from_b26(msg.ref);
        public.data.column[index][index][msg.name] = msg.value;
    };

    public.change_row = function(msg) {
        public.data.row[msg.ref][msg.ref][msg.name] = msg.value;
    };

    public.change_cell = function(msg) {
        var ind = HN.Util.parse_ref(msg.ref);
        public.data.cell[ind.y][ind.x][msg.name] = msg.value;
    };

    public.handle_change = function(msg) {
        this["change_"+msg.reftype](msg);
        public.update_max(msg);
    };

    public.handle_delete = function(msg) {
        this["delete_"+msg.reftype](msg);
        public.update_max(msg);
    };

    public.delete_cell = function(msg) {
        var ind = HN.Util.parse_ref(msg.ref);
        delete public.data.cell[ind.y][ind.x][msg.name];
    };

    public.delete_row = function(msg) {
        delete public.data.row[msg.ref][msg.ref][msg.name];
    };
  
    // This function is to 'poke' data into the main model of the page
    // It is used on updates to a cell to push the 'new' value into the model
    // and stop any display flicker
    public.poke_cell_value = function(cell, value) {
        public.ensure({"reftype":"cell", "ref":HN.Util.coord_to_ref(cell)});
        public.data.cell[cell.y][cell.x]['value'] = value;
    };  

    public.load_data(loaded);

    return public;
};