HN.Data = function(options) {
    
    var api           = {},
        rootPath      = document.location.pathname,
        opaque        = {},
        pages         = {},
        updateRequest = null,
        tpl           = $("body").attr("data-view");
    
    //
    api.readCell = function(path, y, x) {

        if( typeof pages[path] == "undefined") { 
            api.addPage(path);
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

    api.lookupCSS = function(path, y, x) {
        var obj = api.readCell(path, y, x);
        return obj.style && 
            api.lookupCSSIndex(path, obj.style);
    };
    
    api.lookupCSSIndex = function(path, i) {
        return pages[path].data.styles[i];
    };
    
    api.key = function(path, key) {
        return pages[path] && pages[path].data[key];
    };

    api.writeKey = function(path, key, data) {
        if( !pages[path] ) { 
            pages[path] = {};
        }
        if( !pages[path].data ) { 
            pages[path].data = {};
        }
        pages[path].data[key] = data;
    };

    api.deleteKey = function(path, key) {
        if( pages[path] && pages[path].data ) {
            delete pages[path].data[key];
        }
    };


    //
    api.getPage = function (path) {
        return pages[path].data;
    };

    //
    api.getPageData = function(path)  {
        return pages[path];
    };

    //
    api.addPage = function(path) {    

        pages[path] = true;
        
        var fun = function(time) {
            
            if( updateRequest != null ) {
                updateRequest.abort();
                updateRequest = null;
            }
                
            updater(time);
        };

        pages[path] = new HN.PageData(path, options, fun, tpl, api);
    };

    api.removePage = function(path) {
        delete pages[path];
    };
    
    // Load opaque data into path free data section (functions, pages)
    // (non page specific)
    api.loadOpaque = function(path, name) {
        $.ajax({
            "url"      : path+"?"+name,
            "dataType" : "json",
            "success"  : function(data) {
                opaque[name] = data;
                HN.Util.dispatch(options.opaqueLoaded, [name, data]);
            }
        });
    };
    
    api.getLoadedUrls = function() {
        var paths = [];
        for (x in pages) {
            // err, dirty
            if( typeof pages[x].max != "undefined" ) { 
                paths.push(x);
            }
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

            if( !data ) { 
                return;
            }

            if( typeof data.timeout === "undefined" ) {
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
            var paths = $.grep(api.getLoadedUrls(), f).join(",");
            var args  = {"updates":time, "paths":paths, "view":tpl};
            setTimeout( function () { 
                updateRequest = $.get(rootPath, args, fun, "json");
            }, 0);
        }
    };

    $(window).unload( function() {
        if( updateRequest != null ) {
            updateRequest.abort();
            updateRequest = null;
        }
    });
    
    return api;
};

// This should be removed and its functionality split into 
// HN.Date for generic data loading / access and HN.Sheet for 
// spreadsheet specific data (maxima etc)
HN.PageData = function(url, options, loaded, tpl, parent)
{
    var api = {};
    // max is set to zero to begin with
    api.max     = {"x": 0, "y": 0};
    api.options = options;
    api.path    = url;
    api.tpl     = tpl;
    
    api.data    = {
        "cell":{}, "column":{}, "row":{}, 
        "page":{}, "styles":{}
    };

    api.update_max = function(msg) {
        if (msg.name === "formula") {
            var ref = HN.Util.parse_ref(msg.ref);
            if ((ref.x >= api.max.x) || (ref.y >= api.max.y)) {
                api.set_max();
            };
        };
    };
    
    api.set_max = function() 
    {
        api.max.x = 0;
        api.max.y = 0;
        for (var y in api.data.cell ) {
            for (var x in api.data.cell[y] ) {
                if (api.data.cell[y][x].formula) {
                    if (parseInt(x) > api.max.x ) {
                        api.max.x = parseInt(x);
                    }
                    if (parseInt(y) > api.max.y ) {
                        api.max.y = parseInt(y);
                    }
                }
            }
        }
    };
    
    api.load_data = function(loaded)  {
        
        var path = api.path,
        args = ( api.path === document.location.pathname )
            ? {"view" : api.tpl}
            : {"via"  : document.location.pathname, "view": api.tpl};
        
        $.ajax({
            "data"     : args, 
            "url"      : api.path,
            "dataType" : "json",
            "success"  : function(data) {

                api.data = data;
                api.set_max();

                HN.Util.dispatch(api.options.dataLoaded, [path]);
                if( loaded != null ) {
                    loaded(data.time);
                }
            }, 
            "error"    : function(data) {
                parent.removePage(path);
                if( data.status == 401 ) {
                    HN.Util.dispatch(api.options.authError, [path]);
                }
            }
        });
    };

    api.set = function(type, index, key, val) {

        api.ensure({"reftype":type, "ref":index});
        
        if(type == "row") {
            api.data.row[index][index][key] = val;
        }
    };

    api.remove = function(type, index, key) {
        if(type == "row") {
            if( api.data.row[index] && api.data.row[index][index] ) {
                delete api.data.row[index][index][key];
            }
        }
    };
    
    api.ensure = function(msg) {
        if( msg.reftype == "cell" ) {
            var coord = HN.Util.parse_ref(msg.ref);
            
            if( typeof api.data.cell[coord.y] == "undefined" ) {
                api.data.cell[coord.y] = {};
            }
            if( typeof api.data.cell[coord.y][coord.x] == "undefined" ) {
                api.data.cell[coord.y][coord.x] = {};
            }
        } else if( msg.reftype == "row" ) {
            
            var index = msg.ref;
            
            if( typeof api.data.row[index] == "undefined" ) {
                api.data.row[index] = {};
            }
            if( typeof api.data.row[index][index] == "undefined" ) {
                api.data.row[index][index] = {};
            }
        } else if( msg.reftype == "column" ) {
            var index = HN.Util.from_b26(msg.ref);
            
            if(typeof api.data.column[index] == "undefined") {
                api.data.column[index] = {};
            }
            if(typeof api.data.column[index][index] == "undefined") {
                api.data.column[index][index] = {};
            }
        }
    };

    api.handle_refresh = function(msg)
    {
        $.ajax({
            "url"      : api.path, 
            "dataType" : "json",
            "success"  : function(data) {
                api.data = data;
                api.set_max();
                HN.Util.dispatch(api.options.dataReloaded, [data]);
            }
        });
    };
    
    api.handle_style = function(msg) {
        api.data.styles[msg.index] = msg.css;
    };

    api.handle_error = function(msg) {
        var args = [HN.Util.parse_ref(msg.ref), msg.original];
        HN.Util.dispatch(api.options.formulaError, args);
    };

    api.change_column = function(msg) {
        var index = HN.Util.from_b26(msg.ref);
        for (k in msg.attrs) 
            api.data.column[index][index][k] = msg.attrs[k];
    };

    api.change_row = function(msg) {
        for (k in msg.attrs) 
            api.data.row[msg.ref][msg.ref][k] = msg.attrs[k];
    };

    api.change_cell = function(msg) {
        var ind = HN.Util.parse_ref(msg.ref);
        cleanse_values(msg.attrs);
        for (k in msg.attrs) {
            api.data.cell[ind.y][ind.x][k] = msg.attrs[k];
        }
        if (msg.name === "value") 
            HN.Util.dispatch(api.options.cellValueChanged,
                             [{"x":ind.x, "y":ind.y, "path":msg.path}]);
    };

    api.handle_change = function(msg) {
        this["change_"+msg.reftype](msg);
        api.update_max(msg);
    };

    api.handle_delete = function(msg) {
        this["delete_"+msg.reftype](msg);
        api.update_max(msg);
    };

    api.delete_cell = function(msg) {
        var ind = HN.Util.parse_ref(msg.ref);
        delete api.data.cell[ind.y][ind.x][msg.name];
        
        if (msg.name === "merge") {
            HN.Util.dispatch(api.options.dataReloaded, [api.data]);
        } else {
            HN.Util.dispatch(api.options.cellValueChanged,
                             [{"x":ind.x, "y":ind.y, "path":msg.path}]);
        }
    
    };

    api.delete_row = function(msg) {
        delete api.data.row[msg.ref][msg.ref][msg.name];
    };
  
    // This function is to 'poke' data into the main model of the page
    // It is used on updates to a cell to push the 'new' value 
    // into the model and stop any display flicker
    api.pokeCellValue = function(cell, value) {
        api.ensure({
            "reftype" : "cell", 
            "ref"     : HN.Util.coord_to_ref(cell)
        });
        api.data.cell[cell.y][cell.x]["value"] = value;
    };  
    
    api.pokeRowHeight = function(row, height) {
        api.ensure({
            "reftype" : "row", 
            "ref"     : row
        });
        api.data.row[row][row]['height'] = height;
    };  

    function cleanse_values(obj) {
        if (! obj["formula"]) {
            obj["formula"] = "";
            obj["rawvalue"] = "";
            obj["value"] = ""
        }
    }
    
    api.load_data(loaded);

    return api;
};