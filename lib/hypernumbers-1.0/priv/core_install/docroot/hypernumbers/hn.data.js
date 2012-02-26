/* jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 10000 */
/*global HN: false, hn: false, $: false, alert: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false, updater: false, lang: false, toolbar: false  */
HN.Data = function (options) {
    
    var api      = {},
        ref      = HN.Util.parseRef(document.location.pathname.toLowerCase()),
        rootPath = ref.path,
        opaque   = {},
        styles   = null,
        pages    = {},
        updateRequest = null,
        tpl = $("body").attr("data-view");
    
    // Set up the API0
    api.refreshView = function (path) {
        pages[path].handle_refresh();
    };
    
    api.readViews = function (path) {
        return pages[path].data.views;
    };
    
    api.unpokeGroup = function (path, view, group) {
        var g = pages[path].data.permissions.views[view].groups;
        pages[path].data.permissions.views[view].groups = HN.Util.remove(g, group);
    };

    api.pokeGroup = function (path, view, group) {
        pages[path].data.permissions.views[view].groups.push(group);
    };

    api.pokePublic = function (path, view, everyone) {
        pages[path].data.permissions.views[view].everyone = everyone;
    };

    api.pokeChampion = function (path, champion) {
        pages[path].data.permissions.champion = champion;
    };

    api.readPermissions = function (path) {
        
        var ret;
        
        if (typeof pages[path] === "undefined") { 
            api.addPage(path);
            return false;
        }
        ret = pages[path].data.permissions;
        return ret;
    
    };
    
    api.readDropDown = function (path, y, x)
    {
        if (typeof pages[path] === "undefined") { 
            api.addPage(path);
            return "";
        }
        
        return pages[path].data.cell[y] && 
            pages[path].data.cell[y][x] && 
            pages[path].data.cell[y][x].input &&
            pages[path].data.cell[y][x].input.select && 
            pages[path].data.cell[y][x].input.select.toString() || 
            pages[path].data.cell[y] && 
            pages[path].data.cell[y][x] && 
            pages[path].data.cell[y][x].input &&
            pages[path].data.cell[y][x].input.dynamic_select ||
            "";
    };

    api.readCell = function (path, y, x) {

        if (typeof pages[path] === "undefined") { 
            api.addPage(path);
            return false;
        }
        
        return pages[path].data.cell[y] &&
            pages[path].data.cell[y][x] || false;
    };

    api.lookupCSS = function (path, y, x) {
        var obj = api.readCell(path, y, x);
        return obj.style && 
            api.lookupCSSIndex(path, obj.style);
    };
    
    api.lookupCSSIndex = function (path, i) {
        return styles[i];
    };
    
    api.key = function (path, key) {
        return pages[path] && pages[path].data[key];
    };

    api.writeKey = function (path, key, data) {
        if (!pages[path]) { 
            pages[path] = {};
        }
        if (!pages[path].data) { 
            pages[path].data = {};
        }
        pages[path].data[key] = data;
    };

    api.deleteKey = function (path, key) {
        if (pages[path] && pages[path].data) {
            delete pages[path].data[key];
        }
    };

    //
    api.getPage = function (path) {
        return pages[path].data;
    };

    //
    api.getPageData = function (path)  {
        return pages[path];
    };

    //
    api.addPage = function (path) {    

        pages[path] = true;
        
        var fun = function (time) {
            
            if (updateRequest !== null) {
                clientAbort = true;
                updateRequest.abort();
                clientAbort = false;
                updateRequest = null;
            }
                
            updater(time);
        };

        pages[path] = new HN.PageData(path, options, fun, tpl, api);
    };

    api.removePage = function (path) {
        delete pages[path];
    };
    
    // Load opaque data into path free data section (functions, pages)
    // (non page specific) - should delete?
    /*api.loadOpaque = function (path, name) {
        $.ajax({
            "url"      : path+"?"+name,
            "dataType" : "json",
            "success"  : function (data) {
                opaque[name] = data;
                HN.Util.dispatch(options.opaqueLoaded, [name, data]);
            }
        });
    };*/
    
    api.getLoadedUrls = function () {
        var paths = [],
            x;
        for (x in pages) {
            // err, dirty
            if (typeof pages[x].max !== "undefined") { 
                paths.push(x);
            }
        }
        return paths;
    };

    api.stylesSet = function () {
        return styles !== null;
    };

    api.setStyles = function (obj) {
        styles = obj;
    };
        
    //
    function updater(time) {

        var siteSuccessFun, dataSuccessFun, completeFun, 
        f, paths, args;

        siteSuccessFun = function (data) {
            
            hn.pages = data.pages;
            hn.groups = data.groups;
            hn.is_admin = data.is_admin;
            lang = data.lang;
            hn.templates = data.templates,
            hn.ctrlPanel.loadTemplates(data.templates);
            hn.ctrlPanel.loadMaps(data.maps);
            hn.pagesPanel.loadTemplates(data.templates);
            toolbar.loadViews();
        };
        
        if (pages.length === 0) {
            return;
        }   
        
        dataSuccessFun = function (data) { 

            var i, len, msg, page, view;
            
            view = $("body").attr("data-view");

            if (!data) { 
                return;
            }

            if (typeof data.timeout === "undefined") {

                for (i = 0, len = data.msgs.length; i < len; i = i + 1) {

                    msg = data.msgs[i];
                    
                    if (msg.type === "style") {
                        styles[msg.index] = msg.css;
                    } else if (msg.type === "site_refresh") {
                        if (view === "spreadsheet") {
                            hn.sitedata.loadSiteData(siteSuccessFun);
                        }
                        // web, wiki and table views don't hold site data
                    } else if (msg.type === "pages_refresh") {
                        hn.refresh_pages = true;
                    } else {
                        page = pages[msg.path];
                        page.ensure(msg);
                        page["handle_" + msg.type](msg);
                    }
                }
                HN.Util.dispatch(options.update, []);
            }
            updateRequest = null;
            updater(data.time);
        };

        completeFun = function(data, status) {
            var Fun;
            if (status !== "success" && status !== "abort") {
                Fun = function () {
                    HN.Util.showDeadBox("Connection with the server " +
                                        "has been lost. Please refresh " +
                                        "the page in your browser");
                };
                setTimeout(Fun, 300);
            }
        };
        
        if (updateRequest === null) {
            f = function (path) {
                return path !== rootPath;
            };
            paths = $.grep(api.getLoadedUrls(), f).join(",");
            args  = {"updates": time, "paths": paths, "view": tpl};
            setTimeout(function () { 
                //updateRequest = $.get(rootPath, args, fun, "json");
                updateRequest = $.ajax({"data"     : args,
                                        "url"      : rootPath,
                                        "dataType" : "json",
                                        "success"  : dataSuccessFun,
                                        "complete" : completeFun});
            }, 0);
        }
    }
    
    return api;
};

// This should be removed and its functionality split into 
// HN.Data for generic data loading / access and HN.Sheet for 
// spreadsheet specific data (maxima etc)
HN.PageData = function (url, options, loaded, tpl, parent)
{
    var api = {};

    // Define some functions
    function cleanse_values(obj) {
        if (! obj.formula) {
            obj.formula  = "";
            obj.rawvalue = "";
            obj.value    = "";
        }
        
        obj.merge = obj.merge || null;
        obj.style = obj.style || null;
    }

    // max is set to zero to begin with
    api.max     = {"x": 0, "y": 0};
    api.options = options;
    api.path    = url;
    api.tpl     = tpl;
    api.data    = {
        "cell":   {},
        "column": {},
        "row":    {},
        "page":   {}
    };
    
    api.groups = null;
    api.views  = [];

    api.update_max = function (msg) {
        if (msg.name === "formula") {
            var ref = HN.Util.parse_ref(msg.ref);
            if ((ref.x >= api.max.x) || (ref.y >= api.max.y)) {
                api.set_max();
            }
        }
    };
    
    api.set_max = function () {
        var y, x;
        api.max.x = 0;
        api.max.y = 0;
        for (y in api.data.cell) {
            if (api.data.cell.hasOwnProperty(y)) {
                for (x in api.data.cell[y]) {
                    if (api.data.cell[y].hasOwnProperty(x)) {
                        if (api.data.cell[y][x].formula) {
                            if (parseInt(x, 10) > api.max.x) {
                                api.max.x = parseInt(x, 10);
                            }
                            if (parseInt(y, 10) > api.max.y) {
                                api.max.y = parseInt(y, 10);
                            }
                        }
                    }
                }
            }
        }
    };
    
    api.load_data = function (loaded)  {
        
        var path = api.path,
            ref = HN.Util.parseRef(document.location.pathname.toLowerCase()),
            args = (api.path === ref.path) ?
              {"view" : api.tpl} :
              {"via"  : ref.path, 
               "view" : api.tpl};
        
        $.ajax({
            "data"     : args, 
            "url"      : api.path,
            "dataType" : "json",
            "success"  : function (data) {
                api.groups = data.groups;
                api.data = data;
                api.views = data.views;
                if (!parent.stylesSet()) {
                    parent.setStyles(data.styles);
                }
                api.set_max();

                HN.Util.dispatch(api.options.dataLoaded, [path]);
                if (loaded !== null) {
                    loaded(data.time);
                }
            }, 
            "error"    : function (data) {
                parent.removePage(path);
                if (data.status === 401) {
                    HN.Util.dispatch(api.options.authError, [path]);
                }
                }
        });
    };

    api.set = function (type, index, key, val) {

        api.ensure({"reftype": type, "ref": index});
        
        if (type === "row") {
            api.data.row[index][index][key] = val;
        }
    };

    api.remove = function (type, index, key) {
        if (type === "row") {
            if (api.data.row[index] && api.data.row[index][index]) {
                delete api.data.row[index][index][key];
            }
        }
    };
    
    api.ensure = function (msg) {

        var coord, index;
        
        if (msg.reftype === "cell") {
            coord = HN.Util.parse_ref(msg.ref);
            
            if (typeof api.data.cell[coord.y] === "undefined") {
                api.data.cell[coord.y] = {};
            }
            if (typeof api.data.cell[coord.y][coord.x] === "undefined") {
                api.data.cell[coord.y][coord.x] = {};
            }
        } else if (msg.reftype === "row") {
            
            index = msg.ref;
            
            if (typeof api.data.row[index] === "undefined") {
                api.data.row[index] = {};
            }
            if (typeof api.data.row[index][index] === "undefined") {
                api.data.row[index][index] = {};
            }
        } else if (msg.reftype === "column") {
            index = HN.Util.from_b26(msg.ref);
            
            if (typeof api.data.column[index] === "undefined") {
                api.data.column[index] = {};
            }
            if (typeof api.data.column[index][index] === "undefined") {
                api.data.column[index][index] = {};
            }
        }
    };

    api.handle_refresh = function (msg) {
        $.ajax({
            "url"      : api.path, 
            "dataType" : "json",
            "success"  : function (data) {
                api.data = data;
                api.set_max();
                HN.Util.dispatch(api.options.dataReloaded, [data]);
            }
        });
    };
    
    api.change_column = function (msg) {
        var index = HN.Util.from_b26(msg.ref),
            k, x, y;
        for (k in msg.attrs) {
            if (msg.attrs.hasOwnProperty(k)) {
                api.data.column[index][index][k] = msg.attrs[k];
            }
        }
        for (y in api.data.cell) {
            if (api.data.cell.hasOwnProperty(y)) {
                for (x in api.data.cell[y]) {
                    if (api.data.cell[y].hasOwnProperty(x)) {
                        delete api.data.cell[y][x].cachedHeight;
                    }
                }
            }
        }
    };

    api.change_row = function (msg) {
        var k;
        for (k in msg.attrs) {
            if (msg.attrs.hasOwnProperty(k)) {
                api.data.row[msg.ref][msg.ref][k] = msg.attrs[k];
            }
        }
    };

    api.change_cell = function (msg) {
        var ind = HN.Util.parse_ref(msg.ref),
        k;
        
        cleanse_values(msg.attrs);
        for (k in msg.attrs) {
            if (msg.attrs.hasOwnProperty(k)) {
                api.data.cell[ind.y][ind.x][k] = msg.attrs[k];
            }
        }
        HN.Util.dispatch(api.options.cellChanged, [api.path, ind.y, ind.x]);
    };

    api.handle_change = function (msg) {
        this["change_" + msg.reftype](msg);
        api.update_max(msg);
    };

    api.handle_delete = function (msg) {
        this["delete_" + msg.reftype](msg);
        api.update_max(msg);
    };

    api.handle_delete_attrs = function(msg) {
        var ind = HN.Util.parse_ref(msg.ref);
        for (a in msg.attrs) {
            if (msg.attrs.hasOwnProperty(a)) {
                delete api.data.cell[ind.y][ind.x][a];
                HN.Util.dispatch(api.options.cellChanged, [api.path, ind.y, ind.x]);
            }
        }
    };

    api.delete_cell = function (msg) {
        var ind = HN.Util.parse_ref(msg.ref);
        delete api.data.cell[ind.y][ind.x][msg.name];
        HN.Util.dispatch(api.options.cellChanged, [api.path, ind.y, ind.x]);
    };

    api.delete_row = function (msg) {
        delete api.data.row[msg.ref][msg.ref][msg.name];
    };
  
    // This function is to 'poke' data into the main model of the page
    // It is used on updates to a cell to push the 'new' value 
    // into the model and stop any display flicker
    api.pokeCellValue = function (cell, value) {
        api.ensure({
            "reftype" : "cell", 
            "ref"     : HN.Util.coord_to_ref(cell)
        });
        api.data.cell[cell.y][cell.x].value = value;
    };  
    
    api.pokeRowHeight = function (row, height) {
        api.ensure({
            "reftype" : "row", 
            "ref"     : row
        });
        api.data.row[row][row].height = height;
    };
    
    api.load_data(loaded);
    return api;
};
