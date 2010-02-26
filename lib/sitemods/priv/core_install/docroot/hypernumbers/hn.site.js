// TODO: Get rid of globals, expose one for use through at the shell
HN.Site = function() {
    
    var public = {};
    
    public.hashUrl = null;
    public.finder  = null;
    public.logger  = new HN.Logger();
    
    init();
    
    public.currentPath = function() {
        return public.hashUrl.getParam("path") || document.location.pathname;
    };

    public.startPoll = function() {
        public.hashUrl = new HashUrl({"changed": urlChanged});
    };
    
    function init() {
        public.logger.start();
    };
    
    function urlChanged() {
        
        var view = public.hashUrl.getParam("view");
        var path = public.hashUrl.getParam("path");
        
        if( public.hashUrl.getParam("path") ) { 
            sheetDataLoader(path); 
        }
        
        if( view == "create" && builder.state() != "CREATE" ) {

            if( builder.state() == "RENDER" ) {
                $("#mainviewbuilder").hide();
                builder.saveCurrentView(true, null);
            }

            $("#viewbuilderintro, #closeviewbtn").show();

            builder.viewCreate();
            layout.state = layout.states.MOST_VIEWBUILDER;
            layout.gridResize();

        } else if( view ) {

            var tmp      = view.split(":");
            var url      = tmp[0];
            var filepath = tmp[1];

            if( !builder.isLoaded(url, filepath) ) {   

                if( builder.state() == "RENDER" ) {
                    builder.saveCurrentView(true, null);
                }

                $("#mainviewbuilder, #closeviewbtn").show();

                builder.loadTplFile(url, filepath);
                layout.state = layout.states.MOST_VIEWBUILDER;
                layout.gridResize();
            }
        } else {

            if( builder.state() == "RENDER" ) {
                builder.saveCurrentView(true, null);
            }
            builder.close();

            $("#viewbuilderintro, #mainviewbuilder, #closeviewbtn").hide();
            layout.state = layout.states.FULL_SPREADSHEET;
            layout.gridResize();
        }
    };
    
    return public;
};

var hn = new HN.Site();
var sheets = [], layout = null, toolbar = null;

var path  = document.location.pathname;

var initialLoad = function(initPath) {

    layout  = new HN.Layout(sheets[initPath]);
    toolbar = new HN.ToolBar(layout);
    builder = new HN.Builder(data, layout);
    
    var lang = sheets[initPath].key("lang");
    data.loadOpaque("/hypernumbers/fns_"+lang+".json", "functions");
    
    HN.UI.init(layout);
    
    var page = data.getPage(initPath);    
    toolbar.user_navigation(page.user);
    
    $("#loading, #cover").fadeOut("slow");
};

var showError = function(ind, original) {
    
    var sheet = this.layout.currentSheet();
    var tab   = this.layout.tabUI();
    var cell  = tab.currentSelectedCell();

    HN.UI.open_dialog(layout, "error");

    $("#errorform").unbind().bind("submit", function(e) {
        e.preventDefault();
        HN.UI.close_dialog(layout, "error");
        // TODO : eugh, leaky
        tab.set_cell(ind.y, ind.x);
        tab.bounds = {"x1":ind.x, "x2":ind.x, "y1":ind.y, "y2":ind.y};
        tab.show_selection();
        tab.c.start_edit(original);
        tab.state = HN.States.EDIT_FULL_CELL;
        return false;
    });
};

var makeTabs = function() {    
    $("#tabs").empty();
    $.each(data.getLoadedUrls(), function() {
        $("<a href='"+this+"'>"+this+"</a>").click( function(e) {            
            window.location.hash = 
                hn.hashUrl.setParam("path", $(this).attr("href"));
            return false;
        }).appendTo($("#tabs"));
    });
};

var updateTabs = function() {
    makeTabs();
    $("#tabs a").removeClass("current");
    $("#tabs a[href="+hn.currentPath(HN.url)+"]").addClass("current");
};

var updateViewMenu = function() { 

    $("#existingviews a[name=view]").remove();
    $.each(data.key(path, "views"), function() {
        if( this.match("/core/") ) {
            return;
        }
        var name     = this.replace(".tpl", "");
        var fullpath = path+":"+name;
        var $item    = $("<li><a name='view' data-path='"+fullpath+"' "
                         +"date-type='view'>"+name+"</a></li>");
        $("#existingviews").append($item);
    });    
};


var sheetDataLoader = function(sheetPath) {
    
    if( typeof sheets[sheetPath] == "undefined" ) {
        return data.addPage(sheetPath);
    }

    updateTabs();
    
    if( sheetPath == hn.currentPath(HN.url) ) {
        
        path = sheetPath;
        document.title = path + " - hypernumbers";

        $("#currentViewPath").text(path);
        
        updateViewMenu();        
        sheets[path].calc_size();
        layout.switchTab(sheets[path]);
        
        // layout.gridResize();
        // layout.selection.show_selection();

        HN.Util.id("copied").style.display = "none"; 
            
        if( hn.finder ) {
            hn.finder.path = path.split("/");
            hn.finder.updateBar();
        }        
    }

    if( builder.isRendering() ) {
        builder.render();
    }
};

var hasLoaded = false;
var options = {

    "dataLoaded": function(path) {
        
        sheets[path] = new HN.Sheet(path, data);
        
        if( !hasLoaded ) {
            hn.startPoll();
            initialLoad(path);
            hasLoaded = true;
        }
        
        sheetDataLoader(path);
    },
    
    "opaqueLoaded": function(name, data) {

        if( name == "pages" ) {
            
            var opts = {

                "activate"     : function() { 
                    $("#finder").show(); 
                },
                "deactivate"   : function() { 
                    $("#finder").hide(); 
                },
                "itemSelected" : function() { 
                    $("#newpage").val(this.asString()); 
                },
                "itemChosen"   : function() {
                    var newPath = HN.Util.correctPath( $("#newpage").val() );
                    window.location.hash = hn.hashUrl.setParam("path", newPath);
                    this.deactivate();
                }
            };

            hn.finder = new Finder($("#finder").eq(0), data, opts);

            $("#pdclose").bind("click", hn.finder.deactivate);
            $("#pdvisit").bind("click", function() {
                hn.finder.chooseCurrent();
                hn.finder.deactivate();
            });
            
            
        } else if( name == "functions" ) {
            toolbar.loadFunctions(data);
        }
    },
    
    "dataReloaded": function(data) {
        data.path = path;
        sheets[path].reload_data(data);
        sheets[path].calc_size();
        sheets[path].calc_size();
        layout.dataUpdated();
    },
    
    "update": function() {
        sheets[path].calc_size();
        layout.dataUpdated();
        if( builder.isRendering() ) {
            builder.render();
        }
    },

    "formulaError": function(index, formula) {
        showError(index, formula);
    }
};



if( $.browser.msie || $.browser.opera ) {
    $("#loading").html(
        "<strong>Error: </strong>Sorry, your browser is "
            +"not supported yet.<br />Please try <a href='http://firefox.com'>"
            +"Firefox</a>, <a href='http://www.apple.com/safari/'>Safari</a>"
            +" or <a href='http://www.google.com/chrome'>Chrome</a>");
} else {
    var data = new HN.Data(options);
    data.addPage(path);
    data.loadOpaque(document.location.pathname, "pages");
}