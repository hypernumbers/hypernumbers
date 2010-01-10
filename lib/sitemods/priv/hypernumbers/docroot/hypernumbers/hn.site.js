var sheets = [], layout, toolbar, finder;

var logger   = new HN.Logger();
var path     = document.location.pathname;

logger.start();

var loaded = function() {

    data.loadOpaque("/hypernumbers/fns_"+sheets[path].data.data.lang+".json", 
                    "functions");

    HN.UI.init(layout);
    
    var page    = data.getPage(path),
    permissions = page.permissions,
    readonly    = $.inArray("write", permissions) == -1,
    title       = document.location.pathname + " - hypernumbers";
    
    toolbar.user_navigation(page.user);
    document.title = title;
    
    if( readonly ) {
        $("#toolbar").addClass("disabled");
        $("#permissions").text("[readonly]");
        title = "[readonly] "+title;
    }
    
    $("#loading, #cover").fadeOut("slow");
};

var show_error = function(ind, original) {

    var select = layout.selection;
    cell = layout.s.cell(ind.y, ind.x);

    HN.UIActions.open_dialog(layout, "error");

    $("#errorform").unbind().bind("submit", function(e) {
        HN.UIActions.close_dialog(layout, "error");
        select.set_cell(ind.y, ind.x);
        select.bounds = {x1:ind.x, x2:ind.x, y1:ind.y, y2:ind.y};
        select.show_selection();
        select.c.start_edit(original);
        select.state = HN.States.EDIT_FULL_CELL;

        e.preventDefault();
        return false;
    });
};

var addTab = function(path) {
    
    $("<a href='"+path+"'>"+path+"</a>").click( function() { 
        load_sheet(path);
        return false;
    }).appendTo($("#tabs"));
};

var load_sheet = function(url) {
    
    path = url;
    
    if( typeof sheets[path] !== "undefined" ) {
        if( sheets[path] != true ) {

            $("#tabs a").removeClass("current");
            $("#tabs a[href="+path+"]").addClass("current");

            layout.s = sheets[path];
            sheets[path].calc_size();
            layout.calcScrollbarSize(Y);
            layout.calcScrollbarSize(X);            
            layout.panes.refresh();
            layout.selection.show_selection();
            finder.path = path.split("/");
            finder.updateBar();
        }
    } else {
        addTab(path);
        data.addPage(path);
    }
};


var options = {

    "dataLoaded": function() {
        
        toolbar = new HN.ToolBar(layout);
        sheets[path] = new HN.Sheet(data.getPageData(path));
        
        if( !layout ) {            
            layout  = new HN.Layout(sheets[path]);
        } else {
            load_sheet(path);
        }
        
        loaded();
    },
    
    "opaqueLoaded": function(name, data) {
        if( name == "pages" ) {
            
            var opts = {
                "activate"   : function() { $("#finder").show(); },
                "deactivate" : function() { $("#finder").hide(); },
                "itemChosen" : function() {
                    var url = ( this.path.length == 1 )
                                ? "/" : "/"+this.path.slice(1).join("/")+"/";
                    load_sheet(url);
                    this.deactivate();
                }
            };
            finder = new Finder($("#finder").eq(0), data, opts);

            $("#pdclose").bind("click", finder.deactivate);
            $("#pdvisit").bind("click", function() {
                finder.chooseCurrent();
                finder.deactivate();
            });


        } else if( name == "functions" ) {
            toolbar.loadFunctions(data);
        }
    },
    
    "dataReloaded": function(data) {
        data.path = path;
        sheets[path].reload_data(data);
        sheets[path].calc_size();
        layout.calcScrollbarSize(Y);
        layout.calcScrollbarSize(X);
        layout.panes.refresh();
        layout.selection.show_selection();
    },

    "update": function() {
        sheets[path].calc_size();
        layout.panes.refresh();
        layout.selection.show_selection();
    },

    "formulaError": function(index, formula) {
        show_error(index, formula);
    }
};

if( !($.browser.msie || $.browser.opera) ) {
    var data = new HN.Data(options);
    data.addPage(path);
    addTab(path);
    data.loadOpaque(document.location.pathname, "pages");
} else {
    $("#loading").html(
        "<strong>Error: </strong>Sorry, your browser is "
            +"not supported yet.<br />Please try <a href='http://firefox.com'>"
            +"Firefox</a>, <a href='http://www.apple.com/safari/'>Safari</a>"
            +" or <a href='http://www.google.com/chrome'>Chrome</a>");    
}