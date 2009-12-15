var sheet, layout, toolbar,

logger   = new HN.Logger(),
path     = document.location.pathname;

logger.start();

var loaded = function() {

    data.loadOpaque("/hypernumbers/fns_"+sheet.data.data.lang+".json", 
                    "functions");

    HN.UI.init_dialogs(layout);
    
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

var options = {

    "dataLoaded": function() {
        
        sheet   = new HN.Sheet(data.getPageData(path));
        layout  = new HN.Layout(sheet);
        toolbar = new HN.ToolBar(layout);
        
        loaded();
    },
    
    "opaqueLoaded": function(name, data) {
        if( name == "pages" ) {
            
            var opts = {
                "activate"   : function() { $("#finder").show(); },
                "deactivate" : function() { $("#finder").hide(); },
                "itemChosen" : function() {
                    var url = ( this.path.length == 1 ) 
                        ? "/" : "/"+this.path.slice(1).join("/")+"/"
                    document.location.href = url;
                }
            };
            var finder = new Finder($("#finder").eq(0), data, opts);

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
        sheet.reload_data(data);
        sheet.calc_size();
        layout.calcScrollbarSize(Y);
        layout.calcScrollbarSize(X);
        layout.panes.refresh();
        layout.selection.show_selection();
    },

    "update": function() {
        sheet.calc_size();
        layout.panes.refresh();
        layout.selection.show_selection();
    },

    "formulaError": function(index, formula) {
        show_error(index, formula);
    }
};

var data = new HN.Data(options);
data.addPage(path);
data.loadOpaque(document.location.pathname, "pages");
