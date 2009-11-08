var sheet, layout, toolbar,

pagemenu = HN.PageMenu(),
logger   = new HN.Logger(),
loaded   = {},
into     = null,
path     = document.location.pathname;

logger.start();

var checkloaded = function() {

    if( loaded.pages && loaded.data ) {
        
        var page    = data.getPage(path),
        permissions = page.permissions,
        readonly    = $.inArray("write", permissions) == -1,
        title       = document.location.pathname + " - hypernumbers";

        document.title = title;
        
        if( readonly ) {
            $("#toolbar").addClass("disabled");
            $("#permissions").text("[readonly]");
            title = "[readonly] "+title;
        }

        if( page["viewed-tour"] === "false" ) {
            $("#loading").fadeOut("slow");
            HN.UIActions.open_dialog(this.layout, "introduction");
        } else {
            $("#loading, #cover").fadeOut("slow");
        }
    }
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


var Intro = function() {

    this.current_page = 1;
    this.pages = $("#introwrapper").children().length;

    var t = this;

    $("#intronext").click( function() {
        var id = ( t.current_page == t.pages )
            ? 1 : t.current_page+1;
        t.show_intro_page(id);
    });

    $("#introback").click( function() {
        var id = ( t.current_page == 1 )
            ? t.pages : t.current_page-1;
        t.show_intro_page(id);
    });

    this.show_intro_page = function(x) {
        this.current_page = x;
        HN.Util.id("intropager").innerHTML = "Page "+this.current_page
            +" of "+this.pages+" Pages";
        $("#introwrapper").children().hide();
        $("#introwrapper").children().eq(x-1).show();
    };

    this.show_intro_page(1);
};

var options = {

    dataLoaded: function() {
//        data.pages[path].load_functions();
        sheet   = new HN.Sheet(data);
        layout  = new HN.Layout(sheet);
        toolbar = new HN.ToolBar(layout);
        intro   = new Intro();

        HN.UI.init_dialogs(layout);
        toolbar.user_navigation(data.getPage(path).user);
        loaded.data = true;

        checkloaded();
    },

    opaqueLoaded: function(name, data) {
        if( name == "pages" ) {
            loaded.pages = true;
            pagemenu.populate(document.location.pathname, data);
            checkloaded();
        }
    },

    functionsLoaded: function() {
        toolbar.loadFunctions(data.pages[path].functions);
    },

    dataReloaded: function(data) {
        sheet.reload_data(data);
        sheet.calc_size();
        layout.calcScrollbarSize(Y);
        layout.calcScrollbarSize(X);
        layout.panes.refresh();
        layout.selection.show_selection();
    },

    update: function() {
        sheet.calc_size();
        layout.panes.refresh();
        layout.selection.show_selection();
    },

    formulaError: function(index, formula) {
        show_error(index, formula);
    }
};

var data = new HN.Data(options);
data.addPage(path);
data.loadOpaque(document.location.pathname, "pages");
