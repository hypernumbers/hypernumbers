var sheet, layout, toolbar,

logger   = new HN.Logger(),
path     = document.location.pathname;

logger.start();

var loaded = function() {

    data.loadOpaque("/hypernumbers/fns_"+sheet.data.lang+".json", "functions");
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
    
    if( page["viewed-tour"] === "false" ) {
        $("#loading").fadeOut("slow");
        HN.UIActions.open_dialog(this.layout, "introduction");
    } else {
        $("#loading, #cover").fadeOut("slow");
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

var Navigation = function(finder) {

    function select(path) {
        if( !finder.active ) {
            finder.select(path);
            finder.activate();
        } else { 
            finder.select(path);
        }
    };

    function deselect() {
        finder.deactivate();
    };

    var path = ["home"];
    $.each(document.location.pathname.split("/"), function() {
        if( this != "" ) {
            path.push(this);
            var url = path.join("/");
            $("#pagebar").append("<div class='crumb'></div>"
                                 +"<a data-href='"+url+"'>"+this+"</a>");
        }
    });

    $("#pagebar").bind("click", function(e) {
        if( e.target.nodeName == "A" ) {
            select(e.target.getAttribute("data-href").split("/"));
        }
    });

    $("#pdclose").bind("click", deselect);
    $("#pdvisit").bind("click", function() { 
        document.location.href = "/"
            +finder.path.slice(1).join("/")+"/";
    });
};


var options = {

    "dataLoaded": function() {

        sheet   = new HN.Sheet(data.getPage(path));
        layout  = new HN.Layout(sheet);
        toolbar = new HN.ToolBar(layout);
        intro   = new Intro();

        loaded();
    },

    "opaqueLoaded": function(name, data) {
        if( name == "pages" ) {

            var opts = {
                "activate"   : function() { $("#finder").show(); },
                "deactivate" : function() { $("#finder").hide(); },
                "itemChosen" : function() {
                    document.location.href = "/"
                        +this.path.slice(1).join("/")+"/";
                }
            };
            var finder = new Finder($("#finder").eq(0), data, opts);
            new Navigation(finder);

        } else if( name == "functions" ) {
            toolbar.loadFunctions(data);
        }
    },
    
    "dataReloaded": function(data) {
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
