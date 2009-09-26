var sheet, layout, toolbar, loaded = {};

var checkloaded = function() {
  if( loaded.pages && loaded.data ) {

    toolbar.addPages(data.pages);

    $("#path").text(document.location.pathname);
    document.title = document.location.pathname + " - hypernumbers";

    if( data.data["viewed-tour"] === "false" ) {
      $("#loading").fadeOut("slow");
        toolbar.run_intro();
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

var options = {

  dataLoaded: function() {
    data.load_functions();
    sheet  = new HN.Sheet(data);
    layout = new HN.Layout(sheet);
    toolbar = new HN.ToolBar(layout);
    HN.UI.init_dialogs(layout);
    toolbar.user_navigation(data.data.user);
    loaded.data = true;
    checkloaded();
  },

  pagesLoaded: function() {
    loaded.pages = true;
    checkloaded();
  },

  functionsLoaded: function() {
    toolbar.loadFunctions(data.functions);
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

var data = new HN.Data(document.location.pathname, options);
data.load_pages();
