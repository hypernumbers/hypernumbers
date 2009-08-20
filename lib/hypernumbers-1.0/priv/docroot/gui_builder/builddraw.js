HN.Tools.Util.draw();

// First up create the various toolboxes
$(document).ready( function() {

  var fn;
  // make menus work
  var opts = {
    callback : function(el) {
      return Toolbar.menuItemPicked(el);
    }
  };
  $("#menu").filemenu(opts);

  // Set up the draggable elements

  // First set up the layouts
  $(".layoutsitem").draggable({helper: 'clone'});

  // Now the elements
  $(".elementsitem").draggable({helper: 'clone'});

  // Now the widgets
  $(".widgetsitem").draggable({helper: 'clone'});

  // Now make the canvas droppable
  $(".canvas").droppable({drop: HN.Tools.Util.drop, greedy: true});

  // Configure the page on click
  // bind the page config box to the canvas item
  fn = function(ev) {
    Register.tabset.tabs('select', 1);
    HN.Tools.Register.selected("canvas");
    HN.Tools.Register.show_config("page_sizes");
  };
  $("#canvas").bind("click", fn);

  var make_draggable = function() {
    $("#bindings").draggable();
  };
  var make_undraggable = function() {
    $("#bindings").draggable('destroy');
  };

  // Configure the bindings preview
  $("#bindings_header").bind("mousedown", make_draggable);
  $("#bindings_header").bind("mouseup", make_undraggable);
  $(".bindings_minimize").bind("click",
                              HN.Tools.Elements.Core.minimize_bindings_table);
  $(".bindings_close").bind("click",
                            HN.Tools.Elements.Core.hide_bindings_table);

  // Do the tabs
  Register.tabset = $("#tabs").tabs();

  // Make the toolboxes and tabs appear
  $("#tab_bar").show();
  $(".box").show();

  // Add a keydown event which is captured at the document level...
  $(document).keypress(HN.Tools.Util.keypress);

  // Finally remove the cover
  $("#cover").fadeOut("fast");

});
