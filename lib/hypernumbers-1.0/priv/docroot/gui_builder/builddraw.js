HN.Tools.Util.draw();

// Finally do all the magic...
$(document).ready( function() {

  var fn1;
  var fn2;

  // Now make the page themeable
  $('#switcher').themeswitcher();

  // Set up the draggable elements

  // First set up the layouts
  $(".layoutsitem").draggable({helper: 'clone'});

  // Now the elements
  $(".elementsitem").draggable({helper: 'clone'});

  // Now the widgets
  $(".widgetsitem").draggable({helper: 'clone'});

  // Now set the canvas up
  HN.Tools.Util.set_up_canvas();

  // Now make a click on the body deselect everything
  $("body").bind("dblclick", HN.Tools.Util.deselect_everything);

  // Now get up the gridlines toggler
  $("#grid_lines").bind("click", HN.Tools.Util.toggle_gridlines);

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

  // Now create the config boxes
  Register.configure_for_gui_builder();

  // Now create the new GUI builder
  HNGui = new HN.Gui.Builder();

  // Do the tabs
  Register.tabset = $("#tabs").tabs();

  // Make the toolboxes and tabs appear
  $("#tab_bar").show();
  $(".box").show();

  // Make the floating help box float...
  $("#floatinghelpbox").draggable();
  $(".floating_close").bind("click", HN.Tools.Register.close_help);

  // Add a keydown event which is captured at the document level...
  $(document).keypress(HN.Tools.Util.keypress);

  // Finally remove the cover
  $("#cover").fadeOut("fast");

});
