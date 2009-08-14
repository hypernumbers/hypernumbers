HN.Tools.Util.draw();

// First up create the various toolboxes
$(document).ready( function() {

  var fn;
  // Set up the draggable elements

  // First set up the layouts
  $(".layoutsitem").draggable({helper: 'clone'});

  // Now the elements
  $(".elementsitem").draggable({helper: 'clone'});

  // Now the widgets
  $(".widgetsitem").draggable({helper: 'clone'});

  // The bindings table is also draggable (but not droppable...)
  $("#bindings").draggable();

  // Now make the canvas droppable
  $(".canvas").droppable({drop: HN.Tools.Util.drop, greedy: true});

  // Configure the page on click
  // bind the page config box to the canvas item
  fn = function(ev) {
    HN.Tools.Register.show_config("page_sizes");
  };
  $("#canvas").bind("click", fn);

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
