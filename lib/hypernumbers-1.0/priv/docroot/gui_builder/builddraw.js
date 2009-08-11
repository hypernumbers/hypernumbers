HN.Tools.Util.draw();

// First up create the various toolboxes
$(document).ready( function() {

  // Set up the draggable elements

  // First set up the layouts
  $(".layoutsitem").draggable({helper: 'clone'});

  // Now the elements
  $(".elementsitem").draggable({helper: 'clone'});

  // Now the widgets
  $(".widgetsitem").draggable({helper: 'clone'});

  // Now make the canvas droppable
  $(".canvas").droppable({drop: HN.Tools.Util.drop, greedy: true});

  // Make the toolboxes appear
  $(".box").show();

  // Add a keydown event which is captured at the document level...
  $(document).keypress(HN.Tools.Util.keypress);

  // Finally remove the cover
  $("#cover").fadeOut("fast");
});
