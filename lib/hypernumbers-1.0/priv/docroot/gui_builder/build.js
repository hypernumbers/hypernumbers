//
// Start of the main....
//
// Now just execute stuff
//
var Tools = new HN.Tools();
var HNGui = new HN.Gui.Layout();
$(document).ready( function() {
  //
  // Set up the context menu
  //
  $(".canvas").contextMenu({menu: 'builderMenu', leftButton: true},
    function(action, el, pos) {
    var element;
    element = HN.Util.id("startmessage");
      if (element) {
        element.parentNode.removeChild(element);
      }
    switch (action) {
      case "help":
        HN.Tools.Util.show_dialog("helpdialog");
        break;
      case "insertlayout":
        HN.Tools.Util.show("templates");
        break;
      case "insertitem":
        HN.Tools.Util.show("templates");
        HN.Tools.Util.show("tools");
        break;
      case "publish":
        console.log("publish me, ya bas!");
        break;
      }
	});

    //
    // Set up the draggable elements
    //
    // First set up the layouts
    $(".templatesitem").draggable({helper: 'clone'});
    //
    // Now the toolbox
    $(".toolsitem").draggable({helper: 'clone'});
    //
    // Now make the canvas droppable
    $(".canvas").droppable({drop: HN.Tools.Util.handle_drop, greedy: true});

    // Finally remove the cover
    $("#cover").fadeOut("fast");
});
