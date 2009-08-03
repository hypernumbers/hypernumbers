// These are some util functions that should be hidden away
var close_help_dialog = function ()
{
  $("#cover").fadeOut("fast");
  $("#helpdialog").fadeOut("fast");
};

var close_page_size_dialog = function ()
{
  $("#cover").fadeOut("fast");
  $("#pagesizedialog").fadeOut("fast");
};

var dialog_open = function(dialog) {
  $(dialog).css("margin-left", -($(dialog).width()/2));
  $(dialog).css("margin-top", -($(dialog).height()/2));
  $(dialog).fadeIn("fast");
  $("#cover").fadeIn("fast");
};

var show_dialog = function(dialog) {
  dialog_open(HN.Util.id(dialog));
};

var show = function(id)
{
  var element;
  element = HN.Util.id(id);
  element.style.display ="block";
};

var hide = function(id)
{
  var element;
  element = HN.Util.id(id);
  element.style.display ="none";
};

$(document).ready( function() {
  //
  // Set up the context menu
  //
  $(".canvas").contextMenu({menu: 'builderMenu', leftButton: true},
    function(action, el, pos) {
    var element;
    element = HN.Util.id("canvas2");
    element.innerHTML="";
    switch (action) {
      case "help":
        show_dialog("helpdialog");
        break;
      case "resizecanvas":
        show_dialog("pagesizedialog");
        break;
      case "insertlayout":
        show("templates");
        break;
      case "insertitem":
        show("tools");
        break;
      case "publish":
        console.log("publish me, ya bas!");
        break;
      }
	});

    //
    // Set up the draggable elements
    //
    $(".templatesitem").draggable({helper: 'clone'});
    $(".canvas").droppable({drop: function()
                            {
                              console.log("dropped!");
                            }
    });
});


var Tools = new HN.Tools();
var HNGui = new HN.Gui.Layout();
