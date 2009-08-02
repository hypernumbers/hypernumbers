var dialog_open = function(dialog) {
  $(dialog).css("margin-left", -($(dialog).width()/2));
  $(dialog).css("margin-top", -($(dialog).height()/2));
  $(dialog).fadeIn("fast");
  $("#cover").fadeIn("fast");
  //$(dialog).find("input").eq(0).focus();
};

var show_help = function() {

  dialog_open(HN.Util.id("helpdialog"));

  $("#errorform").unbind().bind("submit", function(e) {
    dialog_close(error);
    e.preventDefault();
    return false;
  });
};

$(document).ready( function() {

  $(".canvas").contextMenu({menu: 'BuilderMenu', leftButton: true},
	function(action, el, pos) {
      console.log(action);
      console.log(el);
      console.log(pos);
      switch (action) {
        case "help":
          show_help();
          break;
        default:
          console.log("Do something for " + action);
        }
	});
  });

$("#cover").fadeOut("fast");
var HNGui = new HN.Gui.Layout();
