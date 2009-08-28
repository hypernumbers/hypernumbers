/**
 * @class HN.Tools.Toolbar
 * provides functionality for the gui builder toolbar
 */
HN.Tools.ToolBar = function()
{
  var that = this;
};

HN.Tools.ToolBar.prototype.menuItemPicked = function(e) {
  if( typeof e == "undefined" ) {
    return true;
  };

  if( e.parentNode && e.parentNode.className.match("disabled") ) {
    return false;
  }

  var action = e.getAttribute("name");
  if(action === "open") {
    this["menuAction_" + action](e.innerHTML);
  } else if(action !== null && action !== "ignore" && action !== "view") {
    this["menuAction_" + action]();
  }
  return true;
};

HN.Tools.ToolBar.prototype.menuAction_open = function(name) {
  $.ajax({url: document.location.pathname + "?get_gui=" + name,
    dataType: "json",
    success: function(data) {
      var elem;
      var parent;
      elem = HN.Util.id("canvas");
      parent = elem.parentNode;
      parent.innerHTML = data.html;
      // Now set up the canvas
      $(".canvas").droppable({drop: HN.Tools.Util.drop, greedy: true});
      // and all the things on it
      $(".layout_target").droppable({drop: HN.Tools.Util.drop, greedy: true});
      $(".element").draggable();
      $(".layout").draggable();
      $(".widget").draggable();
      HNGui.reregister();
    }
  });
};

HN.Tools.ToolBar.prototype.menuAction_save_as = function() {
  HN.Util.id("save_name").value = "";
  HN.Tools.Util.dialog_open("#savedialog");
};

HN.Tools.ToolBar.prototype.menuAction_help = function() {
  HN.Tools.Util.dialog_open("#helpdialog");
};