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
  $.ajax({url:      document.location.pathname + "?get_gui=" + name,
          dataType: "json",
          success:  function(data) {
            var elem;
            var parent;
            var items;
            var i;
            elem = HN.Util.id("canvas");
            parent = elem.parentNode;
            parent.innerHTML = data.html;

            // Reregister the hypernumbers so that all the classes we
            // need to deal with have unique ids
            HNGui.reregister();

            // Now set up the elements and stuff
            items = $(".element");
            for (i = 0; i < items.length; i++) {
              HN.Tools.Util.set_up_element(items[i].id,
                                           items[i].getAttribute("configid"));
            };

            items = $(".layout");
            for (i = 0; i < items.length; i++) {
              HN.Tools.Util.set_up_layout(items[i].id,
                                           items[i].getAttribute("configid"));
            };

            items = $(".widget");
            for (i = 0; i < items.length; i++) {
              HN.Tools.Util.set_up_widget(items[i].id,
                                           items[i].getAttribute("configid"));
            };

            // Need to deselect the last element...
            HN.Tools.Util.deselect_everything();

            // Now set up the canvas
            HN.Tools.Util.set_up_canvas();

            // and all the things on it
            // first up make them drag'n'droppable...
            $(".layout_target").droppable({drop: HN.Tools.Util.drop, greedy: true});
            $(".element").draggable();
            $(".layout").draggable();
            $(".widget").draggable();

            // get the gridlines up
            HN.Tools.Util.toggle_gridlines();
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