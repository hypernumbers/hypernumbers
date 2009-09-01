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
            var elements;
            var configid1;
            var layouts;
            var new_id;
            var configid2;
            // var widgets;
            var i;
            elem = HN.Util.id("canvas");
            parent = elem.parentNode;
            parent.innerHTML = data.html;

            // Reregister the hypernumbers so that all the classes we
            //  need to deal with have unique ids
            HNGui.reregister();

            // Now set up the canvas
            HN.Tools.Util.set_up_canvas();

            // and all the things on it
            // first up make them drag'n'droppable...
            $(".layout_target").droppable({drop: HN.Tools.Util.drop, greedy: true});
            $(".element").draggable();
            $(".layout").draggable();
            $(".widget").draggable();

            // now bind all the double click functions...
            elements = document.getElementsByClassName("element");
            for (i = 0; i < elements.length; i++) {
              configid1 = elements[i].getAttribute("configid");
              HN.Tools.Util.set_up_element(elements[i].id, configid1);
            };

            layouts = document.getElementsByClassName("layout");
            for (i = 0; i < layouts.length; i++) {
              configid2 = layouts[i].getAttribute("configid");
              new_id = HN.Tools.Util.get_new_id();
              layouts[i].setAttribute("id", new_id);
              HN.Tools.Util.set_up_layout(new_id, configid2);
            };

            // widgets = document.getElementsByClassName("widget");
            // console.log(widgets);

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