/**
 * @class HN.Tools.Toolbar
 * provides functionality for the gui builder toolbar
 */
HN.Tools.ToolBar = function()
{
  var that = this;
};

HN.Tools.ToolBar.prototype.menuItemPicked = function(e) {
  console.log("menuItemPicked fired...");
  if( typeof e == "undefined" ) {
    return true;
  };

  if( e.parentNode && e.parentNode.className.match("disabled") ) {
    return false;
  }

  var action = e.getAttribute("name");
  if( action !== null ) {
    console.log(action);
    this["menuAction_" + action]();
  }
  return true;
};

HN.Tools.ToolBar.prototype.menuAction_open = function() {
  //console.log("in open...");
};

HN.Tools.ToolBar.prototype.menuAction_save = function() {
  HN.Util.id("save_name").value = "";
  HN.Tools.Util.dialog_open("#savedialog");
};

HN.Tools.ToolBar.prototype.menuAction_help = function() {
  HN.Tools.Util.dialog_open("#helpdialog");
};