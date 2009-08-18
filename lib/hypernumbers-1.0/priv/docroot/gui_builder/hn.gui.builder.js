/**
 * @class HN.GUI.Builder
 */

HN.Gui.Builder = function() {
  this.register = new HN.Gui.Register("gui_builder");
};

HN.Gui.Builder.prototype.reregister = function() {
  this.register.abort_updates();
  this.register = new HN.Gui.Register("gui_builder");
};