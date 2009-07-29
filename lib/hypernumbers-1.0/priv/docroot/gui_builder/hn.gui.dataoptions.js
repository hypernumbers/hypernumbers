HN.Gui.DataOptions = function(p, b) {

  var that = this;
  that.opts = {

    dataLoaded: function() {
      that.opts.redraw("dataLoaded");
    },

    pagesLoaded: function() {
      that.opts.redraw("pagesLoaded");
    },

    functionsLoaded: function() {
      that.opts.redraw("functionsLoaded");
    },

    dataReloaded: function() {
      that.otps.redraw("dataReloaded");
    },

    update: function() {
      that.opts.redraw("update");
    },

    formulaError: function(index, formula) {
      that.opts.redraw("formulaError");
    },

    redraw: function(args) {
      console.log("redraw called: " + args);
    },

    // Now add some data...

    bindings: b,

    pages: p

  };
  return that.opts;
};