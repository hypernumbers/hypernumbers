HN.Gui.DataOptions = function(b) {

  var that = this;
  that.data_loaded = false;
  that.opts = {

    // These are the standard call backs that are required
    // by HN.Data
    dataLoaded: function() {
      that.opts.bind_gui();
      that.opts.draw_gui();
      that.data_loaded = true;
    },

    pagesLoaded: function() {
      // do nothing for now...
    },

    functionsLoaded: function() {
      // do nothing for now...
    },

    dataReloaded: function() {
      that.opts.draw_gui();
    },

    update: function() {
      that.opts.draw_gui();
    },

    formulaError: function(index, formula) {
      that.opts.display_error(index, formula);
    },

    // These are the internal functions that are used to service the callbacks

    bind_gui: function() {
      var i;
      var x;
      var arr = [];
      var binds = that.opts.bindings;
      var a;
        for (i = 0; i < binds.length; i += 1) {
          a = new HN.Gui.HTML(binds[i]['id'],
                              binds[i]['componenttype'],
                              binds[i]['to'],
                              binds[i]['from_page'],
                              binds[i]['from_ref'],
                              binds[i]['from_type'],
                              binds[i]['post'],
                              binds[i]['validity'],
                              binds[i]['errors']);
            arr[arr.length] = a;
        };
      that.opts.gui_array = arr;
    },

    draw_gui: function() {
      var i;
      var arr = that.opts.gui_array;
        for (i = 0; i < arr.length; i += 1) {
          arr[i]();
        };
    },

    display_error: function(index, formula) {
      console.log("display_error called with " + index + " for " + formula);
    },

    // Now add some data...
    // The bindings are passed in when this function is called

    bindings: b,

    // This array will be created when the data is loaded
    // for the first time
    gui_array: []

  };
  return that.opts;

};