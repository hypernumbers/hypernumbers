// a global variable into which we will load a hypernumbers page
var main_layout, details_layout, main_loaded = {}, details_loaded = {};

// A function to check if loading is complete
// this function also puts a fade-out over the page to make the page
// user-accessible only when the loading is completed
var checkloaded = function() {
  if( main_loaded.data && details_loaded.data ) {
    $("#loading, #cover").fadeOut("slow");
  }
};

//
// Functions for main
//

// this function will draw the GUI
var draw_main_gui = function() {
  main_layout.write_item("logo", "<a href=\"/\">" +
                         DF.Util.get_value(data, 4, 4) + "</a>");
  main_layout.write_menu(DF.Util.get_value(data, 4, 5));
  main_layout.write_footer(DF.Util.get_value(data, 4, 6));
  main_layout.write_item("copyright", DF.Util.get_value(data, 4, 7));
  main_layout.write_title(DF.Util.get_value(data, 4, 8));
  main_layout.write_item("bumf", DF.Util.get_value(data, 4, 9 ));
  main_layout.write_breadcrumbs();
};

//
// Functions for details
//

var draw_details_gui = function() {
  details_layout.write_details();
};

//
// Options
//

// A set of options that we are going to pass into the hypernumbers page
// Notice that these are all functions which will be called on certain events
//
// Some of these events (eg dataLoaded, pagesLoaded and functionsLoaded)
// relate to setting up the page on the first time it is viewed.
//
// The others (eg dataReloaded, update, formulaError) are for when
// there are updates caused by the server-side page being updated
var main_page_options = {

  dataLoaded: function() {
    main_loaded.data = true;
    main_layout = new DF.Layout(data);
    draw_main_gui();
    checkloaded();
  },

  pagesLoaded: function() {
    main_loaded.pages = true;
    checkloaded();
  },

  functionsLoaded: function() {
  },

  dataReloaded: function() {
    draw_main_gui();
  },

  update: function() {
    draw_main_gui();
  },

  formulaError: function(index, formula) {
  }
};

var details_page_options = {

  dataLoaded: function() {
    details_loaded.data = true;
    details_layout = new DF.Details.Layout(details);
    draw_details_gui();
    checkloaded();
  },

  pagesLoaded: function() {
    details_loaded.pages = true;
  },

  functionsLoaded: function() {
  },

  dataReloaded: function() {
    draw_details_gui();
  },

  update: function() {
    draw_details_gui();
  },

  formulaError: function(index, formula) {
  }
};


// This is where all the work happens. We declare a new HN.Data object
// and pass in 2 parameters - one describes the page to load
// and one contains the options
var data;
var details;

data = new HN.Data("/", main_page_options);

switch (document.location.pathname)
{
  case "/": details = new HN.Data("/news/", details_page_options);
            break;
  default:  details = new HN.Data(document.location.pathname,
                                  details_page_options);
};
