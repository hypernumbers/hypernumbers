// a global variable into which we will load a hypernumbers page
var loaded = {};

// A function to check if loading is complete
// this function also puts a fade-out over the page to make the page
// user-accessible only when the loading is completed
var checkloaded = function() {
  if( loaded.data ) {

    $("#path").text(document.location.pathname);
    document.title = document.location.pathname + " - hypernumbers Blank GUI";

    $("#loading, #cover").fadeOut("slow");
  }
};

// this function will draw the GUI
var draw_gui = function() {
  console.log("in draw_gui");
  var i;
  var j;
  var id;
  var element;
  var string;
  for (i = 1; i < 6; i++) {
      if (data.data.cell[i] !== undefined){
        for (j = 1; j < 6; j++) {
          if (data.data.cell[i][j] !== undefined){
            id = HN.Util.to_b26(j)+i;
            element = HN.Util.id(id);
            string = "<p>formula: " + data.data.cell[i][j].formula + "</p>";
            string = string + "<p>rawvalue: " + data.data.cell[i][j].rawvalue + "</p>";
            string = string + "<p>value: " + data.data.cell[i][j].value + "</p>";
            string = string + "<p>style: " + data.data.cell[i][j].style + "</p>";
            element.innerHTML=string;
          };
        }
      }
    }
};

// A set of options that we are going to pass into the hypernumbers page
// Notice that these are all functions which will be called on certain events
//
// Some of these events (eg dataLoaded, pagesLoaded and functionsLoaded)
// relate to setting up the page on the first time it is viewed.
//
// The others (eg dataReloaded, update, formulaError) are for when
// there are updates caused by the server-side page being updated
var options = {

  dataLoaded: function() {
    loaded.data = true;
    checkloaded();
  },

  pagesLoaded: function() {
    loaded.pages = true;
    checkloaded();
  },

  functionsLoaded: function() {
  },

  dataReloaded: function() {
  },

  update: function() {
    draw_gui();
  },

  formulaError: function(index, formula) {
  }
};

// This is where all the work happens. We declare a new HN.Data object
// and pass in 2 parameters - one describes the page to load
// and one contains the options
var data = new HN.Data(document.location.pathname, options);
