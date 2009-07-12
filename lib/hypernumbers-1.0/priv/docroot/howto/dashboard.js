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

var get_max = function() {
  var max;
  max = 5;
  for( var i in data.data.cell ) {
      if( parseInt(i) > max ) {
        max = parseInt(i);
      }
  }
  return max;
};

var get_td = function(indicator) {
  switch (indicator) {
    case "red":       return "<td><div class=\""+indicator+"\"></td>";
    case "amber":     return "<td><div class=\""+indicator+"\"></td>";
    case "green":     return "<td><div class=\""+indicator+"\"></td>";
    case "undefined": return "<td><div class=\""+indicator+"\"></td>";
    case "error":     return "<td><div class=\""+indicator+"\"></td>";
    default:          return "<td><div class=\"error\"></td>";
  }
};

// this function will write the title
var write_title = function() {
  if (data.data.cell[2] && data.data.cell[2][5] &&
      (data.data.cell[2][5].value !== undefined)) {
    var element;
    element = HN.Util.id("title");
    element.innerHTML=data.data.cell[2][5].value;
  }
};

var write_dashboard = function() {
  var i;
  var id;
  var element;
  var string;
  var max;
  // now start building the string
  string ="<table>";
  max = get_max();
  for (i = 5; i < max + 1; i++) {
    if (data.data.cell[i] && data.data.cell[i][4] &&
        (data.data.cell[i][4].value !== undefined)) {
      string = string + "<tr><td class=\"lable\">"+data.data.cell[i][4].value + "</td>";
      if (data.data.cell[i][5]) {
          if (data.data.cell[i][5].value === undefined) {
            string = string + get_td("error");
          } else {
            string = string + get_td(data.data.cell[i][5].value) + "</tr>";
         }
        } else {
           string = string + get_td("undefined");
         }
      }
  }
  string = string + "</table>";
  element = HN.Util.id("dashboard");
  element.innerHTML=string;
};

// this function will draw the GUI
var draw_gui = function() {
  write_title();
  write_dashboard();
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
    draw_gui();
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
