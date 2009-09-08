/*
 * @class HN.Tools.Widgets.Simple is the simple layout class
 * This class is the prototype for more simple layouts to be added to the
 * GUI Builder
 *
 * These layouts work by first initialising themselves and then,
 * finally, loading themselves into the toolbox register
 */

// Set up a constructor - bit of overkill for this simple example
HN.Tools.Widgets.Simple = function() {
  var widgets = {};
  var c1;
  var w1;

  // First define the config boxes
  c1 = "<div id=\"simpleconfig1\" class=\"standardconfig\">"
    + "<p class=\"toolstitle\">Login</p><hr />"
    + "The Login Widget is not configurable."
    + "</div>";

  w1 = {
         toolboxlable:  "Login",
         classname:     "simple_login",
         htmlfn:        function() {return HN.Tools.Widgets.Simple.make_login();},
         configbox:     c1,
         configboxfn:   HN.Tools.Widgets.Simple.configure1,
         aftercreatefn: HN.Tools.Widgets.Simple.afterloginfn(),
         helptext:      "This is a box that lets the user login..."
  };

  // first up define the layouts
  widgets =
    {
      simple1: w1
    };
  return widgets;
};

//
// Dialog Box functions
//

HN.Tools.Widgets.Simple.configure1 = function()
{
  //console.log("in simple.configure1...");
};

HN.Tools.Widgets.Simple.make_login = function() {
  var elem;
  var html;
  elem = document.createElement("div");
  html = "<div id = \"loggedout\"><a href=\"/_user/login/\">Login</a></div>"
    + "<div id = \"loggedin\">Logged in as <a id=\"home\"></a>"
    + "<span class=\"spacer\"> </span><a id=\"logout\">(logout)</a></div>";
  elem.innerHTML = html;
  elem.setAttribute("class", "widget");
  return elem;
};

//
// After Create Fns
// Called after an instance is created...
//
HN.Tools.Widgets.Simple.afterloginfn = function () {
  return function() {
    var page;
    var user;
    console.log("HN.Tools.Widgets.Simple.afterloginfn called...");
    // Each page has the user stored under it
    // The user should be the same for all pages
    // Get the user of the 0th page - the page that the
    // Gui Builder is being displayed on at the mo...
    page = document.location.protocol + "//"
      + document.location.host
      + document.location.pathname;
    user = HN.Tools.Util.get_user();
    // there is a bug here - the afterloginfn is called before the
    // reregister function has completed... bodging it...
    if (user && user !== "anonymous") {
      HN.Util.id("loggedout").style.display = "none";
      HN.Util.id("loggedin").style.display = "block";
      HN.Util.id("home").innerHTML = user;
      HN.Util.id("home").setAttribute("href", "/u/"+user+"/");

      HN.Util.addEvent(HN.Util.id("logout"), "mousedown", function(e) {
        HN.Util.eraseCookie("auth");
        window.location.reload( true );
        });
    } else {
      HN.Util.id("loggedin").style.display = "none";
      HN.Util.id("loggedout").style.display = "block";
    }
  };
};

// Now insert into the toolbox register
// The 'toolbox' name is the name that will appear in the toolbox
// the 'classname' is the clasname that will be applied to the toolbox
// item
// ** IT IS THE LAYOUT CREATORS RESPONSIBILITY TO CREATE A STYLESHEET
//    FOR THEIR WIDGET
// Usually this just involves adding an icon for the toolbox
var simple = new HN.Tools.Widgets.Simple();
HN.Tools.Util.new_widget(simple['simple1']);
