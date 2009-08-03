/**
 * @class HN.Tools.templates
 * provides support for the layouts
 */

HN.Tools.Templates = function() {
  var fn;
  var elements;
  var i;
  fn = function(e) {
    console.log("handle clicks on the templates");
    console.log(this);
  };
  // attach the function to all of the template items
  elements = document.getElementsByClassName("templatesitem");
  for (i = 0; i < elements.length; i += 1) {
    //HN.Util.addEvent(elements[i], "click", fn);
  };
};