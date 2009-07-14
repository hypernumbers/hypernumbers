/**
 * @class DF.Layout
 * Supplies the layout library for different layouts
 */
DF.Layout = function(data)
{
  this.data = data.data;
  this.max = {x:1, y:1};
};

DF.Layout.prototype.get_max = function() {
  return DF.Util.get_max(this);
};

DF.Layout.prototype.write_item = function(id, value) {
  var element;
  if (typeof id === 'string' && value) {
    element = HN.Util.id(id);
    element.innerHTML=value;
  }
};

DF.Layout.prototype.write_menu = function(value) {
  var items;
  var i;
  var acc;
  var proper;
  items = value.split(";");
  acc = "";
  for (i = 0; i < items.length; i++) {
    // dont' make blank elements into menu items
    if (items[i] !== "") {
      proper = DF.Util.make_proper(items[i]);
      acc +=  "<li><a href=\"" + proper + "\">"
        + items[i] + "</a></li>";
    }
  }
   this.write_item("menucontainer", "<ul id=\"menu\">"
                   + "<li><a href=\"/\">home</a></li> " + acc + "</ul>");
};

DF.Layout.prototype.write_footer = function(value) {
  var items;
  var i;
  var acc;
  var proper;
  items = value.split(";");
  acc = "";
  for (i = 0; i < items.length; i++) {
    // dont' make blank elements into footer items
    if (items[i] !== "") {
      proper = DF.Util.make_proper(items[i]);
      acc +=  "<a href=\"" + proper + "\">"
        + items[i] + "</a>";
    }
  }
  this.write_item("footercontainer", "<span id =\"footer\">"
                    + acc + "</span>");
};

DF.Layout.prototype.write_title = function(value) {
  document.title = value;
};

DF.Layout.prototype.write_breadcrumbs = function() {
  var crumbs;
  var length;
  var path;
  var i;
  var acc;
  crumbs = document.location.pathname.split("/");
  length = crumbs.length;
  path = "";
  acc = "";
  this.write_item("breadcrumbs", "");
  for (i = 0; i < length - 1; i++) {
    path += crumbs[i] + "/";
    acc += "<li><a href=\"" + path + "\">" + crumbs[i] + "</a></li>";
  };
  this.write_item("breadcrumbs", "<ul>"
                    + "<li><a href=\"/\">home</a></li> "  + acc + "</ul>");
};

