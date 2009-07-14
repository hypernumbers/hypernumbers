/**
 * @class DF.Details.Layout
 * Supplies the layout library for different layouts
 */
DF.Details = {};

DF.Details.Layout = function(data)
{
  this.data = data.data;
  this.max = {x:1, y:1};

};

DF.Details.Layout.prototype.get_max = function() {
  return DF.Util.get_max(this);
};

DF.Details.Layout.prototype.write_details = function() {
  var that;
  var max;
  var string;
  var substring;
  var i;
  var element;
  that = this;
  max = that.get_max();
  string = "";
  // stick the last three news items in
  for (i = max.y; i > max.y - 3; i--) {
    substring ="";
    substring += "<p class = \"details_headline\">"
      + DF.Util.get_value(this, [1],[i]) + "</p>";
    substring += "<p class = \"details_author\">"
      + DF.Util.get_value(this, [2],[i]) + "</p>";
    substring += "<p class = \"details_date\">"
      + DF.Util.get_value(this, [3],[i]) + "</p>";
    substring += "<table><tr><td><p class = \"details_text\">"
      + DF.Util.get_value(this, [4],[i]) + "</p></td>";
    substring += "<td width=\"30%\"><p class = \"details_image\">"
      + DF.Util.get_value(this, [5],[i]) + "</p></td></table>";
    string += "<div class=\"detail\">" + substring + "</div>";
  };
  element = HN.Util.id("details");
  element.innerHTML=string;
};