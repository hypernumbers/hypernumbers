/**
 * @class HN.Tools.Toolbar
 * provides functionality for the gui builder toolbar
 */
HN.Tools.ToolBar = function()
{
  var that = this;
  this.guis = [];
  this.load_guis();
  return this;
};

HN.Tools.ToolBar.prototype.menuItemPicked = function(e) {
  console.log("menu triggered...");
  console.log(e);
  if( typeof e == "undefined" ) {
    return true;
  };

  if( e.parentNode && e.parentNode.className.match("disabled") ) {
    return false;
  }

  var action = e.getAttribute("name");
  if(action === "open") {
    console.log("open selected...");
    this["menuAction_" + action](e.innerHTML);
  } else if(action !== null && action !== "ignore" && action !== "view") {
    console.log(action + " selected...");
    this["menuAction_" + action]();
  }
  return true;
};

HN.Tools.ToolBar.prototype.menuAction_open = function(name) {
  console.log(name);
  $.ajax({url:      document.location.pathname + "?gui=/dogfood2/" + name,
          dataType: "json",
          success:  function(data) {
            // data.replace("<", "&lt;");
            // data.replace(">", "&gt;");
            // console.log(data);
            var elem = document.getElementById("display");
            // console.log(elem);
            elem.value = data;
          }
  });
};

HN.Tools.ToolBar.prototype.menuAction_save_as = function() {
  HN.Util.id("save_name").value = "";
  HN.Tools.Util.dialog_open("#savedialog");
};

HN.Tools.ToolBar.prototype.make_menus = function() {
  var opts;
  // First up make the various menus
  this.make_file_menus();
  // First up make the various menus
  opts = {callback : function(el) {
          return Toolbar.menuItemPicked(el);
          }
  };
  $("#menu").filemenu(opts);

};

HN.Tools.ToolBar.prototype.make_file_menus = function() {
  var elem1;
  var elem2;
  var html1;
  var html2;
  var arrow;
  var i;
  elem1 = HN.Util.id("gui_open");
  elem2 = HN.Util.id("gui_view");
  if (this.guis.length !== 0) {
    html1 = "<ul class=\"parent\">";
    html2 = html1;
    for (index in this.guis) {
      html1 += "<li><a class=\"parent hover\" name=\"open\">"
        + this.guis[index].file + "</a></li>";
      html2 += "<li><a href=\"./?gui=" + this.guis[index].path
        + this.guis[index].file
        + "\"class=\"parent hover\" name=\"view\" target=\"_blank\">"
        + this.guis[index].file + "</a></li>";
    }
    html1 += "</ul>";
    html2 += "</ul>";
    arrow = "<div class=\"arrow\"></div>";
    elem1.innerHTML = "<a name=\"ignore\">" + elem1.childNodes[0].textContent
      + arrow + "</a>" + html1;
    elem2.innerHTML = "<a name=\"ignore\">" + elem2.childNodes[0].textContent
      + arrow + "</a>" + html2;
  } else {
    elem1.innerHTML = "<a name=\"ignore\" class=\"disabled\">("
      + elem1.childNodes[0].text + ")</a>";
    elem2.innerHTML = "<a name=\"ignore\" class=\"disabled\">("
      + elem2.childNodes[0].text + ")</a>";
  }
};


HN.Tools.ToolBar.prototype.load_guis = function() {
  var that = this;
  $.ajax({url: document.location.pathname + "?guis",
          dataType: "json",
          success: function(data) {
            that.guis = data;
            that.make_menus();
            }
         });
};

HN.Tools.ToolBar.prototype.save_page = function() {
  console.log("in save page...");
  var name;
  var newpage;
  var json;
  var page;
  var elem;
  // close the dialog box
  HN.Tools.ToolBar.dialog_close("#savedialog");
  name = HN.Util.id("save_name").value;
  elem = document.getElementById("display");
  newpage = elem.value;
  console.log(newpage);
  json = {save_gui: {name: name,
                     form: newpage}};
  page = document.location.protocol + "//"
    + document.location.host
    + document.location.pathname;
  $.post(page, JSON.stringify(json));
  // Now re-make the menus
  // (might need a small delay...)
  this.load_guis();
};

HN.Tools.ToolBar.dialog_close = function (dialog)
{
  $("#cover").fadeOut("fast");
  $(dialog).fadeOut("fast");
};
