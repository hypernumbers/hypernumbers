//
//  SpreadSheet Container
//
HN.SpreadSheet = function(root)
{
  this.root   = HN.Util.id(root);
  this.sheet  = 0;
  this.sheets = [];
  this.user   = null;
  this.layout = null;

  this.add_sheet(document.location.pathname+"?attr");
  this.layout = new HN.Layout(this,name,this.cur_sheet());
};

HN.SpreadSheet.prototype.do_auth = function()
{
  var user = this.cur_sheet().data.user;
  this.user = user;

  if( user !== "anonymous" ) {
    var el = HN.Util.id("logout");
    HN.Util.id("loggedin").style.display = "block";
    HN.Util.id("home").innerHTML = user;
    HN.Util.id("home").setAttribute("href", "/u/"+user+"/");
    HN.Util.addEvent(el, "mousedown", function(e) {
      HN.Util.eraseCookie("auth");
      window.location.reload( true );
    });
    HN.Util.addEvent(HN.Util.id("lang"), "mousedown", function(e) {
      var el = e.target.nodeName == "A" ? e.target : e.target.parentNode;
      HN.Callbacks.setLanguage(el.getAttribute("name"));
    });
  } else {
    HN.Util.id("anonymous").style.display = "block";
  }
};

HN.SpreadSheet.prototype.cur_sheet = function()
{
  return this.sheets[this.sheet];
};

HN.SpreadSheet.prototype.add_sheet = function(name)
{
  this.sheets[this.sheets.length] = new HN.Sheet(this, name);
};

