var formulae = new Array();
var parents  = new Array();
var children = new Array();
var css_attrs = ["font-style","font-weight","text-align","text-decoration","color"];

var loc = document.location;
console.log(loc);
var url = loc.pathname;
console.log(url);

var auth = false;

var handle_attr = function(refxml)
{
    var el = $(refxml);
    
    var ref   = el.attr("ref");
    var name  = ((el.children()[0]).nodeName).toLowerCase();
    var value = el.text();
    
  if (name == "value")
    {
	 $("#"+ref).text(value);	 
    }
}
    

var init = function()
{
  console.log("in expensesscript init");
    var so = new SWFObject("/swf/JsSocket.swf", 
			   "mymovie", 
			   "470", 
			   "200", 
			   "8", 
			   "#FFFFFF");
    so.addParam("allowScriptAccess", "always");
    so.write("flashcontent");    
};

$(function()
{
    // reuse this script in different places
    init();
    load_data();
 });

load_data = function()
{
    
    var get_attr = function(data) 
    {
	var fun = function(y) { handle_attr(this); };
	$(data).find("ref").each(fun);
    }    
     
    $.ajax({
	type: "GET",
	url: url+"?attr",
	dataType: "xml",
	success: get_attr
    });
}

