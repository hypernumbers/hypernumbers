var formulae = new Array();
var parents  = new Array();
var children = new Array();
var css_attrs = ["font-style","font-weight","text-align","text-decoration","color"];

var loc = document.location;
var url = loc.pathname;

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
    build_form();
    init();
    load_data();
 });

build_form = function()
{
            loadRange("/forms/expenses/");
            $("input[type='submit']").click(function()
            {
                post_vars.sort(function(a,b)
                {
                    return a.destination.charCodeAt(0) < 
                        b.destination.charCodeAt(0) ? -1 : 1;
                });
                
                var str = "<create>";

                for(i=0; i< post_vars.length; i++)
                {
                    var getChecked = function(arr)
                    {
                        for(x=0; x< arr.length; x++)
                        {
                            if((arr[x])[0].checked)
                                return arr[x].attr('rawvalue');
                        }
                    }
                
                    var val = (post_vars[i].type != "radio") 
                        ? post_vars[i].input.val()
                        : getChecked(post_vars[i].input);
                        
                    str += "<formula><![CDATA["+val+"]]></formula>";
                }
                str += "</create>";
                
                $.post(url+"?attr&lastrow",str,null,"xml");                
            });
}

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

